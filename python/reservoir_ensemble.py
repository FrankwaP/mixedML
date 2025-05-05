# -*- coding: utf-8 -*-
from typing import Literal, Optional, Any, Union

from joblib import Parallel, delayed
from numpy import mean, median, ndarray
from reservoirpy import Model, Node  # type: ignore
from reservoirpy.nodes import ESN
from reservoirpy import verbosity
from reservoirpy.type import Data

verbosity(0)


def _remove_copy_suffix(obj: Union[Model, Node]) -> None:
    copysuffix = "-(copy)"
    lcopysuffix = len(copysuffix)
    name = obj.name
    if name.endswith(copysuffix):
        obj._name = name[:-lcopysuffix]


def _fix_copy_name(model: Model):
    _remove_copy_suffix(model)
    for nname in model.node_names:
        node = model.get_node(nname)
        _remove_copy_suffix(node)


def _predict_single(
    model: Model, X: Data, predict_controls: dict[str, Any]
) -> Data:
    _fix_copy_name(model)
    return model.run(X, **predict_controls)


def _fit_single(
    model: Model, X: Data, y: Data, fit_controls: dict[str, Any]
) -> Model:
    _fix_copy_name(model)
    model.fit(X, y, **fit_controls)
    return model


def get_esn_ensemble(
    esn_controls: [str, Any],
    seed_list: list[int],
    agg_func: Literal["mean", "median"],
    n_procs: Optional[int] = None,
):
    return ReservoirEnsemble(
        model_list=[ESN(**dict(**esn_controls, seed=s)) for s in seed_list],
        agg_func=agg_func,
        n_procs=n_procs,
    )


class ReservoirEnsemble:
    # VotingRegressor has been considered, but:
    #  1. reservoirs uses "run" instead of "predict"
    #  2. reservoirs uses options for "fit" (like warmups)
    # … so at least here we're sure it's controlled

    def __init__(
        self,
        model_list: list[Model],
        agg_func: Literal["mean", "median"],
        n_procs: Optional[int] = None,
    ):
        self.agg_func = self._get_agg_func(agg_func)
        self.model_list = model_list
        self._model_names = [m.name for m in model_list]
        self._nodes_names = [m.node_names for m in model_list]

        if n_procs:
            self._nprocs = min(n_procs, len(model_list))
        else:
            self._nprocs = len(model_list)

        self.pool_open()

    def pool_open(self):
        # https://joblib.readthedocs.io/en/stable/parallel.html
        # backend="loky" does not work when using reticulate
        # it seems quite hard to understand why, and the fact that another
        # backend works suggest that it is not a coding problem
        self._pool = Parallel(n_jobs=self._nprocs, backend="multiprocessing")

    def pool_close(self):
        self._pool.close()

    @staticmethod
    def _get_agg_func(agg_func):
        agg_funcs = {
            "mean": lambda x: mean(x, axis=0),
            "median": lambda x: median(x, axis=0),
        }

        try:
            return agg_funcs[agg_func]
        except KeyError:
            raise ValueError(f"agg_func must be one of {agg_funcs.keys()}")

    def _fix_copy_names(self):
        for model in self.model_list:
            _fix_copy_name(model)

    def fit(self, X: Data, y: Data, fit_controls={}) -> None:
        self.model_list = self._pool(
            delayed(_fit_single)(m, X, y, fit_controls)
            for m in self.model_list
        )
        self._fix_copy_names()

    def predict(self, X: Data, run_controls={}) -> list[Data]:
        model_preds = self._pool(
            delayed(_predict_single)(m, X, run_controls)
            for m in self.model_list
        )

        test1 = all(isinstance(mpred, list) for mpred in model_preds)
        test2 = all(isinstance(mpred, ndarray) for mpred in model_preds)
        assert test1 or test2

        model_preds = [
            mpred if isinstance(mpred, list) else [mpred]
            for mpred in model_preds
        ]
        # list(Models) > list(Series) > array(Timesteps x Features)
        mod1_pred = model_preds[0]
        N_series = len(mod1_pred)

        agg_pred = [
            self.agg_func([mpred[serie] for mpred in model_preds])
            for serie in range(N_series)
        ]

        assert len(agg_pred) == N_series
        assert all(ap.shape == mp.shape for ap, mp in zip(agg_pred, mod1_pred))

        return agg_pred

    run = predict
