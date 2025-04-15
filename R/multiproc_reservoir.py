# -*- coding: utf-8 -*-
from itertools import repeat
from typing import Literal

from multiprocessing import Pool

from numpy import array, mean, median
from numpy import int8, int16, int32, int64
from numpy import float16, float32, float64, float128
from numpy.typing import NDArray
from reservoirpy import Model  # type: ignore
from reservoirpy.nodes import Model, Reservoir, Ridge  # type: ignore
from reservoirpy import verbosity

verbosity(0)


def _mp_run(model: Model, X: NDArray) -> NDArray:
    return model.run(X)


def _mp_fit(
    model: Model,
    X: NDArray,
    y: NDArray,
    options_fit: dict,
) -> Model:
    # we need to return a model because reservoirpy uses namespace and add "-(Copy)"
    # when copying for multiprocessing, so the link with the original model is lost
    model.fit(X, y, **options_fit)
    return model


def _get_dtype(dtype):
    """will be usefull at some point"""
    dtypes = {
        "float16": float16,
        "float32": float32,
        "float64": float64,
        "float128": float128,
        "int8": int8,
        "int16": int16,
        "int32": int32,
        "int64": int64,
    }
    try:
        return dtypes[dtype]
    except KeyError:
        raise ValueError(f"dtype must be one of {dtypes.keys()}")


class ReservoirEnsemble:
    # VotingRegressor has been considered, but:
    #  1. reservoirs uses "run" instead of "predict"
    #  2. reservoirs uses options for "fit" (like warmups)
    # … so at least here we're sure it's controlled

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

    def __init__(
        self,
        list_models: list[Model],
        n_procs: int,
        agg_func: Literal["mean", "median"],
    ):
        self.agg_func = self._get_agg_func(agg_func)
        self.list_models = list_models
        self._nprocs = min(n_procs, len(list_models))

    def fit(self, X: NDArray, y: NDArray, **options_fit) -> None:
        assert X.ndim == 3
        assert y.ndim == 3 and y.shape[2] == 1
        if not self._nprocs:
            for m in self.list_models:
                m.fit(X, y, **options_fit)
        else:
            with Pool(self._nprocs) as pool:
                self.list_models = pool.starmap(
                    _mp_fit,
                    zip(
                        self.list_models,
                        repeat(X),
                        repeat(y),
                        repeat(options_fit),
                    ),
                )

    def run(self, X: NDArray) -> NDArray:
        assert X.ndim == 3
        if self._nprocs == 0:
            preds = [m.run(X) for m in self.list_models]
        else:
            # with Pool(self._nprocs, initializer=_mp_initializer) as pool:
            with Pool(self._nprocs) as pool:
                preds = pool.starmap(_mp_run, zip(self.list_models, repeat(X)))
        return self.agg_func(array(preds))

    predict = run


if __name__ == "__main__":

    ...
