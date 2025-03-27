#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from itertools import repeat

from multiprocessing import Pool

from numpy import array
from numpy.typing import NDArray
from reservoirpy import Model  # type: ignore
from reservoirpy.nodes import ESN, Reservoir, Ridge  # type: ignore
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
    # when copying for multiprocessing
    # print(".", end="")
    model.fit(X, y, **options_fit)
    return model


class ReservoirEnsemble:
    # VotingRegressor has been considered, but:
    #  1. reservoirs uses "run" instead of "predict"
    #  2. reservoirs uses options for "fit" (like warmups)
    # … so at least here we're sure it's controlled

    def __init__(
        self,
        reservoir_kwargs: dict,
        ridge_kwargs: dict,
        list_seeds: list[int],
        n_procs: int,
        dtype: str,
    ):
        assert "dtype" not in reservoir_kwargs
        assert "dtype" not in ridge_kwargs
        # I have modified Rdige code so it takes **kwargs
        self._models = [
            Reservoir(**reservoir_kwargs, seed=seed, dtype=dtype)
            >> Ridge(**ridge_kwargs, dtype=dtype)
            for seed in list_seeds
        ]
        self._nprocs = n_procs

    def fit(self, X: NDArray, y: NDArray, **options_fit) -> None:
        assert X.ndim == 3
        assert y.ndim == 3 and y.shape[2] == 1
        if not self._nprocs:
            for m in self._models:
                m.fit(X, y, **options_fit)
        else:
            with Pool(self._nprocs) as pool:
                self._models = pool.starmap(
                    _mp_fit,
                    zip(
                        self._models, repeat(X), repeat(y), repeat(options_fit)
                    ),
                )

    def run(self, X: NDArray) -> NDArray:
        assert X.ndim == 3
        if self._nprocs == 0:
            preds = [m.run(X) for m in self._models]
        else:
            # with Pool(self._nprocs, initializer=_mp_initializer) as pool:
            with Pool(self._nprocs) as pool:
                preds = pool.starmap(_mp_run, zip(self._models, repeat(X)))
        return array(preds).mean(axis=0)
