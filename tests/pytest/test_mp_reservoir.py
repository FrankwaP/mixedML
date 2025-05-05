# -*- coding: utf-8 -*-

from reservoirpy.nodes import Reservoir, Ridge
from reservoirpy.datasets import mackey_glass


from python.reservoir_ensemble import ReservoirEnsemble, get_esn_ensemble


model1 = Reservoir(units=5) >> Ridge(ridge=1e-5)
model2 = Reservoir(units=6) >> Ridge(ridge=1e-5)

data = mackey_glass(2000)

X1 = data[:50]
y1 = data[1:51]


X2_1 = data[:50]
X2_2 = data[50:60]

y2_1 = data[1:51]
y2_2 = data[51:61]

X2 = [X2_1, X2_2]
y2 = [y2_1, y2_2]


def test_no_mp():

    resmod = ReservoirEnsemble(
        model_list=[model1, model2], n_procs=0, agg_func="mean"
    )

    resmod.fit(X1, y1)
    resmod.run(X1)


def test_mp():
    resmod = ReservoirEnsemble(
        model_list=[model1, model2], n_procs=5, agg_func="median"
    )
    resmod.fit(X1, y1)
    resmod.run(X1)


def test_ens():

    resmod = get_esn_ensemble(
        {"units": 5, "ridge": 1e-5},
        n_procs=5,
        agg_func="median",
        seed_list=[1, 2, 3],
    )
    resmod.fit(X1, y1)
    resmod.run(X1)


def test_ens_2():

    resmod = get_esn_ensemble(
        {"units": 5, "ridge": 1e-5},
        n_procs=5,
        agg_func="median",
        seed_list=[1, 2, 3],
    )
    resmod.fit(X2, y2)
    resmod.run(X2)


test_no_mp()
