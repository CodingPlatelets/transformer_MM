import math
import numpy as np

ln2 = 0.6931471805599453
bias1 = 1.353
k1 = 0.3585
bias2 = 0.344

def exp_poly(x):
    z = abs(x) // ln2
    p = x + z * ln2
    print(f'p: {p:.5f}')

    lp = k1 * (p + bias1) ** 2 + bias2
    
    return lp / (2 ** z)


def test_softmax():
    # generate a list from -3.5 to 0 with 0.1 step
    xs = np.arange(-10.5, 0, 0.5)
    for x in xs:
        acc = np.exp(x)
        cur = exp_poly(x)
        miss = abs(acc - cur) / acc * 100
        print(f"x: {x:.5f}, exp(x): {acc:.5f}, exp_poly(x): {cur:.5f}, miss: {miss:.5f}%")
        print()

test_softmax()