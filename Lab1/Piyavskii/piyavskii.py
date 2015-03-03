import sys
import math

import matplotlib.pyplot as plt


class GFunction(object):
    def __init__(self, f, a, b, l, x0):
        self.f = f
        self.l = l
        self.x0 = x0
        self.arg_min = a if self.calculate(a) < self.calculate(b) else b

    def calculate(self, x):
        return self.f(self.x0) - self.l * abs(self.x0 - x)


class PiecewiseLinearFunction(object):
    def __init__(self, f, g, a, b, l):
        self.functions = [g]
        self.f = f
        self.a = a
        self.b = b
        self.l = l
        self.candidates_for_min = [a, b]
        self.x0s = [g.x0]

    def compose_with_g(self, g):
        self.functions.append(g)
        left = None
        for x0 in self.x0s:
            if x0 < g.x0 and (left is None or left < x0):
                left = x0
        if left is not None:
            left_intersection = ((g.x0 + left) - (self.f(g.x0) - self.f(left)) / self.l) / 2
            if left_intersection > self.a:
                self.candidates_for_min.append(left_intersection)
        right = None
        for x0 in self.x0s:
            if x0 > g.x0 and (right is None or right > x0):
                right = x0
        if right is not None:
            right_intersection = ((g.x0 + right) + (self.f(g.x0) - self.f(right)) / self.l) / 2
            if right_intersection < self.b:
                self.candidates_for_min.append(right_intersection)
        self.x0s.append(g.x0)

    def arg_min(self):
        res = self.candidates_for_min[0]
        for c in self.candidates_for_min:
            if self.calculate(c) < self.calculate(res):
                res = c
        return res

    def calculate(self, x):
        return max(g.calculate(x) for g in self.functions)


def piyavskii(f, a, b, l):
    x_min = (a + b) / 2
    g = GFunction(f, a, b, l, x_min)
    p = PiecewiseLinearFunction(f, g, a, b, l)
    args = [i / 100. for i in range(int(a) * 100, int(b) * 100)]
    for _ in range(150):
        x_min = p.arg_min()
        print('x_min =', x_min)
        print(p.candidates_for_min)
        p.compose_with_g(GFunction(f, a, b, l, x_min))

    plt.plot(args, [p.calculate(x) for x in args])
    plt.show()
    return p.arg_min()

print(piyavskii(lambda x: math.sin(x), 1., 6., 1.))
