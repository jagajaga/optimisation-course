import math
import random

import matplotlib.pyplot as plt


class LipschitzFunction(object):
    def __init__(self, f, a, b, l):
        self.f = f
        self.a = a
        self.b = b
        self.l = l
        self.calls_count = 0

    def __call__(self, x, not_increase_counter=False):
        if not not_increase_counter:
            self.calls_count += 1
        return self.f(x)


class GFunction(object):
    def __init__(self, lipschitz_function, x0):
        self.f = lipschitz_function
        self.a = lipschitz_function.a
        self.b = lipschitz_function.b
        self.l = lipschitz_function.l
        self.x0 = x0
        self.arg_min = self.a if self(self.a) < self(self.b) else self.b

    def __call__(self, x, not_increase_counter=False):
        return self.f(self.x0, not_increase_counter) - self.l * abs(self.x0 - x)


class PiecewiseLinearFunction(object):
    def __init__(self, g):
        self.functions = [g]
        self.f = g.f
        self.a = g.a
        self.b = g.b
        self.l = g.l
        self.candidates_for_min = [self.a, self.b]
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
            if self(c) < self(res):
                res = c
        return res

    def __call__(self, x, not_increase_counter=False):
        return max(g(x, not_increase_counter) for g in self.functions)


def piyavskii(lipschitz_function, eps):
    a = lipschitz_function.a
    b = lipschitz_function.b
    x_min = random.uniform(a, b)
    x_min = (a + b) / 2
    p = PiecewiseLinearFunction(GFunction(lipschitz_function, x_min))
    args_for_plot = [i / 100. for i in range(int(a) * 100, int(b) * 100)]
    while True:
        old_x_min = x_min
        x_min = p.arg_min()
        yield x_min
        if abs(x_min - old_x_min) < eps:
            break
        p.compose_with_g(GFunction(lipschitz_function, x_min))

    plt.plot(args_for_plot, [p(x, True) for x in args_for_plot])
    plt.show()
    return p.arg_min()


def main():
    f = lambda x: abs(math.sin(x) - .5)
    lipschitz_function = LipschitzFunction(f, -10., 5., 1.)
    eps = 1e-4
    iterations_count = 0
    print("Running Piyavskii's method with eps =", eps)
    for x in piyavskii(lipschitz_function, eps):
        iterations_count += 1
        print(x)

    print('Iterations count =', iterations_count)
    print('Calls count = ', lipschitz_function.calls_count)


if __name__ == '__main__':
    main()
