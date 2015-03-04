import math
import random
import timeit
import bisect

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
        self.candidates_for_min = {self.a, self.b}
        self.x0s = [g.x0]
        self.arg_min = g.arg_min

    def compose_with_g(self, g):
        self.functions.append(g)
        left_idx = bisect.bisect_left(self.x0s, g.x0) - 1
        if left_idx >= 0:
            left = self.x0s[left_idx]
            left_intersection = ((g.x0 + left) - (self.f(g.x0) - self.f(left)) / self.l) / 2
            if left_intersection >= self.a:
                self.candidates_for_min.add(left_intersection)
        right_idx = bisect.bisect_right(self.x0s, g.x0)
        if right_idx < len(self.x0s):
            right = self.x0s[right_idx]
            right_intersection = ((g.x0 + right) + (self.f(g.x0) - self.f(right)) / self.l) / 2
            if right_intersection <= self.b:
                self.candidates_for_min.add(right_intersection)
        self.candidates_for_min.remove(g.x0)
        self.update_min()
        bisect.insort(self.x0s, g.x0)

    def update_min(self):
        self.arg_min = None
        cur_min = None
        for c in self.candidates_for_min:
            v = self(c)
            if cur_min is None or self(c) < cur_min:
                self.arg_min = c
                cur_min = v

    def __call__(self, x, not_increase_counter=False):
        return max(g(x, not_increase_counter) for g in self.functions)


def piyavskii(lipschitz_function, eps):
    print("Running Piyavskii's method with eps = {}".format(eps))
    start = timeit.default_timer()

    a = lipschitz_function.a
    b = lipschitz_function.b
    x_min = random.uniform(a, b)
    x_min = (a + b) / 2
    p = PiecewiseLinearFunction(GFunction(lipschitz_function, x_min))
    args_for_plot = [i / 100. for i in range(int(a) * 100, int(b) * 100)]
    while True:
        old_x_min = x_min
        x_min = p.arg_min
        yield x_min
        if abs(x_min - old_x_min) < eps:
            break
        p.compose_with_g(GFunction(lipschitz_function, x_min))

    print('Running time = {} sec'.format(timeit.default_timer() - start))

    plt.plot(args_for_plot, [p(x, True) for x in args_for_plot])
    plt.show()


def main():
    f = lambda x: abs(math.sin(x) - .5)
    lipschitz_function = LipschitzFunction(f, -1., 5., 1.)
    eps = 1e-4
    iterations_count = 0
    for x in piyavskii(lipschitz_function, eps):
        iterations_count += 1
        print(x)

    print('Iterations count = {}'.format(iterations_count))
    print('Calls count = {}'.format(lipschitz_function.calls_count))


if __name__ == '__main__':
    main()
