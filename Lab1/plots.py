import matplotlib.pyplot as plt


def show_plot(f):
    args = []
    values = []
    for l in f.readlines():
        ll = l.rstrip()[1:-1]
        kv = ll.split(',')
        args.append(float(kv[0]))
        values.append(float(kv[1]))

    plt.plot(args, values)
    plt.show()

with open('dichotomy') as f:
    show_plot(f)

with open('golden ratio') as f:
    show_plot(f)

with open('fibonacci') as f:
    show_plot(f)

