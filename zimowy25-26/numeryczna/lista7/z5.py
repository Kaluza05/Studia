from functools import reduce
from math import cos, pi
import matplotlib.pyplot as plt
import numpy as np

N = 1000

def plot_polynomial(points, plot_range : tuple[float,float] = (-1,1)):
    polynomial = lambda x : reduce(lambda a,b : a * b, [x - p for p in points])


    # 2. dużo punktów (np. 1000)
    xs = np.linspace(*plot_range, N)

    # 3. wartości funkcji
    ys = polynomial(xs)

    # 4. wykres
    plt.plot(xs, ys)
    plt.xlabel("x")
    plt.ylabel("f(x)")
    plt.grid(True)
    plt.show()  

def plot_chebyshev(n):
    points = [cos((2*k+1)*pi/(2*n+2)) for k in range(n+1)]

    plot_polynomial(points)

def plot_equidistant(n):
    points = [-1 + 2*i/n for i in range(n+1)]

    plot_polynomial(points)

def plot_both(n, plot_range : tuple[float,float] = (-1,1)):
    points_c = [cos((2*k+1)*pi/(2*n+2)) for k in range(n+1)]
    points_eq = [-1 + 2*i/n for i in range(n+1)]

    f = lambda x : reduce(lambda a,b : a * b, [x - p for p in points_c])
    g = lambda x : reduce(lambda a,b : a * b, [x - p for p in points_eq])

    xs = np.linspace(*plot_range, N)

    plt.plot(xs, f(xs), label='f(x) = x^2', color='blue')
    plt.plot(xs, g(xs), label='g(x) = 2x + 5', color='red')

    # Dodajemy legendę
    plt.legend()

    # Dodajemy tytuł i etykiety osi
    plt.title("Wykres funkcji f i g")
    plt.xlabel("x")
    plt.ylabel("y")

    # Pokaż wykres
    plt.grid(True)
    plt.show()


deg = 20
#plot_equidistant(deg)
#plot_chebyshev(deg)
plot_both(deg)

"""
wielomian z wezlami chebyszewa ma podobne wartosci na calym przedziale natomiast
z rownoodleglymi jest nierownomierny rozklad wartosci (na krancach wieksze)

"""

