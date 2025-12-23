from math import cos,sin

def f(x):
    return 162 * (1 - cos(5 * x)) / x**2

def f2(x):
    return 162 * (sin(5 * x)**2)  / (x**2 * (1+cos(5*x)))

for i in range(1,20):
    i = 10**(-i)
    print(i, f(i), f2(i))