import matplotlib.pyplot as plt
import numpy as np

def factorial(n):
    cum_prod = 1
    for i in range(1,n+1):
        cum_prod *= i
    return cum_prod

def bernstein(n,k,x):
    newton = factorial(n) / (factorial(k) * factorial(n-k))

    return newton * x**k * (1-x)**(n-k)

def rational_bezier(wis, wages, x):
    #nie wiem czy n nie ma byc stopnia rownego ilosci wierzcholkow
    n = len(wis) - 1
    denom = sum(wages[i] * bernstein(n,i,x) for i in range(n+1))
    num_x = sum(wages[i] * bernstein(n,i,x) * wis[i][0] for i in range(n+1))
    num_y = sum(wages[i] * bernstein(n,i,x) * wis[i][1] for i in range(n+1))

    return (num_x/ denom, num_y / denom)

control_p = [(39.5, 10.5), (30, 20), (6, 6), (13, -12), (63, -12.5), (18.5, 17.5), (48, 63),
(7, 25.5), (48.5, 49.5), (9, 19.5), (48.5, 35.5), (59, 32.5), (56, 20.5)]

wages = [1, 2, 3, 2.5, 6, 1.5, 5, 1, 2, 1, 3, 5, 1]

points = np.linspace(0,1,100)
values = [rational_bezier(control_p,wages,p) for p in points]


plt.plot(*zip(*values))
plt.scatter(*zip(*control_p))
plt.show()