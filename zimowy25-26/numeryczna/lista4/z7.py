def newton(x0,a, epsilon, f):
    n = 1
    
    while abs(f(x0)) > epsilon:
        x0 = x0/2 + a/(2*x0)
        #x0 = x0 - (x0**2 - a)/(2*x0)
        n += 1

    return x0

def find_sqrt(m,c,epsilon = 10**(-15)):
    if c % 2 != 0:
        c = c - 1
        m = 2*m

    new_c = c // 2
    new_a = newton(1,m, epsilon, lambda x : x**2 - m)
    return new_a * 2**new_c

ms = [1-1/2**n for n in range(5)] #m-y dostesowania
cs = [i for i in range(10)]       #c do testowania

from itertools import product
for m,c in product(ms,cs):
    print(m*2**c, find_sqrt(m,c))
