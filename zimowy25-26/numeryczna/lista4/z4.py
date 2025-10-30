from math import cos

def sgn(x):
    return 1 if x > 0 else 0 if x == 0 else -1

def bisection_method(f,a0,b0,epsilon):
    a_sign = sgn(f(a0))
    n = 0
    while (b0-a0) / 2**(n) > epsilon:
        m = (a0 + b0)/2
        m_sign = sgn(f(m))
        if m_sign == a_sign:
            a0 = m
        else:
            b0 = m
        n += 1
    return m

def f(x):
    return 3*x**2 - 5 * cos(7*x - 1)

EPSILON = 10**(-6)
start_points = [((1+3*k)/7, (4+3*k)/7) for k in [-4,-3,-2,-1,0,1,2,3]]
for a,b in start_points:
    found = bisection_method(f,a,b,EPSILON)
    print(found, f(found))