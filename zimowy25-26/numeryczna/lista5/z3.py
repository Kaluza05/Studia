def sgn(x):
    return 1 if x > 0 else 0 if x == 0 else -1

def bisection(a0,b0,f, epsilon =  1e-6, max_steps = 1000):
    a_sign = sgn(f(a0))
    steps = 0
    m = a0
    while steps < max_steps and abs(f(m)) > epsilon:
        m = (a0 + b0)/2
        m_sign = sgn(f(m))
        if m_sign == a_sign:
            a0 = m
        else:
            b0 = m

        steps += 1
    return m, steps

def newton(x0,f,f_der, epsilon = 1e-6, max_steps = 1000):
    steps = 0
    while steps < max_steps and abs(f(x0)) > epsilon:
        x0 = x0 - f(x0) / f_der(x0)
        steps += 1

    return x0, f(x0), steps

def secant(x0,x1,f, epsilon = 1e-6, max_steps = 1000):
    steps = 0
    while steps < max_steps and abs(f(x0)) > epsilon:
        x1,x0 = x1 - f(x1) * (x1-x0) / (f(x1) - f(x0)), x1
        steps += 1
        
    return x0, f(x0), steps

def regula_falsi(x0,x1,f, epsilon = 1e-6, max_steps = 1000):
    #f(x0) < 0 < f(x1)
    steps = 0
    m = x0
    while steps < max_steps and abs(f(m)) > epsilon:
        #calculating next m value like in secant method, and changing x1 or x0 like in bisection method
        m = x0 - f(x0) * (x0 - x1) / (f(x0) - f(x1))
        if f(m) > 0:
            x1 = m
        else:
            x0 = m

        steps += 1
        
    return m, f(m), steps


f = lambda x : x**5 - x + 1
f_der = lambda x : 5*x**4 - 1
start = -2
start2 = 2

print(bisection(start,start2,f))
print(newton(start,f,f_der,))
print(secant(start,start2,f))
print(regula_falsi(start,start2,f))