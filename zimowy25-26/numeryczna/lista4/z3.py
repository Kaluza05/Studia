def f(x):
    return x - 0.49

def sgn(x):
    return 1 if x > 0 else 0 if x == 0 else -1

def bisection_method(f,a0,b0,steps):
    a_sign = sgn(f(a0))
    for _ in range(steps):
        m = (a0 + b0)/2
        m_sign = sgn(f(m))
        if m_sign == a_sign:
            a0 = m
        else:
            b0 = m
        yield m 


for i,m in enumerate(bisection_method(f,0,1,5)):
    oszacowanie = 1 / 2**(i+1)
    print(abs(0.49 - m), oszacowanie)