from math import sqrt

def f(x):
    a = x**15
    b =(12150 * a) + 9
    c = sqrt(b)
    d = (c - 3) / a
    print(a,b,c,d)
    return d

def f2(x):
    a = x**15
    b = sqrt(12150 * a + 9) + 3
    return 12150 / b


print(f(0.01))
print(f2(0.01))