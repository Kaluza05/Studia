from math import sqrt

def xk(n):
    assert n >= 1
    if n == 1:
        return 2
    

    help1 = 1 - (xk(n-1)/2**(n-1))**2
    help2 = 2 * (1 - sqrt(help1))
    help3 = sqrt(help2)
    help4 = 2**(n-1) * help3
    print(n, help4)


    return help4


def xk2(n)-> float:
    assert n >=1

    if n == 1:
        return 2
    
    help0 = xk2(n-1)
    a = help0 / 2**(n-1)
    help1 = (1 + sqrt(1 - a)* sqrt(1+a))
    help2 = 2 / help1
    help3 = help0 * sqrt(help2)
    print(n, help3)
    return help3

#print(xk(100))
print(xk2(100))