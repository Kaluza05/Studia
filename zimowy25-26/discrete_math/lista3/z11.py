def Reukl(a,b):
    xa = 1
    ya = 0
    xb = 0
    yb = 1
    while b > 0:
        divisor = a // b
        xa,ya,xb,yb = xb,yb, xa - divisor * xb, ya - divisor * yb
        a,b = b, a- divisor * b

    return a, xa,ya,b, xb,yb

start = (69,1313)
print(Reukl(*start))