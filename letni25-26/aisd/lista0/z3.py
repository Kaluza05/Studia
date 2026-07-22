def pow(x,n):
    res = 1
    while n > 0:
        if n % 2 == 0:
            x = x**2
            n = n//2
        else:
            res *= x
            x = x**2
            n = n//2
    
    return res

for i in range(0,10):
    print(i,pow(2,i))