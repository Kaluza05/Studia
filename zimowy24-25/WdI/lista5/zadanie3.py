
def gcd (n , m ):
    if (m ==0):
        return n
    if (n < m ):
        return gcd (m , n)
    oddcnt = n %2 + m %2
    if ( oddcnt ==2):
        return gcd (n -m , m)
    if ( oddcnt ==0):
        return 2* gcd ( n //2 , m //2)
    if (n %2==0):
        return gcd (n //2 , m)
    else :
        return gcd (n , m //2)

'''
po jednej iteracji albo zmniejszamy którąś o 2, albo zmniejszamy jedną i robiąc to staje się parzysta
więc po dwóch iteracjach któraś jest zmniejszona o 2   n'm'<=nm/2
zatem O(log(n*m)) = O(log(n)+log(m))
'''

def gcd_petla(n,m):
    gcd = 1
    while m!=0:
        if n<m:
            n,m = m,n
        oddcnt = n %2 + m %2
        if ( oddcnt ==2):
            n = n-m
        if ( oddcnt ==0):
            gcd *=2
            n,m = n//2,m//2
        if (n %2==0):
            n = n//2
        else :
            m = m//2
    return gcd*n

print(gcd_petla(3,6))