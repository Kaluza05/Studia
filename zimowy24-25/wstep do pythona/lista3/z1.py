from math import sqrt


def prime(n):
    if n == 1:
        return False
    for i in range(2,int(sqrt(n))+1):
        if n % i == 0:
            return False
    return True
   

def liczby_szczesliwe():
    szczesliwe = [i for i in range(1,100_000) if ('777' in str(i)) and prime(i)]
    return szczesliwe,len(szczesliwe)


print(liczby_szczesliwe())