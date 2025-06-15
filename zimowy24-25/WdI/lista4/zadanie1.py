from test_print import test
#zadanie 1
#złożoność O(1)
def zad_1(n):
    if n%2 == 0:
        return n
    return (-1)*n

#zadanie 2
#złożoność O(n)
def zad_2(n):
    suma = 0
    for i in range(1,n+1):
        if i%2 == 0:
            suma += 1/i
        else:
            suma -= 1/i
    return suma

#zadanie 3
#złożoność O(log(n))
def zad_3(n,x):
    def potega(x,k):
        if k ==0:
            return 1
        if k%2==0:
            return potega(x**2,k//2)
        else:
            return x*potega(x**2,(k-1)//2)
    if x==1:
        return (n*(n+1))//2
    
    x_n = potega(x,n)
    licznik = n*x_n*x-(n+1)*x_n+1
    mianownik = potega(x-1,2)
    wynik = (licznik*x)//mianownik
    return wynik


def zad_3_naiwne(n,x):
    return sum([i*x**i for i in range(1,n+1)])




for i in [1,9,10]:
    #test(zad_1,i)
    pass

for i in [1,2,4,5,7]:
    #test(zad_2,i)
    pass

for i in range(10):
    test(zad_3,i,1)
    #test(zad_3_naiwne,i,1)

