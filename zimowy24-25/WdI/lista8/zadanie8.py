def hanoi(n:int,a:list,b:list,c:list):
    '''
    specyfikacja
    wejście:
    n-liczba pięter wieży
    a,b,c - tablice zawierające uporządkowane piętra wieży
    wyjście :

    '''
    
    if n>0:
        print(n,a,b,c)
        hanoi(n-1,a,c,b)
        c.insert(0,a.pop(0))
        hanoi(n-1,b,a,c)

k=4
a=list(range(k))#[::-1]
b=[]
c=[]
hanoi(k,a,b,c)
print(a,b,c)