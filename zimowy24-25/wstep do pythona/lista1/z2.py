seen = dict()
def silnia(n):
    if n == 0:
        return 1
    if n in seen:
        return seen[n]
    a = n*silnia(n-1)
    seen[n] = a   #przechowujemy poprzednie wywołania funkcji silnia, żeby nie trzeba było ich na nowo obliczać
    return a

def koncowka(n):
    if len(str(n)) >= 2:
        if str(n)[-2] != '1':
            if str(n)[-1] in {'2','3','4'}:
                return 'y'
    else:
        if str(n) in {'2','3','4'}:
            return 'y'
        elif str(n) == '1':
            return 'ę'
    return ''


for i in range(101):
    length = len(str(silnia(i)))
    print(f'{i}! ma {length} cyfr{koncowka(length)}')

'''
2 sposób na liczenie długości
from math import log10
ceil(log10(silnia(n))) 

10^(k-1)<silnia(n) <= 10^(k) , k-liczba cyfr silnia(n)
k-1 < log10(silnia(n))<= k  , ceil(x) zaokrągla liczbę do najmniejszej liczby całkowitej większej od x
int(log10(silnia(n))) +1 = k
'''