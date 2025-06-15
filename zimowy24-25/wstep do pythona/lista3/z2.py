from math import sqrt,log10
def prime(n):
    if n == 1:
        return False
    for i in range(2,int(sqrt(n))+1):
        if n%i==0:
            return False
    return True

def hiperszczesliwa(search_range,k:int):
    hiper = []
    '''
     tworzy jedynie liczby które zawierają '7'*k oraz są w (10^(k+1),right_range)
    '''
    def construct_numbers(right_range:tuple,k:int):
        a = int((log10(right_range) - k))
        want = set()
        for i in range(10**a):
            i_str = str(i).zfill(a)
            for j in range(a+1):
                num = i_str[:(a-j)] + '7'*k + i_str[(a-j):]
                if len(str(int(num))) == 10: #jeśli zfill uzupełni 0 z przodu i próbujemy stworzyć 10-cyfrową liczbę zaczynającą się na 0( nie można)
                    want.add(int(num))
        return want
    
    for i in construct_numbers(search_range,k): #sprawdza czy liczba z k '7' jest pierwsza
        if prime(i):
            hiper.append(i)
    return hiper

a=hiperszczesliwa(10_000_000_000,7)
print(a,len(set(a)))
