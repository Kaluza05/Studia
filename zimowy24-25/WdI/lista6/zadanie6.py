def sito(n):
    prime_lst = [1 for _ in range(n+1)]
    prime_lst[0]=0
    prime_lst[1]=0
    for i in range(2,n):
        if i**2>n:
            break
        if prime_lst[i]==1:
            for d in range(2,(n)//i +1):
                prime_lst[i*d] = 0
    primes = [i for i in range(n+1) if prime_lst[i]]

    return primes

def s_sqrt(n):
    x_n = 1
    for _ in range(10):
        x_n = 1/2 * (x_n+(n/x_n))
    return x_n

#print(s_sqrt(10000000))

def sito_zakres(m,n): # m < n< m+ 10_000 < 100_000_000
    '''
    liczby pierwsze pomiędzy [2,sqrt(n)] O(sqrt(n)ln(ln(n)))
    odchaczać liczby jeśli któraś z nich dzieli [m,n]  O(sqrt(n))
    spece complexicty O(sqrt(n) +n-m)

    '''
    my_sqrt = int(s_sqrt(n))+1
    sito_n = sito(my_sqrt)
    sito_testowe = sito(n)
    primes_in_m_n = [1 for i in range(m,n+1)]
    #print(sito_n)
    for i in sito_n:
        for d in range(m//i ,(n)//i +1):
            if i*d-m<0:
                continue
            else:
                primes_in_m_n[i*d-m] = 0
    
    a = [i for i in sito_testowe if i>=m]# premitywnie całe sito robi do n i bierze większe od m
    b = [i for i in range(m,n+1) if primes_in_m_n[i-m]] #robi sito do sqrt(n) i wywala nie pierwsze z m,n 
    return [i for i in range(m,n+1) if primes_in_m_n[i-m]]

sito_zakres(6000,10000)