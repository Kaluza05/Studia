def potegowanie(x,k,licz=0):
    if k==0:
        #print(licz)
        return licz-1
    if k%2==0: return potegowanie(x*x,k//2,licz+1)
    return potegowanie(x*x,(k-1)//2,licz+2)

def najwiecej_najmniej(k):
    min_n = 2**k
    max_n = 0
    for i in range(2**k,2**(k+1)+1):
        a = potegowanie(2,i)
        if a<min_n:
            min_n=a
        if a>max_n:
            max_n=a
    return min_n,max_n

print(najwiecej_najmniej(4))
print(potegowanie(2,2**4,0))
'''
najmniej mnożeń dla 2^k ilość mnożeń k+1  bo 2^k podzielimy k razy przez 2 żeby otrzymać 1 i potem ostatnie mnożenie a*1
najwięcej dla 2^(k+1)-1  2k+1  bo 2^(k+1)-1 dzielimy k razy żeby otrzymać 1 
(ale ponieważ ciągle są nieparzyste to mnożymy i potęgujemy, więc 2 operacje mnożenia wykonujemy) 
i potem ostatnie mnożenie a*1

a)kolejne potęgi przez jakie mnożymy to tylko a^(2^k)
b)kolejne potęgi to a,a^2,a^4,...,a^(2^k)
'''