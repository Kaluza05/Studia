from math import sqrt
def sito(n):
    sito_bool = [1 for i in range(n)]
    sito_bool[1],sito_bool[0] = 0,0
    for i in range(2,int(sqrt(n))+1):
        if sito_bool[i]:
            for j in range(2,n//i ):
                sito_bool[i*j] = 0
    return sito_bool

def palindrome(a,b):
    primes = sito(b)
    palindrome_primes = []
    for i in range(a,b-1):
        print(i,primes[i])
        if str(i)==str(i)[::-1] and primes[i]:
            palindrome_primes.append(i)
    return palindrome_primes

print(sito(30))
print(palindrome(1,20))