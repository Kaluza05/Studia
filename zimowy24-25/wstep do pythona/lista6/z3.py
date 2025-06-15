from math import sqrt

def prime(n):
    for i in range(2,int(sqrt(n))+1):
        if n%i==0:
            return False
    return True
def prime_divisors(n):
    divs = set()
    for i in range(2,n//2 +1):
        if prime(i) and n%i == 0:
            divs.add(i)
    return divs


print(prime_divisors(2*7*5*3*17))