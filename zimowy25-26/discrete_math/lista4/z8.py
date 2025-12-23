for i in range(20):
    n = 2**i + 1
    print(i, n, n%3, n%5)

def prime(n):
    if n <= 1: return False

    for i in range(2,int(n**0.5)+1):
        if n% i ==0 :
            return False
    return True

for i in range(40):
    p = 2**i
    
print((2**48 +1)% (2**8 + 1))