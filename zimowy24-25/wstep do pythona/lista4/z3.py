from random import randint
import time

def randperm3(n):
    lst = list(range(n))
    perm = []
    for i in range(n):
        a= randint(0,n-1-i)
        perm.append(lst[a])
        lst[a] = lst[-1]
        lst.pop()
    return perm
#zrobic w jednej liscie 
for _ in range(5):
    print(randperm3(10))

b=time.time()
a=randperm3(10**6)
print(time.time()-b)
#print(a)
