from functools import lru_cache

@lru_cache
def r(n,m):
    if m <= 0:
        return 0
    if n == 1:
        if m <= 10:
            return 1
        return 0
    
    return sum(r(n-1,m-i) for i in range(11))

for i in range(1,5):
    print(r(i,28))