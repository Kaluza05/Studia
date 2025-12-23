import timeit
import pandas as pd


def pierwsze_imperatywna(n: int)-> list[int]:
    pierwsze = []
    def pierwsza(m):
        if m <= 1: return False
        for d in range(2,int(i**0.5)+1):
            if i % d ==0:
                return False
        return True

    for i in range(1,n+1):
        if pierwsza(i):
            pierwsze.append(i)
    
    return pierwsze

def pierwsze_skladana(n: int)-> list[int]:
    return [i for i in range(1, n+1) if i >=2 and [d for d in range(2,int(i**0.5)+1) if i % d == 0] == [] ]

def pierwsze_funkcyjna(n: int)-> list[int]:
    return list(filter(lambda i : i >=2 and all(map(lambda d : i % d, range(2,int(i**0.5) + 1 )))  , range(1,n+1)))


N = 10000
rows = dict()
for i in range(0,N + 1,1000):
    time_imp = timeit.timeit(lambda : pierwsze_imperatywna(i), number= 5)
    time_skl = timeit.timeit(lambda : pierwsze_skladana(i), number= 5)
    time_fun = timeit.timeit(lambda : pierwsze_funkcyjna(i), number= 5)
    rows[i] = {'imperative' : time_imp, 'skladana' : time_skl, "functional" : time_fun}

time_table = pd.DataFrame(rows).T
print(time_table)