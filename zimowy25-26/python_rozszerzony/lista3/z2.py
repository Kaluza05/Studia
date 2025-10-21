import timeit
import pandas as pd


def doskonale_imperatywna(n: int)-> list[int]:
    doskonale = []
    for i in range(1, n+1):
        suma_dzielniki = 0
        for d in range(1,i):
            if i % d == 0:
                suma_dzielniki += d 

        if i == suma_dzielniki:
            doskonale.append(i)
    
    return doskonale

def doskonale_skladana(n: int)-> list[int]:
    return [i for i in range(1, n+1) if i == sum(d for d in range(1,i) if i%d == 0)]

def doskonale_funkcyjna(n: int)-> list[int]:
    return list(filter(lambda i : i == sum(filter(lambda d : i % d == 0, range(1, i))), range(1,n+1)))


N = 10000
rows = dict()
for i in range(0,N + 1,1000):
    time_imp = timeit.timeit(lambda : doskonale_imperatywna(i), number= 1)
    time_skl = timeit.timeit(lambda : doskonale_skladana(i), number= 1)
    time_fun = timeit.timeit(lambda : doskonale_funkcyjna(i), number= 1)
    rows[i] = {'imperative' : time_imp, 'skladana' : time_skl, "functional" : time_fun}

time_table = pd.DataFrame(rows).T
print(time_table)