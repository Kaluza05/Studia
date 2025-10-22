import timeit
import pandas as pd


def doskonale_imperatywna(n: int)-> list[int]:
    
    def is_doskonala(m):
        sum_d = 1
        for i in range(2,int(m**0.5)+1):
            if m%i == 0:
                if i == int(m**0.5):
                    sum_d += i
                else:
                    sum_d += i + m//i
        
        return m == sum_d

    ds = []

    for i in range(n+1):
        if is_doskonala(i):
            ds.append(i)

    return ds


def doskonale_skladana(n: int)-> list[int]:
    return [i for i in range(1, n+1)
            if i == 1 + sum(d + i//d if d != int(i**0.5) else d 
            for d in range(2,int(i**0.5)+1) if i%d == 0)]

def doskonale_funkcyjna(n: int)-> list[int]:
    return list(filter(
        lambda i : i == 1+ sum(
            map(
                lambda x : x + i // x if x !=int(i**0.5) else x,
                filter(lambda d : i % d == 0, 
                        range(2,int(i**0.5)+1)))),
                        range(1,n+1)))


N = 20000
rows = dict()
for i in range(0,N + 1,2000):
    time_imp = timeit.timeit(lambda : doskonale_imperatywna(i), number= 1)
    time_skl = timeit.timeit(lambda : doskonale_skladana(i), number= 1)
    time_fun = timeit.timeit(lambda : doskonale_funkcyjna(i), number= 1)
    rows[i] = {'imperative' : time_imp, 'skladana' : time_skl, "functional" : time_fun}

time_table = pd.DataFrame(rows).T
print(time_table)

print(doskonale_imperatywna(10000))
print(doskonale_skladana(10000))
print(doskonale_funkcyjna(10000))