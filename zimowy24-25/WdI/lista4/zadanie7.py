#złożoność O(log(n))
def liczba_k_cyfrowa(n):
    k=0
    kubelki = [0 for _ in range(0,10)]
    while n>0:
        n,m = divmod(n,10)
        kubelki[m] += 1
    for i in kubelki:
        if i != 0:
            k+=1
    return k
