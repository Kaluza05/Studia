from random import randint

K = 1000
N = 1_000_000

def rand_seq():
    return [randint(0,K-1) for i in range(N)]

def sort_in_place(tab,l,r,pivot):
    pass
def quick_sort(tab,l,r):
    if r-l== 1:return tab
    pivot = tab[l]
    s= (l+r)//2
    quick_sort(tab,l,s)
    quick_sort(tab,1,r)

def f(tab:list):
    counts = [0 for _ in range(K)]
    for i in tab:
        counts[i] +=1
    return [ j for j in range(K) for _ in range(counts[j])]#

def comb_list_of_lists(tab):
    return [j for i in tab for j in i]

from time import time

b = rand_seq()

s = time()
f(b)
print(time()-s)
d = time()
b.sort()
print(time()-d)