def fibonacci(k,r):
    if r == 1:
        return 0
    tab = [1,1]
    for i in range(2,k+1):
        tab[i%2] = (tab[1]%r + tab[0]%r)%r
    return tab[k%2]


def fib(n):
    tab = [1,1]
    for i in range(2,n+1):
        tab[i%2] = tab[1] + tab[0]
    return tab[n%2]


k=900
r=1
print(fibonacci(k,r))
print(fib(k)%r)