def is_free(x,y,b):
    for i in range(x):
        if b[i] == y or abs(x-i) == abs(y-b[i]):
            return 0
    return 1

def ile_hetmanow(n):
    k = 1
    b = [-1 for _ in range(n)]
    b[0] = 0
    licz = 0
    while  0 <= k <= n and b[0] < n:
        if k == n:
            b[n-1] = -1
            k -= 2
            b[k] += 1
            licz += 1
        else:
            b[k] += 1
            while b[k] < n and not is_free(k,b[k],b):
                b[k] += 1
            if b[k] < n: k += 1
            else: b[k] = -1; k -= 1
    return licz

print(ile_hetmanow(8))
