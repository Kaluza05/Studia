def sito(n):
    prime_lst = [1 for i in range(n+1)]
    for i in range(2,n):
        if i**2>n:
            break
        if prime_lst[i]==1:
            for d in range(2,n//i):
                prime_lst[i*d] = 0
    return prime_lst

print(sito(100))