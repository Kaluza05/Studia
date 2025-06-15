#złożoność log(n+m)
def podobne(n,m):
    def wystapienia(liczba):
        kubelki = [0 for _ in range(0,10)]
        while liczba>0:
            liczba,m = divmod(liczba,10)
            kubelki[m] += 1
        return kubelki
    a,b = wystapienia(n),wystapienia(m)
    print(a,b)
    for i in range(10):
        
        if a[i] != b[i]:
            return False
    return True

print(podobne( 123412 , 223411))
print(podobne(123412 , 11234))