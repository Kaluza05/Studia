#O(log_a(n))
def max_pow(n,a):
    pow = 1
    old_a = a
    while True:
        if n%a==0:
            a *= old_a
            pow+=1
        else:
            return pow-1

#O(2k)      
def potegi_p(n,seq): #k długośc seq
    p = 0
    for i in seq:
        if max_pow(n,i) > p:
            p = max_pow(n,i)
    spelniajace = [] #jeśli bez tablic pythonowych to wypisywać wartości w if-ie
    for j in seq:
        if n%(j**p) == 0:
            spelniajace.append(j)
    return p,spelniajace

print(max_pow(128,4))
print(potegi_p(63000,[2,3,5]))

'''
poprawnośc:
max_pow zwraca największą wartość p jaka że a^p|n
wyszukujemy największej wartości p w ciągu seq
wyszukujemy liczby, których max_pow == p
'''