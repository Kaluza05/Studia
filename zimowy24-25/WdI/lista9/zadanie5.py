

from time import time
def magic_square(n,k,a,used:set = set()):
    if k == n**2: return valid(a,n)
    for i in range(1,n**2+1):
        if i in used: continue
        a[k] = i
        used.add(i)
        #print(repeats(a,n,k),a,k)
        if magic_square(n,k+1,a,used): return 1
        used.remove(i)
    return 0


def valid(tab,n):
    magic = ((1+n**2)*n)//2
    suma_przek1 = 0
    suma_przek2 = 0
    for i in range(n):
        suma_wiersz = 0
        suma_kol = 0
        for j in range(n):
            suma_wiersz += tab[i*n+j]
            suma_kol += tab[i+j*n]
            if j == i:
                suma_przek1 += tab[i*n+j]
            if j == n-1-i:
                suma_przek2 += tab[i*n+j]
        if suma_wiersz != magic or suma_kol != magic: return 0
    if suma_przek1 != magic or suma_przek2 != magic: return 0
    return 1


n=3
a = [0 for _ in range(n**2)]
s = time()
print(magic_square(n,0,a),a)
print(time()-s)
#testowa = [2,7,6,9,5,1,4,3,8]
#print(valid(testowa,int(len(testowa)**(1/2))))