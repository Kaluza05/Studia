def find_min(a,l,r):
    if l-r == 0:
        return a[l]
    s = (l+r)//2
    mi1 =find_min(a,l,s)
    mi2 = find_min(a,s+1,r)
    if mi1 < mi2:
        return mi1
    else:
        return mi2
    
from random import shuffle
x = list(range(5,20))
shuffle(x)
print(x)
print(find_min(x,0,len(x)-1))