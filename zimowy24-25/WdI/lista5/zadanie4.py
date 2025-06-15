max_num = 1000
def G(n):
    tab = [1,1,1]
    for i in range(3,n+1):
        tab[i%3] = tab[0]+tab[1]+tab[2]
    return tab[n%3]

#O(n)
#pamięciowa O(1)- 3 miejsca w tablicy
#print(G(4))

import numpy as np
mat1 = np.array([[1,1,1],[1,0,0],[0,1,0]])
def potegowanie_mac(mat,n):
    if n==0:
        return np.array([[1,0,0],[0,1,0],[0,0,1]])
    if n%2==0:
        return potegowanie_mac(np.matmul(mat,mat,dtype=np.int64),n//2)
    else:
        return np.matmul(mat,potegowanie_mac(np.matmul(mat,mat,dtype=np.int64),(n-1)//2))
print(mat1)
n=50
print(G(n))
#działa dla małych n
print(np.matmul(potegowanie_mac(mat1,n),[[1],[1],[1]],dtype=np.int64)[2][0])