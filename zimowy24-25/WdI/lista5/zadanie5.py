'''
T(n, 0) = n dla n ≥ 0
T(0, m) = m dla m > 0
T(n, m) = T(n - 1, m) + 2T(n, m - 1) w przeciwnym przypadku.

'''

def T(n,m):
    if m==0:return n
    if n==0:return m 
    return T(n-1,m)+2*T(n,m-1)

#print(T(3,4))
#T(3,4) = 486


'''
do obliczenia T(n,m) potrzeba tylko T(n-1,m) i T(n,m-1) 
T(n-2,m) T(n-1,m-1) T(n-1,m-1) T(n,m-2)
T(3,4):
T(2,4) T(3,3)
T(1,4) T(2,3) T(2,3) T(3,2)
T(0,4) T(1,3) T(1,3) T(2,2) T(2,2) T(3,1)
T(0,3) T(1,2) T(1,2) T(2,1) T(2,1) T(3,0)
T(0,2) T(1,1) T(1,1) T(1,0)
T(1,0) T(0,1)

T(1,0)T(2,0)T(0,1)T(0,2)T(0,3)T(0,0)
T(1,1)T(2,1)T(1,2)T(1,3)
T(2,2)T(1,3)
T(2,3)

T(n,1)
T(n,0) T(n-1,1)
T(n-1,0) T(n-2,1)
T(1,m)
T(0,m)T(1,m-1)T(1,m-2)
z T(n,0) T(m,0) obliczymy T(n,1) T(1,m) z Tego T(n,2) T(2,m)
T(n,2) trzeba T(n-1,2) T(n,1)

T(1,1)
T(0,1) + T(1,0)

T(2,2)
T(1,2) T(2,1)
T(0,2) T(1,1) T(1,1) T(2,0)

T(1,0) T(2,0) T(0,1) T(0,2) T(0,3)
T(1,1) T(2,1)

wartosc w T(n,m) zależy od wartości w T(n,m-1) oraz w T(n-1,m)


'''
def fTi(n,m):#n-wiersze m-kolumny
    
    tab_2 = [i for i in range(n+1)]
    for i in range(1,m+1):
        curr_m = i
        #print(tab_2)
        #print(tab_3,i)
        for j in range(1,n+1):
            #print(curr_m)
            tab_2[j] = 2*tab_2[j] + curr_m
            curr_m = tab_2[j]
        
    return tab_2[-1]
    

print(fTi(3,4))
print(T(3,4))

def fTiter(n,m):
    tab = [[j if i == 0 else (i if j == 0 else 0) for i in range(n+1)] for j in range(m+1)]
    for i in range(1,m+1):
        for j in range(1,n+1):
            if tab[i][j] == 0 :
                tab[i][j] = tab[i][j-1] + 2*tab[i-1][j]
    print(tab)
    return tab[m][n]