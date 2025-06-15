def hetmany (n , k , a ):
    if k == n : return poprawne (a),a
    for i in range ( n ):
        a [ k ] = i
        if hetmany (n , k +1 , a )[0]: return 1,a
    return 0,a

def poprawne(a):     #skoro na wdi to for i in range(n) for j in range(n) el = a[i] el2 = a[j]
    for i,el in enumerate(a):
        for j,el2 in enumerate(a):
            if i != j and (el == el2 or abs(i-j) == abs(el-el2)):
                return 0
    return 1

n=8
a = [0 for _ in range(n)]
print(hetmany(n,0,a))

'''
a = [-1,-1,...,-1]
a)hetmany(n,0,a)
b)poprawne(a,n)
c)normalna wersja sprawdza mniej pozycji, bo przy każdym dostawieniu hetmana sprawdzamy czy możemy go tam dostawić,
 a tutaj sprawdzamy dopiero na koncu czy ustawienie jest valid


'''