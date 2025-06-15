def krzyzyk(n):
    typ1 = ' '*n+'*'*n+' '*n+'\n'
    typ2 = '*'*(3*n)+'\n'
    print(typ1*n+typ2*n+typ1*n)

krzyzyk(5)