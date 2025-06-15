def krzyzyk(n,k):
    typ1 = (' '*k + '#'*k)*n
    typ2 = ('#'*k + ' '*k)*n
    war1 = (typ1 + '\n')*k
    war2 = (typ2 + '\n')*k
    krzyz = (war1 + war2)*n
    print(krzyz)


krzyzyk(4,3)