def wieza(n:int)->list:
    wieza = []
    for i in range(n):
        for _ in range(3):
            wiersz = ' '*((n-i))+'*'*(2*i+1)+' '*((n-i))
            wieza.append(wiersz)
    return wieza[::-1]


def wieze(lista:list[int])->list:
    wieze = [wieza(num) for num in lista]
    c = []
    for i in range(len(max(wieze))):
        b = ' '.join(wieze_wiersz[i] if i<len(wieze_wiersz) else ' '*(len(wieze_wiersz[0])) for wieze_wiersz in wieze)
        c.append(b)
    return c[::-1]


print(*wieze([2,5,3]),sep='\n')