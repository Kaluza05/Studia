def podziel(s):
    s = s.strip()
    podzielone = []
    prev = -1
    for i,el in enumerate(s):
        if el == ' ':
            podzielone.append(s[prev+1:i])
            prev=i
    podzielone.append(s[prev+1:])
    return podzielone

wyraz = " Ala ma kota "
print(podziel(wyraz))
print(wyraz.split())