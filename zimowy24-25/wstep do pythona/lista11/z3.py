class Komitet:
    def __init__(self,nazwa,glosy):
        self.nazwa = nazwa
        self.glosy = glosy

def iloraz(komitet:Komitet,M:int):
    return [{komitet.nazwa:komitet.glosy/i} for i in range(1,M+1)]

komitety = []
ilorazy = []
M=100
for i in komitety:
    ilorazy += iloraz(i,M)