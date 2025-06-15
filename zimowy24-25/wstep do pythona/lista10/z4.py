from copy import deepcopy


def monotone(N,A,B):
    if N == 0:
        return [[]]
    
    bez_najwiekszego = monotone(N-1,A,B-1)
    z_najwiekszym = [[B]+subset for subset in bez_najwiekszego]
    return bez_najwiekszego+z_najwiekszym

def strictly_decreasing(N,A,B):
    def inside(n,b,curr:list=[]):
        if n == 0 or b==A-1:
            wynik.append(curr[:])
            return
        curr.append(b)
        inside(n-1,b-1,curr)
        curr.pop()
        inside(n-1,b-1)

    wynik = []
    inside(N,B)
    return wynik

def just_dectreasing(N,A,B):
    def inside(B,curr:list=[]):#lista pusta moze byc zmodyfikowana jako wartosc domysla
        if len(curr) == N or B==A-1:
            wynik.append(curr[:])
            return
        
        curr.append(B)
        inside(B,curr)

        curr.pop()
        inside(B-1,curr)
    
    wynik = []
    inside(B)
    return wynik

def perms(tab):
    def generate_perms(index,curr_sum:int):
        if index == len(tab):
            wynik.add(curr_sum)
            return
        
        curr_sum += tab[index]
        generate_perms(index+1,curr_sum)

        curr_sum -= tab[index]
        generate_perms(index+1,curr_sum)
    
    wynik = set()
    generate_perms(0,0)
    return wynik

def perms_comp(tab):
    if not tab:
        return [[]]
    
    bez_pierwszego = perms_comp(tab[1:])
    z_pierwszym = [[tab[0]] + bez for bez in bez_pierwszego]
    return bez_pierwszego+z_pierwszym

def equivalence_relations(zbior:set):
    hehe = list(zbior)
    podzialy = []
    #albo bierzemy element w podziale do pierwszego zbioru albo nie
    
    def podzial(curr_zbior:list[set],size:int):
        if size == len(zbior):
            #print(curr_zbior[:])
            podzialy.append(deepcopy(curr_zbior)) #curr_zbior[:] | [set(s) for s in curr_zbior]
            return
        elem = hehe[size]
        for i in range(len(curr_zbior)):
            curr_zbior[i].add(elem)
            podzial(curr_zbior,size+1)
            curr_zbior[i].remove(elem)
        
        curr_zbior.append({elem})
        podzial(curr_zbior,size+1)
        curr_zbior.pop()


    podzial([],0)
    return podzialy

lis = [4,2,8]
#print(perms(lis))
#print(perms_comp(lis))
#print(*monotone(*lis),sep='\n')
print(*just_dectreasing(*lis),sep='\n')


#print(*equivalence_relations(lis),sep='\n')