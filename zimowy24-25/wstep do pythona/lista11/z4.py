from copy import deepcopy

def equivalence_relations(zbior:set[tuple])->list:
    hehe = list(zbior)
    podzialy = []
    #albo bierzemy element w podziale do pierwszego zbioru albo nie
    
    def podzial(curr_zbior:list[set],size:int)->None:
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

a = {('koalicja obywatelska',157),('pis',194),('konfa',18),('trzecia droga',65),('lewica',26)}
#print(*equivalence_relations(a),sep='\n')
for wybory in equivalence_relations(a):
    for koalicja in wybory:
        if sum([i[1] for i in koalicja]) > 194 and 'pis' not in [i[0] for i in koalicja]:
            print(koalicja)

print('\n'*5)
print('większośc konstytucyjna')
print('\n'*5)
for wybory in equivalence_relations(a):
    for koalicja in wybory:
        if sum([i[1] for i in koalicja]) > 307: #307 większosc konstytucyjna
            print(koalicja)

print('\n'*5)
print('mandaty sojuszu')
print('\n'*5)

for wybory in equivalence_relations(a):
    podzialy = []
    for koalicja in wybory:
        podzialy.append(' ,'.join([i[0] for i in koalicja]) +' : '+str(sum([i[1] for i in koalicja])))
    print(podzialy)