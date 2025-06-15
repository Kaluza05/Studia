import random
from collections import defaultdict as dd

pol_ang = dd(lambda:[])

with open('pol_ang.txt','r',encoding='utf8') as data:
    data = data.read().splitlines()
    for x in data:
        x = x.strip()
        L = x.split('=')
        if len(L) != 2:
            continue    
        pol, ang = L
        pol_ang[pol].append(ang)
    

def tlumacz(polskie):
    wynik = []
    for s in polskie:
        if s in pol_ang:
            most_used = sorted(pol_ang[s],key = lambda x: (popularity_dict[x] if x in popularity_dict else 0),reverse=True)
            max_freq = popularity_dict[most_used[0]]
            print(popularity_dict['miss'],popularity_dict['girl'])
            to_use = []
            for i in most_used:
                if popularity_dict[i] < max_freq:
                    break
                else:
                    to_use.append(i)
            wynik.append(random.choice(to_use))
        else:
            wynik.append('[?]')
    return ' '.join(wynik)
           
            
with open('brown.txt','r',encoding='utf8') as data:
    data = data.read().split()
    popularity_dict = {}
    for i in data:
        if i in popularity_dict:
            popularity_dict[i] += 1
        else:
            popularity_dict[i] = 1

zdanie = 'chłopiec z dziewczyna pójść do kino'.split()

for i in range(15):
    print (tlumacz(zdanie))     