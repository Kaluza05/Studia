from numpy.random import choice
from random import random

def gram_2(words):
    m = []
    for word in words:
        tokens = [token for token in word]
        n_grams = zip(*[tokens[i:] for i in range(3)])
        n_grams = [''.join(i) for i in n_grams]
        m += n_grams
    return m

def new_name(names:list,min_len:int = 4,max_len = 10):
    grams = gram_2(names)
    freq_dict = {}
    for i in grams:
        if i[:2] not in freq_dict:
            freq_dict[i[:2]] = {i[2]:1}
        else:
            if i[2] not in freq_dict[i[:2]]:
                freq_dict[i[:2]][i[2]] = 1
            else:
                freq_dict[i[:2]][i[2]] += 1

    a = choice(names)
    #print(a)
    word = a[:2]
    
    n=2
    while n<=max_len and (random()> 0.2 if n>=min_len else 1):
        probability = [i/sum(list(freq_dict[word[-2:]].values())) for i in list(freq_dict[word[-2:]].values())]
        next_letter = choice(list(freq_dict[word[-2:]].keys()),p=probability)
        #print(word)
        word += next_letter
        n+=1

    return word


with open('lista_imion.txt','r',encoding='utf8') as data:
    imiona = data.read().splitlines()
    for i in range(5):
        print(new_name(imiona))
