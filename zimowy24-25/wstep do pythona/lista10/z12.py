ALPHABET = 'aąbcćdeęfghijklłmnńoóprsśtuwyzźż'#'aąbcćdeęfghijklłmnńoóprsśtquwyzźż'

def cezar(word:str,k:int = 0):
    return ''.join([ALPHABET[(ALPHABET.index(letter)+k)%len(ALPHABET)] if letter in ALPHABET else letter for letter in word ])
from time import time
s = time()
with open('popularne_slowa2023.txt','r',encoding='utf8') as data:
    data = data.read().splitlines()
    data_in_set = set(data)
    max_cezar = 0
    the_word = ''
    for word in data:
        if (length := len(word))>max_cezar:
            every_cypher = [cezar(word,k) for k in range(1,len(ALPHABET))]
            for maybe_word in every_cypher:
                if maybe_word in data_in_set:
                    max_cezar = length
                    the_word = word

                    print(the_word,max_cezar)

print(time()-s)