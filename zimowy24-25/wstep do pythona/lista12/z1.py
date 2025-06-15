ALPHABET = 'aąbcćdeęfghijklłmnńoóprsśtuwvyzżź'

def words_from_word(org_word,aval_words):
    ans = []
    for i in range(len(org_word)):
        for letter in ALPHABET:
            check = org_word[:i] + letter + org_word[i+1:]
            if check in aval_words:
                ans.append(check)
    return ans
from random import choice
def word_to_word_bfs(start,end):
    if len(start)!=len(end): return None
    aval_words = {i for i in slowa if len(i)==len(start)}
    visited = [(start,[start])]
    checked = []
    while visited:
        i = visited[0]
        visited = visited[1:]
        checked.append(i[0])
        [visited.append((j,i[1]+[j])) for j in words_from_word(i[0],aval_words) if j not in checked]
        for j in visited:
            if end == j[0]:
                return j[1]
    return



with open('popularne_slowa2023.txt','r',encoding='utf8') as data:
    slowa = data.read().splitlines()

print(word_to_word_bfs('woda','wino'))