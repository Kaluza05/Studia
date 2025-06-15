def longest_non_polish_sentence(text):
    words = text.split()
    l=0
    r=1
    cur_best = (1,0)
    while r<len(words):
        #print(words[r],check_polish(words[r]))
        #print(l,r)
        if check_polish(words[r]) == True:
            l=r+1
        else:
            #print(r,l,'lepsze?',cur_best)
            if r-l > cur_best[0]-cur_best[1]:
                cur_best = (r,l)
        r+=1
    #print(cur_best)
    return words[cur_best[1]:cur_best[0]+1]

def check_polish(word):
    if set(word) & set('ąęółńćżźśćń') == set() and word in slownik:
        return False
    return True

#print(longest_non_polish_sentence('Ala ma kota ale nie ma łabędzia jaka szkoda a b c d e f g'))
with open('lalka-tom-pierwszy.txt','r',encoding='utf8') as data, open('popularne_slowa2023.txt','r',encoding='utf8') as slownik:
    #print(data)
    slownik = set(slownik.read().splitlines())
    text = data.read()
    print(longest_non_polish_sentence(text))