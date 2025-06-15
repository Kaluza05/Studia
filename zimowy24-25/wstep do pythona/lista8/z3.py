def two_word_perm(text:str)->str:
    to_perm = text.lower().replace(' ','')
    texts_len = len(to_perm)
    perm_count = {i:to_perm.count(i) for i in to_perm}

    avalible_words = [i for i in slowa if układalne(to_perm,i)]
    same_len = {}
    
    min_range,max_range = (len(min(avalible_words,key=len)),len(max(avalible_words,key=len))+1)
    for i in range(min_range,max_range+1):
        same_len[i] = [word for word in avalible_words if len(word) == i]
    for i in range(min_range,(max_range+1+min_range)//2 +1):
        if texts_len - i >= min_range and texts_len-i <= max_range:
            every_combination = cartesian_prod(same_len[i],same_len[texts_len-i],perm_count)
            print(*every_combination,sep='\n')
    return every_combination

def cartesian_prod(A,B,word_c):
    pairs = set()
    for a in A:
        for b in B:
            bool_val = True
            a_c = {i:list(a).count(i) for i in a}
            b_c = {i:list(b).count(i) for i in b}
            for i in word_c:
                a_cnt = a_c[i] if i in a_c else 0
                b_cnt = b_c[i] if i in b_c else 0
                if word_c[i]-a_cnt-b_cnt != 0:
                    bool_val = False
                    break
            if bool_val:
                pairs.add((a,b))
                        
    return pairs

def układalne(given_word:str,to_get:str)->bool:
    given_dict = {i:given_word.count(i) for i in given_word}
    to_get_dict = {i:to_get.count(i) for i in to_get}
    for i in to_get_dict.keys():
        if i not in given_dict.keys(): return False
        if given_dict[i] - to_get_dict[i] < 0: return False
    return True


with open('popularne_slowa2023.txt','r',encoding='utf8') as data:
    slowa=set(data.read().splitlines())


#two_word_perm('Bolesław Prus')
#two_word_perm('Czesław Miłosz')
two_word_perm('Kamil Kałużny')