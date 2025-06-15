def three_word_perm(text:str)->str:
    to_perm = text.lower().replace(' ','')

    avalible_words = {i for i in slowa if układalne(to_perm,i)}
    same_len = {}
    min_range,max_range = (len(min(avalible_words,key=len)),len(max(avalible_words,key=len))+1)
    for i in range(min_range,max_range+1):
        same_len[i] = {word for word in avalible_words if len(word) == i}

    for i in avalible_words:
        new_input = word_diff(to_perm,i)
        new_av = {i for i in avalible_words if układalne(new_input,i)}
        for j in new_av:
            newer = word_diff(new_input,j)
            new_av2 = {i for i in new_av if układalne(newer,i)}
            for k in new_av2:
                newest = word_diff(newer,k)
                
                if word_diff(to_perm,i+j+k) == '' and len(to_perm) == len(i+j+k):
                    print(i,j,k)

def word_diff(A,B):
    a_c = {i:list(A).count(i) for i in A}
    b_c = {i:list(B).count(i) for i in B}
    new_c = {}
    #print(a_c,b_c)
    for i in A:
        b_cnt = b_c[i] if i in b_c else 0
        new_c[i] = a_c[i]-b_cnt
    #print(new_c)
    
    return ''.join([i*new_c[i] if new_c[i]>=1 else '' for i in new_c])

def układalne(given_word:str,to_get:str)->bool:
    given_dict = {i:given_word.count(i) for i in given_word}
    to_get_dict = {i:to_get.count(i) for i in to_get}
    for i in to_get_dict.keys():
        if i not in given_dict.keys(): return False
        if given_dict[i] - to_get_dict[i] < 0: return False
    return True


with open('popularne_slowa2023.txt','r',encoding='utf8') as data:
    slowa=set(data.read().splitlines())


#three_word_perm('Bolesław Prus')
#two_word_perm('Czesław Miłosz')
three_word_perm('Kamil Kałużny')

'''

    for i in range(min_range,max_range+1):
        same_len[i] = {word for word in avalible_words if len(word) == i}
    for i in range(min_range,(max_range+1+min_range)//2 +1):
        if texts_len - i >= 2*min_range and texts_len-i <= max_range-min_range:
            
            for s in same_len[i]:
                #print(to_perm,s)
                new_input = word_diff(to_perm,s)
                new_avalible_words = {k for k in avalible_words if układalne(new_input,k)}
                #print(new_input,same_len[i])
                for check_3 in new_avalible_words:
                    
                    if word_diff(new_input,check_3) == '':
                        print(s,check_3)
                        #print(check_3,new_input)
'''