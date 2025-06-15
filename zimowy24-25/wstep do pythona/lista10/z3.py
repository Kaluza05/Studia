from random import randint

def make_puzzle(lamigowka:str)->dict:
    word1,_,word2,_,solution = lamigowka.split()
    letters_in_use = {i:0 for i in word1+word2+solution}
    if len(letters_in_use) >= 10: return {}
    return find_combination(word1,word2,solution,letters_in_use)

def word_to_num(word:str,word_num_dict:dict)->int:
    return sum([word_num_dict[el]*10**i for i,el in enumerate(word[::-1])])

def find_combination(word1:str,word2:str,sol:str,digit_dict:dict,curr_letter:int = 0)->set:
    if curr_letter == len(digit_dict):
        if valid(word1,word2,sol,digit_dict):return digit_dict
        return {}
    letter = list(digit_dict.keys())[curr_letter]
    for i in range(0,10):
        digit_dict[letter] = i
        if repeating(digit_dict,curr_letter): continue
        if find_combination(word1,word2,sol,digit_dict,curr_letter+1): return digit_dict
    return {}

def repeating(digit_dict:dict,place:int)->bool:
    used = set()
    for i in list(digit_dict.values())[:place]:
        if i not in used: used.add(i)
        else: return True
    return False

def valid(word1,word2,sol,digit_dict)->bool:
    nums = list(map(lambda x:word_to_num(x,digit_dict),[word1,word2,sol]))
    if (nums[0]+nums[1] == nums[2]
    and (0 not in [digit_dict[i[0]] for i in [word1,word2,sol]])
    and not repeating(digit_dict,len(digit_dict))): return True
    return False

from time import time
st = time()
print(make_puzzle('send + more = money'))
print(time()-st)