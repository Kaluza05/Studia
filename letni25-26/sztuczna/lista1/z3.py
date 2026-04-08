"""
we start by creating a Prefix tree using the reversed words from a given dictionary.
Then we iterate over the text "walking back" by going down the Trie and find a word to add with biggest sum of squares
we keep track of "sentences" cache[i] which are valid splits using spaces.
Then we also remember the length of the longest word that we added,
And do it for all i = 1,..,len(text).
Then we reconstruct the text with spaces by walking back from that list of word length, ex.

alamakota cas word length list of [1,0,3,0,2,0,0,0,4] which when going back
takes 4 last letters and jump by 4 then takes 2 letters and jumps by 2 then
takes 3 letters and jump by 3 and ends the algorith.
"""

class Trie:
    def __init__(self,dictionary, start_word = '', word_exists = False):
        self.root = start_word
        self.existing_word = word_exists
        self.nodes = dict()
        self._create_trie(dictionary)

    def _create_trie(self,dictionary):
        if dictionary is None or dictionary == []:
            return
        
        first_w = dictionary[0]
        curr_letter = first_w[0]
        new_dict = []
        for w in dictionary:
            if w[0] != curr_letter:
                new_start = self.root + curr_letter
                self.nodes[curr_letter] = Trie(new_dict, start_word= new_start, word_exists= len(first_w) == 1)
                first_w = w
                curr_letter = w[0]
                new_dict = []

            
            if w[1:] == '':
                continue
            else:
                new_dict.append(w[1:])

        new_start = self.root + curr_letter
        self.nodes[curr_letter] = Trie(new_dict,start_word= new_start, word_exists= len(first_w) == 1)

        return
    
    def __str__(self, level=0):
        indent = '  ' * level
        make = indent + 'root ' + self.root + ' e? '+ ('yes' if self.existing_word else 'no')+ '\n'

        for i, (key, node) in enumerate(self.nodes.items()):
            make += indent + f"{i} {key}\n"
            make += node.__str__(level + 1)

        return make
    
    def __iter__(self):
        return iter(self.nodes)



from time import time

def find_split(text, trie):
    

    temp_trie = trie
    cache = [-1 for _ in range(len(text))]
    cache[0] = 0
    longest = [0]*len(text)
    for i in range(0,len(text)):
        k = 0
        max_val = float('-inf')
        longest_word = 0
        while i >= k:
            letter = text[i-k]

            if letter in temp_trie:
                temp_trie = temp_trie.nodes[letter]
                
                if temp_trie.existing_word:
                    if k == i:
                        if (1+k)**2 > max_val:
                            longest_word = k+1
                            max_val = (1+k)**2
                            
                    else:
                        if i == 0:
                            max_val = 1
                            longest_word = 1
                            
                        elif cache[i-k-1] + (1+k)**2 > max_val:
                            longest_word = k+1
                            max_val = cache[i-k-1] + (1+k)**2
                            
                
                k += 1
                
                
            else:
                break

        temp_trie = trie
        longest[i] = longest_word
        cache[i] = max_val
        
    
    return cache,longest

def find_different_splits(text, trie):
    #finds the amount the text could be reconstructed into

    temp_trie = trie
    cache = [-1 for _ in range(len(text))]
    cache[0] = 0
    possible_word_lens = [0]*len(text)
    for i in range(0,len(text)):
        k = 0
        total_splits = 0
        possible_len = []
        while i >= k:
            letter = text[i-k]

            if letter in temp_trie:
                temp_trie = temp_trie.nodes[letter]
                
                if temp_trie.existing_word:
                    if k == i: #whole text up to that len is a word
                        total_splits += 1
                        possible_len.append(k+1)
                            
                    else:
                        if i == 0:
                            total_splits += 1
                            possible_len.append(1)

                            
                        if cache[i-k-1] != 0:
                            total_splits += cache[i-k-1]
                            possible_len.append(k+1)
                            
                            
                
                k += 1
                
                
            else:
                break


        temp_trie = trie
        possible_word_lens[i] = possible_len
        cache[i] = total_splits
        
    
    return cache,possible_word_lens


def reconstruct(text,word_lengths):
    # print(text,word_lengths)
    # print(list(text))
    if word_lengths[-1] == 0:
        return ''
    n = len(text)
    result_word = ''
    cut_off = 0
    
    while cut_off < n:
        # if text == 'pościanachwtejkomnaciemieszkaniekobice':
            # print(cut_off)
        k = word_lengths[n-1-cut_off]
    
        word = text[n-k-cut_off:n - cut_off]
        result_word =  word + ' ' + result_word 

        cut_off += k

    result_word = result_word[:-1] #deleting the last space
    return result_word

def get_random_split(text,amounts,back_up):
    from random import choices
    if amounts[-1] == 0:
        return ''
    n = len(text)
    result_word = ''
    cut_off = 0
    while cut_off < n:
        total_weights = 0
        weights = []
        for b_up in back_up[n-1-cut_off]:
            weight = amounts[n-1-cut_off-b_up]
            total_weights += weight
            weights.append(weight)

        weights = [w/total_weights for w in weights]

        back_by = choices(back_up[n-1-cut_off],weights)[0]
        word = text[n-back_by-cut_off:n - cut_off]
        result_word = word + ' ' + result_word
        cut_off += back_by
    
    result_word = result_word[:-1] #deleting the last space
    return result_word
    
with open('pracownia1/words_for_ai1.txt', 'r', encoding='utf-8') as f:
    dictionary = [line.strip() for line in f if len(line) != 0]

    st = time()
    rev_dict = sorted([w[::-1] for w in dictionary])
    
    text_split = ''
    trie = Trie(rev_dict) #trie on reversed words to basicaly go back
    print('finishes building trie', time() - st)

# text = 'tamatematykapustkinielubi'
# _,l1 = find_split(text,trie)
# c,l = find_different_splits(text,trie)
# print(c)
# print(l)
# print(get_random_split(text,c,l))
# print(l1)
# print(reconstruct(text,l1))

with open('lista1/clear-tadeusz-nospace.txt','r', encoding='utf-8') as inp, \
    open('lista1/clear-tadeusz-correct.txt','r', encoding='utf-8') as answers:
    max_correct = 0
    random_correct = 0
    total = 0
    for line,ans in zip(inp,answers):
        # if total % 10**2 == 0:
            # print(total,line,ans)
        line = line.strip()
        ans = ans.strip()

        c1,l1 = find_split(line,trie)
        c,l2 = find_different_splits(line,trie)
        # print(c1,l1,c,l2)
        max_len = reconstruct(line,l1)
        random_rec = get_random_split(line,c,l2)
        
        if max_len == ans:
            max_correct += 1
        if random_rec == ans:
            random_correct += 1

        total += 1

    print(total,max_correct,random_correct,max_correct/total,random_correct/total)
    

