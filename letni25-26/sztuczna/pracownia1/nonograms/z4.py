"""
we are doing a sliding window on the string bin.
First we look how many 1's are in the first n characters, then we "slide" the window keeping track of the numer
of ones in that window, and keeping track of the maximum amount of ones to occur in a window.
Then numer of moves is just 
(n - max_ones) - representing how many 0's we need to flip to 1 in that window and
(all_ones - max_ones) - representing how many 1's outside of that window we need to flip
"""

def opt_dist(bin: str, n: int):
    if n > len(bin):
        return 'impossible'
    
    all_ones = bin.count('1')
    ones_in = bin[:n].count('1')
    k = n
    max_ones = ones_in
    while k < len(bin):
        # print('max_ones', max_ones)
        # print(k,k-n+1)
        ones_in += int(bin[k]) - int(bin[k-n])
        k += 1
        if ones_in > max_ones:
            max_ones = ones_in
    
    return (n - max_ones) + (all_ones - max_ones)




with open('zad4_input.txt','r') as f:
    inputs = []
    for i in f.read().splitlines():
        a,b = i.split()
        inputs.append((a,int(b)))
    

answers = '\n'.join(str(opt_dist(block,l)) for block,l in inputs)

with open('zad4_output.txt','w') as f:
    f.write(answers)