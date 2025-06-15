def PPN(word:str):
    perm = ''
    n = 1
    used = {}
    for i in word:
        if i not in used:
            used[i] = n
            n+=1
        perm += str(used[i]) + '-'

    perm = perm[:-1]
    return perm

print(PPN('tak'))