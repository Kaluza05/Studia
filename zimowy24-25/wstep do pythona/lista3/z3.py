def usun_w_nawiasach(word):
    if ('(' or ')') not in word:
        return word
    left = word.index('(')
    right = word.index(')')
    return usun_w_nawiasach(word[:left]+word[right+1:])

print(usun_w_nawiasach('Ala ma kota (lalalala)'))
print(usun_w_nawiasach('Ala ma (psa oraz )kota (lalalala)'))
print(usun_w_nawiasach('Ala ma kota'))