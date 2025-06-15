with open('popularne_slowa2023.txt','r',encoding='utf8') as data:
    data = data.read().splitlines()
    anagram_word = {}
    for i in data:
        desc = ''.join(sorted(i))
        if desc not in anagram_word:
            anagram_word[desc] = [i]
        else:
            anagram_word[desc] += [i]
    for i in anagram_word:
        if len(anagram_word[i]) >=5:
            print(*anagram_word[i],sep='   ')