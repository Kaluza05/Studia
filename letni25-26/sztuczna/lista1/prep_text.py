ALPHABET = list('qwertyuiopasdfghjklzxcvbnmąćęłńśżźó')

with open('clear-tadeusz.txt','r', encoding= 'utf-8') as text, \
    open('clear-tadeusz-correct.txt','w', encoding= 'utf-8') as correct, \
    open('clear-tadeusz-nospace.txt','w', encoding= 'utf-8') as guess:
    p = text.read().splitlines()
    p_space = [l.strip() for l in p if l != '']
    p_no_space = [l.replace(' ','') for l in p_space]
    ans = '\n'.join(p_space)
    guess_lines = '\n'.join(p_no_space)

    correct.write(ans)
    guess.write(guess_lines)