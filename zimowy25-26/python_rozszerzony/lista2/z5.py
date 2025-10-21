from string import punctuation

def kompresja(text):
    compressed = []
    prev_letter = text[0]
    curr_count = 0

    for letter in text:
        if letter != prev_letter:
            compressed.append((curr_count, prev_letter))
            prev_letter = letter
            curr_count = 1
        else:
            curr_count += 1
    
    compressed.append((curr_count, letter))

    return compressed


def dekompresja(tekst_compressed):
    return ''.join(letter * count for count, letter in tekst_compressed)


pol_path = "C:\\Users\\kkalu\\python programy\\lalka-tom-pierwszy.txt"
with open(pol_path, 'r', encoding= 'utf-8') as pol:
    pol = ''.join(i for i in pol.read() if i not in punctuation)
    b = kompresja(pol)
    print(len(b) / len(pol), len(b), len(pol))

word = 'suuuper'
a = kompresja(word)
print(word)
print(a)
print(dekompresja(a))