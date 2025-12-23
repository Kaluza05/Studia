from random import sample

def uprosc_zdanie(tekst, dl_slowa, liczba_slow):
    words = tekst.split(' ')
    words_short = [word for word in words if len(word) <= dl_slowa]

    if len(words_short) > liczba_slow:
        get_indecies = sorted(sample(range(len(words_short)), liczba_slow))
        return list(words_short[i] for i in get_indecies)
    
    return words_short

zdanie = "Podział peryklinalny inicjałów wrzecionowatych \
kambium charakteryzuje się ścianą podziałową inicjowaną \
w płaszczyźnie maksymalnej."

print(uprosc_zdanie(zdanie, 10, 5))