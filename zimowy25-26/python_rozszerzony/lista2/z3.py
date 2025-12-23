from string import ascii_lowercase

ALPHABET = ascii_lowercase

def get_freq(text : str, alphabet : str = ALPHABET):
    """
    from a text gets a frequency of all letters in that text
    """

    freq_dict = {i : text.count(i) for i in alphabet}
    total_alphabet = sum(freq_dict.values())
    freq_dict = {key : val / total_alphabet for key, val in freq_dict.items()}
    return freq_dict

def get_freqs_langs(texts : dict[str,str], alphabet : str = ALPHABET):
    """
    gets frequencies for different languages, but each language can contain only one text
    """
    return {lang : get_freq(texts[lang], alphabet = alphabet) for lang in texts}
        

def MSE_dict(d1, d2):
    """we assume that both dictionaries have the same keys"""
    return sum((d1[key] - d2[key])**2 for key in d1)


def guess_lang(text, lang_freqs, alphabet = ALPHABET):
    text_freq = get_freq(text, alphabet= alphabet)

    mses_for_langs = {}
    for lang in lang_freqs:
        mses_for_langs[lang] = MSE_dict(text_freq, langs_freqs[lang])

    print(mses_for_langs)
    return min(mses_for_langs, key= lambda x : mses_for_langs[x])


pol_path = "C:\\Users\\kkalu\\python programy\\lalka-tom-pierwszy.txt"
eng_path = "C:\\Users\\kkalu\\python programy\\brown.txt"

with open(pol_path, 'r', encoding= 'utf-8') as pol, open(eng_path, 'r', encoding= 'utf-8') as eng:
    pol = pol.read()
    eng = eng.read()
    langs_freqs = get_freqs_langs({'eng': eng, 'pl' : pol})


print(guess_lang('ala ma kota.', lang_freqs= langs_freqs, alphabet= ALPHABET))