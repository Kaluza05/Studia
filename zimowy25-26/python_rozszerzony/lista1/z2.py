def is_palindrom(word : str)-> bool:
    word_formated = ''.join(map(lambda x : x.lower() if x.isalpha() else '', word))

    return word_formated == word_formated[::-1]

print(is_palindrom('Kobyła ma mały bok.'))
print(is_palindrom("Eine güldne, gute Tugend: Lüge nie!"))
print(is_palindrom("Míč omočím."))