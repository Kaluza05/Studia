def układalne(given_word:str,to_get:str)->bool:
    given_dict = {i:given_word.count(i) for i in given_word}
    to_get_dict = {i:to_get.count(i) for i in to_get}
    for i in to_get_dict.keys():
        if i not in given_dict: return False
        if given_dict[i] - to_get_dict[i] < 0: return False
        return True
    print(given_dict)

print(układalne('lokomotywa','kot'))