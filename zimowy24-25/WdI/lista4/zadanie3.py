from zadanie2 import nwd
wywolania = 0
#złożoność O(log_2(suma numbers))
def nwd_mult(numbers):
    if len(numbers) == 2:
        return nwd(*numbers)
    
    a,*b = numbers
    return nwd(a,nwd_mult(b))

'''
poprawność
jeśli d|(a_1,a_2,...,a_n)
to d | (a_2,...,a_n) oraz d|a_1
czyli d dzieli nwd(a_2,...,a_n)
zatem nwd(a_1,a_2,...,a_n) = nwd(a_1,nwd(a_2,...,a_n))
'''