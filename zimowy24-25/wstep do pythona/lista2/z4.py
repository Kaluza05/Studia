from duze_cyfry import daj_cyfre

def liczba(n):
    do_wypisania = [daj_cyfre(int(i)) for i in str(n)]
    for num in range(5):
        print(' '.join([do_wypisania[i][num] for i in range(len(do_wypisania))]))
liczba(314159265)