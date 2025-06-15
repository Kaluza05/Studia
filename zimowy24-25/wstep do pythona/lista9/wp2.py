liczby = {

      1 : 'jeden',

      2 : 'dwa',

      3 : 'trzy',

      4 : 'cztery',

      5 : 'pięć',

      6 : 'sześć',

      7 : 'siedem',

      8 : 'osiem',

      9 : 'dziewięć',

      10 : 'dziesięć',

      11 : 'jedenaście',

      12 : 'dwanaście',

      13 : 'trzynaście',

      14 : 'czternaście',

      15 : 'piętnaście',

      16 : 'szesnaście',

      17 : 'siedemnaście',

      18 : 'osiemnaście',

      19 : 'dziewiętnaście',

      20 : 'dwadzieścia',

      30 : 'trzydzieści',

      40 : 'czterdzieści',

      50 : 'pięćdziesiąt',

      60 : 'sześćdziesiąt',

      70 : 'siedemdziesiąt',

      80 : 'osiemdziesiąt',

      90 : 'dziewięćdziesiąt',

      100 : 'sto',

      200 : 'dwieście',

      300 : 'trzysta',

      400 : 'czterysta',

      500 : 'pięćset',

      600 : 'sześćset',

      700 : 'siedemset',

      800 : 'osiemset',

      900 : 'dziewięćset'

    }

def text_from_num(N):
    if N<=20:
        return liczby[N]
    if N<100:
        return liczby[10*(N//10)] +' '+ liczby[N%10]
    return liczby[100*(N//100)] + ' ' + text_from_num(N%100)
    
    
def word(lst):
    new = ''
    for i in lst:
        try:
            i = int(i)
            new += text_from_num(i)
        except:
            new +=i
        new += ' '
    return new

a = ['Ala', 'ma', 357, 'kotów', 'i', '213', 'kanarków']
print(word(a))