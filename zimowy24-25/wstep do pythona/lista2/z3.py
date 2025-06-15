from math import sqrt
def kolko(n):
    a = [[' ' for _ in range(n)] for _ in range(n)]
    for x in range(n//2,n): #ta pętla robi połowę kola
        y = round(sqrt((n/2)**2-(x-n/2)**2)+n/2)
        y_range = {y,n-y}
        for j in range(min(y_range),max(y_range)):
            a[j][x] = '*'
    new_a = []
    for i in range(n): #ta kopiuje tą połowę na pierwszą część koła (w tą stronę lepiej się zaokrąglało)
        copy_side = a[i][n//2 +1:]
        new = copy_side[::-1] + ['*'] + copy_side
        new_a.append(new)
    circle = [''.join(new_a[i][j] for j in range(n)) for i in range(n)]
    return circle 
    

    
    
def balwan(n):
    def popraw(k,n): #dodaje spacje żeby kola leżały na sobie
        a=kolko(k)
        nowe = []
        for i in range(k):
            ze_spacjami = ' '*((n-k)//2) + a[i]
            nowe.append(ze_spacjami)
        return '\n'.join(nowe)
    print(popraw(n-4,n))
    print(popraw(n-2,n))
    print(popraw(n,n))

#balwan(9)
#print()

def kulka2(n,k):#nieparzyste najlepiej
    for i in range(n):
        print(' '*((k-n)//2),end='')
        for j in range(n):
            mid = (n-1)/2
            if  (mid+1/2)**2 >= (i-mid)**2 + (j-mid)**2:
                print('*',end='')
            else:
                print(' ',end='')
        print('\n',end='')


def balwan2(n):
    kulka2(n-4,n)
    kulka2(n-2,n)
    kulka2(n,n)

balwan2(9)