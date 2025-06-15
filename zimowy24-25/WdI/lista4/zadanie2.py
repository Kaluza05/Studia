from test_print import test

#złożoność O(log(a+b))
def nwd(a,b):
    if a<b:
        a= a+b
        b = a-b
        a= a-b

    if b==0 or a==b:
        return a 
    return nwd(a%b,b)

def nww(a,b):
    return (a*b)//nwd(a,b)

def uprosc(a,b):
    dzielnik = nwd(a,b)
    return f'{a//dzielnik}/{b//dzielnik}'


if __name__ == '__main__':
    for i,j in zip([1,496,67,48,9,77,76],[1,22,464,7896,74,853,2]):
        #test(nwd,i,j)
        #test(nww,i,j)
        test(uprosc,i,j)