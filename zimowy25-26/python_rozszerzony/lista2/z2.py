from sys import setrecursionlimit

with open("magic.txt",'r') as file: 
    MAGIC_NUMBER = int(file.read())

setrecursionlimit(MAGIC_NUMBER)

def sudan(n,x,y):
    assert x >= 0
    used = 0
    cache = dict()
    def F(n,x,y):
        nonlocal used
        if (n,x,y) in cache:
            used += 1
            return cache[(n,x,y)]
        
        elif n == 0:
            output = x + y
            
        elif y == 0:
            output = x
        else:
            help = F(n, x, y-1)
            output = F(n-1, help,help+y)

        cache[(n,x,y)] = output
        return output
    
    return F(n,x,y), used


def F1(n,x,y):
    if n == 0:
        output = x + y
        
    elif y == 0:
        output = x
    else:
        help = F1(n, x, y-1)
        output = F1(n-1, help,help+y)

    return output

#print(sudan(2,9,2))


#print(sudan(2,11253,1))
print(F1(2,9,2))
#(2,5,2) (3,1,1)
#biggest that work for me without reaching recursion depth
