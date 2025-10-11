def sudan(n,x,y):
    assert x >= 0
    cache = dict()
    def F(n,x,y):
        if (n,x,y) in cache:
            return cache[(n,x,y)]
        if n == 0:
            output = x + y
            cache[(n,x,y)] = output
            return output
        if y == 0:
            output = x
            cache[(n,x,y)] = output
            return output
        else:
            output = F(n-1, F(n, x, y-1),F(n, x, y-1)+y)
            cache[(n,x,y)] = output
            return output
    
    return F(n,x,y)

print(sudan(2,5,2))
#(2,5,2) (3,1,1)
#biggest that work for me without reaching recursion depth