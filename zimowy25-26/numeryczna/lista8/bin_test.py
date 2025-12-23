def bin_s(xs,x):
    a = 0
    b = len(xs) - 1
    if x >= xs[b]:
        return b
    elif x <= xs[a]:
        return 0
    
    while xs[a] < x <  xs[b]:
        m = (b + a) // 2 

        if xs[m] > x:
            b = m
        elif xs[m] < x:
            a = m +1
        else:
            return m
        
    
    return a


xs1 = [1,4,8,11,15]
xs2 = [1,2,3,4]
x = [i/2 for i in range(0,12,1)]
print(x)
print(bin_s(xs1,6))
#print(*[(i,bin_s(xs1,i), xs1) for i in x], sep = '\n')

#print(*[(i,bin_s(xs2,i), xs2) for i in x], sep = '\n')