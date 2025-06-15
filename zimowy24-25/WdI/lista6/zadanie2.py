def selection_sort(a,n):
    def search_min(b,m):
        min_val = b[0]
        index = 0
        for i in range(m):
            if b[i]<min_val:
                min_val=b[i]
                index = i
        return min_val,index
    
    for i in range(n):
        to_replace = search_min(a[i:],n-i)
        if a[i] == to_replace[0]:pass
        else:
            temp = a[i]
            a[i] = to_replace[0]
            a[i+to_replace[1]] = temp

    return a

my_a = [1,3,4,6,8,7,3,5,7,4,8,9,0]
#my_a = list(range(10))
print(selection_sort(my_a,len(my_a)))