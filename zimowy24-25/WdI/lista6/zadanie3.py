def bubble_sort(a,n):
    for i in range(n):
        for j in range(n-1-i):
            if a[j]<=a[j+1]:
                pass
            else:
                temp = a[j]
                a[j] = a[j+1]
                a[j+1] = temp
    return a

my_a = [1,3,4,6,8,7,3,5,7,4,8,9,0]
print(bubble_sort(my_a,len(my_a)))