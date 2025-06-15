def usun_duplikaty(lst):
    lst_with_index = list(enumerate(lst))
    print(lst_with_index)
    lst_with_index.sort(key=lambda x: x[1])
    b= []
    prev=None
    for i in lst_with_index:
        if i[1] != prev:
            b.append(i)
        prev = i[1]

    b.sort(key=lambda x: x[0])
    return [i[1] for i in b] 

a=[1,5,6,8,5,4,3,7,8,89,2,5,4,3]
print(usun_duplikaty(a))