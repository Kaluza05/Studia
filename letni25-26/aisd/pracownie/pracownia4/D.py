#1
def best_train_quality(ls : list[int]):
    n = len(ls)
    
    print('orginal', ls)

    ends = [1] * n
    starts = [1] * n


    for i in range(1,n):
        if ls[i-1] < ls[i]:
            ends[i] = ends[i-1] + 1

    for i in range(n-2,-1,-1):
        if ls[i] < ls[i+1]:
            starts[i] = starts[i+1] + 1

    print('starts ',starts)
    print('ends   ', ends)

    best_q = 1
    for i in range(n):
        for j in range(i+1,n):
            if ls[i] < ls[j]:
                best_q = max(best_q,ends[i] + starts[j])

    return best_q


# print(best_train_quality([ 5, 3, 4, 9, 2, 8, 6, 7, 1]))
# print(best_train_quality([1, 2, 3, 10, 4, 5, 6]))
# print(best_train_quality([3, 3, 3]))
# print(best_train_quality([4, 3, 2, 1]))

#2
def bin_search(xs,v):
    #chcemy najwieksze i  t.ze v > xs[i]
    l = 0
    r = len(xs) - 1
    i = -1 

    while l <= r:
        m = (l + r) // 2 

        if xs[m] < v: # szuakmy na prawo
            i = m
            l = m + 1
        else:
            r = m - 1
    

    return i


def btq_2(ls : list[int]):
    """
    trzymamy tablice - dla kazdej dlugosci najmniejsza mozliwa koncówka dla subsequence dł i + 1
    binary searchujemy po tej tablicy (jest ona posortowana) i rozwazamy ten wynik jako max
    
    """
    print(ls)
    ds = [ls[0]]
    n = len(ls)

    max_quality = 1

    l = 0
    r = 0
    while l < n and r < n:
        #zwiekszamy subsequence
        while r+1 < n and ls[r] < ls[r + 1]:
                r += 1

        # print(l,r)
        # print(ls[l:r+1])
        #czyli teraz juz powiekszylismy na ile moglismy
        #wyszukujemy bin_searchem najmnijeszego i z ds ktore nam pasuje

        # print('current i: ', i, ls[i-1],ls[i])
        # print(curr_subarray_len,len(ds),ds)
        curr_subarray_len = 1
        while l < r:
            #dla tego l teraz najlepsze co da sie osiagnac to pos + 1 + r - l + 1
            pos = bin_search(ds,ls[l])
            if pos !=  -1:
                max_quality = max(max_quality, pos + 1 + r - l + 1)


            l += 1
            curr_subarray_len += 1

            if curr_subarray_len == len(ds) + 1:
                ds.append(ls[l])
            else:
                ds[curr_subarray_len-1] = min(ds[curr_subarray_len-1], ls[l])

        l = l + 1
        r = r + 1

        # ds musi być zrobione do momentu l a nie dalej
        #jak juz przesuwamy l to wtedy mozemy zwiekszac ds
        

    # print(ds)

    return max_quality


print(btq_2([ 5, 3, 4, 9, 2, 8, 6, 7, 1]))
print(btq_2([1, 2, 3, 10, 4, 5, 6]))
print(btq_2([3, 3, 3]))
print(btq_2([4, 3, 2, 1]))