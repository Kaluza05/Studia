def min_pot_overall(n,m):
    def min_pot(n,m,k):
        if n*n >= m: return k
        return min_pot(n*n,m,k+1)
    

    def bin_search(n,m,k):
        if k == 0:return n
        b = 2**k
        e = 2*b
        N=n
        n = n**b
        while b < e: #wiemy ze n^2^i<=m i n^2^(i+1)>m
            mid_point = (b+e)//2
            #print('b,e:',b,e)
            #print('mid,b:',mid_point,b)
            #print('K,N:',K,N**(mid_point))
            new_n = n*N**(mid_point-b)
            if new_n < m:
                n = new_n
                b=mid_point + 1
                
            else:e=mid_point
        #print(N**(mid_point+1),K*N)
        return n*N,b,e  #K*N bo pętla się kończy gdy b = e i przez to nie mnożymy K po raz ostatni
    return bin_search(n,m,min_pot(n,m,0))


'''
min_pot_overall zajmuje log(k)
bin_search zajmuje log(k), ale ma w sobie potęgowanie, ale łącznie podnosimy do k czyli zajmie ono log(k)

'''


#print(min_pot_overall(2,612))