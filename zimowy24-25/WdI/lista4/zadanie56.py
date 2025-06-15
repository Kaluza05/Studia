#zamienia z systemu dziesiątkowego 
#O(log_k(n))
def zamien_na_k_system(n,k):
    dzielniki = []
    while n>0:
        dzielniki.append(n%k)
        n //= k
    return dzielniki[::-1]

#O(len(zapis))
def zamien_z_k_systemu(zapis,k):
    wyn = 0
    l = 0
    for i in zapis[::-1]:
        wyn += i*k**l
        l+=1
    return wyn

#złożoność O(log_k(n)+log_k(n)+log_k(n)) = O(log_k(n))
def palindrom_k_arny(n,k):
    n_k = zamien_na_k_system(n,k)
    odwr_n_k = n_k[::-1]                            #zadanie 7 z listy 2
    if n == zamien_z_k_systemu(odwr_n_k,k):
        return True
    return False

