class ListItem :
    def __init__ ( self , value ):
        self.val = value
        self.next = None

def wypisz(lis:ListItem):
    while lis:
        print(lis.val)
        lis = lis.next
    print()

def wypisz_rev(lis:ListItem):
    if lis:
        wypisz_rev(lis.next)
        print(lis.val)
    else: print()

def add_front(lis:ListItem,val:int):
    new = ListItem(val)
    new.next = lis
    return new

def del_front(lis:ListItem):
    if lis: return lis.next
    return None

def del_back(lis:ListItem):
    if not lis.next: return None
    lis.next = del_back(lis.next)
    return lis

def add_back(lis:ListItem,val:int):
    if not lis: return ListItem(val)
    lis.next = add_back(lis.next,val)
    return lis

def add_list_back(lis1:ListItem,lis2:ListItem):
    if not lis1: return lis2
    lis1.next = add_list_back(lis1.next,lis2)
    return lis1

def del_val(lis:ListItem,value:int):
    if not lis: return None
    if lis.val == value: return del_val(lis.next,value)
    lis.next = del_val(lis.next,value)
    return lis

def reverse_order(lis:ListItem):
    if lis.next:
        ogon = reverse_order(lis.next)
        old = lis
        lis.next = None
        return add_list_back(ogon,old)
    else:
        return lis
    
def signed_list(lis:ListItem):
    pos_head=pos_tail = None
    neg_head=neg_tail = None
    curr = lis
    if not lis: return None,None
    while curr:
        print('curr')
        wypisz(curr)
        print('lis')
        wypisz(lis)
        next_el = curr.next
        curr.next = None
        if curr.val >0:
            if not pos_head: pos_head = pos_tail = curr
            else:
                pos_tail.next = curr
                pos_tail = pos_tail.next
        elif curr.val<0:
            if not neg_head: neg_head = neg_tail = curr
            else:
                neg_tail.next = curr
                neg_tail = neg_tail.next
        curr = next_el
    wypisz(lis)
    return pos_head,neg_head

    

'''
zad.5 trzymać gdzieś wartość ostatniego elementu listy
np. zainicjować self.last = val
i w operacjach to tez jakos zmieniac
'''

#tab = ListItem(1)
#tab = add_front(tab,2)
#tab = add_front(tab,4)
#tab = add_front(tab,7)
##wypisz(tab)
#tab = del_back(tab)
##wypisz(tab)
#tab = add_back(tab,9)
#tab = add_back(tab,20)
##wypisz(tab)
#tab2 = ListItem(11)
#tab2 = add_front(tab2,7)
##wypisz(tab2)
#tab3 = add_list_back(tab,tab2)
##wypisz(tab3)
#tab3 = del_val(tab3,7)
##wypisz(tab3)
##tab3 = add_back(tab3,6)
#wypisz(tab3)
##wypisz_rev(tab3)
#tab4 = reverse_order(tab3)
#wypisz(tab4)
tab5 = ListItem(8)
tab5 = add_back(tab5,-5)
tab5 = add_back(tab5,4)
tab5 = add_back(tab5,-2)
tab5 = add_back(tab5,-7)
tab5 = add_back(tab5,5)
#wypisz(tab5)
tab6,tab7 = signed_list(tab5)
wypisz(tab6)
wypisz(tab7)
#wypisz(tab5)
#wypisz(tab7) 