class ListItem:
    def __init__(self,value):
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

def scal(lis:ListItem,lis2:ListItem):
    while lis and lis2:
        if lis.val <= lis2.val:
            lis.next = scal(lis.next,lis2)
            return lis
        else:
            lis2.next = scal(lis,lis2.next)
            return lis2
    if not lis and lis2:
        return add_list_back(lis,lis2)

    elif lis and not lis2:
        return add_list_back(lis2,lis)
    
tab1 = ListItem(1)
tab1 = add_back(tab1,4)
tab1 = add_back(tab1,7)
tab2 = ListItem(3)
tab2 = add_back(tab2,5)
tab2 = add_back(tab2,8)
#wypisz(tab1)
#wypisz(tab2)
#a = scal(tab1,tab2)
#add_list_back(tab1,tab2)
wypisz(tab1)
wypisz(tab2)
#wypisz(a)
wypisz(scal(tab1,tab2))
