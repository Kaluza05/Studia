class ListItem:
    def __init__(self,val,right = None,left = None):
        self.value = val
        self.right = right
        self.left = left

class TwoWayList:
    def __init__(self,val:int):
        self.head = self.last = ListItem(val)
    def wypisz(self):
        def inside(lis:ListItem):
            if not lis: 
                print()
                return
            print(lis.value,end=' ')
            inside(lis.right)
        inside(self.head)

    def add_front(self,val:int):
        new = ListItem(val)
        new.right = self.head
        if self.head:
            self.head.left = new
        self.head = new
        #self.right = new.right
        return self
    
    def add_back(self,val:int):
        def _add_back(lis:ListItem,val:int):
            if not lis.right:
                new = ListItem(val)
                new.left = lis
                lis.right = new
                return lis
            lis.right = _add_back(lis.right,val)
            return lis
        return _add_back(self.head,val)

    def del_back(self):
        a = self.last.left
    
    
a = TwoWayList(5)
a.add_back(4)
a.add_back(8)
b = a.add_front(2)
a.wypisz()
a.del_back()
a.wypisz()
a.del_back()
a.wypisz()
a.del_back()
a.wypisz()
a.del_back()
a.wypisz()
#class ListItem:
#    def __init__(self,value):
#        self.value = value
#        self.left = None
#        self.right = None

#def wypisz(lis:ListItem):
#    while lis:
#        print(lis.value)
#        lis = lis.right
#
#
#def add_front(lis:ListItem,value):
#    new = ListItem(value)
#    new.right = lis
#    return new
#
#def del_front(lis:ListItem):
#    new = lis.next
#    new.left = None
#    return new
#
#def add_back(lis:ListItem,value):
#    if not lis:
#        new = ListItem(value)
#        new.left = lis
#        return new
#    lis.right = add_back(lis.right,value)
#    return lis
#
#def del_back(lis:ListItem):
#    if not lis.right: return None
#    lis.right = del_back(lis.right)
#    return lis


#tab = ListItem(4)
#tab = add_front(tab,5)
#tab = add_front(tab,9)
##wypisz(tab)
#tab = add_back(tab,3)
##tab = add_back(tab,10)
#wypisz(tab)