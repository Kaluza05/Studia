import matplotlib.pyplot as plt

class TreeItem:
    def __init__ (self,value:int)->None:
        self.val = value
        self.left = None
        self.right = None

class BST:
    def __init__(self,val:int,left = None,right = None):
        self.root = TreeItem(val)
        self.root.left = left
        self.root.right = right

    def __str__(self):
        elems = []
        self._wypisz(self.root,elems)
        return '\n'.join(map(str,elems))

    def __len__(self):
        return self._tree_depth(self.root)

    def _wypisz(self,node:TreeItem,elems:list)->None:
        if not node: return
        self._wypisz(node.left,elems)
        elems.append(node.val)
        self._wypisz(node.right,elems)

    
    def _tree_depth(self,node:TreeItem)->int:
        if not node: return 0
        left_depth = self._tree_depth(node.left)+1
        right_depth = self._tree_depth(node.right)+1

        if left_depth > right_depth: return left_depth
        else:                        return right_depth

    def node_to_tree(self,node:TreeItem):
        return BST(node.val,node.left,node.right)
    
    def count_elems(self)->int:
        def _count_elems(node:TreeItem):
            if node:
                a = _count_elems(node.left)
                b = _count_elems(node.right)
                return 1+a+b
            return 0
        return _count_elems(self.root)

    def wypisz_dodatnie(self):
        def _wypisz_dodatnie(node:TreeItem,elems:list):
            if node:
                if node.val >0:
                    _wypisz_dodatnie(node.left,elems)
                    elems.append(node.val)
                _wypisz_dodatnie(node.right,elems)
        elems = []
        _wypisz_dodatnie(self.root,elems)
        return ''.join(map(str,elems))
    
    def find_smallest(self)->TreeItem:
        if not self.root:  return None
        curr = self.root
        while curr.left:
            curr = curr.left
        return TreeItem(curr.val)

    def add_tree(self,other)->None:
        '''
        find smallest element in self, add whole tree to left node of that value
        '''
        curr = self.root
        while curr.left != None:
            curr = curr.left
        curr.left = other.root
        return curr

    def add_elem(self,val:int)->None:
        curr = self.root
        while curr:
            #self.node_to_tree(curr).visualize()
            if val == curr.val: return  #element jest już w drzewie nie ma co dodawać
            if val > curr.val:
                if not curr.right:
                    curr.right = TreeItem(val)
                    return
                curr = curr.right
            else:
                if not curr.left:
                    curr.left = TreeItem(val)
                    return
                curr = curr.left
        return
    
    def del_elem(self,val:int)->None:
        parent = None
        curr = self.root
        while curr and curr.val != val:
            parent = curr
            if val > curr.val:  curr = curr.right
            else:               curr = curr.left

        if not curr: return  #val nie ma w drzewie
        if not curr.right:  #nie ma prawego dziecka
            if parent.left.val == val: parent.left = curr.left
            else: parent.right = curr.left
        elif not curr.left: #nie ma lewego dziecka
            if parent.left.val == val: parent.left = curr.right
            else: parent.right = curr.right
        #ma lewe i prawe dziecko do prawego przypinamy lewe
        else:
            if  parent:
                temp = self.node_to_tree(curr.right)
                temp.add_tree(self.node_to_tree(curr.left))

                if parent.left.val == val:  parent.left = temp.root
                else:                       parent.right = temp.root
            else:
                temp = self.node_to_tree(self.root.right)
                temp.add_tree(self.node_to_tree(self.root.left))
                self.root = temp.root

    def rotate(self,node:int,child:str):
        parent = None
        curr = self.root
        while curr and curr.val != node:
            parent = curr
            if curr.val > node: curr = curr.left
            else: curr = curr.right
        if not curr: return
        if child == 'right':
            '''
            podmienic prawe dziecko za root,
            pod prawe dziecko root podpiąc lewe dziecko prawego,
            pod lewe dziecko nowego root podpiac stary root
            '''
            if not parent:
                temp = self.root.right
                self.root.right = temp.left
                temp.left = self.root
                self.root = temp
                return
            temp = curr.right
            if not temp: return
            curr.right = temp.left
            temp.left = curr
            parent.right = temp
        else:
            if not parent:
                temp = self.root.left
                self.root.left = temp.right
                temp.right = self.root
                self.root = temp
                return
            temp = curr.left
            if not temp: return
            curr.left = temp.right
            temp.right = curr
            parent.left = temp

    def visualize(self):
        if not self.root:
            print("Tree is empty.")
            return

        fig, ax = plt.subplots(figsize=(10, 8))
        ax.axis('off')

        def plot_node(node, x, y, dx, dy):
            if not node:
                return
            ax.text(x, y, str(node.val), fontsize=12, ha='center', va='center',
                    bbox=dict(boxstyle="circle", facecolor="lightblue", edgecolor="black"))

            # Plot left child
            if node.left:
                ax.plot([x, x - dx], [y, y - dy], 'k-')
                plot_node(node.left, x - dx, y - dy, dx / 2, dy)

            # Plot right child
            if node.right:
                ax.plot([x, x + dx], [y, y - dy], 'k-')
                plot_node(node.right, x + dx, y - dy, dx / 2, dy)

        # Start plotting from the root node
        plot_node(self.root, 0, 0, 1.5, 1)
        plt.show()

def is_BST(node,min_val=float('-inf'),max_val=float('inf'))->bool:
    if not node: return True
    if not (node.val > min_val and node.val < max_val): return False
    return is_BST(node.left,min_val,node.val) and is_BST(node.right,node.val,max_val)


#a = BST(4)
#a.add_elem(3)
#a.add_elem(6)
#a.add_elem(5)
#a.add_elem(2)
#a.add_elem(1)
b = BST(8)
b.add_elem(4)
b.add_elem(2)
b.add_elem(1)
print(len(b))
#print(is_BST(b.root))
b.add_elem(3)
b.add_elem(6)
b.add_elem(5)
b.add_elem(7)
b.add_elem(12)
b.add_elem(10)
b.add_elem(9)
b.add_elem(11)
b.add_elem(14)
b.add_elem(13)
b.add_elem(15)
print(len(b))
b.visualize()
##print(b)
##print(a)
##b.add_tree(a)
##print(b)
#
##b.del_elem(1)
##b.visualize()
#b.rotate(4,'left')
#b.visualize()
#print(count_elems(a))
#print(tree_depth(a))