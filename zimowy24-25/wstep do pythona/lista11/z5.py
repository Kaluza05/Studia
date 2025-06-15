def in_tree(tree, e):
    if tree == []:
        return False
    n, left, right = tree
    if n == e:
        return True
    if e < n:
        return in_tree(left, e)
    return in_tree(right, e)

def tree_to_list(tree):
    if tree == []:
        return []
    n, left, right = tree
    return tree_to_list(left) + [n] + tree_to_list(right)

def add_to_tree(e, tree):
    pass


class Set:
    def __init__(self, *elems):
        self.tree = []
        pass
            
    def __contains__(self, e):
        return in_tree(self.tree, e) 
    
    def __and__(self):
        pass

    def __neg__(self):
        pass

    def __len__(self):
        pass

    def add(self, e):
        add_to_tree(e, self.tree)    
        