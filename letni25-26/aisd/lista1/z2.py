class Heap:
    def __init__(self,xs,cmp = 'min'):
        self.cmp = (lambda x, y : x[0] > y[0]) if cmp == 'min' else (lambda x, y : x[0] < y[0])
        self.heap = ['error'] + xs
        self.n = len(xs)
        self.build_heap()

    def __str__(self):
        return repr(self.heap)
    
    def move_up(self,i):
        k = i
        while True:
            j = k
            if j > 1 and self.cmp(self.heap[j // 2],self.heap[j]):
                k = j // 2
                self.heap[j],self.heap[k] = self.heap[k],self.heap[j]
            
            if j == k:
                return

    def move_down(self,i):
        k = i
        while True:
            j = k
            if 2*j<= self.n and self.cmp(self.heap[k], self.heap[2*j]): #jesli K[j] < od dziecka zamien
                k = 2 * j
            if 2*j < self.n and self.cmp(self.heap[k], self.heap[2*j+1]): # K[j] < K[2j+1]
                k = 2*j + 1

            if j == k:
                return
            
            self.heap[j],self.heap[k] = self.heap[k],self.heap[j]
            return
            
    def build_heap(self):
        for i in range((self.n+1)//2,0,-1):
            self.move_down(i)

    def min(self):
        return self.heap[1]

    def pop(self) -> None:
        if self.n == 0:
            raise RecursionError('empty heap')
        
        min_el = self.heap[1]
        self.heap[1],self.heap[self.n] = self.heap[self.n], self.heap[1]
        self.heap.pop()
        self.n -= 1
        self.move_down(1)
        return min_el
     

    def insert(self,x) -> None:
        self.heap = self.heap + [x]
        self.n += 1
        # print('before',self.heap)
        self.move_up(self.n)
        # print('after',self.heap)

    def empty(self) -> bool:
        return self.n == 0

def min_range(xss):
    print(*xss,sep='\n')
    min_els = [(xs[0],i) for i,xs in enumerate(xss)]
    max_els = [(xs[-1],i) for i,xs in enumerate(xss)]
    min_pointers = [0 for _ in range(len(xss))]
    max_pointers = [len(xs)-1 for xs in xss]

    min_heap = Heap(min_els, cmp = 'min')
    max_heap = Heap(max_els, cmp = 'max')

    a,i = min_heap.min()
    b,j = max_heap.min()
    exit_val = 0

    while (not min_heap.empty()) and (not max_heap.empty()):
        
        min_el,i = min_heap.min()
        a = min_el
        if min_pointers[i] >= max_pointers[i]:
            exit_val = 1
            break
        max_el,j = max_heap.min()
        b = max_el
        if min_pointers[j] >= max_pointers[j]:
            exit_val = 2
            break
        else:
            if i == j and min_pointers[i] == max_pointers[j] - 1:
                ptr_min = min_pointers[i]
                new_min = xss[i][ptr_min+1]
                min_pointers[i] += 1
                min_heap.pop()
                min_heap.insert((new_min,i))
            else:
                ptr_min = min_pointers[i]
                ptr_max = max_pointers[j]
                # print(max_pointers)
                new_min = xss[i][ptr_min+1]
                new_max = xss[j][ptr_max-1]
                min_pointers[i] += 1
                max_pointers[j] -= 1
                min_heap.pop()
                max_heap.pop()
                min_heap.insert((new_min,i))
                max_heap.insert((new_max,j))


    if exit_val == 1 or min_heap.empty(): #can't move more right because of the min heap so we just move the max heap
        while not max_heap.empty():
            el,i =  max_heap.min()
            b = el
            if min_pointers[i] >= max_pointers[i]:
                # b = el
                break
            else:
                ptr = max_pointers[i]
                new = xss[i][ptr-1]
                max_pointers[i] -= 1
                max_heap.pop()
                max_heap.insert((new,i))

    elif exit_val == 2 or max_heap.empty(): #can't move more left because of the max heap so we just move the min heap
        while not min_heap.empty():
            el,i =  min_heap.min()
            a = el
            if min_pointers[i] >= max_pointers[i]:
                # a = el
                break
            else:
                ptr = min_pointers[i]
                
                new = xss[i][ptr+1]
                min_pointers[i] += 1
                min_heap.pop()
                min_heap.insert((new,i))

    return f"wynik: [{a}, {b}]"

print(min_range([[0,1,3,4],[2,3,4,5,6],[8,10,11]]))
# print(min_range([[0,1,2,3,4,5]]))
# a = Heap([(10,0),(7,0),(3,0),(9,0),(5,0)],cmp = 'max') poprawnie buduje
# print(a)