class Heap:
    def __init__(self,xs,cmp = 'min'):
        self.cmp = (lambda x, y : x > y) if cmp == 'min' else (lambda x, y : x < y)
        self.heap = xs
        self.n = len(xs)
        self.build_heap()

    def __str__(self):
        return repr(self.heap)
    
    def move_up(self,i):
        k = i
        while True:
            j = k
            print(j,self.heap)
            if j > 0 and self.cmp(self.heap[(j+1 // 2)-1],self.heap[j]):
                k = (j+1 // 2)-1
                self.heap[j],self.heap[k] = self.heap[k],self.heap[j]
            
            if j == k:
                return

    def move_down(self,i):
        k = i
        while True:
            j = k
            if 2*j+1<= self.n-1 and self.cmp(self.heap[k], self.heap[2*j+1]): #jesli K[j] < od dziecka zamien
                k = 2 * j + 1
            if 2*j+1 < self.n-1 and self.cmp(self.heap[k], self.heap[2*j+2]): # K[j] < K[2j+1]
                k = 2*j + 2

            if j == k:
                return
            
            self.heap[j],self.heap[k] = self.heap[k],self.heap[j]
            return
            
    def build_heap(self):
        print('building')
        print((self.n)//2 - 1)
        for i in range((self.n)//2 - 1,-1,-1):
            print('building',i)
            self.move_down(i)

    def min(self):
        return self.heap[0]

    def pop(self):
        if self.n == 0:
            raise RecursionError('empty heap')
        
        min_el = self.heap[0]
        self.heap[0],self.heap[self.n-1] = self.heap[self.n-1], self.heap[0]
        self.heap.pop()
        self.n -= 1
        self.move_down(0)
        return min_el
     

    def insert(self,x) -> None:
        self.heap = self.heap + [x]
        self.n += 1
        # print('before',self.heap)
        self.move_up(self.n-1)
        # print('after',self.heap)

    def empty(self) -> bool:
        return self.n == 0
    

h = Heap([1,4,8,2,3,6,-1])
print(h)
h.insert(9)
print(h)