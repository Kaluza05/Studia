class Heap:
    def __init__(self,xs,cmp = 'min'):
        self.cmp = (lambda x, y : x[1] > y[1]) if cmp == 'min' else (lambda x, y : x[1] < y[1])
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
            if j > 0 and self.cmp(self.heap[(j-1) // 2],self.heap[j]):
                k = (j-1) // 2
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
        for i in range(self.n//2 - 1,-1,-1):
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


def dijkstra(edges,n):
    ds = [float('inf')]*n
    ds[0] = 0



    print(ds)
    heap = Heap([(0,0)])
    visited = [0]*n
    while not heap.empty():
        v,d = heap.pop()
        visited[v] = 1

        for edge in edges[v]:
            print('new edges',v, edge[0], 'weight:',edge[1])
            u,d_edge = edge
            if visited[u]:
                continue
            print('distances',ds[u],d+d_edge,d,d_edge)
            if ds[u] > d + d_edge:
                ds[u] = d + d_edge
                
                heap.insert((u,ds[u]))
        

    return ds


def solve():
    #pomiedzy miastami moze istniec wiecej niz jedna droga
    #wez ta droge o minimalnej odleglosci
    with open('input.txt','r') as inp:
        lines = inp.read().splitlines()
        n,m,k = [int(i) for i in lines[0].split()]
        # print(lines)
        print(n,m,k)
        prep_roads = [tuple(int(j) for j in i.split()) for i in lines[1:m+1]]
        roads = [[] for _ in range(n)]
        #dodaj nową drogę jesli jej nie bylo, a jak byla to porownaj czy jest krotsza
        #to moze okazac sie zludne, moze lepiej bedzie nie odfiltrowywac tego bo nasza dijkstra i tak tą drozsza krawedzia nie przejdzie
        for i in prep_roads:
            from1,to1,d = i
            roads[from1-1].append((to1-1,d))
            roads[to1-1].append((from1-1,d))

        print(prep_roads)


        print(roads)
        print()
        
        

        to_go = [int(i)-1 for i in lines[-k:]]

        print(to_go)
        print()

        distances = dijkstra(roads,n) #dijkstra starting from 1
        final_sum = 2 * sum(distances[i] for i in to_go)
        print(distances)
        if final_sum == float('inf'):
            return "NIE"
        return final_sum
        

        

print(solve())