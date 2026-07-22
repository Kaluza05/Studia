DIRS = [(0,1),(0,-1),(1,0),(-1,0)]


def find_time(x,xs):
    pass
    #xs posortowane, chcemy najwiekszei  t ze xs[i] < x
    # l = 0
    # r = len(xs)-1
    # while l < r:
    #     m = (l+r)//2
    #     print(x, m, xs[m])
    #     time.sleep(0.1)
    #     if x > xs[m]:
    #         l = m + 1
    #     elif x == xs[m]:
    #         return m-1 if m != 0 else -1
    #     elif x < xs[m]:
    #         r = m

    # return l-1
def find_time(x, xs):
    l = 0
    r = len(xs)  # ważne: półotwarty przedział [l, r)

    while l < r:
        m = (l + r) // 2
        if xs[m] < x:
            l = m + 1
        else:
            r = m

    return l - 1

# print('dla : ',0,find_time(0,[1,2,3,4,5]))
# print('dla : ',1,find_time(1,[1,2,3,4,5]))
# print('dla : ',2,find_time(2,[1,2,3,4,5]))
# print('dla : ',3,find_time(3,[1,2,3,4,5]))
# print('dla : ',4,find_time(4,[1,2,3,4,5]))
# print('dla : ',5,find_time(5,[1,2,3,4,5]))
# print('dla : ',6,find_time(6,[1,2,3,4,5]))
class UnionFind:
    def __init__(self,xs):
        self.parents = {x : x for x in xs}
        self.size = {x : 1 for x in xs}
        
        #kazdy ma wskaznik na rodzica
        #reprezentujemy 'drzewo'

    def find(self,x):
        if x == self.parents[x]:
            return x
        
        #kompresja sciezki
        self.parents[x] = self.find(self.parents[x])
        return self.parents[x]

    def union(self,a,b):
        #returns if a union was succesful
        x = self.find(a)
        y = self.find(b)

        if x == y:
            return 0 
        
        
        #poz wieksze podpinamy mniejsze
        if self.size[x] < self.size[y]:
            x,y = y,x

        self.parents[y] = x
        self.size[x] += self.size[y]

        return 1

    


def solve(n,m,board,times):
    #chcemy tu miec wszystkie komorki ktore nie beda zalane w dniu t i zalane w czasie t+1
    print(f'n m to : {n} {m}')
    #moga byc te same wartosci w times
    pos_for_time = {t : [] for t in times}
    

    for i in range(m):
        for j in range(n):
            t = find_time(board[i][j],times)
            if t == -1:
                continue
            pos_for_time[times[t]].append((i,j))

    print(pos_for_time)
    rev_times = times[::-1]
    print(rev_times)
    prev = None
    islands_prev = 0
    counts = []

    uf = UnionFind([(j,i) for i in range(n) for j in range(m)])

    for t in rev_times:
        
        if t == prev:
            counts.append(counts[-1])
            continue

        
        elems = pos_for_time[t]


        count_unions = 0

        for y,x in elems:
            neighbours = [(x+dx,y+dy) for dx,dy in DIRS if 0 <= x+dx and x + dx < n and 0 <= y+dy and y+dy < m]
            
            
            for xp,yp in neighbours:
                
                # print(x,y, xp,yp)
                if board[yp][xp] > t: #nie jest zatopiony
                    # print(t,board[yp][xp])
                    r = uf.union((y,x),(yp,xp)) #0 or 1
                    count_unions += r

        islands_now = islands_prev + len(elems) - count_unions   
        counts.append(islands_now)
        islands_prev = islands_now
        prev = t

    return counts[::-1]
    #zaczynamy od ostatniego i robimy union find-a


with open('inp3.txt','r') as inp:
    data = inp.read().splitlines()
    y,x = data[0].split()
    x,y = int(x),int(y)
    board = [[int(i) for i in r.split()] for r in data[1:1+y]]

    times = [int(i) for i in data[-1].split()]
    print(data)
    print(x,y)
    print(board)
    print(times)
    islands = solve(x,y,board,times)
    print(islands)