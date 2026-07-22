def n_cities(n,m,board):
    #board is n x m
    def is_in(i,j):
        return i >= 0 and j >=  0 and i < n and j < m
    
    def remove_city(i,j):
        sq = board[i][j]
        if sq == 'A':
            return
        
        elif sq == 'B':
            board[i][j] = 'A'
            if is_in(i,j-1) and board[i][j-1] in ['D','E','F']:
                remove_city(i,j-1)
            if is_in(i+1,j) and board[i+1][j] in ['C','D','F']:
                remove_city(i+1,j)
        elif sq == 'C':
            board[i][j] = 'A'
            if is_in(i,j-1) and board[i][j-1] in ['D','E','F']:
                remove_city(i,j-1)
            if is_in(i-1,j) and board[i-1][j] in ['B','E','F']:
                remove_city(i-1,j)  
        elif sq == 'D':
            board[i][j] = 'A'
            if is_in(i,j+1) and board[i][j+1] in ['B','C','F']:
                remove_city(i,j+1)
            if is_in(i-1,j) and board[i-1][j] in ['B','E','F']:
                remove_city(i-1,j)
        elif sq == 'E':
            board[i][j] = 'A'
            if is_in(i,j+1) and board[i][j+1] in ['B','C','F']:
                remove_city(i,j+1)
            if is_in(i+1,j) and board[i+1][j] in ['C','D','F']:
                remove_city(i+1,j)
        elif sq == 'F':
            board[i][j] = 'A'
            if is_in(i+1,j) and board[i+1][j] in ['C','D','F']:
                remove_city(i+1,j)
            if is_in(i-1,j) and board[i-1][j] in ['B','E','F']:
                remove_city(i-1,j)
            if is_in(i,j+1) and board[i][j+1] in ['B','C','F']:
                remove_city(i,j+1)
            if is_in(i,j-1) and board[i][j-1] in ['D','E','F']:
                remove_city(i,j-1)

    cities = 0
    for i in range(n):
        for j in range(m):
            if board[i][j] != 'A':
                cities += 1
                print('removing: ',i,j)
                print(*board,sep='\n')
                remove_city(i,j)
            
    return cities

# with open('inputs.txt','r') as input:
#     input = input.read().splitlines()
#     n,m = input[0].split()
#     n,m = int(n),int(m)
#     board = [list(i) for i in input[1:]]
#     print(n,m,board)
#     print(n_cities(n,m,board))
#     print('after',board)


#wersja iteracyjna
class Stack:
    def __init__(self):
        self.stack = []
    def add(self,x):
        self.stack.append(x)
        return
    
    def pop(self):
        if self.stack == []:
            raise EnvironmentError
        x,rest = self.stack[-1],self.stack[:-1]
        self.stack = rest
        return x
    
    def is_empty(self):
        return self.stack == []
    
def n_cities_iter(n,m,board):
    #board is n x m
    def is_in(i,j):
        return i >= 0 and j >=  0 and i < n and j < m
    
    def remove_city(i,j):
        stack = Stack()
        
        stack.add((i,j))
        while not stack.is_empty():
            (a,b) = stack.pop()
            sq = board[a][b]
            if sq == 'A':
                continue
            
            elif sq == 'B':
                board[a][b] = 'A'
                if is_in(a,b-1) and board[a][b-1] in ['D','E','F']:
                    stack.add((a,b-1))
                if is_in(a+1,b) and board[a+1][b] in ['C','D','F']:
                    stack.add((a+1,b))
            elif sq == 'C':
                board[a][b] = 'A'
                if is_in(a,b-1) and board[a][b-1] in ['D','E','F']:
                    stack.add((a,b-1))
                if is_in(a-1,b) and board[a-1][b] in ['B','E','F']:
                    stack.add((a-1,b))  
            elif sq == 'D':
                board[a][b] = 'A'
                if is_in(a,b+1) and board[a][b+1] in ['B','C','F']:
                    stack.add((a,b+1))
                if is_in(a-1,b) and board[a-1][b] in ['B','E','F']:
                    stack.add((a-1,b))
            elif sq == 'E':
                board[a][b] = 'A'
                if is_in(a,b+1) and board[a][b+1] in ['B','C','F']:
                    stack.add((a,b+1))
                if is_in(a+1,b) and board[a+1][b] in ['C','D','F']:
                    stack.add((a+1,b))
            elif sq == 'F':
                board[a][b] = 'A'
                if is_in(a+1,b) and board[a+1][b] in ['C','D','F']:
                    stack.add((a+1,b))
                if is_in(a-1,b) and board[a-1][b] in ['B','E','F']:
                    stack.add((a-1,b))
                if is_in(a,b+1) and board[a][b+1] in ['B','C','F']:
                    stack.add((a,b+1))
                if is_in(a,b-1) and board[a][b-1] in ['D','E','F']:
                    stack.add((a,b-1))

    cities = 0
    for i in range(n):
        for j in range(m):
            if board[i][j] != 'A':
                cities += 1
                remove_city(i,j)
                
    return cities

with open('inputs.txt','r') as input:
    input = input.read().splitlines()
    n,m = input[0].split()
    n,m = int(n),int(m)
    board = [list(i) for i in input[1:]]
    print(n,m,board)
    print(n_cities_iter(n,m,board))
    print('after',board)