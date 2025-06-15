from random import choice

class Breakthorugh: #kolor 1-biały -1-czarny
    def __init__(self):
        self.board = [[-1 if i in {0,1} else (1 if i in {6,7} else "_") for _ in range(8)] for i in range(8)]
        self.color = 1
        self.white = [(i+6,j) for i in range(2) for j in range(8)]
        self.black = [(i,j) for i in range(2) for j in range(8)]

    def won(self)->bool:
        if self.color == 1 and 1 in self.board[0]: return True,self.color
        elif self.color == -1 and -1 in self.board[-1]: return True,self.color
        return False

    def valid_moves(self,pos_x,pos_y): #kolor 1,-1
        moves = []
        if self.color == 1:
            for i in [-1,0,1]:
                if i == 0 and self.board[pos_x-1][pos_y] in {1,-1}:
                    continue
                elif i in {1,-1} and self.board[pos_x-1][pos_y] == 1:
                    continue
                if pos_y+i in range(8):
                    moves.append((pos_x-1,pos_y+i))
        elif self.color == -1:
            for i in [-1,0,1]:
                if i == 0 and self.board[pos_x+1][pos_y] in {1,-1}:
                    continue
                elif i in {1,-1} and self.board[pos_x+1][pos_y] == -1:
                    continue
                if pos_y+i in range(8):
                    moves.append((pos_x+1,pos_y+i))
        return moves

    def move(self):
        #if self.color == 1:
        #    b = []
        #    for i in self.white:
        #        b += self.valid_moves(*i)
        #else:
        #    b = []
        #    for i in self.black:
        #        b += self.valid_moves(*i)
        while True:
            if self.color == 1: pos_x,pos_y = choice(self.white)
            else: pos_x,pos_y = choice(self.black)
            
            moves = self.valid_moves(pos_x,pos_y)
            if not moves:continue
            else:
                move = choice(moves)
                #print((pos_x,pos_y),move,self.color)
                self.board[pos_x][pos_y] = '_'
                self.board[move[0]][move[1]] = self.color
                if self.color == 1:
                    self.white.remove((pos_x,pos_y))
                    self.white.append((move[0],move[1]))
                else:
                    self.black.remove((pos_x,pos_y))
                    self.black.append((move[0],move[1]))

                break

    def __str__(self):
        return '\n'.join([' '.join([str(j) if j!=-1 else '0' for j in i]) for i in self.board])
    def play(self):
        while True:
            self.move()
            if self.won():
                break
            self.color = -self.color
            print(self)
            print('\n\n')
        #print(*self.board,sep='\n')
        print(self.won())
        print(self)

a = Breakthorugh()
a.play()
