from itertools import chain

#player 0 starts

import time
import sys

class Jungle:

    PONDS = {(x, y) for x in [1, 2, 4, 5] for y in [3, 4, 5]} #10
    TRAPS = {(2, 0), (4, 0), (3, 1), (2, 8), (4, 8), (3, 7)} #11
    DENS = [(3, 8), (3, 0)] #12
    DIRS = [(0, 1), (1, 0), (-1, 0), (0, -1)]

    POSITIONS1 = {
    (0, 0),  # lew
    (6, 0),  # tygrys
    
    (1, 1),  # pies
    (5, 1),  # kot
    
    (0, 2),  # słoń
    (2, 2),  # wilk
    (4, 2),  # lampart
    (6, 2),  # szczur
    }
    POSITIONS0 = {
    (0, 6),  # lew
    (2, 6),  # tygrys
    (4, 6),  # pies
    (6, 6),  # kot
    
    (1, 7),  # słoń
    (5, 7),  # wilk
    
    (0, 8),  # lampart
    (6, 8),  # szczur
    }
    BOARD_INIT = {
    # GÓRA (gracz 1, dodatnie)
    (0, 2): -1,  # R
    (5, 1): -2,  # C
    (1, 1): -3,  # D
    (4, 2): -4,  # W
    (2, 2): -5,  # J
    (6, 0): -6,  # T
    (0, 0): -7,  # L
    (6, 2): -8,  # E

    # DÓŁ (gracz 0, ujemne)
    (6, 6): 1, # r
    (1, 7): 2, # c
    (5, 7): 3, # d
    (2, 6): 4, # w
    (0, 8): 6, # t
    (4, 6): 5, # j
    (6, 8): 7, # l
    (0, 6): 8, # e
}

    DISPLAY_TILE = {
        12: '*', 
        11: '#',
        10: '~',
        9: '.',
        8: 'e',
        7: 'l',
        6: 't',
        5: 'j',
        4: 'w',
        3: 'd',
        2: 'c',
        1: 'r',
        -1: 'R',
        -2: 'C',
        -3: 'D',
        -4: 'W',
        -5: 'J',
        -6: 'T',
        -7: 'L',
        -8: 'E',
    }
    temp = [[9] * 7 for _ in range(9)]
    for x,y in PONDS:
        temp[y][x] = 10
    for x,y in TRAPS:
        temp[y][x] = 11
    for x,y in DENS:

        temp[y][x] = 12
    DEFAULT_BOARD = [r[:] for r in temp]
    #so board is supposed to be used as board[y][x]

    def __init__(self, board = None, player = None):
        

        if board is None:
            self.initialize_board()
            self.p1_locations = {x for x in Jungle.POSITIONS1}
            self.p0_locations = {x for x in Jungle.POSITIONS0}
            #move list trzyma pare move, co stalo na polu lub None zeby ulatwic undo
            self.move_list = []
        
        else:
            p1 = {(i,j) for j in range(len(board)) for i in range(len(board[0])) if Jungle.DISPLAY_TILE[board[j][i]] in 'rcdwjtle'}
            p0 = {(i,j) for j in range(len(board)) for i in range(len(board[0])) if Jungle.DISPLAY_TILE[board[j][i]] in 'RCDWJTLE'}

            self.board = [r[:] for r in board]
            self.p1_locations = p1
            self.p0_locations = p0
            self.move_list = [] if player is None or player == 0 else [None] #sztucznie bo to tylko i tak dla gry randomowej z symluacjami, nie musimy cofac

    def initialize_board(self):
        self.board = [r[:] for r in Jungle.DEFAULT_BOARD]


        for pos,val in Jungle.BOARD_INIT.items():
            x,y = pos

            self.board[y][x] = val


    def draw(self,file = None):
        for y in range(9):
            res = ''
            for x in range(7):
                tile = self.board[y][x]
                to_display = Jungle.DISPLAY_TILE[tile]
                res += to_display
            print(res, file = file)
        print(file = file)
    
    def animal_rank(self,pos):
        x,y = pos
        
        animal = self.board[y][x]
        if animal > 8:
            raise PendingDeprecationWarning
        
        return abs(animal)


    def moves_for_animal(self, pos):
        x,y = pos
        animal = self.board[y][x]

        #na plasnszy beda zwierzeta jako 1 -1 2 -2 itp
        if animal > 8:#mialo byc zwierze
            
            
            print('pozycja bledna',pos,animal, file= sys.stderr)
            print(self.p1_locations, file = sys.stderr)
            print(self.p0_locations, file = sys.stderr)
            print('plansza', file = sys.stderr)
            # self.draw(file = sys.stderr)
            raise EnvironmentError 
        
        possible = []
        player = 1 if animal > 0 else 0

        for dx,dy in Jungle.DIRS:
            x_move = x + dx
            y_move = y + dy
            #wyjscie za plansze
            if not (0 <= x_move and x_move < 7 and 0 <= y_move and y_move < 9):
                continue
            
            #ruch na den
            #nie musimy sprawdzac czy cos tam stoi
            if (player == 0 and (x_move,y_move) == (3,8) ) or (player == 1 and (x_move,y_move) == (3,0)):
                # possible.append((x,y,x_move,y_move))
                continue

            #zawsze mozna zbic cos w polapce

            #ruch na wode
            if (x_move,y_move) in Jungle.PONDS:
                if (x,y) in Jungle.PONDS: #wiemy ze ruch szczurem na wode
                    possible.append((x,y,x_move,y_move))
                    continue

                if abs(animal) == 1:
                    #ruch na wode z lądu szczurem
                    #jesli 
                    #podobno mozna bic nawet gdy stoi tam szczur
                    # if abs(self.board[y_move][x_move]) == 1:
                    #     continue
                    #inaaczej to ruch na puste pole wiec mozna
                    possible.append((x,y,x_move,y_move))
                
                if abs(animal) not in [6,7]: #tylko tygrys i lew moga "na wode" czyli tak na prawde skoczyc nad nią
                    continue

                blocked = False
                while (x_move,y_move) in Jungle.PONDS: #sprawdzamy czy jest szczur tam gdzie chcemy

                    #przesuwamy
                    if abs(self.board[y_move][x_move]) == 1: #jesli szczur
                        blocked = True
                        break

                    x_move += dx
                    y_move += dy

                if blocked:
                    continue
                #jesli nie blocked to schodzimy dalej i moze dodamy ten ruch


            animal2 = self.board[y_move][x_move]
            #zwykle ruchy
            if animal2 > 8: #wtedy mozna sie ruszyc
                possible.append((x,y,x_move,y_move))
                continue

            player2 = 1 if animal2 > 0 else 0

            if player == player2: # nie mozna na swoja bierke wejsc
                continue

            if (x_move,y_move) in Jungle.TRAPS:
                possible.append((x,y,x_move,y_move))
                continue

            rank1,rank2 = self.animal_rank((x,y)), self.animal_rank((x_move,y_move))
            #bicie silniejszą albo bicie slonia szczurem
            if (rank1 >= rank2 and not (abs(animal) == 8 and abs(animal2) == 1)) or (abs(animal) == 1 and abs(animal2) == 8):
                possible.append((x,y,x_move,y_move))

        return possible


    def valid_move(self,move):
        x1,y1,x2,y2 = move
        pos1 = (x1,y1)
        pos2 = (x2,y2)

        return pos2 in self.moves_for_animal(pos1)

    def is_animal(self,pos):
        x,y = pos
        return self.board[y][x] > 8

    def get_moves(self, player):

        if player == 1:
            
            return list(chain.from_iterable(self.moves_for_animal(pos) for pos in self.p1_locations))
        else:
            
            return list(chain.from_iterable(self.moves_for_animal(pos) for pos in self.p0_locations))

    def move_piece(self,player,pos1,pos2):
        if pos1 is None:
            if player == 1:
                self.p1_locations.add(pos2)
            else:
                self.p0_locations.add(pos2)

            return
        else:
            x1,y1 = pos1

        if pos2 is None:
            x2,y2 = (-1,-1)
        else:
            x2,y2 = pos2

        if player == 1:
            self.p1_locations.remove((x1,y1))

            if (x2,y2) == (-1,-1):
                pass
            else:
                self.p1_locations.add((x2,y2))
        else:
            
            self.p0_locations.remove((x1,y1))
            if (x2,y2) == (-1,-1):
                pass
            else:
                self.p0_locations.add((x2,y2))

    def do_move(self, move, player):
        #zakladamy ze ruch da sie wykonac, tura sie zgadza itp
        #czyli zaczyna player 0
        if player != len(self.move_list) %2:
            print(move, file = sys.stderr)
            print('move list', self.move_list, file = sys.stderr)
            print('my player',player, file = sys.stderr)
            self.draw(file = sys.stderr)

            assert player == len(self.move_list) %2

        if move is None:
            self.move_list.append(None)
            return

        x1,y1,x2,y2 = move

        if not move in self.get_moves(player):
            print('nie ma takiego ruchu',move,player, file = sys.stderr)
            print('sila', self.animal_rank((x1,y1)), self.animal_rank((x2,y2)), file = sys.stderr)
            self.draw(file = sys.stderr)

            assert move in self.get_moves(player)


        animal1 = self.board[y1][x1]
        animal2 = self.board[y2][x2]

        
        #aktualizujemy ze sie przesunelismy
        self.move_piece(player,(x1,y1),(x2,y2))

        #to co bijemy albo None jesli nic nie bilismy
        if animal2 <= 8: #bylo to bicie
            self.move_piece(1-player,(x2,y2),None)
            is_capture = animal2
        else:
            is_capture = None
        
        self.move_list.append((move,is_capture))

        self.board[y1][x1] = Jungle.DEFAULT_BOARD[y1][x1]
        self.board[y2][x2] = animal1


    def undo_move(self):
        mv = self.move_list.pop()
        player = len(self.move_list) % 2

        if mv is None:
            return mv,player
        
        move,capture = mv

        x1,y1,x2,y2 = move

        # time.sleep(0.1)
        # self.draw()
        # print(self.p1_locations)
        # print(self.p0_locations)
        # print((x1,y1),(x2,y2))
        # print(player, capture)


        

        self.board[y1][x1] = self.board[y2][x2] #cofamy nasza jednostke

        #cofamy co gdzie stoi
        
        self.move_piece(player,(x2,y2),(x1,y1))
        #cofamy jednostke przeciwnika
        if capture is None:
            self.board[y2][x2] = Jungle.DEFAULT_BOARD[y2][x2]
        else:
            self.board[y2][x2] = capture
            self.move_piece(1-player,None,(x2,y2))

        return move,player

        

    def terminal(self):
        if len(self.p1_locations) == 0 or len(self.p0_locations) == 0:
            return True
        if (3,0) in self.p0_locations or (3,8) in self.p1_locations:
            return True
        return False
    
    def is_win(self):
        if self.terminal():
            if len(self.p1_locations) == 0 or (3,0) in self.p0_locations:
                return 1
            elif len(self.p0_locations) == 0 or (3,8) in self.p1_locations:
                return -1

        return 0


if __name__ == '__main__':
    game = Jungle()
    game.draw()
    print(game.p1_locations)
    print(game.p0_locations)
    game.do_move((6,6,6,5),0)
    game.draw()
    game2 = Jungle()
    game2.draw()
    # print(game.p1_locations)
    # print(game.p0_locations)
    # game.undo_move()
    # game.draw()
    # print(game.p1_locations)
    # print(game.p0_locations)