import sys
from random import choice
from jungle import Jungle
from random import getrandbits
import time

INF = 10**9

ELEPHANT_POS1 = [
    [0,   0,   0,  -1000,   0,    -10,   -20],
    [0,   0,   0,     0,    0,  10,  -20],
    [0,   0,   0,    50,   20,   10,  -20],
    [0,   0,   0,   100,    0,    0, -100],
    [10,  0,   0,   200,    0,    0, -100],
    [20,  0,   0,   300,    0,    0, -100],
    [20, 100, 200,  400,  200,  100,  20],
    [50, 200, 400,  700,  400,  200,  50],
    [100,400, 700, 1000,  700,  400, 100]
]



ELEPHANT_POS0 = [
    [100, 400, 700, 1000, 700, 400, 100],
    [50,  200, 400,  700, 400, 200,  50],
    [20,  100, 200,  400, 200, 100,  20],
    [-100, 0,   0,   300,   0,   0,  20],
    [-100, 0,   0,   200,   0,   0,  10],
    [-100, 0,   0,   100,   0,   0,   0],
    [-20,  10,  20,   50,   0,   0,   0],
    [-20, -10,  0,    0,    0,   0,   0],
    [0,    0,   0, -1000,   0,   0,   0]
]

def play_random(g : Jungle, player):
    if len(g.move_list) > 900:
        return 0 
    
    if g.is_win() != 0:
        return g.is_win()

    moves = g.get_moves(player)
    if len(moves) == 0:
        move = None
    else:
        move = choice(moves)
    
    g.do_move(move,player)

    res =  play_random(g,1-player)

    # g.undo_move()

    return res


class PlayerSym:
    def __init__(self,weights = None):
        if weights == None:
            weights = (1,1,1)
        assert len(weights) == 3
        self.w1,self.w2,self.w3= weights
        self.reset()

    def say(self, what):
        sys.stdout.write(what)
        sys.stdout.write('\n')
        sys.stdout.flush()

    def hear(self):
        line = sys.stdin.readline().split()
        return line[0], line[1:]
    

    def reset(self):
        self.game = Jungle()
        self.my_player = 1
        self.say('RDY')

        self.TT = dict()
        self.zobrist = self.init_hash()

    def init_hash(self):
        NUM_SQUARES = 7 * 9
        NUM_PIECE_TYPES = 8

        z = [[[0 for _ in range(2)] for _ in range(NUM_PIECE_TYPES)] for _ in range(NUM_SQUARES)]

        for sq in range(NUM_SQUARES):
            for pt in range(NUM_PIECE_TYPES):
                for c in [0, 1]:
                    z[sq][pt][c] = getrandbits(64)

        self.side_to_move_hash = getrandbits(64)

        self.hash = 0

        for (x,y) in Jungle.POSITIONS1:
            # print(x,y,y*7+x,abs(Jungle.BOARD_INIT[(x,y)])-1, file= sys.stderr)
            self.hash ^= z[y*7+x][abs(Jungle.BOARD_INIT[(x,y)])-1][1]

        for (x,y) in Jungle.POSITIONS0:
        #    print(x,y,y*7+x,abs(Jungle.BOARD_INIT[(x,y)])-1, file= sys.stderr)
           self.hash ^= z[y*7+x][abs(Jungle.BOARD_INIT[(x,y)])-1][0]
        
        return z
        
    def update_hash(self,move,player):
        if move is None:
            self.hash ^= self.side_to_move_hash
            return
        
        x1,y1,x2,y2 = move


        animal = self.game.board[y1][x1]

        self.hash ^= self.side_to_move_hash

        #zabranie i postawienie naszej figury
        # print(7*y1+x1,abs(animal),player, file = sys.stderr)
        self.hash ^= self.zobrist[7*y1+x1][abs(animal)-1][player]
        self.hash ^= self.zobrist[7*y2+x2][abs(animal)-1][player]

        animal2 = self.game.board[y2][x2]

        # nie ma tam nic, nie trzeba zabierać
        if abs(animal2) > 8:
            return
        
        self.hash ^= self.zobrist[7*y2+x2][abs(animal2)-1][1-player]


        return
    

    def move(self,move,player):

        self.update_hash(move,player)
        self.game.do_move(move, player)

    def undo_move(self):
        
        
        move,player = self.game.undo_move()
        self.update_hash(move,player)

    def eval_position(self):
        #ocena z perspektywy gracza na dole

        #stosunek materiału

        weighted_material_diff = 0
        #distances weighed ith eleghant distance
        dist_diff = 0

        elephant_pos = 0

        if self.game.board[0][3] != 12:
            return 10**10
        elif self.game.board[8][3] != 12:
            return -10**10


        for y in range(9):
            for x in range(7):
                animal = self.game.board[y][x]
                if animal <= 8:
                    #gracz przeciwny
                    weighted_material_diff += animal
                    if animal > 0:
                        
                        if animal == 8:
                            # print('slon tu stoi',x,y, ELEPHANT_POS0[y][x], file = sys.stderr)
                            elephant_pos += ELEPHANT_POS0[y][x]
                            dist_diff += 9-y
                        
                        else:
                            dist_diff += 9-y
                    else:
                        if animal == -8:
                            # print('slon tu stoi',x,y, ELEPHANT_POS1[y][x], file = sys.stderr)
                            elephant_pos -= ELEPHANT_POS1[y][x]
                            dist_diff -= y
                        else:
                            dist_diff -= y

        
        final_val = self.w1*weighted_material_diff + self.w2*dist_diff + self.w3*elephant_pos

        # print(self.game.move_list[-1], file = sys.stderr)
        # print(weighted_material_diff, dist_diff, elephant_pos, final_val, file = sys.stderr)

        return final_val




    def order_best(self,moves : list,tt_move):
        ordering = []
        # 1. TT first
        if tt_move in moves:
            moves.remove(tt_move)
            ordering.append(tt_move)

        # 3. reszta bez sortowania
        ordering += moves

        return ordering
    
    def order_moves(self,moves,tt_move,ply,player_to_move):
        if moves is None:
            return None
        
        if ply > 2:
            return self.order_best(moves,tt_move)
        
        
        def score(move):
            x1,y1,x2,y2 = move
            s = 0
            if move == tt_move:
                s += 10**10

            animal = self.game.board[y1][x1]
            target = self.game.board[y2][x2]

            # ruch slonia
            if abs(animal) == 8:
                s += 10**2

                if player_to_move == 0:
                    move_val = ELEPHANT_POS0[y2][x2]
                else:
                    move_val = ELEPHANT_POS1[y2][x2]

                s += 10* move_val


            if abs(target) <= 8: #BICIE
                s += 10**5 * (abs(target)-abs(animal))


            return s

        return sorted(moves, key=score, reverse=True)

    def alpha_beta(self,depth, alpha, beta, ply, player_to_move):
        original_alpha = alpha
        h = self.hash

        # --- TT lookup ---
        if h in self.TT:
            val, tt_depth, flag, tt_move = self.TT[h]

            if tt_depth >= depth:
                if flag == 0:   # EXACT
                    return val, tt_move
                elif flag == 1: # LOWERBOUND
                    alpha = max(alpha, val)
                elif flag == 2: # UPPERBOUND
                    beta = min(beta, val)

                if alpha >= beta:
                    return val, tt_move

            best_tt_move = tt_move
        else:
            best_tt_move = None

        # --- leaf ---
        if depth == 0 or self.game.terminal():
            val = self.eval_position()
            if player_to_move == 1:
                val = -val
            return val, None

        moves = self.game.get_moves(player_to_move)

        if not moves:
            # PASS
            self.move(None,player_to_move)
            val, _ = self.alpha_beta(depth-1, -beta, -alpha, ply+1,1-player_to_move)
            val = -val
            self.undo_move()
            return val, None

        # --- ordering ---
        moves = self.order_moves(moves, best_tt_move, ply,player_to_move)

        best_move = None
        best_value = -float('inf')

        for move in moves:

            # --- make move ---
            self.move(move,player_to_move) #musimy wiedziec czyj ruch jest 0-nasz 1-przeciwnika
            

            val, _ = self.alpha_beta(depth-1, -beta, -alpha, ply+1,1-player_to_move)
            val = -val

            # --- undo ---
            self.undo_move()

            if val >= beta:
                # cutoff
                self.TT[h] = (beta, depth, 1, move)  # LOWERBOUND
                return beta, move

            if val > best_value:
                best_value = val
                best_move = move

            if val > alpha:
                alpha = val

        # --- zapis do TT ---
        if best_value <= original_alpha:
            flag = 2  # UPPERBOUND
        elif best_value >= beta:
            flag = 1  # LOWERBOUND
        else:
            flag = 0  # EXACT

        self.TT[h] = (best_value, depth, flag, best_move)

        return best_value, best_move
    

    def search(self, depth = 3):
        best_move = None

        #iterative deepening, fills up the TT
        
        for d in range(1,depth+1):
            _, move = self.alpha_beta(d, -INF, +INF, 0,self.my_player)
            if move != None:
                best_move = move

        return best_move
    

    def loop(self):
        while True:
            cmd, args = self.hear()
            if cmd == 'HEDID':
                unused_move_timeout, unused_game_timeout = args[:2]
                move = tuple((int(m) for m in args[2:]))
                if move == (-1, -1, -1, -1):
                    move = None
                
                        
                self.move(move,1-self.my_player)
            elif cmd == 'ONEMORE':
                self.reset()
                continue
            elif cmd == 'BYE':
                break
            else:
                assert cmd == 'UGO'
                #assert not self.game.move_list
                self.my_player = 0

            moves = self.game.get_moves(self.my_player)
            # time.sleep(0.5)
            # print(*self.game.board, sep = '\n', file = sys.stderr)
            # print()
            if moves:
                move = self.search()
                # print(move, file = sys.stderr)
                self.move(move,self.my_player)
            else:
                self.move(None, self.my_player)
                move = (-1, -1, -1, -1)
            self.say('IDO %d %d %d %d' % move)

def load_weights():
    if len(sys.argv) >= 4:
        return list(map(float, sys.argv[1:4]))
    else:
        return [0.0, 0.0, 0.0]
    
if __name__ == '__main__':
    weights = load_weights()
    # print(weights,file = sys.stderr)
    # print(weights,file = sys.stderr)
    player = PlayerSym(weights)
    player.loop()
    # print(player.w1,player.w2,player.w3,player.w4)
    # player.loop()