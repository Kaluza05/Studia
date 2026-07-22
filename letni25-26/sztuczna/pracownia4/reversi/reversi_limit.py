import random
import sys
import time

INF = 10**9

MOVE_MULT = [[500 , -100, 100,  50,  50, 100, -100,  500],
            [-100, -500, -50, -50, -50, -50, -500, -100], 
            [100 ,  -50, 100,   0,   0, 100,  -50,  100], 
            [50  ,  -50,   0,   0,   0,   0,  -50,   50], 
            [50  ,  -50,   0,   0,   0,   0,  -50,   50], 
            [100 ,  -50, 100,   0,   0, 100,  -50,  100], 
            [-100, -500, -50, -50, -50, -50, -500, -100], 
            [500 , -100, 100,  50,  50, 100, -100,  500]]

MOVE_MULT = [[x / 10**2 for x in r] for r in MOVE_MULT]

N = 2**10

class Reversi:
    M = 8
    DIRS = [(0, 1), (1, 0), (-1, 0), (0, -1),
            (1, 1), (-1, -1), (1, -1), (-1, 1)]

    def __init__(self):
        self.board = self.initial_board()
        self.fields = set()
        self.move_list = []
        # self.history = []
        self.flipped_list = []
        self.white_cells = 2
        self.black_cells = 2
        for i in range(self.M):
            for j in range(self.M):
                if self.board[i][j] is None:
                    self.fields.add((j, i))

    def initial_board(self):
        B = [[None] * self.M for _ in range(self.M)]
        B[3][3] = 1
        B[4][4] = 1
        B[3][4] = 0
        B[4][3] = 0
        return B

    def draw(self, channel = sys.stderr):
        for i in range(self.M):
            res = []
            for j in range(self.M):
                b = self.board[i][j]
                if b is None:
                    res.append('.')
                elif b == 1:
                    res.append('#')
                else:
                    res.append('o')
            print(''.join(res), file= channel)
        print('', file = channel)

    def moves(self, player):
        res = []
        for (x, y) in self.fields:
            if any(self.can_beat(x, y, direction, player)
                   for direction in self.DIRS):
                res.append((x, y))
        return res
    
    def move_diff(self):
        diff = 0
        for (x, y) in self.fields:
            for direction in self.DIRS:
                result = self.beating(x,y,direction)
                if result is None:
                    continue
                elif result == 1:
                    diff -= 1
                else:
                    diff += 1

        return diff

    def beating(self, x, y, d):
        dx, dy = d
        x += dx
        y += dy
        cnt = 0
        if self.get(x, y) is not None:
            opp = self.get(x, y)
        else:
            return None
        
        while self.get(x, y) == opp:
            x += dx
            y += dy
            cnt += 1
        return 1 - opp if cnt > 0 and self.get(x, y) == 1 - opp else None
    

    def can_beat(self, x, y, d, player):
        dx, dy = d
        x += dx
        y += dy
        cnt = 0
        while self.get(x, y) == 1 - player:
            x += dx
            y += dy
            cnt += 1
        return cnt > 0 and self.get(x, y) == player

    def get(self, x, y):
        if 0 <= x < self.M and 0 <= y < self.M:
            return self.board[y][x]
        return None



    def do_move(self, move, player):
        self.move_list.append(move)
        
        

        if move is None:
            self.flipped_list.append((player,[]))
            return
        
        if player == 0:
            self.white_cells += 1
        else:
            self.black_cells += 1

        x0, y0 = move
        self.board[y0][x0] = player
        self.fields.remove(move)
        total_flips = []
        for dx, dy in self.DIRS:
            x, y = x0, y0
            to_beat = []
            x += dx
            y += dy
            while self.get(x, y) == 1 - player:
                to_beat.append((x, y))
                x += dx
                y += dy
            if self.get(x, y) == player:
                
                total_flips += to_beat
                for (nx, ny) in to_beat:
                    if player == 0:
                        self.white_cells += 1
                        self.black_cells -= 1
                    else:
                        self.black_cells += 1
                        self.white_cells -= 1

                    self.board[ny][nx] = player

        self.flipped_list.append((player,total_flips))

        return total_flips

    def undo_move(self):
        move = self.move_list.pop()
        player,flips = self.flipped_list.pop()


        if move is None:
            return move,player,[]
        
        if player == 0:
            self.white_cells -= 1
            self.white_cells -= len(flips)
            self.black_cells += len(flips)
        else:
            self.black_cells -= 1
            self.black_cells -= len(flips)
            self.white_cells += len(flips)

        x,y = move
        self.board[y][x] = None

        for x,y in flips:
            self.board[y][x] = 1-player

        self.fields.add(move)

        return move,player,flips

    def result(self):
        res = 0
        for y in range(self.M):
            for x in range(self.M):
                b = self.board[y][x]
                if b == 0:
                    res -= 1
                elif b == 1:
                    res += 1
        return res

    def terminal(self):
        if not self.fields:
            return True
        if len(self.move_list) < 2:
            return False
        return self.move_list[-1] is None and self.move_list[-2] is None


class Player(object):
    def __init__(self, early_weights, middle_weights, late_weights):
        self.early_weights = early_weights
        self.middle_weights = middle_weights
        self.late_weights = late_weights
        self.reset()
        # self.evals = 0

    def reset(self):
        self.game = Reversi()
        self.my_player = 1
        self.say('RDY')

        self.nodes = 0
        self.TT = dict() #transposition table
        self.zobrist = self.init_hash()
        self.killer_moves = dict()

    def init_hash(self):
        z = [[0] * 2 for _ in range(64)]
        for sq in range(64):
            for c in [0,1]:
                z[sq][c] = random.getrandbits(64)

        self.side_to_move_hash = random.getrandbits(64)

        
        self.hash = z[8*3+3][1] ^ z[8*4+3][0] ^ z[8*3+4][0] ^ z[8*4+4][1]
        return z
    
    def say(self, what):
        sys.stdout.write(what)
        sys.stdout.write('\n')
        sys.stdout.flush()

    def hear(self):
        line = sys.stdin.readline().split()
        return line[0], line[1:]
    

    def update_hash(self,move,player,to_change):
        if move is None:
            self.hash ^= self.side_to_move_hash
            return
        
        x,y = move
        self.hash ^= self.side_to_move_hash

        self.hash ^= self.zobrist[8*x+y][player]

        for x,y in to_change:
            pos = 8*x+y
            self.hash ^= self.zobrist[pos][1-player]
            self.hash ^= self.zobrist[pos][player]

        return


    def move(self,move,player):

        to_flip = self.game.do_move(move, player)
        self.update_hash(move,player,to_flip)

    def undo_move(self):
        
        
        move,player,flipped = self.game.undo_move()
        self.update_hash(move,player,flipped)


    def eval_position(self):
        # self.evals += 1

        my_discs = self.game.white_cells
        opp_discs = self.game.black_cells

        disc_score = my_discs - opp_discs
        progress = (my_discs + opp_discs) / 64


        player = 0

        mobility_score = self.game.move_diff() #my_moves - opp_moves

        frontier_score = 0
        empty_adj_score = 0
        pos_score = 0

        board = self.game.board


        for i in range(8):
            for j in range(8):
                cell = board[j][i]

                if cell is None:
                    continue

                is_player = (cell == player)

                # placement
                if is_player:
                    pos_score += MOVE_MULT[j][i]
                else:
                    pos_score -= MOVE_MULT[j][i]

                empty_neighbors = 0
                is_frontier = False

                for di, dj in Reversi.DIRS:
                    ni = i + di
                    nj = j + dj

                    if 0 <= ni < 8 and 0 <= nj < 8:
                        if board[nj][ni] is None:
                            empty_neighbors += 1
                            is_frontier = True
                            # break

                if is_player:
                    empty_adj_score += empty_neighbors
                    if is_frontier:
                        frontier_score += 1
                else:
                    empty_adj_score -= empty_neighbors
                    if is_frontier:
                        frontier_score -= 1


        #early game
        if progress < 0.3:
            weights = self.early_weights
        #middle game
        elif progress < 0.7:
            weights = self.middle_weights
        #late game
        else:
            weights = self.late_weights

        scores = [mobility_score, disc_score, pos_score,frontier_score,empty_adj_score]
        return sum([s * w for s,w in zip(scores,weights)])



    def order_best(self,moves : list,tt_move,ply):
        ordering = []
        # 1. TT first
        if tt_move in moves:
            moves.remove(tt_move)
            ordering.append(tt_move)

        # 2. killer moves next
        for km in self.killer_moves.get(ply, []):
            if km in moves:
                moves.remove(km)
                ordering.append(km)

        # 3. reszta bez sortowania
        ordering += moves

        return ordering


    def order_moves(self,moves,tt_move,ply,player_to_move):
        if moves is None:
            return None
        
        # if ply <= 2:
        #     return self.order_best(moves,tt_move,ply)
        
        
        def score(move):
            x,y = move
            if move == tt_move:
                return 10**9

            if move in self.killer_moves.get(ply,[]):
                return 10**7

            return MOVE_MULT[y][x]


        return sorted(moves, key=score, reverse=True)


    def store_killer(self,move,ply):
        if ply not in self.killer_moves:
            self.killer_moves[ply] = []

        km : list = self.killer_moves[ply]

        if move not in km:
            km.insert(0, move)   # najnowszy na początek

        if len(km) > 2:
            km.pop() 

    def alpha_beta(self,depth, alpha, beta, ply, player_to_move, start,time_limit):
        self.nodes += 1

        if self.nodes & (N-1) == 0:
            # self.nodes = 0
            if time.time() - start >= time_limit:
                raise TimeoutError
    
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

        moves = self.game.moves(player_to_move)

        if not moves:
            # PASS
            self.move(None,player_to_move)
            try:
                val, _ = self.alpha_beta(depth-1, -beta, -alpha, ply+1,1-player_to_move,start,time_limit)

            except TimeoutError:
                self.undo_move()
                raise TimeoutError
            
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
            
            try:
                val, _ = self.alpha_beta(depth-1, -beta, -alpha, ply+1,1-player_to_move,start,time_limit)
                
            except TimeoutError:
                self.undo_move()
                raise TimeoutError
            
            val = -val
            # --- undo ---
            self.undo_move()

            if val >= beta:
                # cutoff
                self.store_killer(move, ply)
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


    def search(self, time_limit = 1):
        self.nodes = 0
        start_time = time.time()
        best_move = None

        # self.evals = 0

        #iterative deepening, fills up the TT
        depth = 1
        while depth < 64:
            if time.time() - start_time >= 0.95 * time_limit:
                break

            try:
                _, move = self.alpha_beta(depth, -INF, +INF, 0,self.my_player,start_time,time_limit)
            # if self.stop:
            #     break
                if move != None:
                    best_move = move

                depth += 1
            except TimeoutError:
                
                return best_move

        return best_move

    

    def get_time_for_move(self,time_constraint,total_time):
        avg_time = 1.5 * total_time / (len(self.game.fields) - 12) if len(self.game.fields) > 12 \
                else 1.5 * total_time / len(self.game.fields)
        
        return min({0.7 * time_constraint, avg_time})
        

    def loop(self):
        while True:
            cmd, args = self.hear()
            if cmd == 'HEDID':
                unused_move_timeout, unused_game_timeout = args[:2]
                move = tuple((int(m) for m in args[2:]))
                if move == (-1, -1):
                    move = None
                self.move(move,1- self.my_player)
                
            elif cmd == 'ONEMORE':
                self.reset()
                continue
            elif cmd == 'BYE':
                break
            else:
                
                self.my_player = 0

            
            # print(avg_time, file = sys.stderr)
            time_constraint, total_time = float(args[0]), float(args[1])
            

            time_per_move = self.get_time_for_move(time_constraint ,total_time)
            # print('my time', time_per_move, file = sys.stderr)
            move = self.search(time_limit = time_per_move)
            # print(f'evals done in time {time_per_move} {self.evals}', file = sys.stderr)
            self.move(move,self.my_player)
            if not move:
                move = (-1, -1)

            self.say('IDO %d %d' % move)


# hashowanie raczej dziala poprawnie
def load_weights():
    if len(sys.argv) >= 1 + 3 * 5:
        return list(map(float, sys.argv[1:(1 + 3 * 5)]))
    else:
        return [0] * (3 * 5)
    

if __name__ == '__main__':
    weights = load_weights()
    early_weights, middle_weights, late_weights = weights[:5], weights[5:10], weights[10:]


    player = Player(early_weights, middle_weights, late_weights)
    player.loop()