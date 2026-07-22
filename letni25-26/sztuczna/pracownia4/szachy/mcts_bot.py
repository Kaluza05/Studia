import chess
import sys
import chess.polyglot
import chess.syzygy
import random
import os
import json

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
book_path = os.path.join(BASE_DIR, "opening", "gm2001.bin")
endgame_path = os.path.join(BASE_DIR, "tablebase")
pst_path = os.path.join(BASE_DIR, "pst", "pst.json")

INF = 10**9
with open(pst_path,'r') as json_str:
    data = json.load(json_str)
    opening = data["opening"]
    endgame = data["endgame"]
    OPENING_PST = {int(c) : {int(p) : v for p,v in t.items()} for c,t in opening.items()}
    ENDGAME_PST = {int(c) : {int(p) : v for p,v in t.items()} for c,t in endgame.items()}

# print(OPENING_PST)
PHASE_MATERIAL = {chess.PAWN : 0, chess.KNIGHT : 1, chess.BISHOP : 1, chess.ROOK : 2, chess.QUEEN : 4, chess.KING : 0}
# print(PHASE_MATERIAL)
STARTING_MATERIAL = 24 # 4 * 2 + 4 * 1 + 4 * 1 + 2 * 4
# print(endgame_path)
# for c in [OPENING_PST, ENDGAME_PST]:
#     for i in {0,1}:
#         for j in c[i]:
#             print(len(c[i][j]))

PIECE_VALUES = {chess.PAWN : 1, chess.BISHOP : 3, chess.KNIGHT : 3, chess.ROOK : 5, chess.QUEEN : 8, chess.KING : 200}

PAWN_STRUCTURE_BONUS = {'isolated' : -15 ,'doubled' : -10, 'passed' : 30}

class WrongMove(Exception):
    pass

class Chess:
    def __init__(self):
        self.board = chess.Board()

    def move(self,move):
        #less safe way of moving, we assume the muve is in legal moves
        self.board.push(move)
        
    def undo(self):
        return self.board.pop()


    def update(self, uci_move):
        try:
            move = chess.Move.from_uci(uci_move)
        except ValueError:
            raise WrongMove

        if move not in self.board.legal_moves:
            raise WrongMove
            
        self.board.push(move)
        out = self.board.outcome()
        if out is None:
            return None
        if out.winner is None:
            return 0
        if out.winner:
            return -1
        else:
            return +1    
    
    def moves(self):
        return self.board.legal_moves
        # return [str(m) for m in self.board.legal_moves]
        
    def draw(self):
        print (self.board)

    def terminal(self):
        return self.board.is_game_over() 
    
    @staticmethod
    def isolated_pawns(board : chess.Board, color : chess.Color):
        pawns = list(board.pieces(chess.PAWN, color))
        penalty = 0

        files = [chess.square_file(sq) for sq in pawns]

        for sq in pawns:
            f = chess.square_file(sq)
            if not any(abs(f - other_f) == 1 for other_f in files):
                penalty += PAWN_STRUCTURE_BONUS["isolated"]

        return penalty

    @staticmethod
    def doubled_pawns(board : chess.Board, color : chess.Color):
        pawns = list(board.pieces(chess.PAWN, color))
        penalty = 0

        files = dict()
        for sq in pawns:
            files[chess.square_file(sq)] = files.get(chess.square_file(sq),0) + 1
        
        for f in files:
            if files[f] > 1: #more than 1 pawn in a file
                penalty += (files[f]-1) * PAWN_STRUCTURE_BONUS["doubled"]

        return penalty
    
    @staticmethod
    def is_passed_pawn(board : chess.Board, sq : chess.Square, color : chess.Color):
        enemy = not color
        file = chess.square_file(sq)
        rank = chess.square_rank(sq)

        enemy_pawns = board.pieces(chess.PAWN, enemy)

        for sq in enemy_pawns:
            f = chess.square_file(sq)
            r = chess.square_rank(sq)

            if abs(f - file) <= 1:
                if (color == chess.WHITE and r > rank) or \
                (color == chess.BLACK and r < rank):
                    return False

        return True


    @staticmethod
    def passed_pawns(board : chess.Board, color : chess.Color):
        bonus = 0

        for sq in board.pieces(chess.PAWN, color):
            if Chess.is_passed_pawn(board, sq, color):
                bonus += PAWN_STRUCTURE_BONUS["passed"] + chess.square_rank(sq) * 5

        return bonus

class Bot:
    def __init__(self):
        self.reset() #jako pierwsze zeby nie rozlaaczylo
        self.ENDGAME_THRESHOLD = 5
        self.OPENING_THRESHOLD = 12
        self.reader = chess.polyglot.open_reader(book_path)
        self.tablebase = chess.syzygy.open_tablebase(endgame_path)
        

    def reset(self):
        self.game = Chess()
        self.my_player = 1
        self.say('RDY')

        self.TT = dict() #transposition table
        self.killer_moves = dict()
        self.phase = STARTING_MATERIAL

    
    def say(self, what):
        sys.stdout.write(what)
        sys.stdout.write('\n')
        sys.stdout.flush()

    def hear(self):
        line = sys.stdin.readline().split()
        return line[0], line[1:]
    

    def move(self,m: chess.Move):
        #capture but not a pawn capture
        if self.game.board.is_capture(m) and not self.game.board.is_en_passant(m):
            captured = self.game.board.piece_at(m.to_square).piece_type
            self.phase -= PHASE_MATERIAL[captured]

        self.game.move(m)

    def undo(self):
        m = self.game.undo()

        #we use assumption that pawn is worth 0
        if self.game.board.is_capture(m) and not self.game.board.is_en_passant(m):
            captured = self.game.board.piece_at(m.to_square).piece_type
            self.phase += PHASE_MATERIAL[captured]


    def update(self, uci_move):
        try:
            # print(uci_move, file = sys.stderr)
            move = chess.Move.from_uci(uci_move)
        except ValueError:
            raise WrongMove

        if move not in self.game.board.legal_moves:
            # print('illegal move', move, self.game.board.legal_moves, file = sys.stderr)
            raise WrongMove
        
        self.move(move)

    @staticmethod
    def eval_pawn_structure(board : chess.Board):
        return (
        Chess.isolated_pawns(board, chess.WHITE)
        - Chess.isolated_pawns(board, chess.BLACK)
        + Chess.doubled_pawns(board, chess.WHITE)
        - Chess.doubled_pawns(board, chess.BLACK)
        + Chess.passed_pawns(board, chess.WHITE)
        - Chess.passed_pawns(board, chess.BLACK)
    )

    def eval_endgame(self):
        return 1

    def eval_position(self):
        board = self.game.board
        if board.is_game_over():
            if board.is_checkmate():
            # side to move is mated
                return -INF if board.turn == chess.WHITE else INF
        #we need 6 elements for max points
        #material
        #mobility
        #position
        #pawn structure
        phase = self.phase / STARTING_MATERIAL

        material = 0
        position_start = 0
        position_end = 0

        for piece_type, value in PIECE_VALUES.items():
            white_pieces = board.pieces(piece_type, chess.WHITE)
            black_pieces = board.pieces(piece_type, chess.BLACK)
            for square in white_pieces:
                position_start += OPENING_PST[chess.WHITE][piece_type][square]
                position_end += ENDGAME_PST[chess.WHITE][piece_type][square]

            for square in black_pieces:
                position_start -= OPENING_PST[chess.BLACK][piece_type][square]
                position_end -= ENDGAME_PST[chess.WHITE][piece_type][square]

            material += len(white_pieces) * value
            material -= len(black_pieces) * value

        
        white_moves = len(list(board.legal_moves))

        board.push(chess.Move.null())
        black_moves = len(list(board.legal_moves))
        board.pop()

        mobility = white_moves - black_moves

        pawn_structure = Bot.eval_pawn_structure(board)

        position = position_start * phase + position_end * (1 - phase)

        pos_hash = board._transposition_key()
        repetition = -2000 if pos_hash in self.TT else 0

        # print(material,mobility,position,pawn_structure, file = sys.stderr)
        #powinny byc wagi na poczatek gry i na koniec i interpolacja pomiedzy nimi
        # print(board)
        # print(10 * material + 10 * mobility + 10 * position + 10 * pawn_structure + repetition)

        return 10 * material + 10 * mobility + 10 * position + 10 * pawn_structure + repetition

    def search_opening(self) -> chess.Move:
        entries = self.reader.find_all(self.game.board)
        moves = []
        weights = []
        for e in entries:
            moves.append(e.move)
            weights.append(e.weight)

        # print(moves,weights)
        move = random.choices(moves, weights=weights, k=1)[0]
        return move

    def search_endgame(self, depth = 2) -> chess.Move:
        # do dopracowania, odpalic alpha beta ale z inna funckja ewaluacji
        # for d in range(1,depth+1):
        #     _, move = self.alpha_beta(d, -INF, +INF, 0,self.my_player)

        #     # if self.stop:
        #     #     break

        #     if move != None:
        #         best_move = move

        # return best_move
        moves = self.game.moves()
        best_move = []
        best_val = (-2,-100,-6)
        # print(moves, file = sys.stderr)
        for m in moves:
            
            self.game.move(m)
        
            val_wdl = self.tablebase.probe_wdl(self.game.board)
            val_dtz = self.tablebase.probe_dtz(self.game.board)
            # print(m,val_wdl,val_dtz)
            #it was judged from the other perspective
            val_wdl = - val_wdl 
            val_dtz = - val_dtz
            # print(self.game.board, file = sys.stderr)
            # print('ruch',m, file = sys.stderr)
            
            # print(self.game.board)
            self.game.undo()

            val_piece = self.game.board.piece_at(m.from_square).piece_type
            val_piece = -1 if val_piece == 6 else val_piece
            # print(m,(val_wdl,val_dtz),file = sys.stderr)
            
            if val_wdl > best_val[0]  \
            or (val_wdl == best_val[0] and val_dtz < best_val[1]) \
            or (val_wdl == best_val[0] and val_dtz == best_val[1] and val_piece > best_val[2]):
                best_val = (val_wdl,val_dtz, val_piece)
                best_move = m
            # elif val_wdl == best_val[0] and val_dtz == best_val[1]:
            #     best_moves.append(m)

        if best_move:
            # print(best_move,best_val,file = sys.stderr)
            return best_move
        else:
            print('no best move???', best_move)


    def pos_diff(self,move : chess.Move):
        #difference between current piece position and earlier position
        piece = self.game.board.piece_at(move.from_square)
        if not piece:
            return 0


        phase = self.phase / STARTING_MATERIAL

        pst_from = (OPENING_PST[piece.color][piece.piece_type][move.from_square] * phase) + \
                (ENDGAME_PST[piece.color][piece.piece_type][move.from_square] * (1 - phase))
        
        pst_to = (OPENING_PST[piece.color][piece.piece_type][move.to_square] * phase) + \
                (ENDGAME_PST[piece.color][piece.piece_type][move.to_square] * (1 - phase))
        
        # print(piece, piece.piece_type,move.from_square,move.to_square)
        # print(pst_from, pst_to)
        return pst_to - pst_from
        # return PST[piece.piece_type][]
        

    #cheap ordering
    def order_best(self,moves : list,tt_move,ply):
        moves = list(moves)
        ordering = []
        # 1. TT first
        if tt_move in moves:
            moves.remove(tt_move)
            ordering.append(tt_move)

        # 3. reszta bez sortowania
        ordering += moves

        return ordering


    def order_moves(self,moves,tt_move,ply):
        if moves is None:
            return None
        
        if ply >= 2:
            return self.order_best(moves,tt_move,ply)
        
        def score(move : chess.Move):
            #if move is a check or a capture value it more
            #
            
            if move == tt_move:
                return 10**9
            
            
            if self.game.board.is_capture(move):
                if self.game.board.is_en_passant(move):
                    return 10**5
                # print('figura', self.game.board.piece_at(move.to_square).piece_type, file = sys.stderr)
                return 10**3 * max((PIECE_VALUES[self.game.board.piece_at(move.to_square).piece_type] \
                            -   PIECE_VALUES[self.game.board.piece_at(move.from_square).piece_type]), 1)

            if self.game.board.gives_check(move):
                return 10**2

            return self.pos_diff(move)


        return sorted(moves, key=score, reverse=True)
    

    def alpha_beta(self,depth, alpha, beta, ply, player_to_move):
        # if time.time() >= self.deadline:
        #     self.stop = True
        #     return 0, None
        original_alpha = alpha
        h = self.game.board._transposition_key()

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

        moves = self.game.moves()

        #impossible i think because otherwise it would be terminal state
        if not moves:
            # PASS
            self.move(None)
            val, _ = self.alpha_beta(depth-1, -beta, -alpha, ply+1,1-player_to_move)

    
            val = -val
            self.undo()
            return val, None

        # --- ordering ---
        #null move added
        moves = self.order_moves(moves, best_tt_move, ply)

        best_move = None
        best_value = -float('inf')

        for move in moves:

            # --- make move ---
            self.move(move) #musimy wiedziec czyj ruch jest 0-nasz 1-przeciwnika
            

            val, _ = self.alpha_beta(depth-1, -beta, -alpha, ply+1,1-player_to_move)
            val = -val

            # --- undo ---
            self.undo()

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
    
    def search(self, depth = 2) -> chess.Move:
        #bedziewmy mieli jakis opening book i ending book
        num_pieces = len(self.game.board.piece_map())
        # print('num pieces:',num_pieces, file= sys.stderr)
        if num_pieces <= self.ENDGAME_THRESHOLD:
            return self.search_endgame(depth = depth * 2)
            
        if self.game.board.fullmove_number <= self.OPENING_THRESHOLD:#move in opening book:
            try:
                return self.search_opening()
            except IndexError:
                #nie ma w opening book
                pass

        # best_move = None
        # return random.choice(list(self.game.moves()))
        # self.stop = False

        #iterative deepening, fills up the TT
        for d in range(1,depth+1):
            _, move = self.alpha_beta(d, -INF, +INF, 0,self.my_player)

            # if self.stop:
            #     break

            if move != None:
                best_move = move

        return best_move
    
        


    def loop(self):
        while True:
            cmd, args = self.hear()
            if cmd == 'HEDID':
                unused_move_timeout, unused_game_timeout = args[:2]
                move = args[2]
                
                self.update(move)
            elif cmd == 'ONEMORE':
                self.reset()
                continue
            elif cmd == 'BYE':
                break
            else:
                assert cmd == 'UGO'
                #assert not self.game.move_list
                self.my_player = 0

            
            move = self.search(depth = 2)
            self.move(move)

            self.say('IDO ' + chess.Move.uci(move))

if __name__ == '__main__':
    # board = chess.Board()
    # board.__hash__()
    # print(board._transposition_key())
    player = Bot()
    # player.pos_diff(chess.Move.from_uci('e2e4'))
    # player.game.board = chess.Board("7K/8/8/8/4k3/P7/1P6/8 w - - 0 1")
    # player.search_endgame()
    # player.game.board = chess.Board('8/7k/5Q2/8/8/8/8/5K2 w - - 0 1')
    # m = player.search_endgame()
    # print(m)
    # player.move(m)
    # print(player.game.board)
    player.loop()
    # m = player.search()
    # print('wybrany ruch',m)
