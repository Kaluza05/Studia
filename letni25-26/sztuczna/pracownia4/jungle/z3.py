from jungle import Jungle
from random import choice
import sys

def play_random(g : Jungle, player):
    if len(g.move_list) > 100:
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
    def __init__(self,N = 1000):
        self.N = N
        self.reset()

    def reset(self):
        self.game = Jungle()
        self.my_player = 1
        self.say('RDY')
        
    def say(self, what):
        sys.stdout.write(what)
        sys.stdout.write('\n')
        sys.stdout.flush()

    def hear(self):
        line = sys.stdin.readline().split()
        return line[0], line[1:]
    

    def choose_move(self,player):
        moves = self.game.get_moves(player)

        def simulate(m,k):
            self.game.do_move(m,player)
            board = [r[:] for r in self.game.board]
            self.game.undo_move()

            result = 0
            for _ in range(k):
                #if player is 0 and 0 wins good counting, if player is 1 then everything is around
                result += play_random(Jungle(board, 1-player),1-player)

            if player == 1:
                result = - result

            return result
        
         #20_000

        ks = [self.N//len(moves) for _ in range(len(moves))]
        i = max(range(len(moves)), key = lambda i : simulate(moves[i],ks[i]))

        return moves[i]
    
    def loop(self):
        while True:
            cmd, args = self.hear()
            if cmd == 'HEDID':
                unused_move_timeout, unused_game_timeout = args[:2]
                move = tuple((int(m) for m in args[2:]))
                if move == (-1, -1, -1, -1):
                    move = None


                self.game.do_move(move,1-self.my_player)
            elif cmd == 'ONEMORE':
                self.reset()
                continue
            elif cmd == 'BYE':
                break
            else:
                assert cmd == 'UGO'
                #assert not self.game.move_list
                self.my_player = 0

            # print(self.game.p0_locations, file = sys.stderr)
            # print(Jungle.POSITIONS0, file = sys.stderr)
            # print(self.game.p1_locations, file = sys.stderr)
            # print(Jungle.POSITIONS1, file = sys.stderr)
            # print('my player 2', self.my_player, file = sys.stderr)
            moves = self.game.get_moves(self.my_player)
            
            if moves:
                move = self.choose_move(self.my_player)
                self.game.do_move(move,self.my_player)
            else:
                self.game.do_move(None, self.my_player)
                move = (-1, -1, -1, -1)

            self.say('IDO %d %d %d %d' % move)


if __name__ == '__main__':
    player = PlayerSym(N = 100)
    # player.loop()
    for _ in range(50):
        m = player.choose_move(0)
    # player.game.draw()
    # g=Jungle()
    # print(play_random(g,0))