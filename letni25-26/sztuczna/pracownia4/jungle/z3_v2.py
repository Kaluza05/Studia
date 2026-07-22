from jungle import Jungle
from random import choice
import sys

def play_random(g : Jungle, player):
    if len(g.move_list) > 200:
        return 0,0
    
    if g.is_win() != 0:
        return g.is_win(),0

    moves = g.get_moves(player)
    if len(moves) == 0:
        move = None
    else:
        move = choice(moves)
    
    g.do_move(move,player)

    res,n_moves =  play_random(g,1-player)

    g.undo_move()

    return res,n_moves+1


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
        
        moves_in_simulation = 0

        def simulate(m):
            nonlocal moves_in_simulation

            self.game.do_move(m,player)

            result = 0
            #if player is 0 and 0 wins good counting, if player is 1 then everything is around
            res,moves = play_random(self.game,1-player)
            result += res
            moves_in_simulation += moves

            if player == 1:
                result = - result

            
            self.game.undo_move()

            return result
        
        #20_000
        i = 0
        move_stats = [0] * len(moves)

        while moves_in_simulation < self.N:
            move = moves[i]

            res = simulate(move)
            move_stats[i] += res

            # print(moves_in_simulation, file = sys.stderr)
            i = (i +1) % len(moves)

            
        i = max(range(len(moves)), key = lambda i : move_stats[i])
        same_stat_moves = [moves[j] for j in range(len(moves)) if move_stats[j] == move_stats[i]]
        # print(move_stats)

        return choice(same_stat_moves)
    
    def loop(self):
        while True:
            cmd, args = self.hear()
            if cmd == 'HEDID':
                unused_move_timeout, unused_game_timeout = args[:2]
                move = tuple((int(m) for m in args[2:]))
                if move == (-1, -1, -1, -1):
                    move = None

                # print('m aking enemy move', move, file = sys.stderr)
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
    player = PlayerSym(N = 20000)
    player.loop()
    # for _ in range(50):
        # m = player.choose_move(0)
    # player.game.draw()
    # g=Jungle()
    # print(play_random(g,0))