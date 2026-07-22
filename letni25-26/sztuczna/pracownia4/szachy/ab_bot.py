import chess
import sys


class Bot:
    def __init__(self):
        self.reset()

    def reset(self):
        self.game = chess.Board()
        self.my_player = 1
        self.say('RDY')

        self.TT = dict() #transposition table
        self.killer_moves = dict()

    
    def say(self, what):
        sys.stdout.write(what)
        sys.stdout.write('\n')
        sys.stdout.flush()

    def hear(self):
        line = sys.stdin.readline().split()
        return line[0], line[1:]
    


    def search(self):
        pass

    def loop(self):
        pass

if __name__ == '__main__':
    board = chess.Board()
    # board.__hash__()
    print(board._transposition_key())