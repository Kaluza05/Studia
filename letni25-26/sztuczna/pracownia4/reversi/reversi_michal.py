import random
import sys
import time
import math


class Reversi:
    M = 8
    DIRS = [(0, 1), (1, 0), (-1, 0), (0, -1), (1, 1), (-1, -1), (1, -1), (-1, 1)]
    BOARD_HASHES = [random.getrandbits(64) for _ in range(M * M * 2)]
    TURN_HASH = random.getrandbits(64)

    def __init__(self):
        self.board = self.initial_board()
        self.fields = set()
        self.move_list = []
        self.history = []
        self.hash_history = []
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
        self.hash = self.BOARD_HASHES[(3 * self.M + 3) * 2 + 1]
        self.hash ^= self.BOARD_HASHES[(4 * self.M + 4) * 2 + 1]
        self.hash ^= self.BOARD_HASHES[(3 * self.M + 4) * 2]
        self.hash ^= self.BOARD_HASHES[(4 * self.M + 3) * 2]
        return B

    def draw(self):
        for i in range(self.M):
            res = []
            for j in range(self.M):
                b = self.board[i][j]
                if b is None:
                    res.append(".")
                elif b == 1:
                    res.append("#")
                else:
                    res.append("o")
            print("".join(res))
        print("")

    def moves(self, player):
        res = []
        for x, y in self.fields:
            if any(self.can_beat(x, y, direction, player) for direction in self.DIRS):
                res.append((x, y))
        return res

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
        assert player == len(self.move_list) % 2
        self.history.append([x[:] for x in self.board])
        self.hash_history.append(self.hash)
        self.move_list.append(move)
        self.hash ^= self.TURN_HASH

        if move is None:
            return
        x, y = move
        x0, y0 = move
        self.board[y][x] = player
        self.fields -= set([move])
        self.hash ^= self.BOARD_HASHES[(y * self.M + x) * 2 + player]
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
                for nx, ny in to_beat:
                    self.board[ny][nx] = player
                    self.hash ^= self.BOARD_HASHES[(ny * self.M + nx) * 2 + player]
                    self.hash ^= self.BOARD_HASHES[(ny * self.M + nx) * 2 + 1 - player]

    def undo_move(self):
        assert self.move_list
        self.board = self.history.pop()
        move = self.move_list.pop()
        if move is not None:
            self.fields.add(move)
        self.hash = self.hash_history.pop()

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


class TranspositionTable:
    EXACT = 0
    LOWER = 1
    UPPER = 2

    STONES = 1
    DEPTH = 2
    VALUE = 3
    FLAG = 4
    BEST_MOVE = 5

    # hits = 0
    # misses = 0

    def __init__(self, size_power=20):
        self.size = 2**size_power
        self.mask = self.size - 1
        # [lock, stones, depth, value, flag, best_move]
        self.table = [None] * self.size

    def _get_index(self, z_hash):
        return z_hash & self.mask

    def store(self, z_hash, current_stones, stones, depth, value, flag, best_move):
        idx = self._get_index(z_hash)
        existing = self.table[idx]

        if existing is None:
            self.table[idx] = (
                (z_hash, stones, depth, value, flag, best_move),
                (z_hash, stones, depth, value, flag, best_move),
            )
            return

        if existing[1][1] < current_stones:
            existing = (existing[0], existing[0])

        if depth > existing[1][2]:
            existing = (existing[1], (z_hash, stones, depth, value, flag, best_move))
        elif existing[1][0] != z_hash:
            existing = ((z_hash, stones, depth, value, flag, best_move), existing[1])

        self.table[idx] = existing

    def lookup(self, z_hash):
        idx = self._get_index(z_hash)
        entry = self.table[idx]
        if not entry:
            # TranspositionTable.misses += 1
            return None
        if entry[1][0] == z_hash:
            # TranspositionTable.hits += 1
            return entry[1]
        if entry[0][0] == z_hash:
            # TranspositionTable.hits += 1
            return entry[0]
        # TranspositionTable.misses += 1
        return None


class Bot:
    @staticmethod
    def mobility_score(game, player):
        my_moves = len(game.moves(player))
        opponent_moves = len(game.moves(1 - player))
        total_moves = my_moves + opponent_moves
        if total_moves == 0:
            return (0.0, 0.0, 0.0)

        my_potential_moves = 0
        opponent_potential_moves = 0
        my_frontier_discs = 0
        opponent_frontier_discs = 0

        frontier = [[False] * game.M for _ in range(game.M)]

        for field in game.fields:
            if (
                field[0] == 0
                or field[0] == game.M - 1
                or field[1] == 0
                or field[1] == game.M - 1
            ):
                continue
            my_potential = False
            opponent_potential = False
            for d in game.DIRS:
                adjacent = (field[0] + d[0], field[1] + d[1])
                if game.get(adjacent[0], adjacent[1]) == 1 - player:
                    my_potential = True
                    opponent_frontier_discs += 1 - frontier[adjacent[0]][adjacent[1]]
                    frontier[adjacent[0]][adjacent[1]] = True
                elif game.get(adjacent[0], adjacent[1]) == player:
                    opponent_potential = True
                    my_frontier_discs += 1 - frontier[adjacent[0]][adjacent[1]]
                    frontier[adjacent[0]][adjacent[1]] = True
            my_potential_moves += my_potential
            opponent_potential_moves += opponent_potential

        total_potential_moves = max(my_potential_moves + opponent_potential_moves, 1)

        moves_score = (my_moves - opponent_moves) / total_moves
        potential_moves_score = (
            my_potential_moves - opponent_potential_moves
        ) / total_potential_moves

        total_frontier_discs = max(my_frontier_discs + opponent_frontier_discs, 1)
        frontier_score = (
            opponent_frontier_discs - my_frontier_discs
        ) / total_frontier_discs

        return (moves_score, potential_moves_score, frontier_score)

    @staticmethod
    def corner_control_score(game, player):
        corners = [(0, 0), (0, 7), (7, 0), (7, 7)]
        score = [0.0, 0.0, 0.0]
        for corner in corners:
            if game.get(corner[0], corner[1]) == player:
                score[0] += 1
            elif game.get(corner[0], corner[1]) == 1 - player:
                score[0] -= 1
            else:
                for d in game.DIRS:
                    adjacent = (corner[0] + d[0], corner[1] + d[1])
                    if game.get(adjacent[0], adjacent[1]) == 1 - player:
                        score[abs(d[0]) + abs(d[1])] += 1
                    elif game.get(adjacent[0], adjacent[1]) == player:
                        score[abs(d[0]) + abs(d[1])] -= 1
        return tuple(score)

    @staticmethod
    def disc_count_score(game, player):
        my_discs = sum(row.count(player) for row in game.board)
        opponent_discs = sum(row.count(1 - player) for row in game.board)
        return (my_discs - opponent_discs) / (my_discs + opponent_discs)

    @staticmethod
    def stable_border_score(game, player):
        stable_coords = {}
        corners = {
            (0, 0): [(0, 1), (1, 0)],
            (0, 7): [(0, -1), (1, 0)],
            (7, 0): [(0, 1), (-1, 0)],
            (7, 7): [(0, -1), (-1, 0)],
        }

        if all(game.get(0, r) is not None for r in range(game.M)):
            for r in range(game.M):
                stable_coords[(0, r)] = game.get(0, r)

        if all(game.get(game.M - 1, r) is not None for r in range(game.M)):
            for r in range(game.M):
                stable_coords[(game.M - 1, r)] = game.get(game.M - 1, r)

        if all(game.get(c, 0) is not None for c in range(game.M)):
            for c in range(game.M):
                stable_coords[(c, 0)] = game.get(c, 0)

        if all(game.get(c, game.M - 1) is not None for c in range(game.M)):
            for c in range(game.M):
                stable_coords[(c, game.M - 1)] = game.get(c, game.M - 1)

        for (r, c), directions in corners.items():
            color = game.get(r, c)
            if color is None:
                continue

            stable_coords[(r, c)] = color

            for dr, dc in directions:
                curr_r, curr_c = r + dr, c + dc

                for _ in range(6):
                    if game.get(curr_r, curr_c) == color:
                        stable_coords[(curr_r, curr_c)] = color
                        curr_r += dr
                        curr_c += dc
                    else:
                        break

        p1_stable = sum(1.0 for col in stable_coords.values() if col == player)
        p2_stable = sum(1.0 for col in stable_coords.values() if col == 1 - player)

        total_stable = p1_stable + p2_stable
        if total_stable == 0:
            return 0.0

        return (p1_stable - p2_stable) / game.M

    @staticmethod
    def evaluate(game, player, player_to_move):
        empty_fields = len(game.fields)
        if game.terminal():
            result = game.result()
            if player:
                return 1000 if result > 0 else -1000 if result < 0 else 0
            else:
                return -1000 if result > 0 else 1000 if result < 0 else 0

        moves = game.moves(player_to_move)
        if not moves:
            return Bot.evaluate_move(game, None, player_to_move, player)

        if len(moves) == 1:
            return Bot.evaluate_move(game, moves[0], player_to_move, player)

        num_discs = 64 - empty_fields
        mobility = (
            Bot.mobility_score(game, player) if num_discs <= 35 else (0.0, 0.0, 0.0)
        )
        # mobility = (0.0, 0.0, 0.0)
        corner_control = Bot.corner_control_score(game, player)
        disc_count = Bot.disc_count_score(game, player)
        stable_border = Bot.stable_border_score(game, player)

        # [moves_score, potential_moves_score, frontier_score, corner_control_score, corner_adjacent_score, corner_diagonal_score, stable_border_score, disc_count_score]
        if num_discs <= 20:
            coeffs = [5, 3, 4, 50, 10, 25, 6, 6]
        elif num_discs <= 50:
            coeffs = [2, 1, 1.5, 80, 15, 30, 10, 25]
        else:
            coeffs = [0, 0, 0, 12, 4, 6, 12, 120]

        values = (*mobility, *corner_control, stable_border, disc_count)
        return sum(c * v for c, v in zip(coeffs, values))

    @staticmethod
    def evaluate_move(game, move, player, root_player, transposition_table=None):
        game.do_move(move, player)
        if transposition_table is not None:
            tt_entry = transposition_table.lookup(game.hash)
            if tt_entry and tt_entry[TranspositionTable.STONES] == 64 - len(
                game.fields
            ):
                game.undo_move()
                return tt_entry[TranspositionTable.VALUE]
        score = Bot.evaluate(game, root_player, 1 - player)
        game.undo_move()
        return score

    turns_since_time_check = 0

    @staticmethod
    def alpha_beta(
        game,
        player,
        depth,
        alpha,
        beta,
        maximizing,
        root_player,
        current_stones,
        deadline,
        transposition_table,
    ):
        Bot.turns_since_time_check += 1
        if Bot.turns_since_time_check >= 100:
            if time.time() > deadline:
                raise TimeoutError()
            Bot.turns_since_time_check = 0

        if depth == 0 or game.terminal():
            return Bot.evaluate(game, root_player, player), None

        tt_entry = transposition_table.lookup(game.hash)
        best_move = None

        if (
            tt_entry
            and tt_entry[TranspositionTable.DEPTH] >= depth
            and tt_entry[TranspositionTable.STONES] == 64 - len(game.fields)
        ):
            flag = tt_entry[TranspositionTable.FLAG]
            if flag == TranspositionTable.EXACT:
                return (
                    tt_entry[TranspositionTable.VALUE],
                    tt_entry[TranspositionTable.BEST_MOVE],
                )
            elif flag == TranspositionTable.LOWER:
                alpha = max(alpha, tt_entry[TranspositionTable.VALUE])
            elif flag == TranspositionTable.UPPER:
                beta = min(beta, tt_entry[TranspositionTable.VALUE])
            if alpha >= beta:
                return (
                    tt_entry[TranspositionTable.VALUE],
                    tt_entry[TranspositionTable.BEST_MOVE],
                )

            best_move = tt_entry[TranspositionTable.BEST_MOVE]

        moves = game.moves(player)

        if not moves:
            game.do_move(None, player)
            try:
                score, _ = Bot.alpha_beta(
                    game,
                    1 - player,
                    depth,
                    alpha,
                    beta,
                    not maximizing,
                    root_player,
                    current_stones,
                    deadline,
                    transposition_table,
                )
            finally:
                game.undo_move()
            return score, None

        if depth >= 3:
            moves = sorted(
                moves,
                key=lambda move: Bot.evaluate_move(
                    game, move, player, root_player, transposition_table
                ),
                reverse=maximizing,
            )

        if best_move is not None and best_move in moves:
            moves.remove(best_move)
            moves.insert(0, best_move)

        original_alpha = alpha
        stones = 64 - len(game.fields)
        best_move = None
        all_moves = []
        if maximizing:
            value = -math.inf
            for move in moves:
                game.do_move(move, player)
                try:
                    score, _ = Bot.alpha_beta(
                        game,
                        1 - player,
                        depth - 1,
                        alpha,
                        beta,
                        False,
                        root_player,
                        current_stones,
                        deadline,
                        transposition_table,
                    )
                finally:
                    game.undo_move()
                all_moves.append((score, move))
                if score > value:
                    value, best_move = score, move
                alpha = max(alpha, value)
                if alpha >= beta:
                    break
        else:
            value = math.inf
            for move in moves:
                game.do_move(move, player)
                try:
                    score, _ = Bot.alpha_beta(
                        game,
                        1 - player,
                        depth - 1,
                        alpha,
                        beta,
                        True,
                        root_player,
                        current_stones,
                        deadline,
                        transposition_table,
                    )
                finally:
                    game.undo_move()
                if score < value:
                    value, best_move = score, move
                beta = min(beta, value)
                if alpha >= beta:
                    break

        if value <= original_alpha:
            flag = TranspositionTable.UPPER
        elif value >= beta:
            flag = TranspositionTable.LOWER
        else:
            flag = TranspositionTable.EXACT
        transposition_table.store(
            game.hash,
            current_stones,
            stones,
            depth,
            value,
            flag,
            best_move,
        )
        if (
            maximizing
            and stones == current_stones
            and stones + depth <= 20
            and value != 0.0
        ):
            better_moves = [
                (s, m) for s, m in all_moves if (value - s) / abs(value) < 0.05
            ]
            # print(
            #     f"Depth {depth} (stones {stones}): {len(better_moves)} better moves out of {len(all_moves)}",
            #     file=sys.stderr,
            # )
            return random.choice(better_moves)
        return value, best_move

    @staticmethod
    def choose_move(game, player, move_time, transposition_table):
        deadline = time.time() + move_time
        moves = game.moves(player)
        if len(moves) == 1:
            return moves[0]
        if not moves:
            return None
        best_move = game.moves(player)[0]  # fallback
        last_depth_time = 0
        for depth in range(1, len(game.fields) + 1):  # iterative deepening
            if deadline - time.time() < last_depth_time:
                # print(
                #     f"Stopping before depth {depth} to avoid timeout, saved {deadline - time.time()}s",
                #     file=sys.stderr,
                # )
                break
            try:
                start = time.time()
                value, move = Bot.alpha_beta(
                    game,
                    player,
                    depth,
                    -math.inf,
                    math.inf,
                    True,
                    player,
                    64 - len(game.fields),
                    deadline,
                    transposition_table,
                )
                if move is not None:
                    best_move = move
                last_depth_time = time.time() - start
                # print(
                #     f"Depth {depth} completed in {last_depth_time:.2f} seconds, best move: {best_move}, value: {value}",
                #     file=sys.stderr,
                # )
            except TimeoutError:
                # print(f"Depth {depth-1} completed", file=sys.stderr)
                break  # return best complete iteration
        # else:
        # print(f"Depth {len(game.fields)} completed (complete)", file=sys.stderr)

        return best_move


class Player(object):
    def __init__(self):
        self.reset()

    def reset(self):
        self.game = Reversi()
        self.my_player = 1
        self.transposition_table = TranspositionTable()
        self.say("RDY")

    def say(self, what):
        sys.stdout.write(what)
        sys.stdout.write("\n")
        sys.stdout.flush()

    def hear(self):
        line = sys.stdin.readline().split()
        return line[0], line[1:]

    def loop(self):
        while True:
            cmd, args = self.hear()
            if cmd == "HEDID":
                move_timeout, game_timeout = tuple(float(x) for x in args[:2])
                move = tuple((int(m) for m in args[2:]))
                if move == (-1, -1):
                    move = None
                self.game.do_move(move, 1 - self.my_player)
            elif cmd == "ONEMORE":
                # print(self.game.result(), file=sys.stderr)
                # print(self.game.move_list, file=sys.stderr)
                self.reset()
                continue
            elif cmd == "BYE":
                break
            else:
                assert cmd == "UGO"
                assert not self.game.move_list
                self.my_player = 0
                move_timeout, game_timeout = tuple(float(x) for x in args[:2])

            time_to_move = min(
                move_timeout * 0.95, game_timeout / (len(self.game.fields) / 2 + 1)
            )

            move = Bot.choose_move(
                self.game, self.my_player, time_to_move, self.transposition_table
            )
            self.game.do_move(move, self.my_player)
            if not move:
                move = (-1, -1)
            self.say(f"IDO {move[0]} {move[1]}")
            # self.game.draw()
            # print(self.game.moves(1 - self.my_player))


if __name__ == "__main__":
    player = Player()
    player.loop()
