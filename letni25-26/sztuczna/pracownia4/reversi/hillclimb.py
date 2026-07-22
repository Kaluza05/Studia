import subprocess
import random
import math
import time
import json
import sys
# =========================
# CONFIG
# =========================


ITERATIONS = 200
GAMES_PER_EVAL = 50

# zakres wag
BOUNDS = [(0,20)] * 15
for i in range(3):
    BOUNDS[5*i+3] = (-20,20)
    BOUNDS[5*i+4] = (-20,20)

print(BOUNDS)
# STEP = {0 : 10, 1 : 10, 2 : 4}

# =========================
# EVALUATION
# =========================

def write_weights_to_sh(weights, path="reversi/my_bot.sh"):
    w = " ".join(str(x) for x in weights)

    content = f"""#!/bin/bash
python3 reversi/reversi.py {w}
"""

    with open(path, "w") as f:
        f.write(content)


def run_prog():
    cmd = [
        "python3",
        "ai_dueler.py",
        "--verbose", "0",
        "--num_games", "100",
        "reversi",
        "reversi/random.sh",
        "reversi/my_bot.sh"
    ]
    

    try:
        out = subprocess.check_output(cmd, timeout=500).decode().strip()
    except subprocess.CalledProcessError:
        return "lose"
    except subprocess.TimeoutExpired:
        return "draw"

    res = out.split('won-tied-lost')
    stats = [int(x.strip()) for x in res[1].split('times')[0].split('-')]

    return stats



def evaluate():
    
    l,d,w = run_prog()
    
    score = -l - 0.1 * d + w

    return score


def evaluate_avg(weights, extra_runs=1):
    key = tuple(weights)

    # jeśli pierwszy raz
    if key not in score_cache:
        score_cache[key] = [0.0, 0]  # [sum, count]

    total, count = score_cache[key]

    # zawsze dogrywamy nowe gry
    for _ in range(extra_runs):
        write_weights_to_sh(weights)
        total += evaluate()

    count += extra_runs
    score_cache[key] = [round(total,2), count]

    return total / count


# =========================
# MUTATION
# =========================

def mutate(weights):
    new_w = weights[:]

    to_change = random.sample(range(len(weights)-1),4)
    step = 16
    for i in to_change:
        new_w[i] += random.randint(-step, step) / 16

    write_weights_to_sh(new_w)

    # clamp
    for i in range(len(new_w)):
        lo, hi = BOUNDS[i]
        new_w[i] = max(lo, min(hi, new_w[i]))

    return new_w


# =========================
# MAIN LOOP
# =========================

def hillclimb():
    current = [5.3125, 0.225, 4.25, -2.6875, 0.375, 3.0, 1.0, 3.5625, -2.6875, 0.9375, 1.125, 6.6875, 2.0, -1.0, 0]

    # 5.3125, 0.225, 4.25, -2.6875, 0.375, 3.0, 1.0, 3.5625, -2.6875, 0.9375, 1.125, 6.6875, 2.0, -1.0, 0
    # current = [0, 0, 0]
    # write_weights_to_sh(current)
    


    current_score = evaluate()

    best = current[:]
    best_score = current_score

    print("Start:", current, best_score, file = sys.stderr)
    i = 0
    while True:
        print(f"{i}: {best} best={best_score}", file = sys.stderr)
        if i % 10 == 0:
            print(i//10,'dumping', file = sys.stderr)
            print(f"{i}: {best} best={best_score}", file = sys.stderr)
            with open('reversi/scores.json','w') as out:
                json_ready = {json.dumps(list(k)): v for k, v in score_cache.items()}
                # print('dumping',json_ready)
                json.dump(json_ready,out)
        # print(score_cache)
        candidate = mutate(list(best))
        candidate_score = evaluate_avg(candidate, extra_runs= 1)
    

        if candidate_score > best_score:
            best = candidate
            best_score = candidate_score

        best = max(score_cache, key = lambda x : score_cache[x][0] / score_cache[x][1])
        evaluate_avg(best) #change best after each iteration
        best_score = score_cache[best][0] / score_cache[best][1]

        # print(f"{i}: {candidate} score={candidate_score}; {best} best={best_score}", file = sys.stderr)
        
        i += 1

    # print("BEST:", best, best_score, file = sys.stderr)

    # with open('jungle/scores.json','w') as out:
    #     json_ready = {json.dumps(list(k)): v for k, v in score_cache.items()}
    #     print(json_ready)
    #     json.dump(json_ready,out)


if __name__ == "__main__":
    
    with open('reversi/scores.json','r') as inp:
        raw = json.load(inp)
        score_cache = {tuple(json.loads(k)): v for k, v in raw.items()}
        print(score_cache)

    hillclimb()
    # print(run_prog(), file = sys.stderr)
    # hillclimb()
    # write_weights_to_sh([1,2,3,4])