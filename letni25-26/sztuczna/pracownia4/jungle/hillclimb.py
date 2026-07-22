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
BOUNDS = [
    (0, 1000),   # w1
    (0, 1000),   # w2
    (0, 10),   # w3
]

STEP = {0 : 10, 1 : 10, 2 : 4}

# =========================
# EVALUATION
# =========================

def write_weights_to_sh(weights, path="jungle/bot.sh"):
    w = " ".join(str(x) for x in weights)

    content = f"""#!/bin/bash
python3 jungle/jungle_bot.py {w}
"""

    with open(path, "w") as f:
        f.write(content)


def run_prog():
    cmd = [
        "python3",
        "ai_dueler.py",
        "--verbose", "0",
        "--num_games", "30",
        "jungle",
        "jungle/bots/baloo.sh",
        "jungle/bot.sh"
    ]
    

    try:
        out = subprocess.check_output(cmd, timeout=60).decode().strip()
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

    idx = random.randint(0, len(weights) - 1)
    step = STEP[idx]
    if idx == 2: #elephant weight
        new_w[idx] += random.randint(-step, step) / 8
    else:
        new_w[idx] += random.randint(-step, step)

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
    current = [145,36,2]
    # current = [0, 0, 0]
    # write_weights_to_sh(current)
    


    current_score = evaluate()

    best = current[:]
    best_score = current_score

    print("Start:", current, best_score, file = sys.stderr)
    i = 0
    while True:
        if i % 100 == 0:
            with open('jungle/scores.json','w') as out:
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
        best_score = score_cache[best][0] / score_cache[best][1]

        # print(f"{i}: {candidate} score={candidate_score}; {best} best={best_score}", file = sys.stderr)
        
        i += 1

    # print("BEST:", best, best_score, file = sys.stderr)

    # with open('jungle/scores.json','w') as out:
    #     json_ready = {json.dumps(list(k)): v for k, v in score_cache.items()}
    #     print(json_ready)
    #     json.dump(json_ready,out)


if __name__ == "__main__":
    
    with open('jungle/scores.json','r') as inp:
        raw = json.load(inp)
        score_cache = {tuple(json.loads(k)): v for k, v in raw.items()}
        print(score_cache)

    hillclimb()
    # print(run_prog(), file = sys.stderr)
    # hillclimb()
    # write_weights_to_sh([1,2,3,4])