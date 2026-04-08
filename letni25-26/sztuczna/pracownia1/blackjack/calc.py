def prob(i):
    return 4/52 if i != 10 else 16/52

def prob_more(i):
        count_more = 0
        for j in range(i,12):
            count_more += 4 if j != 10 else 16
        return count_more / 52

def chance_to_bust(i):
    if i <= 10:
        return 0
        
    return prob_more(22-i)

def chance_for_dealer_win(dealer,me):
    outcomes = [prob(i) if dealer + i > me and dealer + i <= 21 else 0 for i in range(2,12)]
    return sum(outcomes)

print(chance_for_dealer_win(14,14))

print(chance_to_bust(10))