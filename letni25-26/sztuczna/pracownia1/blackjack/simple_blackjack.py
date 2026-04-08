"""
agent1 calculates EV['stand'] and EV['bet']
and choses action with bigger EV.
agent2 also calculates EV's, but in addition if he sees that he's in a losing position he takes cards from the deck (even over 21)
until the deck is in our favour.
"""


import random
from collections import Counter
from functools import lru_cache

DEFAULT_PROB = tuple(round((1/13 if i != 10 else 4/13),3) for i in range(2,12))


def modify_stats(deck_stats):
    total = sum(deck_stats.values())
    return tuple(round(deck_stats[i]/total,3) if i in deck_stats else 0 for i in range(2,12))

@lru_cache(None)
def ev_stand(dealer,me, deck_stats = DEFAULT_PROB):

    count = 0
    for i,p in enumerate(deck_stats):
        card = i + 2
        new_dealer = dealer + card
        if new_dealer <= 21:
            if new_dealer > me:
                count -= p
            elif new_dealer == me:
                count += 0
            else:
                count += 0.5 * p
    
    return count


@lru_cache(None)
def v(dealer,me, deck_stats = DEFAULT_PROB):
    
    if me > 21:
        return -1
    
    ev_s = ev_stand(dealer,me,deck_stats)
    ev_h = 0

    for i,p in enumerate(deck_stats):
        card = i + 2
        ev_h += p * v(dealer,me+card,deck_stats) 
    
    return max(ev_h,ev_s)

p1 = 0.5
p2 = 0.45
def favourable_deck(deck_stats):
    return sum(deck_stats[i-2] for i in [2,3,4,5]) > p1 or sum(deck_stats[i-2] for i in [8,9,10,11]) < p2

#precompute strategy for agent1 which uses only dealers card
STRATEGY = {}
for dealer in range(2,12):
    for me in range(2,22):
        STRATEGY[(dealer,me)] = (
            'stand'
            if v(dealer,me,DEFAULT_PROB) == ev_stand(dealer,me,DEFAULT_PROB)
            else
            'hit'
        )


def initial_deck():
    color = list(range(2,10)) + 4 * [10] + [11]
    deck = 4 * color
    random.shuffle(deck)
    return deck


class Deck:
    def __init__(self, N=1):
        deck = initial_deck()
        self.deck = N * deck
        random.shuffle(self.deck)
        self.position = 0
    
    def get_card(self):
        if self.position >= len(self.deck):
            return None
            
        rv = self.deck[self.position]
        self.position += 1
        return rv
   
    def get_stats(self):
        return Counter(self.deck[self.position:])
        
    def __len__(self):
        return len(self.deck) - self.position
                      
                      

def agent_default(_dealer_card, my_value, _deck_stats=None):
    "TODO: add more intelligence"
        
    if my_value < 14:
        return 'hit'
    return 'stand'


def agent1(dealer_card, my_value):
    if my_value <= 10:
        return 'hit'
    if my_value >= 21:
        return 'stand'
    
    return STRATEGY[(dealer_card,my_value)]

ev_threshold = -0.5
def agent2(dealer_card, my_value, deck_stats = None):
    if deck_stats == None:
        deck_stats = DEFAULT_PROB
    else:
        deck_stats = modify_stats(deck_stats)

    if my_value <= 10:
        return 'hit'
    
    ev_hand = v(dealer_card, my_value, deck_stats)
    if ev_hand > ev_threshold:
        if ev_hand == ev_stand(dealer_card, my_value, deck_stats):
            return 'stand'
        return 'hit'
    else:
        if favourable_deck(deck_stats):  #we hit until we get a deck that suits us (deck consisting of lower cards)
            return 'stand'
        return 'hit'



def game(deck: Deck):
    card1 = deck.get_card()
    
    # stats = deck.get_stats()
    
    card2 = deck.get_card()
    
    agent_sum = 0
    
    while True:
        if len(deck) == 0:
            break
            
        agent_sum += deck.get_card()
        # if agent1(card1, agent_sum) == 'stand':
        if agent2(card1, agent_sum,deck.get_stats()) == 'stand':
            break  

    dealer_sum = card1 + card2
    
    if dealer_sum > 21:
        dealer_sum = 0
        
    if agent_sum > 21:
        agent_sum = 0
        
    if agent_sum > dealer_sum:
        return 0.5
    
    if agent_sum < dealer_sum:    
        return -1
        
    return 0


def cassino(N):
    DECKS = 8
    score = 0
    deck = Deck(N=DECKS)
    for _ in range(N):
        if len(deck) < 6:
            deck = Deck(N=DECKS)

        score += game(deck)        
    return score / N    

print ('Average reward', cassino(10**4))