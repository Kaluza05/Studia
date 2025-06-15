from murek import murek
from turtle import *
from random import choice

def kwadrat_kolor(command,size=10):
    murek(command,size)

def spirala(length,size=10):
    curr_command = ''
    for i in range(1,length+1):

        curr_command += i*'f'+'r'+choice('ypg')
    murek(curr_command,size)


tracer(0)
#kwadrat_kolor('p'+4*'ffyffrp')
spirala(30)
update()
done()

