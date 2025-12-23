import numpy as np
import matplotlib.pyplot as plt
from math import sin, cos , pi

t = [0, 2, 4, 6, 8, 10,]
h = [ 1 ,1.6 ,1.4 ,0.6 ,0.2 ,0.8]

s = [sin(2*pi*t/12) for t in t]
c = [cos(2*pi*t/12) for t in t]


sum_s = sum(s)
sum_c = sum(c)

sum_sc = sum(s*c  for s,c in zip(s,c))
sum_sh = sum(s* h for s,h in zip(s,h))
sum_ch = sum(c* h for c,h in zip(c,h))
sum_s2 = sum(s**2 for s in s)
sum_c2 = sum(c**2 for c in c)
sum_h  = sum(h)

print(6,sum_s,sum_c,      sum_h)
print(sum_s,sum_s2,sum_sc,sum_sh)
print(sum_c,sum_sc,sum_c2,sum_ch)
print(3**0.5)