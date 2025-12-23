import matplotlib.pyplot as plt
import numpy as np

ts = [ 0, 10, 20, 30, 40, 80, 90, 95]
ys = [68.0, 67.1, 66.4, 65.6, 64.6, 61.8, 61.0, 60.0]

sum_ts = sum(ts)
sum_sq_ts = sum(t**2 for t in ts)
sum_ys = sum(ys)
sum_ysts = sum(y * t for y,t in zip(ys,ts))

num_a   = sum_ysts / sum_ts - sum_ys / 8
denom_a = sum_sq_ts / sum_ts - sum_ts / 8

num_b   = sum_ysts / sum_sq_ts - sum_ys / sum_ts
denom_b = sum_ts / sum_sq_ts - 8/sum_ts

a = num_a / denom_a
b = num_b / denom_b

x = np.linspace(-1,96,2)
y = a*x + b
print(a,b)

plt.plot(x,y)
plt.scatter(ts, ys, color='red', label='punkty')
plt.show()