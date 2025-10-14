import numpy as np

def f(x):
    return 162 * (1 - np.cos(5 * x)) / (x ** 2)

i_values = np.arange(1, 21)

x_single = np.power(10, -i_values, dtype=np.float32)
f_single = f(x_single)

x_double = np.power(10, -i_values, dtype=np.float64)
f_double = f(x_double)

# Wyniki
print(" i   x (single)   f(x) single       x (double)   f(x) double")
print("-----------------------------------------------------------------------")
for i, xs, fs, xd, fd in zip(i_values, x_single, f_single, x_double, f_double):
    print(f"{i:2d}  {xs: .3e}   {fs: .8e}   {xd: .3e}   {fd: .16e}")