y_i1 = 1
y_i2 = - 1/9
for i in range(2,50):
    temp = y_i2
    y_i2 = 98/9 * y_i2 + 11 / 9 * y_i1
    y_i1 = temp
    print(f'y_{i} = {y_i2}')