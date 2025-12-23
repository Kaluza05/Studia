import numpy as np

def d(x,y):
    d = np.power(x,2, dtype = np.float32)
    d = d- np.power(x,2, dtype = np.float32)
    return d

def d2(x,y):
    d = x + y
    d = d * (x - y)
    return d

print(d(np.float32(10**20),np.float32(10**20)))
print(d2(np.float32(10**20),np.float32(10**20)))