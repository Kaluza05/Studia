from losowanie_fragmentow import losuj_fragment

def haslo(n):
    if n >= 6: #gwarantuje, że nie zostaniemy z potrzebą wylosowania hasła o długości 1(niemożliwe)
        a = losuj_fragment()
        return a + haslo(n-len(a))
    
    elif n == 5:
        while True:
            a=losuj_fragment()
            if len(a) == 3:
                return a + haslo(2)
            elif len(a) == 2: 
                return a + haslo(3)
    elif n == 4:
        while True:
            a = losuj_fragment()
            if len(a) == 4:
                return a
            elif len(a) == 2:
                return a+haslo(2)
    elif n == 3:
        while True:
            a = losuj_fragment()
            if len(a) == 3:
                return a
    elif n == 2:
        while True:
            a = losuj_fragment()
            if len(a)==2:
                return a

for i in range(10):
    print(haslo(8))
for i in range(10):
    print(haslo(12))