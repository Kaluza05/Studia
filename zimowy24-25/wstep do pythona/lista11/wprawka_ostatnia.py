OPERATORS = ['+','-','*','']
def find_2025():
    def inside(exp,ops=0):
        if eval(''.join(exp)) == 2025:
            wynik.append(''.join(exp))
            return
        if ops>=9:return
        for i in OPERATORS:
            exp[2*ops+1] = i
            inside(exp,ops+1)
            exp[2*ops+1] = ''

    wynik = []
    inside(['1','','2','','3','','4','','5','','6','','7','','8','','9','','0'])
    return wynik

print(find_2025())