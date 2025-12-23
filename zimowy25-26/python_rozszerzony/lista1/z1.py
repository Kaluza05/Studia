from decimal import Decimal

def vat_faktura(zakupy : list[float])-> float:
    return 0.23 * sum(zakupy)

def vat_paragon(zakupy : list[float])-> float:
    return sum(map(lambda x : x * 0.23, zakupy))

def vat_faktura_dec(zakupy : list[Decimal])-> float:
    return Decimal('0.23') * sum(zakupy)

def vat_paragon_dec(zakupy : list[Decimal])-> float:
    return sum(map(lambda x : x * Decimal('0.23'), zakupy))

def find_difference():
    for i in range(1,1000):
        for j in range(1,1000):
            i = i / 100
            j = j / 100
            if vat_faktura([i,j]) != vat_paragon([i,j]):
                print(i,j)
                
zakupy = [0.04, 0.01]
zakupy_decimal = [Decimal('0.04'), Decimal('0.01')]

print(vat_faktura(zakupy))
print(vat_paragon(zakupy))

print(vat_faktura_dec(zakupy_decimal))
print(vat_paragon_dec(zakupy_decimal))