with open('popularne_slowa2023.txt') as data:
    data = data.read().splitlines()
    wspak = {i[::-1] for i in data}
    added = set()
    pary_na_wspak = set()
    for i in data:
        if i in wspak and i not in added:
            pary_na_wspak.add((i,i[::-1]))
            added.add(i[::-1])


    print(pary_na_wspak)