polskawe = { 'ż' : 'z', 'ó' : 'o', 'ł' : 'l', 'ć' : 'c', 'ę' : 'e', 'ś' : 's', 'ą' : 'a', 'ź' : 'z', 'ń' : 'n'}

def without_special(text):
    for el in polskawe.keys():
            text = text.replace(el,polskawe[el])
    return text

with open('popularne_slowa2023.txt','r',encoding='utf8') as data:
      data = data.read().splitlines()
      same_dict = {}

      for i in data:
          b = without_special(i)
          if b in same_dict:
                same_dict[b][1] += 1
                same_dict[b][0].append(i)
          else:
               same_dict[b] = [[i],1]

      more_than_4 = []
      for i in same_dict:
            if same_dict[i][1] >=4:
                more_than_4.append(same_dict[i][0])

      print(*more_than_4,sep='\n')
      print(len(more_than_4))
      #print(data[:100])