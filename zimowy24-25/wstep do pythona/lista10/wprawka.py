import matplotlib.pyplot as plt

def histogram(tab):
    histogram_dict = {}
    for i in tab:
        if i in histogram_dict:
            histogram_dict[i] += 1
        else:
            histogram_dict[i] = 1
    return histogram_dict

def to_percantage(histo_dict:dict):
    length = sum(histo_dict.values())
    return {i:round(histo_dict[i]/length,3) for i in histo_dict}

def guess_lang(word):
    pol__freq = 0
    eng_freq = 0
    for i in word:
        if i not in histogram_eng: return 'pol'
        elif i not in histogram_pol: return 'eng'
        pol__freq += histogram_pol[i]/histogram_eng[i]
        eng_freq += histogram_eng[i]/histogram_pol[i]
    return 'pol' if pol__freq > eng_freq else 'eng'

def draw_histo(a,b):
    pass

with open('brown.txt','r') as eng, open('lalka-tom-pierwszy.txt','r',encoding='utf8') as pol:
    ang = [i.lower() for i in eng.read() if i.isalpha()]
    pol = [i.lower() for i in pol.read() if i.isalpha()]
    histogram_eng = to_percantage(histogram(ang))
    histogram_pol = to_percantage(histogram(pol))
    
    #print(histogram_eng)
    #print(histogram_pol)
    #print(guess_lang('boy'))
    for i in histogram_pol:
        if i not in histogram_eng:
            histogram_eng[i] = 0

import matplotlib.pyplot as plt
import numpy as np

# Example data
categories = sorted(histogram_pol.keys())
polish = [histogram_pol[i] for i in categories]
english = [histogram_eng[i] for i in categories]

# Bar positions
x = np.arange(len(categories))
width = 0.4  # Width of each bar

# Plot bar graphs
plt.bar(x - width/2, polish, width=width, label='pol')
plt.bar(x + width/2, english, width=width, label='eng')

# Add labels and legend
plt.xticks(x, categories)
plt.xlabel('Categories')
plt.ylabel('Prawdopodobieństwo wystąpienia')
plt.title('Polsko-Angielskie częstotliwości')
plt.legend()

# Show plot
plt.show()