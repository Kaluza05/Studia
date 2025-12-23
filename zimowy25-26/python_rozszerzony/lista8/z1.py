import matplotlib.pyplot as plt
import os
import json
from dotenv import load_dotenv
import pandas as pd
import requests
import matplotlib.dates as mdates
from datetime import datetime
import numpy as np

load_dotenv()

MARKET_API_KEY = os.getenv("MARKET_API_KEY")
MARKET_URL = 'https://api.marketstack.com/v2/'


SET_DATE = datetime.strptime('2000-01-01', "%Y-%m-%d").toordinal()

FILE_DIR = os.path.dirname(os.path.abspath(__file__))

class LinearRegression:
    def __init__(self, steps : int = 1e3, alpha : float = 1e-2):
        self.pred = None
        self.steps = steps
        self.alpha = alpha

    def fit(self,X,y)-> None:
        #X, y - suppose are np.arrays
        steps = 0
        w,b = 1,0
        size = len(y)

        mean = X.mean()
        std =  X.std()
        X = (X - mean) / std
        
        while steps < self.steps:
            y_pred = w * X + b

            dw : float = 2/size * np.sum(X * (y_pred - y))
            db : float = 2/size * np.sum(y_pred - y)
            w -= dw * self.alpha                      #going agains the gradient to find minimum
            b -= db * self.alpha
            steps += 1

        w,b = w / std, b - w * mean / std        
        self.pred = w,b

    def get_coeffs(self):
        return self.pred

    def predict(self,x):
        return self.pred[0] * x + self.pred[1]
    

def create_url(base_url : str, access_key : str, subpage = None,  **kwargs : dict[str,str]):
    """
    adds parameters to the provided url along with the access key if needed, this works for one of the sites, lets see 
    first if the same pattern holds for other pages
    """
    subpage = '' if subpage is None else subpage
    return base_url + subpage + f"?access_key={access_key}" + ''.join('&' + name + '=' + str(param) for name,param in kwargs.items())

    
def fetch(url, file_name, replace = False):
    """
    fetch data to use for future plots
    """
    data_file = FILE_DIR + '/' + file_name + '.json'
    if os.path.exists(data_file) and not replace:
        with open(data_file, 'r') as f:
            return pd.DataFrame(json.load(f))
    else:
        data = requests.get(url)
        data_json = data.json()
        with open(data_file,'w') as f:
            json.dump(data_json['data'], f , indent = 4, ensure_ascii= False)

        return pd.DataFrame(data_json['data'])
        #if we dont have the results already we will want to write to them

        
def plot_results(data1 : pd.DataFrame, data2 : pd.DataFrame, rolling = None, data1_label = None, data2_label = None)-> None:
    """
    plots data contained in a dataframe where index is a datetime Y-M-D date, with columns being a superset of {'price'}
    """
    if data1_label is None:
        data1_label = ''
    if data2_label is None:
        data2_label = ''

    years = sorted(set(data1.index.year))

    if rolling:
        make_rolling = lambda df : df['price'].rolling(window = rolling, min_periods= 1).mean()
        data1['price'] = make_rolling(data1)
        data2['price'] = make_rolling(data2)
    
    #plot separately for each year
    fig,ax = plt.subplots(nrows = len(years),ncols = 1, figsize = (10, 4*len(years)))

    if len(years) == 1:
        ax = [ax]

    COLOR1 = 'blue'
    COLOR2 = 'red'
    for y, ax in zip(years,ax):
        yearly1 = data1[data1.index.year == y]
        yearly2 = data2[data2.index.year == y]

        ax.xaxis.set_major_locator(mdates.MonthLocator())   # one tick per month
        ax.xaxis.set_major_formatter(mdates.DateFormatter('%b'))


        ax.set_title(f"Year {y}")
        
        

        ax2 = ax.twinx()

        ax.plot(yearly1.index, yearly1["price"], label = data1_label, color = COLOR1)
        ax.set_ylabel(f"{data1_label} price $", color = COLOR1)
        ax.tick_params(axis='y', labelcolor = COLOR1)

        ax2.plot(yearly2.index, yearly2["price"], label = data2_label, color = COLOR2)
        ax2.set_ylabel(f"{data2_label} price $", color = COLOR2)
        ax2.tick_params(axis='y', labelcolor = COLOR2)

        lines_1, labels_1 = ax.get_legend_handles_labels()
        lines_2, labels_2 = ax2.get_legend_handles_labels()

        ax.legend(lines_1 + lines_2, labels_1 + labels_2, loc="upper left")
    plt.show()



    
def predict_future_values(data : pd.DataFrame, pred_date : str):
    """
    predicts future value based on the data avalible about the past.

    assume that prices go up linearly- linear regression
    """

    #convert to datetime then to ordinal, days since year 1, exuivalently could start from 1970...
    

    date_dt = datetime.strptime(pred_date, "%Y-%m-%d")
    to_pred = date_dt.toordinal() - SET_DATE

    X = data['date_days']
    y = data['price']

    model = LinearRegression()
    model.fit(X,y)

    return model.predict(to_pred)

def get_pred(data : pd.DataFrame)-> None:
    model = LinearRegression()
    model.fit(data['data_days'],data['price'])
    coeffs = model.get_coeffs()

    preds = pd.DataFrame()
    preds['price'] = coeffs[0] * data['data_days']+ coeffs[1]

    return preds

def plot_pred(data : pd.DataFrame, label = '')-> None:
    preds = get_pred(data)

    plt.plot(data['price'], label = label)
    plt.plot(preds, label = 'predicion')
    plt.legend()
    plt.show()

def clean_df(df : pd.DataFrame)-> pd.DataFrame:
    df = df.copy()
    df = df[['close','volume','date']]
    df['date'] = df['date'].str[:10]
    df['date'] = pd.to_datetime(df['date'], format = "%Y-%m-%d")
    df = df.set_index('date')
    df = df.sort_index()
    df = df.rename(columns= {'close' : 'price'})
    df['volume'] = df['volume'].astype(int)

    return df


SYMBOL1 = 'USO'
SYMBOL2 = 'GLD'
DATE_FROM = '2022-01-01'
DATE_TO = '2025-11-01'

oil_url = create_url(MARKET_URL, 
                          access_key= MARKET_API_KEY, 
                          subpage= 'eod', 
                          symbols = SYMBOL1,
                          date_from = DATE_FROM,
                          data_to = DATE_TO,
                          limit = 1000)

gold_url = create_url(MARKET_URL, 
                          access_key= MARKET_API_KEY, 
                          subpage= 'eod', 
                          symbols = SYMBOL2,
                          date_from = DATE_FROM,
                          data_to = DATE_TO,
                          limit = 1000)

print(oil_url)
oil_data = fetch(oil_url, 'oil_data')
gold_data = fetch(gold_url, 'gold_data')



oil_data = clean_df(oil_data)
gold_data = clean_df(gold_data)

oil_data['data_days'] = oil_data.index.map(pd.Timestamp.toordinal).values - SET_DATE
gold_data['data_days'] = gold_data.index.map(pd.Timestamp.toordinal).values - SET_DATE


plot_results(oil_data, gold_data,rolling=30, data1_label='oil',data2_label='gold')

plot_pred(oil_data, label = 'oil')
plot_pred(gold_data, label = 'gold')


"""
get better data cuz what u mean im saying get me data from 2022-2025 and it gets me 2024-2025

"""