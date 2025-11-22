from dotenv import load_dotenv
import os
import asyncio, aiohttp
import pandas as pd
import json

load_dotenv()

MARKET_API_KEY = os.getenv("MARKET_API_KEY")
MARKET_URL = 'https://api.marketstack.com/v2/'
TAXI_URL = 'https://files.cloudgdansk.pl/f/xml/wykaz-taksowek-z-licencjami.json'


SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

def create_url(base_url : str, access_key : str, subpage = None,  **kwargs : dict[str,str]):
    """
    adds parameters to the provided url along with the access key if needed, this works for one of the sites, lets see 
    first if the same pattern holds for other pages
    """
    subpage = '' if subpage is None else subpage
    return base_url + subpage + f"?access_key={access_key}" + ''.join('&' + name + '=' + param for name,param in kwargs.items())


async def fetch(session, url):
    """
    given a complete url gets data from the site asynchronously
    """
    async with session.get(url) as response:
        return await response.json()


async def main(): #saved the json files in the folder in which the file is

    
    urls = [
        create_url(MARKET_URL, access_key= MARKET_API_KEY, subpage= 'eod', symbols = 'MSFT'),
        TAXI_URL
    ]

    async with aiohttp.ClientSession() as session:
        tasks = [fetch(session, url) for url in urls]
        results = await asyncio.gather(*tasks)
        
        for i, result in enumerate(results):
            with open(f'{SCRIPT_DIR}/{i}.json', 'w', encoding= 'utf-8') as f:
                json.dump(result, f, indent=4, ensure_ascii= False)


def get_data(file_name, json_field):
    with open(f'{SCRIPT_DIR}/{file_name}', "r", encoding= 'utf-8') as f:
        data = json.load(f)

    loaded_data = pd.DataFrame(data[json_field])
    return loaded_data


market = get_data('0.json','data')
taxis = get_data('1.json','results')

print(market.head())
print(taxis.head())

#asyncio.run(main())
