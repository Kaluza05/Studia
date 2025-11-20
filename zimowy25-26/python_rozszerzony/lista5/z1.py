import requests
#from requests import HTTPError
from bs4 import BeautifulSoup

def get_links(page_content : BeautifulSoup):
    """
    returns a generator of all links from the current website
    """
    for link in page_content.find_all('a', href = True):
        yield link["href"]

def get_content(page_content : BeautifulSoup) -> str:
    """
    returns whole text from the current page
    """
    return page_content.get_text(strip = True)

def crawl(page, depth, action): 
    visited = set()
    def traverse_linked_pages(curr_page, depth):
        if depth == 0 or curr_page in visited:
            return
        
        visited.add(curr_page)
        
        try:
            #could use timeout
            response = requests.get(curr_page)
            response.raise_for_status()
        except:
            return #get out of that page if the request failed

        soup = BeautifulSoup(response.text, 'html.parser')

        all_links = get_links(soup)
        page_content = get_content(soup)

        #print(all_links)
        #print(page_content)

        yield curr_page, action(page_content)

        for new_page in all_links:
            yield from traverse_linked_pages(new_page, depth - 1)

        
    """
    zarys:
        cachowanie
        if depth == 0:end we went too deep into the search
    get all pages on current page
    get page content
    run action on the page content
    traverse_linked_pages(depth - 1)
    """

    yield from traverse_linked_pages(page, depth)


#url = 'https://stackoverflow.com/questions/59347372/how-extract-all-urls-in-a-website-using-beautifulsoup'
#action = lambda x : "python" if "python" in x else ""
#for i in crawl(url, 2, action):
#    print(i)


for url, wynik in crawl("http://www.ii.uni.wroc.pl", 2,lambda tekst : 'Python'in tekst):
    print(f"{url}: {wynik}")