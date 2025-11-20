import requests
from bs4 import BeautifulSoup
import threading
from queue import Queue

N_THREADS = 20


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
    lock = threading.Lock()
    to_visit = Queue()
    results = Queue()
    visited = set()

    def traverse_linked_pages():
        while True:
            item = to_visit.get()
            if item is None:
                to_visit.task_done()
                break

            curr_page, curr_depth = item

            if curr_depth == 0:
                to_visit.task_done()
                continue 
                
            try:
                #could use timeout
                response = requests.get(curr_page, timeout= 5)
                response.raise_for_status()
            except:
                to_visit.task_done()
                continue #get out of that page if the request failed

            soup = BeautifulSoup(response.text, 'html.parser')

            all_links = get_links(soup)
            page_content = get_content(soup)


        #print(all_links)
        #print(page_content)
            results.put((curr_page, action(page_content)))

            for new_page in all_links:
                with lock:
                    if new_page in visited:
                        continue
                    visited.add(new_page)   
                to_visit.put((new_page,curr_depth - 1))

            to_visit.task_done()

    threads : list[threading.Thread] = []
    for _ in range(N_THREADS):
        thread = threading.Thread(target=traverse_linked_pages, daemon = True)
        thread.start()
        threads.append(thread)
    
    to_visit.put((page,depth))

        # yield results while workers are running
    while True:
        try: 
            item = results.get(timeout=2)
            
            yield item
        except:
            if to_visit.empty():
                to_visit.join()
                break

    [to_visit.put(None) for _ in threads]
    
    [t.join() for t in threads]


for url, wynik in crawl("http://www.ii.uni.wroc.pl", 2,lambda tekst : 'Python' in tekst):
    print(f"{url}: {wynik}")