from __future__ import annotations
from tables import *
import requests

API_URL = "http://127.0.0.1:5000"

"""LOAD DATA"""
def load_data(file_name):
    payload = {"file_name": file_name}
    r = requests.get(f"{API_URL}/init-db", json = payload)

    return r.json()

"""PEOPLE"""

def list_people():
    """Lista wszystkich osób"""
    r = requests.get(f"{API_URL}/people")
    if r.status_code == 200:
        return r.json()
    return []


def get_person_by_email(email):
    """Pobranie osoby po emailu"""
    r = requests.get(f"{API_URL}/people/{email}")
    if r.status_code == 404:
        return None
    return r.json()

def add_person(name, email):
    """Dodanie nowej osoby"""
    payload = {"name": name, "email": email}
    r = requests.post(f"{API_URL}/people", json=payload)
    if r.status_code in (200, 201):
        return r.json()
    return {"error": r.json()}

def update_person(new_name,email):
    payload = {"name": new_name}
    r = requests.put(f"{API_URL}/people/{email}", json=payload)
    print(r.status_code)
    if r.status_code in (200, 201):
        return r.json()
    return {"error": r.json()}

def remove_person(email):
    r = requests.delete(f"{API_URL}/people/{email}")
    if r.status_code == 404:
        return r.json()
    return r.json()


# ----------------- BOOKS -----------------

def get_book_by_title(title):
    """Pobranie książki po tytule"""
    r = requests.get(f"{API_URL}/books/{title}")
    if r.status_code == 404:
        return None
    return r.json()

def add_book(title, author, year):
    """Dodanie nowej książki"""
    payload = {"title": title, "author": author, "year": year}
    r = requests.post(f"{API_URL}/books", json=payload)
    if r.status_code in (200, 201):
        return r.json()
    return {"error": r.json()}

def list_books():
    """Lista wszystkich książek"""
    r = requests.get(f"{API_URL}/books")
    print(r.status_code)
    if r.status_code == 200:
        return r.json()
    return []

# ----------------- RENT / RETURN -----------------

def rent_book(person_email, book_title):
    """Wypożyczenie książki"""
    payload = {"email": person_email, "title": book_title}
    r = requests.post(f"{API_URL}/rent", json=payload)
    return r.json(), r.status_code

def return_book(person_email, book_title):
    """Zwrot książki"""
    payload = {"email": person_email, "title": book_title}
    r = requests.post(f"{API_URL}/return", json=payload)
    return r.json(), r.status_code