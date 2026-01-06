from __future__ import annotations
from sqlalchemy import select
from tables import *
import json
import db

def add_book(session, title : str, year : int, author: str):
    stmt = select(Author).where(Author.name == author)
    author_query = session.scalar(stmt)
    #check if the author exists in the database
    if author_query is None:
        author = Author(name=author)
        session.add(author)
        session.flush()

    book = Book(
        title=title,
        year=year,
        author=author,
        in_stock = True
    )

    session.add(book)

def rent_book(session, email, book_title):
    """
    rents a book for a given person
    takes:
    -email
    -book title"""
    get_book = select(Book).where(Book.title == book_title)
    get_person = select(Person).where(Person.email == email)

    book = session.scalar(get_book)
    person = session.scalar(get_person)

    if book is None:
        raise ValueError(f"Book not found: {book_title}")
    
    if person is None:
        raise ValueError(f"Person does not exist: {email}")

    if book.borrower is not None:
        raise ValueError("Book is already borrowed")

    book.in_stock = False
    book.borrower = person

def return_book(session, email, book_title):
    """
    returns a book for a given person
    takes:
    -email
    -book title"""
    get_book = select(Book).where(Book.title == book_title)
    get_person = select(Person).where(Person.email == email)

    book = session.scalar(get_book)
    person = session.scalar(get_person)
    #check if the person already rented something
    if person is None:
        raise ValueError(f"Somebody not registered tried to return a book: {email}")
    
    if book is None:
        raise ValueError(f"Book not found in the library {book_title}")
    
    #check if that person rented that book
    if not book.borrower:
        raise ValueError("Nobody rents that book currently")
    
    if book.borrower_id != person.id:
        raise ValueError(f"Book is borrowed by another person: {book.borrower.surname}")
    
    book.in_stock = True
    book.borrower = None #set that nobody currently has that book


def add_person(session,name,email):
    stmt = select(Person).where(Person.email == email)
    person = session.scalar(stmt)

    
    #check if the email exists in the database
    if person is not None:
        raise ValueError(f"person with the email: {email} already exists in the database")
    
    person = Person(name=name, email = email)
    session.add(person)
    session.flush()


def load_data(session,file_name):
    #full file name
    data_path = db.FILE_DIR + '/' + file_name
    with open(data_path,'r') as f:
        data : dict = json.load(f)

    #data format in file is authors, books, people

    authors_map : dict = {}  # mapping name -> author object
    for a in data.get("authors", []):
        author = Author(name=a["name"])
        session.add(author)
        session.flush()  # gives id
        authors_map[a["name"]] = author

    for b in data.get("books", []):
        author = authors_map.get(b["author"])
        if not author:
            # if there was no author
            author = Author(name=b["author"])
            session.add(author)
            session.flush()
            authors_map[b["author"]] = author

        book = Book(title=b["title"], year=b["year"], author=author, in_stock = True)
        session.add(book)

    for p in data.get("people", []):
        person = Person(name=p["name"], email=p["email"])
        session.add(person)
