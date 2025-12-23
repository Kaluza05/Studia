from __future__ import annotations
from sqlalchemy import Integer, ForeignKey, String, select
from sqlalchemy.orm import DeclarativeBase, relationship, mapped_column, Mapped, validates, sessionmaker, Session
from sqlalchemy import create_engine
from typing import List, Optional
from datetime import date
import json
import os
import argparse

FILE_DIR = os.path.dirname(os.path.abspath(__file__))

class Base(DeclarativeBase):
    pass


class Author(Base):
    __tablename__ = 'Authors'
    id = mapped_column(Integer, primary_key= True)
    name = mapped_column(String, unique= True)

    books : Mapped[List[Book]] = relationship('Book', back_populates = 'author')
    
    @validates("name")
    def validate_name(self, key, name : str):
        print(name)
        if name and name.replace(' ','').isalpha():
            return name
        raise ValueError(f"Wrong name passed for author: {name}")
        
class Book(Base):

    __tablename__ = "Books"
    id = mapped_column(Integer, primary_key=True)
    title = mapped_column(String)
    year   = mapped_column(Integer)
    in_stock = mapped_column(Integer)
    
    author_id = mapped_column(Integer, ForeignKey('Authors.id'))
    author = relationship("Author", back_populates="books")

    borrower_id : Mapped[Optional[int]] = mapped_column(ForeignKey('People.id'), nullable = True)
    borrower : Mapped[List[Person]] = relationship('Person', back_populates = 'borrowed_books')

    def __repr__(self):
        return f"{self.title} ({self.year})"

    @validates("title")
    def validate_title(self, key, title):
        if title is None:
            raise ValueError("Book title cannot be empty")
        return title.strip()

    @validates("year")
    def validate_year(self, key, year):
        current_year = date.today().year

        if year > current_year:
            raise ValueError("Book from the future")
        
        return year
    
class Person(Base):
    __tablename__ = "People"
    id = mapped_column(Integer, primary_key=True)
    name = mapped_column(String, unique = False, nullable = False)
    email    = mapped_column(String,unique = True, nullable=False)

    borrowed_books : Mapped[List[Book]] = relationship('Book', back_populates='borrower')

    @validates("email")
    def validate_email(self, key, email):
        try:
            _,domain = email.split('@')
        except:
            raise FileNotFoundError(f"cos nie poszlo z emailem: {email}")
        
        if domain in ['gmail.com', 'wp.pl', 'uwr.edu.pl']:
            return email
        raise ValueError(f"Email not from the correct domain: {domain}")


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
        author=author
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
    data_file = FILE_DIR + '/' + file_name
    with open(data_file,'r') as f:
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

        book = Book(title=b["title"], year=b["year"], author=author)
        session.add(book)

    for p in data.get("people", []):
        person = Person(name=p["name"], email=p["email"])
        session.add(person)



def parse_args():
    parser = argparse.ArgumentParser(description="Program simulating a bookstore")
    
    subparsers = parser.add_subparsers(dest="model", required=True)

    # ----- Książki -----
    book_parser = subparsers.add_parser("books", help="Book manager")
    book_parser.add_argument("--add", action="store_true", help="Add a new book")
    book_parser.add_argument("--title", type=str, help="Book title")
    book_parser.add_argument("--year", type=int, help="Year of publishing")
    book_parser.add_argument("--author", type=str, help="Author of the book")
    book_parser.add_argument("--list", action="store_true", help="List of every book")
    # ----- Osoby -----
    person_parser = subparsers.add_parser("people", help="People manager")
    person_parser.add_argument("--add", action="store_true", help="Add a new person")
    person_parser.add_argument("--name", type=str, help="Persons name")
    person_parser.add_argument("--email", type=str, help="Persons email")
    person_parser.add_argument("--rent",action="store_true",help="Rent a book")
    person_parser.add_argument("--return_book",action="store_true",help="Return a book")
    person_parser.add_argument("--book", type=str, help="Book title to rent or return")
    person_parser.add_argument("--list", action="store_true", help="Lists out every person")

    #---- Inicjalizacja bazy danych ----
    subparsers.add_parser("init-db",help="Initialize database from JSON")

    return parser.parse_args()
   
def main(debug = False):
    args = parse_args()

    bookstore_path = FILE_DIR + '/' + 'bookstore.db'
    engine = create_engine(f"sqlite:///{bookstore_path}", echo=debug)
    Session = sessionmaker(bind=engine)
    Base.metadata.create_all(engine)

    with Session() as session:
        try:
            if args.model == "books":

                if args.add:
                    if not all([args.title, args.year, args.author]):
                        raise ValueError("Insufficient arguments passed")
                    #print(args.author)
                    add_book(session, title = args.title, year = args.year, author= args.author)
                    session.commit()

                elif args.list:
                    books = session.query(Book).all()
                    for b in books:
                        status = (
                            f"Rented by : {b.borrower.email}"
                            if b.borrower else "avalible"
                        )
                        print(f"{b.id}: {b.title} ({b.year}) - {status}")

            elif args.model == 'people':
                if args.add:
                    if not all([args.name, args.email]):
                        raise ValueError("Insufficient information")
                    
                    add_person(session,name = args.name,email = args.email)
                    session.commit()

                elif args.list:
                    people = session.query(Person).all()
                    for p in people:
                        status = (
                            f"rented {', '.join(repr(b) for b in p.borrowed_books)}"
                            if p.borrowed_books else "nothing rented"
                        )
                        print(f"{p.email} :  {status}")

                elif args.rent:
                    if args.book is None or args.email is None:
                        raise ValueError("Provide --book and --email")

                    rent_book(session, email = args.email, book_title = args.book)
                    session.commit()

                elif args.return_book:
                    if not args.book or not args.email:
                        raise ValueError("Provide --book and --email")

                    return_book(session, email = args.email, book_title = args.book)
                    session.commit()

            elif args.model == "init-db":
                load_data(session, "init.json")
                session.commit()

        except ValueError as e:
            print(f"Error: {e}")

        except Exception as e:
            print(f"Unexpected error : {e}")


if __name__ == '__main__':
    main(debug = False)