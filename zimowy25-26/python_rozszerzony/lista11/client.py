from __future__ import annotations
from tables import *
import db
import argparse
import os

FILE_DIR = os.path.dirname(os.path.abspath(__file__))


def parse_args():
    parser = argparse.ArgumentParser(description="Program simulating a bookstore")
    
    subparsers = parser.add_subparsers(dest="model", required=True)

    # ----- Mode    -----
    parser.add_argument(
        "--mode",
        choices=["local", "api"],
        default="local",
        help="Access data directly (local) or via REST API (api)"
    )
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
    person_parser.add_argument("--update", action="store_true", help="Add a new person")
    person_parser.add_argument("--delete", action="store_true", help="Add a new person")
    person_parser.add_argument("--name", type=str, help="Persons name")
    person_parser.add_argument("--email", type=str, help="Persons email")
    person_parser.add_argument("--rent",action="store_true",help="Rent a book")
    person_parser.add_argument("--return_book",action="store_true",help="Return a book")
    person_parser.add_argument("--book", type=str, help="Book title to rent or return")
    person_parser.add_argument("--list", action="store_true", help="Lists out every person")

    #---- Inicjalizacja bazy danych ----
    subparsers.add_parser("init-db",help="Initialize database from JSON")

    return parser.parse_args()
   

def handle_local(session,args):
    from local import add_book,rent_book, return_book, add_person, load_data
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

def handle_api(args):
    from api import add_book,rent_book, return_book, add_person, update_person, \
        remove_person, load_data, list_books, list_people
    try:
        if args.model == "books":
            if args.add:
                if not all([args.title, args.year, args.author]):
                    raise ValueError("Insufficient arguments passed")
                add_book(title = args.title, year = args.year, author= args.author)
            elif args.list:
                books = list_books()
                print(books)
                for b in books:
                    print(f"{b['author']} {b['title']} ({b['year']}); avalible : {b['avalible']}")

        elif args.model == 'people':
            if args.add:
                if not all([args.name, args.email]):
                    raise ValueError("Insufficient information")
                
                add_person(name = args.name,email = args.email)

            elif args.update:
                if not all([args.name, args.email]):
                    raise ValueError("Insufficient information")
                
                update_person(new_name = args.name,email = args.email)

            elif args.delete:
                if not args.email:
                    raise ValueError("Insufficient information")
                
                remove_person(email = args.email)

            elif args.list:
                print('listing people...')
                people = list_people()
                for p in people:
                    borr = p['borrowed'] if p['borrowed'] else "nothing rented"
                    print(f"{p['name']} <{p['email']}> : {borr}")
            elif args.rent:
                if args.book is None or args.email is None:
                    raise ValueError("Provide --book and --email")
                rent_book(email = args.email, book_title = args.book)
            elif args.return_book:
                if not args.book or not args.email:
                    raise ValueError("Provide --book and --email")
                return_book(email = args.email, book_title = args.book)

        elif args.model == "init-db":
            load_data("init.json")
    except ValueError as e:
        print(f"Error: {e}")
    except Exception as e:
        print(f"Unexpected error : {e}")


def main(debug = False):
    db.init_db(debug = debug)
    args = parse_args()
    print(args)
    with db.SessionLocal() as session:
        if args.mode == 'local':
            handle_local(session, args)
        elif args.mode == 'api':
            handle_api(args)
            


if __name__ == '__main__':
    main(debug = False)