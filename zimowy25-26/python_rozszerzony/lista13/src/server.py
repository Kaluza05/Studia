from __future__ import annotations
from flask import Flask, request, jsonify
import db
from tables import *
from sqlalchemy import select
import json

db.init_db(debug=False)
print(db.FILE_DIR)
app = Flask(__name__)

Base.metadata.create_all(bind=db.engine)   


class AlchSession:
    def __enter__(self):
        self.session = db.SessionLocal()
        return self.session
    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_val is not None:
            self.session.rollback()
        else:
            self.session.commit()
            self.session.close()
        return True


@app.get("/people")
def list_people():
    with AlchSession() as dbs:
        people = dbs.query(Person).all()
        payload =  jsonify([p.to_dict() for p in people])

    return payload


@app.get("/people/<string:email>")
def get_person(email):
    with AlchSession() as dbs:
        person = dbs.query(Person).filter(Person.email == email).first() #there should be only one person of that email


    if person is None:
        return {"error": "Person not found"}, 404
        
    return jsonify(person.to_dict())


@app.post("/people")
def create_person():
    with AlchSession() as dbs:
        data = request.json
        check_email = dbs.query(Person).filter_by(email = data['email']).first()
        if check_email:
            dbs.close()
            return {"error": "Email already registered"}, 404

        person = Person(**data)
        dbs.add(person)
        dbs.commit()
        dbs.refresh(person)
        return jsonify(person.to_dict()), 201

@app.put("/people/<string:email>")
def update_person(email):
    with AlchSession() as dbs:
        person = dbs.query(Person).filter_by(email = email).first()
    
        if not person:
            return jsonify({"error": "Person not found"}), 404

        data = request.json
        person.name = data["name"]
        dbs.commit()
        dbs.refresh(person)

        payload = jsonify(person.to_dict())
        print(payload)

        return payload


@app.delete("/people/<string:email>")
def delete_person(email):
    with AlchSession() as dbs:
        person = dbs.query(Person).filter_by(email = email).first()
        if not person:
            return jsonify({"error": "Person not found"}), 404

        dbs.delete(person)
        dbs.commit()

        return jsonify({"status": "deleted"})
    
"""BOOKS"""

@app.get("/books")
def list_books():
    with AlchSession() as dbs:
        books = dbs.query(Book).all()
    return jsonify([b.to_dict() for b in books])

@app.post("/books")
def create_book():
    with AlchSession() as dbs:
        data = request.json
        check_book = dbs.query(Book).filter_by(title = data['title']).first()
        if check_book:
            dbs.close()
            return {"error": "Book already registered"}, 409

        check_author = dbs.query(Author).filter_by(name = data['author']).first()
        if check_author:
            author = check_author
        else:
            author = Author(data['author'])
        book = Book(title = data['title'], author = author, year = data['year'])
        dbs.add(book)
        dbs.commit()
        dbs.refresh(book)
        return jsonify(book.to_dict()), 201
    

"""INIT DATABASE"""



@app.get("/init-db")
def load_data():
    with AlchSession() as dbs:
        data_js = request.json 
        data_path = db.FILE_DIR + '/' + data_js["file_name"]
        with open(data_path,'r') as f:
            data : dict = json.load(f)

        #data format in file is authors, books, people

        authors_map : dict = {}  # mapping name -> author object
        for a in data.get("authors", []):
            author = Author(name=a["name"])
            dbs.add(author)
            dbs.flush()  # gives id
            authors_map[a["name"]] = author

        for b in data.get("books", []):
            author = authors_map.get(b["author"])
            if not author:
                # if there was no author
                author = Author(name=b["author"])
                dbs.add(author)
                dbs.flush()
                authors_map[b["author"]] = author

            book = Book(title=b["title"], year=b["year"], author=author, in_stock = True)
            dbs.add(book)

        for p in data.get("people", []):
            person = Person(name=p["name"], email=p["email"])
            dbs.add(person)

        return {"status": "ok", "imported": True}, 201
    


"""RENT / RETURN """

@app.post("/rent")
def rent_book():
    with AlchSession() as dbs:
        data = request.json
        get_book = select(Book).where(Book.title == data['title'])
        get_person = select(Person).where(Person.email == data['email'])

        book = dbs.scalar(get_book)
        person = dbs.scalar(get_person)

        if book is None:
            raise ValueError(f"Book not found: {data['title']}")
        
        if person is None:
            raise ValueError(f"Person does not exist: {data['email']}")

        if book.borrower is not None:
            raise ValueError("Book is already borrowed")

        book.in_stock = False
        book.borrower = person

@app.post("/return")
def return_book():
    with AlchSession() as dbs:
        data = request.json
        get_book = select(Book).where(Book.title == data["title"])
        get_person = select(Person).where(Person.email == data["email"])

        book = dbs.scalar(get_book)
        person = dbs.scalar(get_person)
        #check if the person already rented something
        if person is None:
            dbs.close()
            raise ValueError(f"Somebody not registered tried to return a book: {data['email']}")
        
        if book is None:
            dbs.close()
            raise ValueError(f"Book not found in the library {data['title']}")
        
        #check if that person rented that book
        if not book.borrower:
            raise ValueError("Nobody rents that book currently")
        
        if book.borrower_id != person.id:
            raise ValueError(f"Book is borrowed by another person: {book.borrower.surname}")
        
        book.in_stock = True
        book.borrower = None #set that nobody currently has that book

if __name__ == "__main__":
    app.run(debug=True)