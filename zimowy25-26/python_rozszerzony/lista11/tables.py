from __future__ import annotations
from sqlalchemy import Integer, ForeignKey, String, Boolean
from sqlalchemy.orm import relationship, mapped_column, Mapped, validates
from typing import List, Optional
from datetime import date
from db import Base

class Author(Base):
    __tablename__ = 'Authors'
    id = mapped_column(Integer, primary_key= True)
    name = mapped_column(String, unique= True)

    books : Mapped[List[Book]] = relationship('Book', back_populates = 'author')
    
    @validates("name")
    def validate_name(self, key, name : str):
        if name and name.replace(' ','').isalpha():
            return name
        raise ValueError(f"Wrong name passed for author: {name}")
        
class Book(Base):

    __tablename__ = "Books"
    id = mapped_column(Integer, primary_key=True)
    title = mapped_column(String)
    year   = mapped_column(Integer)
    in_stock = mapped_column(Boolean)
    
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
    
    def to_dict(self):
        return {
            'id' : self.id,
            'title' : self.title,
            'year' : self.year,
            'avalible' : 'Yes' if self.in_stock else 'No',
            'author' : self.author.name
        }
    
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
    
    def to_dict(self):
        return {
            'id' : self.id,
            'name' : self.name,
            'email' : self.email,
            'borrowed' : [book.to_dict() for book in self.borrowed_books]
        }