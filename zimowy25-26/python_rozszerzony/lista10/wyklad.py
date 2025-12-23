from __future__ import annotations
from sqlalchemy import Table, Column, Integer, ForeignKey, String
from sqlalchemy.orm import DeclarativeBase, relationship, mapped_column, Mapped, validates, sessionmaker, Session
from sqlalchemy import create_engine

from typing import List

class Base(DeclarativeBase):
    pass

zapisy = Table( "student_wyklad",
Base.metadata,
Column("wyklad_id", ForeignKey("Wyklady.id")),
Column("student_id", ForeignKey("Studenci.id")))


class Wykladowca(Base):
    __tablename__ = "Wykladowcy"
    id = mapped_column(Integer, primary_key=True)
    nazwisko = mapped_column(String)
    wyklada: Mapped[List[Wyklad]] = relationship("Wyklad", back_populates="wykladowca")

    @validates("nazwisko")
    def validate_nazwisko(self, key, nazwisko):
        if len(nazwisko) < 3:
            raise ValueError("Nazwisko za krótkie")
        return nazwisko

class Wyklad(Base):
    __tablename__ = "Wyklady"
    id = mapped_column(Integer, primary_key=True)
    nazwa = mapped_column(String)
    ## Związek z Wykładowcą
    wykladowca_id = mapped_column(Integer,
    ForeignKey('Wykladowcy.id'))
    wykladowca = relationship("Wykladowca",
    back_populates="wyklada")

    zapisani: Mapped[List[Student]] = relationship(secondary=zapisy)

class Student(Base):
    __tablename__ = 'Studenci'
    id = Column(Integer, primary_key=True)
    nazwisko = Column(String)
    zapisany: Mapped[List[Wyklad]] = relationship(secondary=zapisy)


engine = create_engine("sqlite:///wyklad.db", echo=True)

with Session(engine) as session:
    wykladowca = Wykladowca(nazwa="Albert Einstein")
    wyklad = Wyklad(nazwa="Fizyka relatywistyczna",
    wykladowca=wykladowca.id)
    session.add(wykladowca)
    session.add(wyklad)
    session.commit()