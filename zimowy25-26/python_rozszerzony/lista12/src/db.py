from sqlalchemy import create_engine
from sqlalchemy.orm import \
    sessionmaker, scoped_session, \
    DeclarativeBase, Session
import os

engine = None
SessionLocal: scoped_session[Session] | None = None
FILE_DIR = None


class Base(DeclarativeBase):
    pass


def init_db(debug: bool = False) -> None:
    global engine, SessionLocal, FILE_DIR
    FILE_DIR = os.path.dirname(os.path.abspath(__file__))
    BOOKSTORE_PATH = FILE_DIR + '/' + 'bookstore.db'
    DATABASE_URL = f"sqlite:///./{BOOKSTORE_PATH}"

    engine = create_engine(
        DATABASE_URL,
        connect_args={"check_same_thread": False},
        echo=debug
    )

    SessionLocal = scoped_session(
        sessionmaker(bind=engine, autocommit=False, autoflush=False))

    Base.metadata.create_all(bind=engine)
