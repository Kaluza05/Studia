from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, scoped_session, DeclarativeBase
import os
from functools import wraps


def cache_results(func):
    cache = {}

    @wraps(func)
    def wrapper(*args, **kwargs):
        key = (args, tuple(sorted(kwargs.items())))
        if key in cache:
            return cache[key]

        result = func(*args, **kwargs)
        cache[key] = result
        return result

    return wrapper


engine = None
SessionLocal = None
FILE_DIR = None
class Base(DeclarativeBase):
    pass
    


def init_db(debug = False):
    global engine, SessionLocal, FILE_DIR
    FILE_DIR = os.path.dirname(os.path.abspath(__file__))
    BOOKSTORE_PATH = FILE_DIR + '/' + 'bookstore.db'
    DATABASE_URL = f"sqlite:///./{BOOKSTORE_PATH}"

    engine = create_engine(
        DATABASE_URL,
        connect_args={"check_same_thread": False},
        echo = debug
    )

    SessionLocal = scoped_session(sessionmaker(bind=engine, autocommit=False, autoflush=False))

    Base.metadata.create_all(bind=engine)