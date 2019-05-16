import pytest

@pytest.fixture(scope="session")
def parser():
    '''
    Creates and returns an fparser object. Since this is expensive we only
    do this once per test session (scope="session" above).
    '''
    from fparser.two.parser import ParserFactory
    return ParserFactory().create()

