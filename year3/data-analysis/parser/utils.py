import lxml.html as l
import requests


def get_page_tree(url):
    page = requests.get(url)
    return l.fromstring(page.text)


def get_number_or_zero(string):
    if string is not None:
        return "".join(string.split(','))
    else:
        return 0