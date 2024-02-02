import requests
import shutil
import lxml.html as l


def get_page_tree(url):
    page = requests.get(url)
    return l.fromstring(page.text)


def clone(element):
    return l.etree.fromstring(l.etree.tostring(element))


root_url = 'https://allpainters.org/'
styles = ['realism', 'impressionism', 'romanticism', 'expressionism']
for i in range(4):
    for page in range(1, 51):
        page_root = get_page_tree(root_url + 'theme/' + styles[i] + '/page/' + str(page))
        paintings = page_root.xpath("//article")
        for painting in paintings:
            p = clone(painting).xpath("//a")[0]
            p_root = get_page_tree(p.get('href'))
            images = p_root.xpath("//article")
            for image in images:
                im = clone(image).xpath("//img")[0]
                path = im.get('src')
                r = requests.get(path, stream=True)
                if r.status_code == 200:
                    title = path.split('/')[-1]
                    with open('images/' + styles[i] + '/' + title, 'wb') as f:
                        r.raw.decode_content = True
                        shutil.copyfileobj(r.raw, f)
                    print(title)
