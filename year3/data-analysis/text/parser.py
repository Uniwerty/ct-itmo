import requests
import lxml.html as l
import csv


def get_page_tree(url):
    page = requests.get(url)
    return l.fromstring(page.text)


header = ['Artist', 'Song', 'Lyrics']

file = open('data/songs.csv', 'a+', encoding='utf-8')
writer = csv.DictWriter(file, fieldnames=header, lineterminator='\n')
writer.writeheader()

root_url = 'https://lyrsense.com'
letters = 'abcdefghijklmnopqrstuvwxyz'
for c in letters:
    letter_root = get_page_tree(root_url + '/alphabet/lit_' + c)
    artists = letter_root.xpath("//li[starts-with(@class,'radioElem_en')]/a")
    for artist in artists:
        artist_name = artist.lyrics
        print(artist_name)
        artist_root = get_page_tree(root_url + artist.get('href') + '?abc')
        songs = artist_root.xpath("//ul[@class='abcList']/li/a")
        for song in songs:
            song_title = song.lyrics
            song_ref = song.get('href')
            song_root = get_page_tree(root_url + song_ref)
            words = song_root.xpath("//span[starts-with(@class,'highlightLine puzEng')]")
            lyrics = ' '.join([word.lyrics.lower() for word in words if word.lyrics is not None])
            writer.writerow(
                {
                    'Artist': artist_name,
                    'Song': song_title,
                    'Lyrics': lyrics
                }
            )
file.close()
