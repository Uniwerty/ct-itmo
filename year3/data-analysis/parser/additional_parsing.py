import pandas as pd
import csv
import lxml.html as l
from utils import get_page_tree, get_number_or_zero


def clone(element):
    return l.fromstring(l.tostring(element))


def replace_spaces(string):
    return string.replace(' ', '_')


def parse_value(features_root, th_root, condition):
    if th_root is not None and len(th_root) > 0 and th_root[0].text is not None and condition(th):
        td = features_root.xpath('//tr/td')
        if td is not None and len(td) > 0 and td[0].text is not None:
            par_index = td[0].text.find('(')
            value = get_number_or_zero(td[0].text[par_index + 1:-3])
            if value.replace('.', '', 1).isdigit():
                return float(value)
    return None


def update_value(dictionary, field, features_root, th_root, condition):
    value = parse_value(features_root, th_root, condition)
    if value is not None:
        dictionary[field] = value


header = ['City', 'Total_area', 'Land_area', 'Water_area', 'Elevation', 'Density']

file = open('us_cities_second_part.csv', 'w+')
writer = csv.DictWriter(file, fieldnames=header, lineterminator='\n')
writer.writeheader()
data = pd.read_csv('us_cities_first_part.csv')
for index in range(0, len(data)):
    city = replace_spaces(data.iloc[index].City)
    state = replace_spaces(data.iloc[index].State)
    root = get_page_tree('https://en.wikipedia.org/wiki/' + city + ',_' + state)
    features = root.xpath('//tr')
    print(city, state)
    city_dict = {'City': city}
    for f in features:
        f_root = clone(f)
        th = f_root.xpath('//tr/th')
        update_value(city_dict, 'Land_area', f_root, th, lambda r: r[0].text[3:] == 'Land')
        update_value(city_dict, 'Water_area', f_root, th, lambda r: r[0].text[3:] == 'Water')
        update_value(city_dict, 'Elevation', f_root, th, lambda r: r[0].text == 'Elevation')
        update_value(city_dict, 'Density', f_root, th, lambda r: r[0].text[3:] == 'Density')

    land = city_dict['Land_area']
    water = city_dict['Water_area']
    if land is not None and water is not None:
        city_dict['Total_area'] = land + water
    writer.writerow(city_dict)
file.close()
