import pandas as pd

data1 = pd.read_csv('us_cities_first_part.csv')
data2 = pd.read_csv('us_cities_second_part.csv').drop(columns='City')
data1.join(data2).to_csv('us_cities.csv')
