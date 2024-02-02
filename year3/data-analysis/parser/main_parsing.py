import csv
from utils import get_page_tree, get_number_or_zero


def get_feature(html_root, string):
    return html_root.xpath(string)[0].text


header = ['City', 'State', 'County', 'Region', 'Division', 'Latitude', 'Longitude',
          'Population', 'Asian', 'White', 'Black', 'Indian', 'Hispanic', 'Hawaiian', 'Other',
          'Females', 'Males', 'Median_age', 'Median_female_age', 'Median_male_age',
          'Households', 'Persons_per_household', 'Household_income']

file = open('us_cities_first_part.csv', 'a+')
writer = csv.DictWriter(file, fieldnames=header, lineterminator='\n')
writer.writeheader()
root = get_page_tree('https://localistica.com/usa/cities/all-cities/')
states = root.xpath("//div[@class='col-md-3']/a")
for state in states:
    state_name = state.text
    state_root = get_page_tree(state.get('href'))
    cities = state_root.xpath("//tr/td/a")
    for city in cities:
        city_root = get_page_tree(city.get('href'))
        population_root = city_root.xpath("//span[@id='ctl03_lblPopulation']")
        if len(population_root) == 0:
            continue
        population = int(get_number_or_zero(population_root[0].text))
        city_name = get_feature(city_root, "//a[@id='ctl09_hlCity']")
        print(city_name, state_name)
        county = get_feature(city_root, "//span[@id='ctl09_lblCounty']/a")
        region = get_feature(city_root, "//span[@id='ctl09_lblRegion']")
        division = get_feature(city_root, "//span[@id='ctl09_lblDivision']")
        coordinates = get_feature(city_root, "//span[@id='ctl09_lblGeoCoordinates']").split(', ')
        latitude = float(coordinates[0])
        longitude = float(coordinates[1])
        tables = city_root.xpath("//table[@class='table table-striped']/descendant::tr/td")
        ethnicity_dict = {}
        for i in range(0, 21, 3):
            ethnicity_dict[tables[i].text] = int(get_number_or_zero(tables[i + 1].text))
        females = int(get_number_or_zero(get_feature(city_root, "//span[@id='ctl04_lblFemaleNumber']")))
        males = int(get_number_or_zero(get_feature(city_root, "//span[@id='ctl04_lblMaleNumber']")))
        median_age = float(get_feature(city_root, "//span[@id='ctl05_lblMedianAge']"))
        median_female_age = float(get_feature(city_root, "//span[@id='ctl05_lblMedianAgeF']"))
        median_male_age = float(get_feature(city_root, "//span[@id='ctl05_lblMedianAgeM']"))
        households = int(get_number_or_zero(get_feature(city_root, "//span[@id='ctl06_lblHouseHoldsPerZipCode']")))
        persons_per_household = float(get_feature(city_root, "//span[@id='ctl06_lblPersonsPerHousehold']"))
        household_income = float(
            get_number_or_zero(get_feature(city_root, "//span[@id='ctl06_lblIncomePerHousehold']")[1:]))
        writer.writerow({
            "City": city_name,
            "State": state_name,
            "County": county,
            "Region": region,
            "Division": division,
            "Latitude": latitude,
            "Longitude": longitude,
            "Population": population,
            "Asian": ethnicity_dict['Asian'],
            "White": ethnicity_dict['White'],
            "Black": ethnicity_dict['Black'],
            "Indian": ethnicity_dict['Indian'],
            "Hispanic": ethnicity_dict['Hispanic'],
            "Hawaiian": ethnicity_dict['Hawaiian'],
            "Other": ethnicity_dict['Other'],
            "Females": females,
            "Males": males,
            "Median_age": median_age,
            "Median_female_age": median_female_age,
            "Median_male_age": median_male_age,
            "Households": households,
            "Persons_per_household": persons_per_household,
            "Household_income": household_income
        })
file.close()
