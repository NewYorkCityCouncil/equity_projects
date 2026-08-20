""" 
Mohamed 

6/9/26 

Bringing in census data on demographic estimates 
    - population
    - pct. black 
    - pct. white
    - pct. hispanic
    - pct. below nyc poverty liine
    - pct. receiving SNAP assistance
    - education 
    - age distribution
    - gini 
"""

import pandas as pd 
import geopandas as gpd 
from census import Census 
from us import states
import numpy as np
import requests

# ct_level_info = pd.read_parquet('data/output/ct_level_arrests_info_6_9_26.parquet')

arrests_all_merged = pd.read_parquet('data/output/arrests_data_all_merged_6_15_26.parquet')
felonies_merged = pd.read_parquet('data/output/arrests_data_felonies_merged_6_15_26.parquet')
misdemeanors_merged = pd.read_parquet('data/output/arrests_data_misdemeanors_merged_6_15_26.parquet')

# census_tracts = arrests_all_merged['true_ct'].tolist()

census_api_key = 'aefce50ef296c24c73983a05b23dabd102d71bcd'

c = Census(census_api_key)#

county_fips = ['061', '081', '047',  '085', '005']
yrs = [str(x) for x in range(2012, 2025)]

demographics_df = pd.DataFrame()

# check if variable is available for that year

for yr in [2009, 2010, 2011, 2012]:
    url = f"https://api.census.gov/data/{yr}/acs/acs5/variables/B15003_017E.json"
    r = requests.get(url)
    print(yr, r.status_code)

for county in county_fips:
    county_estimates = pd.DataFrame()
    for yr in yrs:
        pop_estimates = c.acs5.state_county_tract(
            fields = (
                'NAME', 
                'B01001_001E', # population total
                'B03002_004E', # Black not hisp
                'B03002_006E', # Asian alone
                'B03002_003E', # White not hisp
                'B19013_001E', # median household income
                'B03002_012E', # Hispanic
                'B19083_001E', # gini
                'B15003_017E', # high school diploma
                'B15003_001E', # pop 25+
                'B15003_022E', # bachelor's degree
                'B01001_005E', # age 10 - 14
                'B01001_006E', # Age 15 - 17
                'B01001_007E', # Age 18 - 19
                'B01001_008E', # Age 20
                'B01001_009E', # Age 21
                'B01001_010E' # Age 22 - 24
                ),
            state_fips='36',
            county_fips=county,
            tract='*',
            year = int(yr)
        )
        df = pd.DataFrame(pop_estimates)
        df['yr'] = yr
        county_estimates = pd.concat([county_estimates, df], ignore_index=True)
    demographics_df = pd.concat([demographics_df, county_estimates], ignore_index=True)

demographics_df = demographics_df.rename(
    columns={
        'B19013_001E': 'median_hh_income',
        'B01001_001E': 'pop',
        'B03002_004E': 'black_not_hisp',
        'B03002_006E': 'asian_not_hisp',
        'B03002_003E': 'white_not_hisp',
        'B03002_012E': 'hispanic',
        'B01001_005E': 'age_10_14',
        'B01001_006E': 'age_15_17',
        'B01001_007E': 'age_18_19',
        'B01001_008E': 'age_20',
        'B01001_009E': 'age_21',
        'B01001_010E': 'age_22_24',
        'B19083_001E': 'gini',
        'B15003_017E': 'hs_diploma',
        'B15003_001E': 'pop25_plus',
        'B15003_022E': 'bachelors_deg'
    }
)

demographics_df['yr'] = demographics_df['yr'].astype(int)

demographics_df = demographics_df.assign(
    non_white_pop = demographics_df['pop'].astype(float) - demographics_df['white_not_hisp']
)

demographics_df = demographics_df.assign(
    white_nh_prop = demographics_df['white_not_hisp'] / demographics_df['pop'],
    black_nh_prop = demographics_df['black_not_hisp'] / demographics_df['pop'],
    asian_nh_prop = demographics_df['asian_not_hisp'] / demographics_df['pop'],
    hispanic_prop = demographics_df['hispanic'] / demographics_df['pop'],
    non_white_prop = demographics_df['non_white_pop'] / demographics_df['pop'],
    youth_prop = (demographics_df['pop'] - demographics_df['pop25_plus']) / demographics_df['pop'],
    bachelors_prop = demographics_df['bachelors_deg'] / demographics_df['pop'],
    hs_prop = demographics_df['hs_diploma'] / demographics_df['pop']
)

demographics_df = demographics_df.assign(
    majority_non_white = np.where(demographics_df['non_white_prop'] > 0.5, 1, 0),
    majority_black_alone = np.where(demographics_df['black_nh_prop'] > 0.5, 1, 0),
    majority_white_alone = np.where(demographics_df['white_nh_prop'] > 0.5, 1, 0),
    majority_hispanic = np.where(demographics_df['hispanic_prop'] > 0.5, 1, 0),
    majority_asian_alone = np.where(demographics_df['asian_nh_prop'] > 0.5, 1, 0)
)

# coding in plurality ethnicity

ethnicity_vars = ['white_nh_prop', 'black_nh_prop', 'asian_nh_prop', 'hispanic_prop']

demographics_df = demographics_df.assign(
    plurality_ethnicity = demographics_df[ethnicity_vars].idxmax(axis=1).str.replace('_prop', '')
)

demographics_df.groupby(['tract', 'yr', 'county']).size().value_counts()

# need to create boroct variable to match correctly 

demographics_df['true_boroct'] = demographics_df['county'].map({
    '005': '2', # Bronx
    '047': '3', # Brooklyn
    '061': '1', # Manhattan
    '081': '4', # Queens
    '085': '5', # Staten Island
}) + demographics_df['tract']

# demographics_df.to_parquet('data/output/ct_level_demographics_09_24.parquet')

# demographics_df = pd.read_parquet('data/output/ct_level_demographics_09_24.parquet') 

# need to add in ethnicity variables 
# check if codes are the same across years 

arrests_ct_merged = pd.merge(
    demographics_df, arrests_all_merged, on=['true_boroct', 'yr'], how='left'
)

felonies_ct_merged = pd.merge(
    demographics_df, felonies_merged, on=['true_boroct', 'yr'], how='left'
)

misdemeanors_ct_merged = pd.merge(
    demographics_df, misdemeanors_merged, on=['true_boroct', 'yr'], how='left'
)

# arrests_ct_merged.to_parquet('data/output/arrests_ct_merged_6_15_26.parquet')
# felonies_ct_merged.to_parquet('data/output/felonies_ct_merged_6_15_26.parquet')
# misdemeanors_ct_merged.to_parquet('data/output/misdemeanors_ct_merged_6_15_26.parquet')


