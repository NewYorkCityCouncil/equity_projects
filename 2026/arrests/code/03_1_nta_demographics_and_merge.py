""" 
Note: Ended up not using this script

Mohamed 

6/11/26 

Bringing in census data on demographic estimates 
    - population
    - pct. black 
    - pct. white
    - pct. hispanic
    - pct. below nyc poverty liine
    - pct. receiving SNAP assistance
"""

import pandas as pd 
import geopandas as gpd 
from census import Census 
from us import states
import numpy as np

arrests_all_merged = pd.read_parquet('data/output/arrests_data_all_merged_nta_6_11_26.parquet')
felonies_merged = pd.read_parquet('data/output/arrests_data_felonies_merged_nta_6_11_26.parquet')
misdemeanors_merged = pd.read_parquet('data/output/arrests_data_misdemeanors_merged_nta_6_11_26.parquet')

ntas = arrests_all_merged['nta2020'].tolist()

census_api_key = 'aefce50ef296c24c73983a05b23dabd102d71bcd'

c = Census(census_api_key)

county_fips = ['061', '081', '047',  '085', '005']
yrs = [str(x) for x in range(2009, 2025)]

demographics_df = pd.DataFrame()

for county in county_fips:
    county_estimates = pd.DataFrame()
    for yr in yrs:
        pop_estimates = c.acs5.state_county_tract(
            fields = (
                'NAME', 'B01001_001E', # population total
                'B01001B_001E', # Black alone
                'B01001D_001E', # Asian alone
                'B01001H_001E', # White alone
                'B01001I_001E', #Hispanic
                # 'B09010_001E', # SNAP Recipients
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