""" 
Mohamed 

6-9-26

Objectives:
    - Create new dataset at the census tract level
    - Bring in demographic information on desired geography
"""

import pandas as pd
import geopandas as gpd
from shapely.geometry import Point
import io
import requests
from functools import reduce

arrests_all = pd.read_parquet('data/output/nypd_arrests_6_5_26.parquet')
complaints_all = pd.read_parquet('data/output/nypd_complaints_6_5_26.parquet')

def get_od_geojson(url):
    response = requests.get(url, verify = False)
    gdf = gpd.read_file(io.BytesIO(response.content))
    return gdf

# get geodata for census tracts  

tracts10_url = 'https://data.cityofnewyork.us/api/v3/views/bmjq-373p/query.geojson'
tracts10_gdf = get_od_geojson(tracts10_url)
tracts10_gdf = tracts10_gdf[['boroct2010', 'ct2010', 'geometry']]

tracts20_url = 'https://data.cityofnewyork.us/api/v3/views/63ge-mke6/query.geojson'
tracts20_gdf = get_od_geojson(tracts20_url)
tracts20_gdf = tracts20_gdf[['boroct2020', 'ct2020', 'geometry']]

# create geocoded column 

arrests_all['geometry'] = arrests_all['lon_lat'].apply(
    lambda x: Point(x['coordinates']) if x is not None else None
    )
arrests_gdf = gpd.GeoDataFrame(arrests_all, crs = 'EPSG:4326')

# need to merge in tracts data and find the census tract that each arrest took place in
# could just go off of 2020 census tracts, though that will mess up any demographic info we want to pull from the census in pre-2020 years
    # could just subset analysis to 2020 (post-Covid when we began to see crime in NYC rise again)

# creating 3 ct columns 
    #1 2010 ct
    #2 2020 ct
    #3 'true' ct based on year

arrests_gdf['arrest_date'] = pd.to_datetime(arrests_gdf['arrest_date']).dt.date
arrests_gdf['yr'] = arrests_gdf['arrest_date'].apply(
    lambda x: x.year
)

# Now do joins for spatial data 

arrests_gdf = gpd.sjoin(
    arrests_gdf, tracts10_gdf, how = 'left', predicate='within'
)

arrests_gdf = arrests_gdf.drop(columns = 'index_right')

arrests_gdf = gpd.sjoin(
    arrests_gdf, tracts20_gdf, how = 'left', predicate='within'
)

arrests_gdf = arrests_gdf.drop(columns = 'index_right')

arrests_gdf['true_ct'] = arrests_gdf.apply(
    lambda x: x['ct2010'] if x['yr'] < 2020 else x['ct2020'], axis = 1
)

arrests_gdf['true_boroct'] = arrests_gdf.apply(
    lambda x: x['boroct2010'] if x['yr'] < 2020 else x['boroct2020'], axis = 1
)

# age variable is all messed up. Need to fill in dumb values with None

valid_age_groups = ['45-64', '65+', '18-24', '<18', '25-44']
arrests_gdf['age_group'] = arrests_gdf['age_group'].where(arrests_gdf['age_group'].isin(valid_age_groups))
arrests_gdf.replace('(null)', pd.NA, inplace=True)

# need to find the proportion of crimes that are index crimes or major crimes (felonies as well), then re-save data

# Create three different base datasets - all arrests, felonies, misdeameanors 
    # create function that sets up all the grouped datasets, then merge together for each variable of interest. Append to a list 

df_list = [
    arrests_gdf, 
    arrests_gdf[arrests_gdf['law_cat_cd'] == 'F'], 
    arrests_gdf[arrests_gdf['law_cat_cd'] == 'M']
    ]

grouping_vars = [['true_boroct', 'yr', 'pd_desc'], ['true_boroct', 'yr'], ['true_boroct', 'yr', 'perp_race'], ['true_boroct', 'yr', 'age_group']]

def get_grouped_data(df, grouping_vars = grouping_vars):
    df_list = []
    for var in grouping_vars:
        if var == ['true_boroct', 'yr']:
            sub_df = (
                df
                .groupby(var)
                .size()
                .reset_index(name = 'total_arrests')
            )
        else:
            sub_df = (
                df
                .groupby(var)
                .size()
                .unstack(fill_value = 0)
                .reset_index()
            )

        df_list.append(sub_df)

    new_df = reduce(lambda left, right: pd.merge(left, right, on = ['true_boroct', 'yr'], how='left'), df_list)

    return new_df

# loop through subset dataframe 

subset_df_list = []

for df in df_list:
    df2 = get_grouped_data(df)
    subset_df_list.append(df2)

subset_df_list[0].to_parquet('data/output/arrests_data_all_merged_6_15_26.parquet')
subset_df_list[1].to_parquet('data/output/arrests_data_felonies_merged_6_15_26.parquet')
subset_df_list[2].to_parquet('data/output/arrests_data_misdemeanors_merged_6_15_26.parquet')
