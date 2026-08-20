''' 
Mohamed 

7/8/26

Merging data onto CTs for index crimes only 
'''

import pandas as pd
import geopandas as gpd
from census import Census
import numpy as np
import io
from functools import reduce
from shapely.geometry import Point
import requests

arrests_all = pd.read_parquet('data/output/nypd_arrests_6_5_26.parquet')

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

# create index column

index_crimes_patterns = ['murder', 'assault', 'rape', 'sexual abuse', 'weapon', 'robbery', 'grand larceny', 'theft', 'burglary', 'sex crimes', 'weap.']

arrests_all['index_crime'] = arrests_all['ofns_desc'].str.contains('|'.join(index_crimes_patterns), case = False, na = False)

# create geocoded column - want to find the prop of census index crimes among all arrests in the CT 

arrests_all['geometry'] = arrests_all['lon_lat'].apply(
    lambda x: Point(x['coordinates']) if x is not None else None
    )

arrests_gdf = gpd.GeoDataFrame(arrests_all, crs = 'EPSG:4326')

arrests_gdf['arrest_date'] = pd.to_datetime(arrests_gdf['arrest_date']).dt.date
arrests_gdf['yr'] = arrests_gdf['arrest_date'].apply(
    lambda x: x.year
)

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

# Dem vars

census_api_key = 'aefce50ef296c24c73983a05b23dabd102d71bcd'



