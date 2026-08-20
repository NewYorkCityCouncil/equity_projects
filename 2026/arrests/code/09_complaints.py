""" 
Mohamed 
July 21, 2026

Objectives:
- Pull in data on complaints to 911
- Get complaints organized by CT - add value for complaints about index crimes
"""

import pandas as pd
import geopandas as gpd
from shapely.geometry import Point
import io
import requests
from functools import reduce
from sodapy import Socrata

def get_open_data_df(soda, limit = 999999999, client = Socrata('data.cityofnewyork.us', None, timeout = 600)):
    results = client.get(soda, limit = limit)
    df = pd.DataFrame.from_records(results)
    return df

# seems like ytd data does not include geographic info 

# complaints_ytd_soda  = '5uac-w243'
complaints_historic_soda = 'qgea-i56i'

# complaints_ytd_df = get_open_data_df(complaints_ytd_soda)
complaints_historic_df = get_open_data_df(complaints_historic_soda)

# complaints_all = pd.concat([complaints_historic_df, complaints_ytd_df], ignore_index=True)

# complaints_all.to_parquet('data/output/nypd_complaints_6_5_26.parquet')

complaints_historic_df.to_parquet('data/output/complaints_historic.parquet')

# complaints_all = pd.read_parquet('data/output/nypd_complaints_6_5_26.parquet')

complaints_historic_df = pd.read_parquet('data/output/complaints_historic.parquet')

# get census tracts

def get_od_geojson(url):
    response = requests.get(url, verify = False)
    gdf = gpd.read_file(io.BytesIO(response.content))
    return gdf

tracts10_url = 'https://data.cityofnewyork.us/api/v3/views/bmjq-373p/query.geojson'
tracts10_gdf = get_od_geojson(tracts10_url)
tracts10_gdf = tracts10_gdf[['boroct2010', 'ct2010', 'geometry']]

tracts20_url = 'https://data.cityofnewyork.us/api/v3/views/63ge-mke6/query.geojson'
tracts20_gdf = get_od_geojson(tracts20_url)
tracts20_gdf = tracts20_gdf[['boroct2020', 'ct2020', 'geometry']]

# create geocoded column in complaints data 

# open data file only has data in lat - lon, we need lon - lat
# create new var called lon_lat that takes [longitude, latitude]

complaints_historic_df['geometry'] = complaints_historic_df.apply(
    lambda row: Point(float(row['longitude']), float(row['latitude']))
    if pd.notna(row['longitude']) and pd.notna(row['latitude'])
    else None,
    axis = 1
)

complaints_gdf = gpd.GeoDataFrame(complaints_historic_df, crs = 'EPSG:4326')

complaints_gdf['complaint_date'] = pd.to_datetime(complaints_gdf['rpt_dt']).dt.date
complaints_gdf['yr'] = complaints_gdf['complaint_date'].apply(
    lambda x: x.year
)

# joins to get CT info 
# finid contemporary boroct

# creating 3 columns:
    # 1 2010 ct
    # 2 2020 ct
    #3 'true' ct based on year

complaints_gdf = gpd.sjoin(
    complaints_gdf, tracts10_gdf, 
    how = 'left', predicate = 'within'
)

complaints_gdf = complaints_gdf.drop(columns = 'index_right')

complaints_gdf = gpd.sjoin(
    complaints_gdf, tracts20_gdf,
    how = 'left', predicate = 'within'
)

complaints_gdf = complaints_gdf.drop(columns = 'index_right')

complaints_gdf['true_boroct'] = complaints_gdf.apply(
    lambda x: x['boroct2010'] if x['yr'] < 2020 else
    x['boroct2020'], axis = 1
)

grouping_vars = [['true_boroct', 'yr', 'pd_desc'], ['true_boroct', 'yr'], ['true_boroct', 'yr', 'susp_race']]

def get_grouped_data(df, grouping_vars = grouping_vars):
    df_list = []
    for var in grouping_vars:
        if var == ['true_boroct', 'yr']:
            sub_df = (
                df
                .groupby(var)
                .size()
                .reset_index(name = 'total_complaints')
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

complaints2 = get_grouped_data(complaints_gdf)

complaints2.to_parquet('data/output/complaints_geo.parquet')
