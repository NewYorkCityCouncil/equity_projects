''' 
Mohamed 

August 3

Objectives
- bring in CT and precinct data
- merge onto precinct with CT centroids 
- merge back onto arrests data
'''

from pandas.core.interchange import dataframe
import pandas as pd 
import geopandas as gpd 
import requests
import pyarrow
import io

arrests_ct_merged = pd.read_parquet('data/output/arrests_ct_merged_6_30_26.parquet')

# bring in precinct data

def get_od_geojson(url):
    response = requests.get(url, verify = False)
    gdf = gpd.read_file(io.BytesIO(response.content))
    return gdf

# bring in data on CTs, find centroids, then merge onto precinct data

# tract data

tracts10_url = 'https://data.cityofnewyork.us/api/v3/views/bmjq-373p/query.geojson'
tracts10_gdf = get_od_geojson(tracts10_url)
tracts10_gdf = tracts10_gdf[['boroct2010', 'ct2010', 'geometry']]

tracts20_url = 'https://data.cityofnewyork.us/api/v3/views/63ge-mke6/query.geojson'
tracts20_gdf = get_od_geojson(tracts20_url)
tracts20_gdf = tracts20_gdf[['boroct2020', 'ct2020', 'geometry']]

# find centroid 

precincts_url = 'https://data.cityofnewyork.us/resource/y76i-bdw7.geojson'

precincts_gdf = get_od_geojson(precincts_url)
precincts_gdf = precincts_gdf[['precinct', 'geometry']]

# want to find the CT centroids

tracts10_gdf_projected = tracts10_gdf.to_crs(tracts10_gdf.estimate_utm_crs())

tracts10_gdf['tract_centroid'] = tracts10_gdf_projected.centroid.to_crs(tracts10_gdf.crs)

tracts10_gdf = tracts10_gdf.set_geometry('tract_centroid')

# tracts10_gdf = tracts10_gdf[['boroct2010', 'ct2010', 'centroid']]

tracts20_gdf_projected = tracts20_gdf.to_crs(tracts20_gdf.estimate_utm_crs())

tracts20_gdf['tract_centroid'] = tracts20_gdf_projected.centroid.to_crs(tracts20_gdf.crs)

tracts20_gdf = tracts20_gdf.set_geometry('tract_centroid')

# tracts20_gdf = tracts20_gdf[['boroct2020', 'ct2020', 'centroid']]

# find which precinct each centroid is in

tracts10_gdf = gpd.sjoin(
    tracts10_gdf, precincts_gdf, how = 'left', predicate = 'within'
)

tracts10_gdf = tracts10_gdf.drop(columns = 'index_right')

tracts20_gdf = gpd.sjoin(
    tracts20_gdf, precincts_gdf, how = 'left', predicate = 'within'
)

tracts20_gdf = tracts20_gdf.drop(columns = 'index_right')

# now need to merge onto arrests data 

arrests_ct2010 = arrests_ct_merged[arrests_ct_merged['yr'] < 2020]

arrests_ct2010 = pd.merge(
    arrests_ct2010, tracts10_gdf,
    left_on = 'true_boroct', right_on = 'boroct2010'
)

arrests_ct2020 = arrests_ct_merged[arrests_ct_merged['yr'] >= 2020]

arrests_ct2020 = pd.merge(
    arrests_ct2020, tracts20_gdf,
    left_on = 'true_boroct', right_on = 'boroct2020'
)

arrests_ct_merged = pd.concat([arrests_ct2010, arrests_ct2020], ignore_index=True)

if not isinstance(arrests_ct_merged, gpd.GeoDataFrame):
    arrests_ct_merged = gpd.GeoDataFrame(arrests_ct_merged, geometry='geometry')

arrests_ct_merged.to_csv('data/output/arrests_ct_merged_8_4.csv')
