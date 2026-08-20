""" 
Mohamed 
6/5/26

Equity Project - Arrests in NYC

Objectives:
- Pull in data on NYPD arrests (historic, ytd) and append
- Pull in data on complaints to 911 (filter out complaints by police officers), use to normalize arrests to get a sense of over-policing 
"""

import pandas as pd 
from sodapy import Socrata 

def get_open_data_df(soda, limit = 999999999, client = Socrata('data.cityofnewyork.us', None, timeout = 600)):
    results = client.get(soda, limit = limit)
    df = pd.DataFrame.from_records(results)
    return df

## arrests 

arrests_historic_soda = '8h9b-rp9u'
arrests_ytd_soda = 'uip8-fykc'

arrests_ytd_df = get_open_data_df(arrests_ytd_soda)
arrests_historic_df = get_open_data_df(arrests_historic_soda)

arrests_all = pd.concat([arrests_historic_df, arrests_ytd_df], ignore_index=True)

arrests_all.to_parquet('data/output/nypd_arrests_6_5_26.parquet')

