import sys
import requests
import pandas as pd
import jwt
import datetime
import time
exec(open('../../../tokens.py').read())


#-----------------------------------------------------------------------------------
# helper functions
#-----------------------------------------------------------------------------------

def pull_xyz(token, page_count, cursor, time="2026-01-01T00:00:00Z"): 
    # check if token is expired
    if is_jwt_token_expired(token):
        token = refresh_jwt_token(token)
    headers = {"X-Auth-Token": "Bearer " + token}

    # Construct the request payload "tagsPrimary": {"eq": "Grocery Store"}
    payload = {"pageSize": page_count,
                "cursor": cursor,
                "validityTime": {"gte": time}}
    response = requests.post(base_url, json=payload, headers=headers)

    if response.status_code == 200:
        # get response, and cursor
        data = response.json()
        return data
    else:
        # error - 401 is from token refresh needed
        print("Request failed with status code:", response.status_code)
        return

def is_jwt_token_expired(token) -> bool:
    current_time = datetime.datetime.utcnow()
    try:
        decoded_token = jwt.decode(token, algorithms=['HS256'], options={"verify_signature": False})
        exp_timestamp = decoded_token['exp']
        exp_datetime = datetime.datetime.utcfromtimestamp(exp_timestamp)

        if exp_datetime < current_time - datetime.timedelta(minutes=1):
            return True  # Token has expired
        else:
            return False  # Token is still valid

    except Exception:
        return True  # Sometimes a decode error will occur

def refresh_jwt_token(token: str) -> str:
    #print('Refreshing JWT Token')

    headers = {
        "Authorization": token
    }
    response = requests.post("https://auth-api.liveapp.com/azure/refresh", headers=headers)

    if response.status_code == 200:

        # Successful request
        data = response.json()

        # Do something with the data here
        token = data.get('token')

        #print(token)
        return token

def time_elapsed(start):
    elapsed = round(time.time() - start, 0)
    if elapsed > 60: 
        elapsed = round(elapsed/60, 1)
        return(f"{elapsed} (minutes)")
    elif elapsed > 3600: 
        elapsed = round(elapsed/3600, 2)
        return(f"{elapsed} (hours)")
    else: 
        return(f"{int(elapsed)} (seconds)")


#-----------------------------------------------------------------------------------
# settings
#-----------------------------------------------------------------------------------

base_url = "https://graphql-enki.liveapp.com/features/648b1584fe16016869b2415a"
token = livexyz_key

page_count = 10000
pages = 0
cursor = None
paginated_data = 0


#-----------------------------------------------------------------------------------
# loop to pull in data
#-----------------------------------------------------------------------------------

start = time.time()
while True:

    # pull data from live xyz
    data = pull_xyz(livexyz_key, page_count, cursor, time="2026-01-01T00:00:00Z")
    cursor = data.get('data', {}).get('features', {}).get('pageInfo', {}).get('cursor')

    # compile data
    data = pd.DataFrame(data['data']['features']['nodes'])
    if pages == 0: paginated_data = [data]
    if pages != 0: paginated_data.append(data)
    
    pages = pages + 1
    print(f"Queried {pages*page_count} records in {time_elapsed(start)}")

    # stop if there's nothing left
    if not cursor:
        break


#-----------------------------------------------------------------------------------
# munge data
#-----------------------------------------------------------------------------------

# concat
combined_data = pd.concat(paginated_data, ignore_index=True)

# only keep shops that are occupied
combined_data = combined_data[combined_data.spaceStatus == "Occupied"]
combined_data = combined_data.dropna(subset=['spaceStatus'])

# manipulate the tags
combined_data = combined_data.dropna(subset=['tagsPrimary'])
combined_data['first_tag'] = combined_data['tagsPrimary'].apply(lambda d: d.get('name'))
combined_data['lat'] = combined_data['entrances'].apply(lambda d: d.get('main').get('lat'))
combined_data['lon'] = combined_data['entrances'].apply(lambda d: d.get('main').get('lon'))
combined_data = combined_data[['stateId', 'resolvedName', 'address', 'lat', 'lon', 'first_tag']]
combined_data.to_csv("data/all_xyz.csv", index=False)


grocery_tags = ["Grocery", "Supermarket"]
bodega_tags = ["Deli", "Convenience", "Gas Station"]
vice_tags = ["Smoke Shop", "Bar", "Liquor", "Wine", "Cocktail", "Pub", "Nightclub", "Cannabis"]
fast_food_tags = ["Fried Chicken", "Burger", "Fast Food", "Hot Dog", "Wings", "Donut", "Candy"]

grocery = combined_data[combined_data['first_tag'].str.contains('|'.join(grocery_tags),regex=True)]
bodega = combined_data[combined_data['first_tag'].str.contains('|'.join(bodega_tags),regex=True)]
vices = combined_data[combined_data['first_tag'].str.contains('|'.join(vice_tags),regex=True)]
fast_food = combined_data[combined_data['first_tag'].str.contains('|'.join(fast_food_tags),regex=True)]

grocery.to_csv("data/grocery_xyz.csv", index=False)
bodega.to_csv("data/bodega_xyz.csv", index=False)
vices.to_csv("data/vices_xyz.csv", index=False)
fast_food.to_csv("data/fast_food_xyz.csv", index=False)
