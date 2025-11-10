### CINCINNATI ENQUIRER
### Cincinnati Police Department
### Scrape SkyDio Drone Flight Paths
### by David Ferrara

import requests
import json
import os
from datetime import datetime

# Output GeoJSON file
geojson_file = "data/flight_paths.geojson" # replace with your desired path

# ArcGIS FeatureServer endpoint
url = "https://services7.arcgis.com/mnhQTdIYDA7UoY2l/arcgis/rest/services/b322b378-b726-4bd3-93fe-bab0b311191b-production/FeatureServer/0/query"

params = {
    "where": "flight_purpose = 'Call for Service'",
    "outFields": "*",
    "f": "geojson",
    "returnGeometry": "true",
    "spatialRel": "esriSpatialRelIntersects",
    "resultOffset": 0,
    "resultRecordCount": 2000,
    "resultType": "standard"
}

def log_message(message):
    log_file = "scrape.log"
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    with open(log_file, "a", encoding="utf-8") as log:
        log.write(f"[{timestamp}] {message}\n")

# Load existing features if file exists
if os.path.exists(geojson_file):
    with open(geojson_file, "r", encoding="utf-8") as f:
        existing_data = json.load(f)
    existing_features = existing_data.get("features", [])
    existing_ids = {f["properties"].get("flight_id") for f in existing_features}
else:
    existing_features = []
    existing_ids = set()

page_size = int(params.get("resultRecordCount", 2000))
offset = 0
fetched = 0
appended = 0

while True:
    params["resultOffset"] = offset
    params["resultRecordCount"] = page_size
    response = requests.get(url, params=params)

    if response.status_code != 200:
        msg = f"❌ Failed to fetch data (offset={offset}): {response.status_code}"
        print(msg)
        log_message(msg)
        break

    data = response.json()
    features = data.get("features", [])
    num = len(features)
    fetched += num

    if num == 0:
        # No more results
        break

    # Append only new features
    for feat in features:
        fid = feat["properties"].get("flight_id")
        if fid not in existing_ids:
            existing_features.append(feat)
            existing_ids.add(fid)
            appended += 1

    # If fewer than page_size returned, we're at the end
    if num < page_size:
        break

    offset += page_size

# Save combined features if any appended
if appended > 0:
    out = {"type": "FeatureCollection", "features": existing_features}
    with open(geojson_file, "w", encoding="utf-8") as f:
        json.dump(out, f)
    msg = f"✅ Fetched {fetched} features, appended {appended} new flight paths to {geojson_file}"
    print(msg)
    log_message(msg)
else:
    msg = f"ℹ️ Fetched {fetched} features. No new flight paths to append."
    print(msg)
    log_message(msg)