# Cincinnati Police Drone Squad flight paths

This project from The Cincinnati Enquirer allows for the scraping and analysis of flight paths from the Cincinnati Police Department's drone program, operated through Skydio. The data is used in several stories published in The Enquirer's Nov. 30 feature: [Cincinnati police's $4.8M drone program takes off. How does the eye in the sky work?](https://www.cincinnati.com/story/news/crime/2025/11/30/cincinnati-police-drones-are-now-first-responders-heres-how-it-works/86280621007/)

You may also modify the ArcGIS endpoint and [scrape any other department using Skydio](https://www.google.com/search?q=site:https://cloud.skydio.com/dashboard/&filter=0).

## Download flight paths

Cincinnati police drone flight paths through Nov. 23, 2025 may be downloaded here: [https://gannett-my.sharepoint.com/:u:/p/dferrara/EY-SeVSuPORIkgGYVLwvzA0BB4iWABOAkPzpx6R1mVzPXA?e=SCLe6Y](https://gannett-my.sharepoint.com/:u:/p/dferrara/EY-SeVSuPORIkgGYVLwvzA0BB4iWABOAkPzpx6R1mVzPXA?e=SCLe6Y)

## How to use this code

- `scrape.py`: Use this script either once or on an interval (once a day, once a week, etc.) to scrape flight paths via the ArcGIS API using the endpoint provided by [Skydio's portal for the Cincinnati Police Department](https://cloud.skydio.com/dashboard/cincinnatipolicedrones).
- `cfs.R`: Use this script to clean up the flight paths, match them with calls for service and analyze the data (with helpful tables for reporting).
- `animated_map.R`: Use this script to generate an interactive Shiny map for visualizing flight paths provided in the data over time.
- `heatmap.R`: Use this script to generate an interactive Shiny heatmap for visualizing flight paths across a city/region over time.

If you plan to scrape the flight paths of another department that uses Skydio, replace the endpoint in `scrape.py` with the appropriate one for your department. More on that below.

## Finding a department's ArcGIS endpoint

1. **Load the Skydio dashboard** for the department, if available.
2. **Open the Network tab in your browser and filter for "featureserver".**
3. **Look for a GET like "FeatureServer?f=json".** The filename provided is the suffix of the endpoint for your department. For example, Cincinnati's is `/mnhQTdIYDA7UoY2l/arcgis/rest/services/b322b378-b726-4bd3-93fe-bab0b311191b-production/FeatureServer`.
6. **Add the suffix to the root URL for the ArcGIS API**, `https://services7.arcgis.com/`. For example, Cincinnati's should be `https://services7.arcgis.com/mnhQTdIYDA7UoY2l/arcgis/rest/services/b322b378-b726-4bd3-93fe-bab0b311191b-production/FeatureServer`.
7. **Add `/0/query` to the end.** For example, the full Cincinnati endpoint is `https://services7.arcgis.com/mnhQTdIYDA7UoY2l/arcgis/rest/services/b322b378-b726-4bd3-93fe-bab0b311191b-production/FeatureServer/0/query`.

## Notes

Please reach out to [David Ferrara, Enquirer reporter,](mailto:dferrara@cincinnati.com) if you have any questions or comments.

- The scrape script can sometimes hit the ArcGIS limit in one API call. If you are scraping all of the flight paths at once, you may need to modify the script or run in intervals.
- Departments can choose whether to publish a particular flight on the Skydio dashboard. Keep in mind that all the flight paths provided may not be all the flights a department has made.
- The CFS matching function is built using Cincinnati's CAD database and GIS centerline data. However, it inherently can produce false positives because the coordinates in Cincinnati's CAD data is skewed within the same block for privacy reasons. Also, a buffer zone is applied because some flights do not directly intersect with a call location, but merely fly nearby.
- At the time this story was published, Cincinnati launched drones from three hubs in the West End, West Price Hill and Northside. Since all flights depart from those locations, it's more likely they'll intersect other active calls for service in those areas, producing false positives.

## Data dictionary

Flight paths obtained via the ArcGIS API return the following attributes:

- `object_id`: Apparent auto-incrementing column for each flight. Some values are skipped, indicating a flight may not have been published.
- `flight_id`: UUID for each flight.
- `vehicle_serial`: Not provided in Cincinnati's data, returned as an empty column.
- `dock_serial`: Not provided in Cincinnati's data, returned as an empty column.
- `user_email`: Not provided in Cincinnati's data, returned as an empty column.
- `takeoff`: Takeoff timestamp in miliseconds with origin as 1970-01-01.
- `landing`: Landing timestamp in miliseconds with origin as 1970-01-01.
- `external_id`: Not provided in Cincinnati's data, returned as an empty column.
- `description`: Not provided in Cincinnati's data, returned as an empty column.
- `flight_purpose`: Cincinnati returns only "Call for Service" and "Training," but other departments may return different values.
- `operation_id`: Not provided in Cincinnati's data, returned as an empty column.
- `organization_id`: UUID for the department.
- `shape_length`: Flight path coordinates from launch to landing.

For more information on [Cincinnati's CAD data](https://data.cincinnati-oh.gov/safety/PDI-Police-Data-Initiative-Police-Calls-for-Servic/gexm-h6bt/about_data) and [centerline database](https://data-cagisportal.opendata.arcgis.com/datasets/f5616d30d58945d6aac2cfe8aa35d373_27/explore), please visit their respective sites.

## Citation

If you use this data or code, please cite it and [link to the published story](https://www.cincinnati.com/story/news/crime/2025/11/30/cincinnati-police-drones-are-now-first-responders-heres-how-it-works/86280621007/).



