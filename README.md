Interactive Maps in R using the leaflet and tigris packages

The R package `leaflet` allows you to create really clean interactive maps.

The workflow is as follows:

1. Use the `tigris` package to get the geospatial data
2. Get the numeric data by say county(or tract, zip code etc.) via a package like `acs` or some API
3. Merge the data using `geo_join`
4. Pipe the result into `leaflet` and tweak any aesthetic preferences you might have.

I've been playing around with this and got some interesting maps relating to 
- ancestry `ancestry.R`
- bike usage `bike_maps.R`
- dating `singles_nyc.R	`, `singlesmaps.R`

Also `ancestry.R` contains a function that outputs the ancestry by region - I have it set to New York but you can easily 
change it - I found LA and Chicago particularly interesting.
