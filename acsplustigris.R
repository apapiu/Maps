library(tigris)
library(acs)
library(rgeos)
library(sp)
#It's a good idea to have "merge" as the final dataframe and "stat" as the variable
#also use "final.df" as the final numeric dataframe

#### COUNTY LEVEL:
states = c("MO", "IL")

#acs data:
geo <- geo.make(state = states, county = "*")
rent <- acs.fetch(table.number = "B25071", geography = geo)

#tigris spatial data:
counties <- counties(state  = states, cb = FALSE)

#acs has "Albany County, New York"
#tigris has "Albany County"
counties@data$NAMELSAD[1:10]
rent@geography$NAME[1:10]

#makes "Albany County, New York" into "Albany County"
rent@geography$NAME <-unlist(lapply(rent@geography$NAME, 
                             function(x) {str_split(x, ",")[[1]][1]}))

stat.df <- data_frame(county = rent@geography$NAME, 
                      stat = rent@estimate[,"B25071_001"])
#merge
merge <- geo_join(counties, stat.df, "NAMELSAD", "county")

#map
pal <- colorNumeric("Blues", NULL)
leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = merge, 
                fillColor = ~pal(stat), fillOpacity = 1, 
                color = "white", weight = 2,
                popup = paste0(merge$county, "<br>", merge$stat))


####ZIP CODE LEVEL: finally no wrangling
#tigris - NYC zip codes:
codes <- c("100", "101","102","103","104","111","112","113","114",
           "070","071","072","073")

zips <- zctas(cb = TRUE, starts_with = codes)

zip.codes.nyc <- zips@data$ZCTA5CE10 #to plug into acs.

geo <- geo.make( zip.code = zip.codes.nyc)
rent <- acs.fetch(table.number = "B25071", geography = geo)

rent.df <- data_frame(zipcode = rent@geography$zipcodetabulationarea,
                       stat = rent@estimate[,"B25071_001"])

merge <- geo_join(zips, rent.df, "ZCTA5CE10", "zipcode")


pal <- colorNumeric("Blues", NULL)
leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = merge, 
                fillColor = ~pal(stat), fillOpacity = .65, 
                color = "white", weight = 1.5,
                popup = paste0(merge$zipcode, "<br>", merge$stat))
