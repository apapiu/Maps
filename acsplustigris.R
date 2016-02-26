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


#ZIP CODE LEVEL:
codes <- c("100","101","102","103","104","111","112","113","114")
#"070","071","072","073")

zips <- zctas(cb = TRUE, starts_with = codes) #tigris
zip.codes.nyc <- zips@data$ZCTA5CE10 #to plug into acs.

geo <- geo.make(zip.code = zip.codes.nyc)

#-------data workspace
rent <- acs.fetch(table.number = "B25071", geography = geo) #acs
rent.df <- data_frame(zipcode = rent@geography$zipcodetabulationarea,
                      stat = rent@estimate[,"B25071_001"])
#--------------------------
age <- acs.fetch(table.number = "B01002", geography = geo, col.names = "pretty") #acs
temp <- age@estimate

age.df <- data_frame(zipcode = age@geography$zipcodetabulationarea,
                     stat = age@estimate[,"Median Age by Sex:  Median age -- Total: "])
#--------------
income <- acs.fetch(table.number = "B19001", geography = geo, col.names = "pretty") #acs
temp <- income@estimate # a little hard to understand
#--------------------
ancestry=acs.fetch(geo=geo, table.name="People Reporting Ancestry",
                   col.names="pretty")
ancestry <- acs.fetch(table.number = "B04007", geography = geo, col.names = "pretty")
#very slowww!!!
#-----------------
bike <- acs.fetch(table.number = "B08301", geography = geo, col.names = "pretty")


#----------------------
final.df <- age.df
#--------------------

merge <- geo_join(zips, final.df, "ZCTA5CE10", "zipcode") #tigris

pal <- colorNumeric(c("Blues"), domain = 22:50)

pal <- colorBin(c("Blues"), domain = 20:50, bins = 7)

leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -74.0059, lat = 40.7127, zoom = 13) %>%
    addPolygons(data = merge, fillColor = ~pal(stat), fillOpacity = .6, 
                color = "white", weight = 1.5,
                popup = paste0("Zip Code", merge$zipcode,"<br>",
                               "Median Age ", merge$stat)) %>%
    addLegend(pal = pal, 
              values = merge$stat, 
              position = "bottomright",
              opacity = .7,
              title = "Median Age by Zip Code") 
