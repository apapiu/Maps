#tigris:

library(tigris)
library(leaflet)
library(sp)

states <- states()
uas <- urban_areas(cb = TRUE)
count <- counties(state = c("New York", "Massachusetts", "Connecticut"))
tracts <- tracts(state = "NY")
pumaz <- pumas(state = "NY", cb = TRUE)
block <- blocks(state = "NY")
zipss <- zctas(cb = TRUE, starts_with = c("37", "38", "72")) #you need cb = TRUE here
df <- zctas(cb = TRUE, starts_with = c("1"))


#just a little test with random data
x <- data.frame(zip = df@data$ZCTA5CE10 ,stat = rnorm(3658))
merge <- geo_join(df, x, "ZCTA5CE10", "zip")
pal <- colorNumeric(palette = "Blues", domain = c(-3,3))
leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = merge, fillColor = ~pal(stat),
                weight = .5, color = "white",
                fillOpacity = 1, 
                smoothFactor = 0.2) # really helps!


