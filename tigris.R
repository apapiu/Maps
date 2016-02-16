#tigris:
library(tigris)
library(leaflet)
library(sp)

states <- states()
uas <- urban_areas(cb = TRUE)
count <- counties(state = c("New York", "Massachusetts", "Connecticut",
                            "New Hampshire", "Vermont"))
tracts <- tracts(state = "NY")
pumaz <- pumas(state = "MO")
block <- blocks(state = "NY")
zipss <- zctas(starts_with = c("37", "38", "72"))
df <- zctas(cb = TRUE, starts_with = c("37", "38", "72"))


leaflet() %>%
    addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(weight = 2, data = count)

