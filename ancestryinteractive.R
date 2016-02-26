library(leaflet)
library(tigris)
library(stringr)
library(dplyr)
library(acs)


state = "NY"
counties = c(5, 47, 61, 81, 85)


tracts <- tracts(state = state,county = counties, cb=TRUE)
geo<-geo.make(state = state, county = counties, tract="*")

ancestry=acs.fetch(geo=geo, table.name="People Reporting Ancestry",
                   col.names="pretty")

anc <- ancestry@estimate
anc <- data.frame(anc)

colnames(anc) <- lapply(colnames(anc), 
                        function(x){strsplit(x, "...", fixed = TRUE)[[1]][2]})
colnames(anc) <- lapply(colnames(anc), 
                        function(x) {gsub(".", "", x, fixed = TRUE)})
geoid <- paste0(str_pad(ancestry@geography$state, 2, "left", pad="0"), 
                str_pad(ancestry@geography$county, 3, "left", pad="0"), 
                str_pad(ancestry@geography$tract, 6, "left", pad="0"))

anc$geoid <- geoid 

eth <- numeric(length(geoid))

names(anc)[103] <- "Trinidad&Tobagoian"
names(anc)[40] <- "French"

for (i in (1:length(geoid))){
    eth[i] <- names(which.max(anc[i,-c(1,6,15, 35, 94, 104, 73,88, 106, 108, 109, 110)]))
} #I eliminate some groups that are vague or are further partitioned in smaller groups

anc$eth <- eth

final.df <- data.frame(anc$geoid, stat = as.factor(anc$eth))

final.df$Italian <- numeric(dim(final.df)[1])
final.df$Italian[final.df$stat == "Italian"] <- 1

final.df$Irish <- numeric(dim(final.df)[1])
final.df$Irish[final.df$stat == "Irish"] <- 1

final.df$Russian <- numeric(dim(final.df)[1])
final.df$Russian[final.df$stat == "Russian"] <- 1

merge<- geo_join(tracts, final.df, "GEOID", "anc.geoid")

pal1 <- colorFactor(c("transparent", "blue"), NULL)
pal <- colorFactor("Paired", domain = merge$stat)


leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -74.0059, lat = 40.7127, zoom = 13) %>%

    addPolygons(data = merge, 
                fillColor = ~pal(stat), fillOpacity = .6, 
                color = "white", weight = 0.3,
                popup = paste0(merge$stat), smoothFactor = 0.2,
                group = "All") %>%
    
    addPolygons(data = merge, 
                fillColor = ~pal1(Italian), fillOpacity = .6, 
                color = "white", weight = 0.3, smoothFactor = 0.2,
                group = "Italians") %>%
    
    addPolygons(data = merge, 
                fillColor = ~pal1(Russian), fillOpacity = .6, 
                color = "white", weight = 0.3, smoothFactor = 0.2,
                group = "Russians") %>%
    
    addPolygons(data = merge, 
                fillColor = ~pal1(Irish), fillOpacity = .6, 
                color = "white", weight = 0.3, smoothFactor = 0.2,
                group = "Irish") %>%
    
    addLayersControl(
        overlayGroups = c("All", "Italians", "Irish", "Russians"),
        options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup(c("Italians", "Irish", "Russians"))
