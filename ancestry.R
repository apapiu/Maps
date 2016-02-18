# we are looking at ncestry by tract:
library(leaflet)
library(tigris)
library(stringr)
library(dplyr)

counties <- "*" #give codes of counties here
state <- c("CA")

map.ancestry("NY", c(5, 47, 61, 81, 85))



#function outputting the map of ancestry by tract in given counties:

map.ancestry <- function(state, counties) {
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
    
    merge<- geo_join(tracts, final.df, "GEOID", "anc.geoid")
    
    pal <- colorFactor("Paired", NULL, n = 6)
    
    leaflet() %>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -74.0059, lat = 40.7127, zoom = 13) %>%
                addPolygons(data = merge, 
                            fillColor = ~pal(stat), fillOpacity = .6, 
                            color = "white", weight = 0.3,
                            popup = paste0(merge$stat), smoothFactor = 0.2) %>%
                addLegend(pal = pal, 
                          values = merge$stat, 
                          position = "bottomright",
                          opacity = .7,
                          title = "Ancestry")
}
tracts <- tracts(state = "NY", cb=TRUE)


#How to get info on Asian people and Central Americans?

#which ancestries are most common?
#freq <- table(eth)
#sort(freq)
#plot(sort(freq))

#where are the romanians at?
#romanians <- final.df[final.df$stat == "Romanian", ]
#tracts <- tracts("36081043701")


