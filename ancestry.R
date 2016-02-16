counties <- c(5, 47, 61, 81, 85)
#gives the spatial thing using tigris
tracts <- tracts(state = 'NY', county = c(5, 47, 61, 81), cb=TRUE)
#acs data extraction:
geo<-geo.make(state=c("NY"),
              county=c(5, 47, 61, 81), tract="*", check = TRUE)

ancestry=acs.fetch(geo=geo, table.name="People Reporting Ancestry",
                   col.names="pretty")

anc <- ancestry@estimate
anc <- data.frame(anc)

colnames(anc) <- lapply(colnames(anc), function(x){str_split(x, "Ancestry...")[[1]][2]})

colnames(anc) <- lapply(colnames(anc), function(x) {gsub(".", "", x, 
                                                         fixed = TRUE)})


geoid <- paste0(str_pad(ancestry@geography$state, 2, "left", pad="0"), 
                              str_pad(ancestry@geography$county, 3, "left", pad="0"), 
                              str_pad(ancestry@geography$tract, 6, "left", pad="0"))


anc$geoid <- geoid

eth <- numeric(2057)

for (i in (1:2057)){
eth[i] <- names(which.max(anc[i,-c(1, 108, 109, 110)]))
}

anc$eth <- eth

final.df <- data.frame(anc$geoid, stat = as.factor(anc$eth))

romanians <- final.df[final.df$stat == "Romanian", ]
tracts <- tracts("36081043701")


merge<- geo_join(tracts, final.df, "GEOID", "anc.geoid")



pal <- colorFactor("Paired", NULL, n = 6)

leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.985428, lat = 40.748817, zoom = 11) %>%
    addPolygons(data = merge, 
                fillColor = ~pal(stat), fillOpacity = .55, 
                color = "white", weight = .75,
                popup = paste0(merge$stat)) %>%
    addLegend(pal = pal, 
              values = merge$stat, 
              position = "bottomright",
              opacity = .7,
              title = "Ancestry")
