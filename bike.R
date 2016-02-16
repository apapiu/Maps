#percentage of people who walk to work by tract:

geo<-geo.make(state=c("NY"),
              county=c(5, 47, 61, 81), tract="*", check = TRUE)

bike<-acs.fetch(endyear = 2012, span = 5, geography = geo,
                  table.number = "B08301", col.names = "pretty")


bike_df <- data.frame(paste0(str_pad(bike@geography$state, 2, "left", pad="0"), 
                               str_pad(bike@geography$county, 3, "left", pad="0"), 
                               str_pad(bike@geography$tract, 6, "left", pad="0")), 
                        bike@estimate[,c("Means of Transportation to Work: Total:",
                                           "Means of Transportation to Work: Bicycle")], 
                        stringsAsFactors = FALSE)


final.df <- data.frame(GEOID = bike_df[,1], 
                       stat = bike_df$Means.of.Transportation.to.Work..Bicycle/
                           bike_df$Means.of.Transportation.to.Work..Total. *100)


tracts <- tracts <- tracts(state = 'NY', county = c(5, 47, 61, 81), cb=TRUE)


merge <- geo_join(tracts, final.df, "GEOID", "GEOID")

pal <- colorNumeric("Blues", domain = c(0,14))

leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = merge, fillColor = ~pal(stat), fillOpacity = .6, 
                color = "white", weight = 1.5,
                popup = paste0(merge$GEOID, "<br>","<h1>This is some text!</h1>",
merge$stat))%>%
    addLegend(pal = pal, 
              values = merge$stat, 
              position = "bottomright",
              opacity = .7,
              title = "Percentage of People biking to work")

#--------------------------
#do this at the zipcta level, unfortunately the acs.fetch is acting up so I download the data.
#people who bike to work

#data munging
bikes <- read.csv("transport_zip.csv", stringsAsFactors = FALSE)
bikes <- select(bikes, zip = GEO.display.label, total = HD01_VD01, 
                bike = HD01_VD18)
bikes <- bikes[-1,]
bikes$zip <- unlist(lapply(bikes$zip, function(x){str_split(x, " ")[[1]][2]}))
bikes$stat <- round(as.numeric(bikes$bike)/as.numeric(bikes$total)*100,2)


final.df <- select(bikes, zip, stat)

codes <- c("100","101","102","103","104","111","112","113","114")
#"070","071","072","073")

zips <- zctas(cb = TRUE, starts_with = codes) #tigris

merge <- geo_join(zips, final.df, "ZCTA5CE10", "zip") #tigris

pal <- colorNumeric(c("Blues"), domain = -1:8)


leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -74.0059, lat = 40.7127, zoom = 13) %>%
    addPolygons(data = merge, fillColor = ~pal(stat), fillOpacity = .6, 
                color = "white", weight = 1.5,
                popup = paste0(merge$stat, "% biking to work")) %>%
    addLegend(pal = pal, 
              values = merge$stat, 
              position = "bottomright",
              opacity = .7,
              title = paste0("Percentage of People <br> Biking to Work"))



