library(dplyr)
library(tigris)
library(acs)
library(stringr) # to pad fips codes
library(rgdal)
library(dplyr)
library(leaflet)

#in this case I get the table from the census data directly
#so I don't have to worry about GEOID
acstable <- read.csv("acstable.csv", stringsAsFactors = FALSE)
food <- select(acstable, GEOID = GEO.id2, 
               total = HC01_EST_VC01, stamps = HC02_EST_VC01)
food <- food[-1,]
food$total <- as.numeric(food$total)
food$stamps <- as.numeric(food$stamps)
food$statis <- food$stamps/food$total


#-----------
acstable1 <- read.csv("median_income.csv", stringsAsFactors = FALSE)
income <- select(acstable1, GEOID = GEO.id2, statis = HD01_VD02)
income <- income[-1, ]
income$statis <- as.numeric(income$statis)
#gives only tracts in Marlyand?


#--------------
geo<-geo.make(state=c("NY"),
              county=c(5, 47, 61, 81), tract="*", check = TRUE)

income<-acs.fetch(endyear = 2012, span = 5, geography = geo,
                  table.number = "B01002", col.names = "pretty")

temp <- income@estimate

temp$

colnames(income@estimate)

age<- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                               str_pad(income@geography$county, 3, "left", pad="0"), 
                               str_pad(income@geography$tract, 6, "left", pad="0")), 
                        income@estimate[,"Median Age by Sex: Median age -- Total:"], 
                        stringsAsFactors = FALSE)
colnames(age) <- c("GEOID", "median_age")

#----------
#NYC counties
counties <- c(5, 47, 61, 81)
tractsny <- tracts(state = 'NY', county = c(5, 47, 61, 81), cb=TRUE)

#merge the data - this is all you need for leaflet!!!
data.merged <- geo_join(spatial_data = tractsny, data_frame = age, 
                        "GEOID", "GEOID", how = "inner") #inner for intersect


pal <- colorQuantile("Blues", NULL, n)

pal <- colorNumeric(palette = "Blues", domain = 15:60)

popup <- paste0("GEOID: ", data.merged$GEOID, "<br>", 
                "Median Age", data.merged$median_age)


leaflet() %>%
    addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.985428, lat = 40.748817, zoom = 12) %>%
    addPolygons(data = data.merged,
                weight = 1, #how thick the lines are
                fillColor = ~pal(data.merged$median_age), #color of fills
                fillOpacity = 0.5, #see through fills
                color = "#b2aeae",
                popup = popup) %>%
    addLegend(pal = pal, 
              values = data.merged$median_age, 
              position = "bottomright")

#--------------
#percentange walking to state:

geo<-geo.make(state=c("NY"),
              county=c(5, 47, 61, 81), tract="*", check = TRUE)

bikes<-acs.fetch(endyear = 2012, span = 5, geography = geo,
                  table.number = "B08101", col.names = "pretty")

temp <- bikes@estimate

bikes.df <- data.frame(paste0(str_pad(bikes@geography$state, 2, "left", pad="0"), 
                        str_pad(bikes@geography$county, 3, "left", pad="0"), 
                        str_pad(bikes@geography$tract, 6, "left", pad="0")), 
                 bikes@estimate[ ,"MEANS OF TRANSPORTATION TO WORK BY AGE: Total:"],
                 bikes@estimate[ ,"MEANS OF TRANSPORTATION TO WORK BY AGE: Walked:"],
                 stringsAsFactors = FALSE)


colnames(bikes.df) <- c("GEOID", "total", "walked")

bikes.df$perwalked <- bikes.df$walked/bikes.df$total*100

data.merged <- geo_join(spatial_data = tractsny, data_frame = bikes.df, 
                        "GEOID", "GEOID", how = "inner") #inner for intersect


pal <- colorQuantile("Blues", domain = 0:100, n = 10,
                     probs = c(0,.05,.1,.15,.2,.25,.3,.4,.5,.75,1))

colorQuantile(palette, domain, n = 4, probs = seq(0, 1, length.out = n + 1), 
             na.color = "#808080", alpha = FALSE)


pal <- colorNumeric("Blues", domain = 0:65)

pal <- colorBin("BuPu", bins = 9, domain = 0:90)

pal <- colorBin(colorRamp(c("green", "blue"), interpolate="spline"),
                bins = 9, domain = 0:90)

popup <- paste0("GEOID: ", data.merged$GEOID, "<br>", 
                "Percentage Walking", data.merged$perwalked)


leaflet() %>%
    addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.985428, lat = 40.748817, zoom = 12) %>%
    addPolygons(data = data.merged,
                weight = 1, #how thick the lines are
                fillColor = ~pal(data.merged$perwalked), #color of fills
                fillOpacity = 0.5, #see through fills
                color = "#b2aeae",
                popup = popup) %>%
    addLegend(pal = pal, 
              values = data.merged$perwalked, 
              position = "bottomright",
              title = "Percent of People Walking to Work",
              labFormat = labelFormat(suffix = "%"))




