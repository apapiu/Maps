library(leaflet)
library(ggplot2)
library(htmlwidgets)
library(tigris)
library(acs)
library(stringr) # to pad fips codes
library(rgdal)
library(dplyr)

#installing rgdal was hard looked it up on stackexchange
#install.packages("tigris")   
#install.packages('rgeos', type="source")
#install.packages('rgdal', type="source")
#install.packages('rgdal', type = "source", configure.args=c('--with-proj-include=/usr/local/include','--with-proj-lib=/usr/local/lib'))

saveWidget(widget = your.map, file="your_map.html", selfcontained = FALSE)

counties <- c(5, 47, 61, 81, 85)

#gives the spatial thing using tigris
tracts <- tracts(state = 'NY', county = c(5, 47, 61, 81), cb=TRUE)

#acs data extraction:
geo<-geo.make(state=c("NY"),
              county=c(5, 47, 61, 81), tract="*", check = TRUE)

income<-acs.fetch(endyear = 2012, span = 5, geography = geo,
                  table.number = "B19001", col.names = "pretty")

income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                               str_pad(income@geography$county, 3, "left", pad="0"), 
                               str_pad(income@geography$tract, 6, "left", pad="0")), 
                        income@estimate[,c("Household Income: Total:",
                                           "Household Income: $200,000 or more")], 
                        stringsAsFactors = FALSE)
temp <- income@estimate

income_df <- select(income_df, 1:3)
rownames(income_df)<-1:nrow(income_df)
names(income_df)<-c("GEOID", "total", "over_200")
income_df$percent <- 100*(income_df$over_200/income_df$total)

#merging the data in the map and the data in the mapdata:

income_merged<- geo_join(tracts, income_df, "GEOID", "GEOID")



popup <- paste0("GEOID: ", income_merged$GEOID, "<br>", "Percent of Households above $200k: ", round(income_merged$percent,2))

pal <- colorNumeric(
    palette = "YlGnBu",
    domain = income_merged$percent
)

m <- leaflet() %>%
     addTiles() %>%
     setView(lng = -73.985428, lat = 40.748817, zoom = 13) %>%
     addProviderTiles("CartoDB.Positron") %>%
     addPolygons(data = income_merged, 
                 fillColor = ~pal(percent), 
                 color = "#b2aeae", # you need to use hex colors
                 fillOpacity = 0.45, 
                 weight = 1, 
                 smoothFactor = 0.4,
                 popup = popup)
m

#providers : CartoDB.Positron, Esri.WorldGrayCanvas, CartoDB.DarkMatter.
# MapBox, Hydda.Base


#________________________________________________________

# api.key.install("my_key_here") You can get your own API key from the Census Bureau


###TRACTS

stltracts <- tracts("MO", c("St. Louis City"))


###Getting the Data thru ACS:
api.key.install(key = "6dd74c58e801a0aec87d86549cc3869cdf9946f5")

#set the geography - in this case stl county and city
#make sure to use geo.lookup and check = TRUE
geo = geo.make(state = c("MO"), county = c(510), tract = "*", check =TRUE)

#fetch the ACS, col.names = "pretty" gives more understandable cols
income <- acs.fetch(endyear = 2012, geography = geo, table.number = "B19001",
                    col.names = "pretty")

#to actually get the dataframe you're after:
temp <- income@estimate

#NOW tracts has GEOID and you need that in the the acsfetch
#GEOID = state code + county code + tract code

#_________________
####FLOW:
#-----------------
GEOID <- income_df$GEOID

data <- data.frame(GEOID, statis = rnorm(length(GEOID)))

#merge the data - this is all you need for leaflet!!!
data.merged <- geo_join(spatial_data = tracts, data_frame = data, 
                        "GEOID", "GEOID")


pal <- colorQuantile("Blues", NULL, n = 6)


#### DEFAULT options with merge and stat:
leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = merge, 
                fillColor = ~pal(stat), fillOpacity = 1, 
                color = "white", weight = 2,
                popup = paste0(merge$stat)) %>%
    addLegend(pal = pal, 
              values = merge$stat, 
              position = "bottomright",
              opacity = .7,
              title = "blah")


                