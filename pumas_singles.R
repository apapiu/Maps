library(acs)
library(data.table)
library(dplyr)
library(leaflet)
library(tigris)


load("/Users/alexpapiu/Documents/R/Data INCU Semi #1/census.Rd")

#singles between 18 and 35
ysingles <- filter(cens, AGEP >=18,AGEP <= 35, MAR %in% 2:5,
                   SCHL %in% 21:24)
 
#have these in increasing order!
NY_puma <- c(3102:3107, 3201:3212, 3701:3710, 3801:3810, 4000:4018, 4100:4114)

#temp <- pumas(state = "CA")
#CA_puma <- temp@data$PUMACE10

nyc <- filter(ysingles, PUMA %in% NY_puma)
singnyc <- table(nyc$SEX, nyc$PUMA)
ratiosforpuma <- singnyc[1,]/singnyc[2,]
pumasnyc <- data.frame(pumas = NY_puma, ratios = ratiosforpuma) 

#make pumas into chars so they fit with the tigris files
pumasnyc$pumas <- unlist(lapply(pumasnyc$pumas, toString)) 
pumasnyc$pumas <- unlist(lapply(pumasnyc$pumas, function(x){paste0("0",x)}))

#now bring in the puma shapefiles, I don't know how to select only some...
shape.nyc <- pumas(state = "NY")


#now let's merge them! use "inner" since we have all state geospatial files
nyc.merged<- geo_join(shape.nyc, pumasnyc, "PUMACE10", "pumas", how = "inner") 

nyc.merged@data$NAMELSAD10 <- lapply(nyc.merged@data$NAMELSAD10, 
       function(x){strsplit(x, "--", fixed = TRUE)}[[1]][2])
nyc.merged@data$NAMELSAD10 <- 


pal <- colorNumeric("RdBu", domain = c(.5,1.5))
pal0 <- colorBin("RdBu", domain = c(.5,1.5))


popup <- paste0(nyc.merged$NAMELSAD10, "<br>",  round(nyc.merged$ratios*100), " single men for 100
                single women")
pal1 <- colorNumeric("RdBu", domain = c(50,150))


leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.985428, lat = 40.748817, zoom = 11) %>%
    addPolygons(data = nyc.merged,
                weight = 1.5, #how thick the lines are
                fillColor = ~pal(nyc.merged$ratios), #color of fills
                fillOpacity = 0.6, #see through fills
                color = "white",
                popup = popup) %>%
    addLegend(pal = pal1, 
              values = nyc.merged$ratios*100, 
              position = "bottomright",
              opacity = .7,
              title = "Eligible men per <br> 100 single women
               <br> (with college degrees <br>
              ages 18 to 35)") 




#-----------------------------------------------------------------
#SINGlE RATIO by PUMA by STATE!

ysingles <- filter(cens, AGEP >=18,AGEP <= 35, MAR %in% 2:5,
                   SCHL %in% 21:24)

singbypumas <- table(ysingles$SEX, ysingles$PUMA)
ratios <- singbypumas[1,]/singbypumas[2,]

#CAREFUL here to make sure things line up properly
pumas <- data.frame(pumas = sort(unique(ysingles$PUMA)), ratios = ratios) 

pumas$pumas <- unlist(lapply(pumas$pumas, toString)) 
pumas$pumas <- unlist(lapply(pumas$pumas, function(x){paste0("0",x)}))

shapes <- pumas(state = c("NY"))

merge <- geo_join(shapes, pumas, "PUMACE10", "pumas", how = "inner")


pal <- colorNumeric("RdBu", domain = c(.5,1.5))
popup <- paste0("PUMA:", merge$pumas, "<br>",  round(merge$ratios*100), " single men for 100
                single women")


leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = merge,
                weight = 2, #how thick the lines are
                fillColor = ~pal(merge$ratios), #color of fills
                fillOpacity = 0.5, #see through fills
                color = "white",
                smoothFactor = 0.2,
                popup = popup)





