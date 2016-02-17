library(plotly)
library(dplyr)

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
df <- filter(df, pop > 200000)

g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'))

plot_ly(df, lon = lon, lat = lat, type = 'scattergeo', locationmode = 'USA-states',
        marker = list(size = pop/150000)) %>%
    layout(geo = g)