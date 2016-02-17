# we are looking at ncestry by tract:
library(leaflet)
library(tigris)
library(stringr)
library(dplyr)

counties <- "*" #give codes of counties here
state <- c("CA")

map.ancestry("CA", "*")



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


@import url("pygment_highlights.css");

/* --- General --- */
    
    body {
        font-family: 'Lora', 'Times New Roman', serif; <!--for the writing under the post title-->
            font-size: 10px;
        color: #404040;
            position: relative;
        background: #FFF;
    }
p {
    line-height: 1.5;
    margin: 30px 0;
}
p a {
    /* text-decoration: underline */
        color: #008AFF;
}
h1,h2,h3,h4,h5,h6 {
    font-family: 'Open Sans', 'Helvetica Neue', Helvetica, Arial, sans-serif;
    font-weight: 800;
}
a {
    color: #008AFF;
}
a:hover,
a:focus {
    color: #0085a1;
}
blockquote {
    color: #808080;
        font-style: italic;
}
hr.small {
    max-width: 100px;
    margin: 15px auto;
    border-width: 4px;
    border-color: inherit;
    border-radius: 3px;
}

.main-content {
    padding-top: 80px;
}
@media only screen and (min-width: 768px) {
    .main-content {
        padding-top: 130px;
    }
}

.main-explain-area {
    font-family: 'Open Sans', 'Helvetica Neue', Helvetica, Arial, sans-serif;
    padding: 15px inherit;
}

.hideme {
    display: none;
}

::-moz-selection {
    color: white;
    text-shadow: none;
    background: #0085a1;
}
::selection {
    color: white;
    text-shadow: none;
    background: #0085a1;
}
img::selection {
    color: white;
    background: transparent;
}
img::-moz-selection {
    color: white;
    background: transparent;
}

img {
    max-width: 100%;
}

.disqus-comments {
    margin-top: 30px;
}

@media only screen and (min-width: 768px) {
    .disqus-comments {
        margin-top: 40px;
    }
}

/* --- Navbar --- */
    
    .navbar-custom {
        background: #F5F5F5;
            border-bottom: 1px solid #EAEAEA;
        font-family: 'Open Sans', 'Helvetica Neue', Helvetica, Arial, sans-serif;
    }

.navbar-custom .nav li a {
    text-transform: uppercase;
    font-size: 12px;
    letter-spacing: 1px;
}

.navbar-custom .navbar-brand,
.navbar-custom .nav li a {
    font-weight: 800;
    color: #404040;
}

.navbar-custom .navbar-brand:hover,
.navbar-custom .navbar-brand:focus ,
.navbar-custom .nav li a:hover,
.navbar-custom .nav li a:focus {
    color: #0085a1;
}

@media only screen and (min-width: 768px) {
    .navbar-custom {
        padding: 20px 0;
        -webkit-transition: background .5s ease-in-out,padding .5s ease-in-out;
        -moz-transition: background .5s ease-in-out,padding .5s ease-in-out;
        transition: background .5s ease-in-out,padding .5s ease-in-out;
    }
    
    .navbar-custom.top-nav-short {
        padding: 0;
    }
}

.navbar-custom .avatar-container {
    opacity: 1;
    position: absolute;
    -webkit-transition: opacity 0.5s ease-in-out;
    -moz-transition: opacity 0.5s ease-in-out;
    transition: opacity 0.5s ease-in-out;
    left: 50%;
    width: 50px;
    margin-top: -25px;
}
.navbar-custom .avatar-container  .avatar-img-border {
    width: 100%;
    border-radius: 50%;
    margin-left: -50%;
    display: inline-block;
    box-shadow: 0 0 8px rgba(0, 0, 0, .8);
    -webkit-box-shadow: 0 0 5px rgba(0, 0, 0, .8);
    -moz-box-shadow: 0 0 8px rgba(0, 0, 0, .8);
}
.navbar-custom .avatar-container  .avatar-img {
    width: 100%;
    border-radius: 50%;
    display: block;
}

.navbar-custom.top-nav-short .avatar-container{
    opacity: 0;
}

.navbar-custom.top-nav-expanded .avatar-container  {
    display: none;
}

@media only screen and (min-width: 768px) {
    .navbar-custom .avatar-container {
        width: 100px;
        margin-top: -50px;
    }
    
    .navbar-custom .avatar-container  .avatar-img-border {
        width: 100%;
        box-shadow: 1px 1px 2px rgba(0, 0, 0, .8);
        -webkit-box-shadow: 1px 1px 2px rgba(0, 0, 0, .8);
        -moz-box-shadow: 1px 1px 2px rgba(0, 0, 0, .8);
    }
    
    .navbar-custom .avatar-container  .avatar-img {
        width: 100%;
    }
}

/* --- Footer --- */
    
    footer {
        padding: 30px 0;
        background: #F5F5F5;
            border-top: 1px #EAEAEA solid;
        margin-top: 50px;
        font-size: 14px;
    }

footer a {
    color: #404040;
}

footer .list-inline {
    margin: 0;
    padding: 0;
}
footer .copyright {
    font-family: Open Sans;
    text-align: center;
    margin-bottom: 0;
}
footer .theme-by {
    text-align: center;
    margin: 10px 0 0;
}

@media only screen and (min-width: 768px) {
    footer {
        padding: 50px 0;
    }
    footer .footer-links {
        font-size: 18px;
    }
    footer .copyright {
        font-size: 16px;
    }
}

/* --- Post preview --- */
    
    .post-preview {
        padding: 20px 0;
        border-bottom: 1px solid #eee;
    }

@media only screen and (min-width: 768px) {
    .post-preview {
        padding: 35px 0;
    }
}

.post-preview:last-child {
    border-bottom: 0;
}

.post-preview a {
    text-decoration: none;
    color: #404040;
}

.post-preview a:focus,
.post-preview a:hover {
    text-decoration: none;
    color: #0085a1;
}

.post-preview .post-title {
    font-size: 30px;
    margin-top: 0;
}
.post-preview .post-subtitle {
    margin: 0;
    font-weight: 300;
    margin-bottom: 10px;
}
.post-preview .post-meta,
.post-heading .post-meta {
    color: #808080;
        font-size: 18px;
    font-style: italic;
    margin: 0 0 10px;
}
.post-preview .post-entry {
    font-family: 'Open Sans', 'Helvetica Neue', Helvetica, Arial, sans-serif;
}
.post-preview .post-read-more {
    font-weight: 800;
}

@media only screen and (min-width: 768px) {
    .post-preview .post-title {
        font-size: 36px;
    }
}

/* --- Post and page layout --- */
    
    header.header-page {
        margin-bottom: 20px;
    }

header.header-page .page-heading {
    text-align: center;
}

header.header-post .post-heading h1 {
    font-size: 35px;
    margin-top: 0;
}

header.header-page .page-heading h1 {
    font-size: 50px;
    margin-top: 0;
}

header.header-post .post-heading .post-subheading,
header.header-page .page-heading .page-subheading {
    font-size: 24px;
    line-height: 1.1;
    display: block;
    font-family: 'Open Sans', 'Helvetica Neue', Helvetica, Arial, sans-serif;
    font-weight: 300;
    margin: 10px 0 0;
}

header.header-post .post-heading .post-subheading {
    margin-bottom: 20px;
}

@media only screen and (min-width: 768px) {
    header.header-post .post-heading h1 {
        font-size: 50px;
    }
}

@media only screen and (min-width: 768px) {
    header.header-page .page-heading h1 {
        font-size: 80px;
    }
}

.caption {
    text-align: center;
    font-size: 14px;
    padding: 10px;
    font-style: italic;
    margin: 0;
    display: block;
    border-bottom-right-radius: 5px;
    border-bottom-left-radius: 5px;
}

/* --- Pager --- */
    
    .pager li a {
        font-family: 'Open Sans', 'Helvetica Neue', Helvetica, Arial, sans-serif;
        text-transform: uppercase;
        font-size: 14px;
        font-weight: 800;
        letter-spacing: 1px;
        padding: 10px 5px;
        background: #FFF;
            border-radius: 0;
        color: #404040;
    }
@media only screen and (min-width: 768px) {
    .pager li a {
        padding: 15px 25px;
    }
}
.pager li a:hover,
.pager li a:focus {
    color: #FFF;
        background: #0085a1;
        border: 1px solid #0085a1;
}

.pager {
    margin: 10px 0 0;
}

.pager.blog-pager {
    margin-top: 0;
}

@media only screen and (min-width: 768px) {
    .pager.blog-pager  {
        margin-top: 10px;
    }
}

/* --- Tables --- */
    
    .blog-post table {
        padding: 0;
    }
.blog-post table tr {
    border-top: 1px solid #cccccc;
    background-color: #ffffff;
        margin: 0;
    padding: 0;
}
.blog-post table tr:nth-child(2n) {
    background-color: #f8f8f8;
}
.blog-post table tr th {
    font-weight: bold;
    border: 1px solid #cccccc;
    text-align: left;
    margin: 0;
    padding: 6px 13px;
}
.blog-post table tr td {
    border: 1px solid #cccccc;
    text-align: left;
    margin: 0;
    padding: 6px 13px;
}
.blog-post table tr th :first-child,
.blog-post table tr td :first-child {
    margin-top: 0;
}
.blog-post table tr th :last-child,
.blog-post table tr td :last-child {
    margin-bottom: 0;
}

