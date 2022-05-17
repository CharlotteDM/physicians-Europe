library("tidyverse")
library("dplyr")
library("shiny")
library("RColorBrewer")
library("rstudioapi")
library("plotly")
library("ggplot2")
library("leaflet")

install.packages("rsconnect")
library("rsconnect")

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

physEurope <- read.csv("Data/physicians.csv", stringsAsFactors = F)
#source of data: https://ec.europa.eu/eurostat/databrowser/view/HLTH_RS_SPEC__custom_2729227/default/table?lang=en

long_lat <- read.csv("Data/long_lat.csv", stringsAsFactors = F)
#source of data: https://gist.github.com/metal3d/5b925077e66194551df949de64e910f6

#changes name of the column
names(long_lat)[names(long_lat) == 'Alpha.2.code'] <- 'geo'

long_lat$geo <- as.character(long_lat$geo)
physEurope$geo <- as.character(physEurope$geo)

#joins data frames
phys_data <- left_join(physEurope, long_lat, by = "geo")

#changes names of the columns
names(phys_data)[names(phys_data) == 'Latitude..average.'] <- 'lat'
names(phys_data)[names(phys_data) == 'Longitude..average.'] <- 'long'

#colors
pal_phys <- colorFactor(palette = "Paired", domain = phys_data[["med_spec"]])

#creates map
leaflet(data = phys_data) %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(
    lat = ~lat,
    long = ~long,
    label = ~pasteO(med-spec, ",", number),
    color = 
  )