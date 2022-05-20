library("tidyverse")
library("dplyr")
library("shiny")
library("RColorBrewer")
library("rstudioapi")
library("leaflet")
library("htmlwidgets")

install.packages("rsconnect")
library("rsconnect")
install.packages("shinyWidgets")
library("shinyWidgets")

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

physEurope <- read.csv("Data/physicians.csv", stringsAsFactors = F)
#source of data: https://ec.europa.eu/eurostat/databrowser/view/HLTH_RS_SPEC__custom_2747500/default/table?lang=en
#physEurope2019 <- filter(physEurope, TIME_PERIOD == 2019)

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
names(phys_data)[names(phys_data) == 'TIME_PERIOD'] <- 'year'
names(phys_data)[names(phys_data) == 'OBS_VALUE'] <- 'number'
names(phys_data)[names(phys_data) == 'med_spec'] <- 'spec'

#removes rows with NA
phys_data <- na.omit(phys_data)

#colors
pal_phys <- colorFactor(palette = "Set3", domain = phys_data[["spec"]])
pal_year <- colorFactor(palette = "Spectral", domain = phys_data[["year"]])
pal_country <- colorFactor(palette = "Accent", domain = phys_data[["Country"]])
#pal_phys <- colorRampPalette(brewer.pal(9,"YlOrRd"))

#creates map
phys_map <- leaflet(data = phys_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lat = ~lat,
    lng = ~long,
    label = ~paste0(spec, ", ", number, ", ", Country, ", ", year),
    color = ~pal_phys(phys_data[["spec"]]),
    fillOpacity = .7,
    radius = 4,
    stroke = F) %>%
  addLegend(
    position = "bottomright",
    title = "Medical specialization",
    pal = pal_phys,
    values = ~spec,
    opacity = .5)


#number of physicians in a given specialization

phys_table <- phys_data %>%
  group_by(spec) %>%
  arrange(-number) %>%
  select(spec, number, year, Country)




#application interface
my_ui <- fluidPage(
  titlePanel("Physicians by medical specialization"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "spec",
        label = "Specialization:",
        choices = unique(phys_data$spec))),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "year",
          label = "Select year:",
          choices = unique(phys_data$year))), 
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "Country",
            label = "Select country:",
            choices = unique(phys_data$Country))), 
    mainPanel(
      leafletOutput(outputId = "phys_map"))))))




#run app
server <- function(input, output, sessions) {}
shinyApp(ui = my_ui, server = server)  



#refine code
my_server <- function(input, output, session) {
  output$phys_map <- renderLeaflet({
    pal_phys <- colorFactor(
      palette = "Spectral",
      domain = phys_data[[input$analysis_var]]
    )
    leaflet(data = phys_data) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addCircleMarkers(
        lat = ~lat,
        lng = ~long,
        label = ~paste0(spec, ", ", number, ", ", Country, ", ", year),
        color = ~pal_phys(phys_data[["spec"]]),
        fillOpacity = .7,
        radius = 4,
        stroke = F) %>%
      addLegend(
        position = "bottomright",
        title = "Specializations",
        pal = pal_phys,
        values = ~spec,
        opacity = .7)})
    output$phys_plot <- renderPlot({
      plot(phys_data()$year, phys_data()$Country, col = phys_data()$spec)})
 }


shinyApp(ui = my_ui, server = my_server) 




