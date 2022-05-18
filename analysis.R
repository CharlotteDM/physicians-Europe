library("tidyverse")
library("dplyr")
library("shiny")
library("RColorBrewer")
library("rstudioapi")
library("leaflet")

install.packages("rsconnect")
library("rsconnect")
install.packages("shinyWidgets")
library("shinyWidgets")

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

physEurope <- read.csv("Data/physicians.csv", stringsAsFactors = F)
#source of data: https://ec.europa.eu/eurostat/databrowser/view/HLTH_RS_SPEC__custom_2729227/default/table?lang=en

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

#colors
pal_phys <- colorFactor(palette = "Spectral", domain = phys_data[["spec"]])




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
    values = ~pal_phys(phys_data[["spec"]]),
    opacity = .5)

#number of physicians in a given specialization

phys_table <- phys_data %>%
  group_by(spec) %>%
  arrange(-number) %>%
  select(spec, number, year, Country)


#new names of columns
#colnames(phys_table) <- c("spec", "number", "year", "country")



#application interface
my_ui <- fluidPage(
  titlePanel("Physicians by medical specialization"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "analysis_var",
        label = "Specialization",
        choices = phys_data$spec)),
    mainPanel(
      leafletOutput(outputId = "phys_map"),
      tableOutput(outputId = "phys_table"))))

#run app  
server <- function(input, output) {}
shinyApp(ui = my_ui, server = server)  

#popup
popup <- reactive({
  return(phys_data %>% select(input$analysis_var))
})

#refine code
my_server <- function(input, output) {
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
        color = ~pal_phys(phys_data[[input$analysis_var]]),
        fillOpacity = .7,
        radius = 4,
        stroke = F) %>%
      addLegend(
        position = "bottomright",
        title = input$analysis_var,
        pal = pal_phys,
        values = ~phys_data[[input$analysis_var]],
        opacity = .7)
    
  })
  output$phys_table <- renderTable({
    phys_table <- phys_data %>%
      group_by(spec) %>%
      arrange(-number) %>%
      select(spec, number, year, Country) 
      colnames(phys_table) <- c(input$analysis_var, "number", "year", "Country")
      phys_table
  })
}
shinyApp(ui = my_ui, server = my_server)


