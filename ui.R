library("tidyverse")
library("dplyr")
library("shiny")
library("RColorBrewer")
library("rstudioapi")
library("leaflet")
library("htmlwidgets")
library("ggplot2")
library("DT")

#install.packages("rsconnect")
library("rsconnect")
#install.packages("shinyWidgets")
library("shinyWidgets")
#install.packages("shinythemes")
library("shinythemes")
#install.packages("shinydashboard")
#library("shinydashboard")
#install.packages("leaflet.extras")
library("leaflet.extras")




##uncomment to set working directory of RStudio - only for local
#path <- dirname(rstudioapi::getActiveDocumentContext()$path)
#setwd(path)

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

#application interface
ui <- fluidPage(
  titlePanel(p("Physicians by medical specialization")),
  theme = shinytheme("sandstone"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "spec",
        label = "Specialization:",
        choices = unique(phys_data$spec)),
      selectInput(
        inputId = "yr",
        label = "Year:",
        choices = 1985:2020)),
      checkboxInput(inputId = "phys_table", #data table
                    label = "Show data table",
                    value = T)),
  mainPanel(
          textOutput("tabs_title"),
          strong("For more information go to the section:"),
          tabsetPanel(
            tabPanel("Map", leafletOutput("phys_map"))),
          DT::dataTableOutput(outputId = "phys_table")
        ))



mod <- function(input, output, session, legend, prox){
  
  observe({
    prox %>% clearControls()
    if (legend()) {
      prox %>% addLegend(position = "bottomright",
                         pal1 = colorFactor("Set3", phys_data$spec), values = ~spec
      )
    }
  }) }



#code
server <- function (input, output, session) {
  
  #color for spec
  pal1 <- colorFactor(
    palette = "Set3",
    domain = phys_data$spec)
  
  #color for year
  pal2 <- colorFactor(
    palette = "Spectral",
    domain = phys_data$analysis_var_sec)
  
  #creates the map
  output$phys_map <- renderLeaflet({
    pal1 <- colorFactor(palette = "Set3", domain = phys_data$spec)
    leaflet(phys_data) %>% 
      addProviderTiles("Stamen.TonerLite") %>% 
      addCircles(
        data = phys_data,
        lat = ~lat,
        lng = ~long,
        label = ~paste("Medical specialization: ", spec,
                       "number: ", number),
        color = ~pal1(spec),
        fillOpacity = .7,
        radius = 4,
        stroke = F) 
  }
  )
  
  
    proxy <- leafletProxy("phys_map", data = phys_data) %>%
    callModule(mod, "mod", reactive(input$spec), proxy)

  #table - good!
  output$phys_table <- DT::renderDataTable(
    if(input$phys_table) {
      DT::datatable(data = phys_data[, c(5, 7:8, 10)],
                    options = list(pageLength = 10),
                    rownames = F)
    }
  )
  
}



shinyApp(ui = ui, server = server, options = list(height = 800)) 


