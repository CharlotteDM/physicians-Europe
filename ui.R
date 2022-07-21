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
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

physEurope <- read.csv("Data/physicians.csv", stringsAsFactors = F)
#source of data: https://ec.europa.eu/eurostat/databrowser/view/HLTH_RS_SPEC__custom_2747500/default/table?lang=en


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


#table
phys_table <- phys_data %>%
  group_by(spec) %>%
  arrange(-number) 


#application interface
ui <- fluidPage(
  titlePanel(p("Physicians by medical specialization 1985-2020")),
  theme = shinytheme("sandstone"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "var1",
        label = "Specialization:",
        choices = unique(phys_data$spec)),
      selectInput(
        inputId = "var2",
        label = "Year:",
        choices = 1985:2020),
      selectInput(
        inputId = "var3",
        label = "Country:",
        choices = unique(phys_data$Country))),
  mainPanel(
          textOutput("tabs_title"),
          strong("For more information go to the section:"),
          tabsetPanel(
          tabPanel("Map", leafletOutput("phys_map", width = 800, height = 500)),
          tabPanel("Chart", plotOutput("phys_chart")),
          tabPanel("Table", tableOutput("phys_table")))
        )))


#code
server <- function (input, output, session) {
  
  #reactive table

  values <- reactiveValues()
  values$phys_data <- data.frame(phys_data$spec, phys_data$year, phys_data$Country)
  newEntry <- observe({
    if(input$var1 > 0) {
      newLine <- isolate(input$var1)
      isolate(values$phys_data <- rbind(values$phys_data, newLine))
    }
    if(input$var2 >= 1985) {
      newLine2 <- isolate(input$var2)
      isolate(values$phys_data <- rbind(values$phys_data, newLine2))
    }
  })
  
  output$phys_table <- renderTable({values$phys_data})
  
  

  
  }
  
  

#output$tabs_title <- renderText({ 
  #"data source: https://ec.europa.eu/eurostat/databrowser/view/HLTH_RS_SPEC__custom_2747500/default/table?lang=en"
#})



shinyApp(ui, server)
