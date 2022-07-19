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
  arrange(-number) %>%
  select(year, number, Country)


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
        label = "Country",
        choices = unique(phys_data$Country)),
      checkboxInput(inputId = "phys_table", #data table
                    label = "Data table",
                    value = T)),
  mainPanel(
          textOutput("tabs_title"),
          strong("For more information go to the section:"),
          tabsetPanel(
          tabPanel("Map", leafletOutput("phys_map")),
          tabPanel("Chart", plotOutput("phys_chart")),
          tabPanel("Table", tableOutput("phys_table")))
        )))
#code
server <- function (input, output, session) {
  
  #color for spec
  pal1 <- colorFactor(
    palette = "Set3",
    domain = phys_data$var1)
  
  #color for year
  pal2 <- colorFactor(
    palette = "Spectral",
   domain = phys_data$var2)
  
  #creates the map
 output$phys_map <- renderLeaflet({
   leaflet(phys_data) %>%
     addTiles() %>%
     addCircles(
       lat = ~lat,
       lng = ~long,
       label = ~paste("Specialization: ", spec,
                      "Number of physicians: ", number,
                      "Country: ", Country,
                      "Year:", year),
       color = ~pal1(phys_data[[input$var1]]),
       fillOpacity = .7,
       radius = 4,
       stroke = F) %>%
   addLegend(
     position = "bottomright",
     title = input$var1,
     pal = pal1,
     values = ~phys_data[[input$var1]],
     opacity = .5)
 })
output$phys_table <- renderTable({
  table <- phys_table %>%
    group_by(phys_table[[input$var1]]) %>% 
    count %>%
    arrange(-n) 
  colnames(table) <- c(input$var1, "Specialization")
  table
})
output$my_plot <- renderPlot({
  ggplot (data = phys_data) +
    geom_point(mapping = aes(x = Country, y = number)) +
    facet_wrap(~ spec, nrow = 3)
    labs(
      title = "Physician's pecialization in Europe",
      caption = "(based on data from: https://ec.europa.eu/eurostat/databrowser/view/HLTH_RS_SPEC__custom_2747500/default/table?lang=en",
      x = "Country",
      y = "Number of physicians") +
    theme(
      plot.title = element_text(color="royalblue4", size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
      axis.title.y = element_text(color="steelblue2", size=14, face="bold"),
      plot.caption.position = "plot",
      legend.position = "none")
  
})
output$tabs_title <- renderText({ 
  "data source: https://ec.europa.eu/eurostat/databrowser/view/HLTH_RS_SPEC__custom_2747500/default/table?lang=en"
})
}


shinyApp(ui, server)
