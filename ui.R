library("tidyverse")
library("dplyr")
library("shiny")
library("RColorBrewer")
library("rstudioapi")
library("leaflet")
library("htmlwidgets")
library("ggplot2")

#install.packages("rsconnect")
library("rsconnect")
#install.packages("shinyWidgets")
library("shinyWidgets")
#install.packages("shinythemes")
library("shinythemes")
#install.packages("shinydashboard")
#library("shinydashboard")

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
  titlePanel("Physicians by medical specialization"),
  theme = shinytheme("sandstone"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "analysis_var",
        label = "Specialization:",
        choices = unique(phys_data$spec)),
      selectInput(
        inputId = "analysis_var",
        label = "Year:",
        choices = 1985:2020),
      selectInput(
        inputId = "analysis_var",
        label = "Country:",
        choices = unique(phys_data$Country),
      )),
        mainPanel(
          textOutput("tabs_title"),
          strong("For more information go to the section:"),
          tabsetPanel(
            tabPanel("Map", leafletOutput("phys_map")), 
            tabPanel("Table", tableOutput("phys_table")),
            tabPanel("Plot", tableOutput("phys_plot"))
          )
        )
      )
    )

#phys_table
phys_table <- phys_data %>%
  group_by(spec) %>%
  arrange(-number) %>%
  select(spec, number, year, Country)


    
#refine code
server <- function (input, output) {
  output$phys_map <- renderLeaflet({
    pal_phys <- colorFactor(
      palette = "Set3",
      domain = phys_data[[input$analysis_var]]
    )
    leaflet(data = phys_data) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addCircleMarkers(
        lat = ~lat,
        lng = ~long,
        label = ~paste("Medical specialization: ", spec,
                       "number: ", number),
        color = ~pal_phys(phys_data[[input$analysis_var]]),
        fillOpacity = .7,
        radius = 4,
        stroke = F) %>%
      addLegend(
        position = "bottomright",
        title = input$analysis_var,
        pal = pal_phys,
        values = ~phys_data[[input$analysis_var]],
        opacity = .5)
  })
  phys_data_filtered <- reactive({
    phys_data[phys_data$year %in% input$analysis_var, ]
  })  
  phys_data_filtered <- reactive({
    phys_data[phys_data$year %in% input$analysis_var, ]
  })

  observe({
    leafletProxy(mapId = "phys_map", data = phys_data_filtered()) %>%
      clearMarkers() %>%  
      addMarkers()
  })
  
  output$phys_table <- renderTable({
    table <- phys_table %>%
      group_by(phys_table[[input$analysis_var]]) %>% 
      count %>%
      arrange(-n) 
    colnames(table) <- c(input$analysis_var, "Number")
    table
  })
  output$phys_plot <- renderPlot({
    ggplot (data = phys_data, (aes(x = input$analysis_var, y = input$analysis_var, color = Country))) +
      geom_point()  +
      labs(
        title = "hhhh",
        caption = "(based on data from: https://ec.europa.eu/eurostat/databrowser/view/HLTH_RS_SPEC__custom_2747500/default/table?lang=en",
        x = "kkk",
        y = "ooo") +
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


shinyApp(ui = ui, server = server, options = list(height = 800)) 


