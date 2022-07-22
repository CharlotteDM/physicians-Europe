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
# phys_table <- phys_data %>%
#   group_by(spec) %>%
#   arrange(-number) 


#application interface
ui <- fluidPage(
  titlePanel(p("Physicians by medical specialization 1985-2020")),
  theme = shinytheme("sandstone"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "spec",
        label = "Specialization:",
        choices = unique(phys_data$spec)),
      selectInput(
        inputId = "year",
        label = "Year:",
        choices = 1985:2020)
      ),
  mainPanel(
          textOutput("tabs_title"),
          strong("For more information go to the section:"),
          tabsetPanel(
          tabPanel("Map", leafletOutput("phys_map")),
          tabPanel("Chart", plotOutput("phys_chart")),
          tabPanel("Table", tableOutput("phys_table")))
      )
  )
)


#code
server <- function (input, output) {
  
  #Radkowe:
  # filteredData <- reactive({
  #   phys_data[phys_data$spec == input$spec[1],]
  #   #phys_data[phys_data$year == input$year[1],]
  # })
  
  
  
  #moje:
  # filtered_data <- reactive({
  #   phys_data %>%
  #     filter(
  #            Specialization %in% input$spec[1],
  #            Year %in% input$year[1]
  #     )
  # })
  
  
  

  
  #render map
  #Radkowe:
  # output$phys_map <- renderLeaflet({
  # 
  #    pal_phys <- colorFactor(
  #    palette = "Set3",
  #    domain = input$spec
  #    )
  # 
  #   leaflet(data = phys_data()) %>%
  #   addProviderTiles("Stamen.TonerLite") %>%
  #   addCircleMarkers(
  #   lat = ~lat,
  #   lng = ~long,
  #   label = ~paste("Specialization: ", spec,
  #                  "Number of physicians: ", number,
  #                  "Country: ", Country,
  #                  "Year:", year),
  #   color = ~pal_phys(input$spec),
  #   fillOpacity = .7,
  #   radius = 4,
  #   stroke = F) %>%
  # addLegend(
  #   position = "bottomright",
  #   title = input$spec,
  #   pal = pal_phys,
  #   values = input$spec,
  #   opacity = .5)
  #    })
  
  
  #Moje
  # pal_phys <- colorFactor(
  #   palette = "Set3",
  #   domain = input$spec
  #   )
  
  #moje
  # output$phys_map <- renderLeaflet({
  #   leaflet(data = phys_data()) %>%
  #   addProviderTiles("Stamen.TonerLite")
  # })
  
  
  #Moje
  # observe({leafletProxy("phys_map") %>%
  #           addCircles(data = filteredData(),
  #                      lng = ~long, lat = ~lat,
  #                      color = pal_phys,
  #                      label = ~paste("Specialization: ", spec,
  #                                     "Number of physicians: ", number,
  #                                     "Country: ", Country,
  #                                     "Year:", year)
  # 
  #                      )})
  
  
  #Radkowe:
  # observe({
  #   leafletProxy("phys_map") %>%
  #     addCircles(data = filteredData(),lng = ~long, lat = ~lat)
  # 
  # })
  
  
  #Mopje
  # #render table
  # output$phys_table <- renderTable({
  # 
  #   ]}
  # #render map
  # output$phys_chart <- renderPlot({
  # 
  #   ]}
  
  
  #Radkowe:  
 #  output$tabs_title <- renderText({ 
 #    txt <- input$spec
 #    print(txt)
 #  })
 # 
 # }
  
  output$phys_map <- renderLeaflet({
    
    #Set basemap
    leaflet(phys_data) %>% 
      addProviderTiles("Stamen.TonerLite") 
  })
  
    #Select Spec
    selected_spec <- reactive({
    phys_data[phys_data$spec %in% input$spec, ] 
  })
  
  observe({
    state_popup <- paste0("<strong> Spec: </strong>", 
                          selected_spec()$spec,
                          "<br><strong> Year: </strong>",
                          selected_year()$year)
    
    #make map
    leafletProxy("phys_map", data = selected_spec()) %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addMarkers(~long, ~lat, clusterOptions = markerClusterOptions()) 
  })
  
  #Select year
  selected_year <- reactive({
    phys_data <- phys_data[!is.na(phys_data$year), ]
    phys_data[phys_data$year %in% input$year, ]
  })
  
  observe({
    state_popup <- paste0("<strong> Spec: </strong>",
                          selected_spec()$spec,
                          "<br><strong> Year: </strong>",
                          selected_year()$year)
    #make map
    leafletProxy("phys_map", data = selected_year()) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addMarkers(~long, ~lat, clusterOptions = markerClusterOptions())
  })
}

  

shinyApp(ui, server, options = list(height = 800))
