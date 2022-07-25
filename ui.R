library("tidyverse")
library("dplyr")
library("shiny")
library("RColorBrewer")
library("rstudioapi")
library("leaflet")
library("htmlwidgets")
library("ggplot2")
library("DT")
#library("rgdal")

#install.packages("rsconnect")
library("rsconnect")
#install.packages("shinyWidgets")
library("shinyWidgets")
#install.packages("shinythemes")
library("shinythemes")
#install.packages("shinydashboard")
library("shinydashboard")
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

#changes names of the specializations
phys_data$spec[phys_data$spec == "GEN"] <- "Generalist Medical Practitioners"
phys_data$spec[phys_data$spec == "GEN_PRAC"] <- "Generalist Practitioners"
phys_data$spec[phys_data$spec == "GEN_OTH"] <- "Generalist Other Medical Practitioners"
phys_data$spec[phys_data$spec == "SPEC"] <- "Specialist Medical Practitioners"
phys_data$spec[phys_data$spec == "PAED"] <- "General Paediatricians"
phys_data$spec[phys_data$spec == "GYN"] <- "Gynaecologists and Obstetricians"
phys_data$spec[phys_data$spec == "PSY"] <- "Psychiatrists"
phys_data$spec[phys_data$spec == "MED"] <- "Medical group of specialists"
phys_data$spec[phys_data$spec == "MED_INT"] <- "Internal Medicine"
phys_data$spec[phys_data$spec == "MED_CAR"] <- "Cardiology"
phys_data$spec[phys_data$spec == "MED_END"] <- "Endocrinology"
phys_data$spec[phys_data$spec == "MED_GAS"] <- "Gastroenterology"
phys_data$spec[phys_data$spec == "MED_RES"] <- "Respiratory Medicine"
phys_data$spec[phys_data$spec == "ONC"] <- "Oncology"
phys_data$spec[phys_data$spec == "MED_IMM"] <- "Immunology"
phys_data$spec[phys_data$spec == "MED_NEU"] <- "Neurology"
phys_data$spec[phys_data$spec == "MED_ORL"] <- "Otorhinolaryngology"
phys_data$spec[phys_data$spec == "MED_RAD"] <- "Radiology"
phys_data$spec[phys_data$spec == "MED_MIC"] <- "Microbiology-bacteriology"
phys_data$spec[phys_data$spec == "MED_HAE"] <- "Haematology"
phys_data$spec[phys_data$spec == "MED_DER"] <- "Dermatology"
phys_data$spec[phys_data$spec == "MED_PAT"] <- "Pathology"
phys_data$spec[phys_data$spec == "MED_OCC"] <- "Occupational Medicine"
phys_data$spec[phys_data$spec == "SURG"] <- "Surgical group of specialists"
phys_data$spec[phys_data$spec == "SURG_GEN"] <- "General Surgery"
phys_data$spec[phys_data$spec == "SURG_NEU"] <- "Neurological Surgery"
phys_data$spec[phys_data$spec == "SURG_PLA"] <- "Plastic Surgery"
phys_data$spec[phys_data$spec == "SURG_OPH"] <- "Opthamology"
phys_data$spec[phys_data$spec == "SURG_ORT"] <- "Orthopedics"
phys_data$spec[phys_data$spec == "SURG_THO"] <- "Thoracic Surgery"
phys_data$spec[phys_data$spec == "SURG_VAS"] <- "Vascular Surgery"
phys_data$spec[phys_data$spec == "SURG_ANE"] <- "Anesthesiology and Intensive Care"
phys_data$spec[phys_data$spec == "SURG_URO"] <- "Urology"
phys_data$spec[phys_data$spec == "SURG_EME"] <- "Accident and Emergency Medicine"
phys_data$spec[phys_data$spec == "OTH"] <- "Other Specialist not elsewhere classified"



#removes rows with NA
phys_data <- na.omit(phys_data)

#colors
pal_phys <- colorFactor(palette = "Set3", domain = phys_data[["spec"]])
pal_year <- colorFactor(palette = "Spectral", domain = phys_data[["year"]])
pal_country <- colorFactor(palette = "Accent", domain = phys_data[["Country"]])
#pal_phys <- colorRampPalette(brewer.pal(9,"YlOrRd"))


#application interface
ui <- fluidPage(
  titlePanel(p("Physicians by medical specialization 1985-2020")),
  theme = shinytheme("sandstone"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "spec",
        label = "Specialization:",
        choices = unique(phys_data$spec),
        selected = "GEN"),
      selectInput(
        inputId = "year",
        label = "Year:",
        choices = 1985:2020,
        selected = 2020)
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

   ###filtered_data
   filteredData <- reactive({ 
     filter(phys_data, phys_data$spec == input$spec, 
            phys_data$year == input$year) 
   })
   


   
   
   output$phys_map <- renderLeaflet({
     leaflet() %>%
       addProviderTiles("Stamen.TonerLite") %>%
       setView(lng = 9.0000,
               lat = 53.0000, zoom = 3) %>%
       addCircleMarkers(data = filteredData(),
                        lat = ~lat, 
                        lng = ~long, 
                        label = ~paste("</br>Specialization:", spec,
                                       "</br>Number of physicians:", number, 
                                       "</br>Country: ", Country, 
                                       "</br>Year:", year),
                        labelOptions = labelOptions(style = list(
                                                      "color" = "navy",
                                                      "font-family" = "serif",
                                                      "font-style" = "italic",
                                                      "font-size" = "12px")),
                        fillOpacity = .7,
                      
                        radius = 8,
                        stroke = F)
   })

   
}


shinyApp(ui, server, options = list(height = 800))





