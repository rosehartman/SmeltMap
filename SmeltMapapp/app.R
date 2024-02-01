#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(deltamapr)
library(sf)
library(ggmap)

load("Smelt2.RData")
BYs <- unique(smelt2$BroodYear)
Releases = unique(smelt2$ReleaseEvent)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Smelt Catch Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("BroodYear","Brood Year:",BYs, multiple = TRUE),
        selectInput("ReleaseEvent","Release Event:",Releases, multiple = TRUE)
    ),
        # map
        mainPanel(
          leafletOutput(outputId = 'smeltmap', height = 800)
        )
    ))


# Define server logic required to draw a histogram
server <- function(input, output) {

  
    output$smeltmap <- renderLeaflet({
   if(is.null(input$BroodYear)) Years = BYs else Years = input$BroodYear
    if(is.null(input$ReleaseEvent)) ReleaseE = Releases else ReleaseE = input$ReleaseEvent
    smeltdat = filter(smelt2, BroodYear %in% Years, ReleaseEvent %in% ReleaseE) %>%
      mutate(Label = paste("FL=", ForkLength, "\n", SampleDate))
        # Make a map
        map <- leaflet() %>%
          addProviderTiles(providers$OpenStreetMap)
        map %>%
          addPolygons(data = WW_Delta, weight = .8, color = "grey", opacity =1) %>%
          addCircleMarkers(data=smeltdat, lng = smeltdat$LongitudeStart, lat = smeltdat$LatitudeStart,
                           label = ~Label, radius =5, opacity =0, fillOpacity = .8,
                           fillColor = ifelse(test = smelt2$LifeStage == "Adult",  # if this...
                                              yes = "red",  
                                              no = ifelse(
                                                test = smelt2$LifeStage == "Juvenile",  
                                                yes = "blue",  
                                                no = "yellow"  
                                              )))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
