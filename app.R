#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#dploy using library(rsconnect) deployApp()

library(shiny)
library(leaflet)
library(tidyverse)
library(deltamapr)
library(sf)
library(ggmap)
library(here)

load(here("Smelt2.RData"))
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
        selectInput("ReleaseEvent","Release Event:",Releases, multiple = TRUE),
        selectInput("Colorby","Color points by",choices = c("LifeStage", "Release Type", "Survey", "Wild/Cultured")),
 radioButtons("Timeslider", "Time slider? Y/N", choices = c("Yes", "No"), selected = "No"), 

conditionalPanel(condition = "input.Timeslider == 'Yes'",
  sliderInput("Month",
                      "Select month",
                      min = 1,  12, 
                      value = 1, step=1, round=F, sep=""
              ),
  sliderInput("Year",
              "Select sampling Year",
              min = min(smelt2$Year),  max=max(smelt2$Year), 
              value =  min(smelt2$Year), step=1, round=F, sep=""
  ))
    ),
        # map
        mainPanel(
          title = "Map",
          "This is a preliminary map of Delta Smelt catches. Data have not undergone quality controls. No garuntees", br(),
          "Please contact Rosemary Hartman (Rosemary.Hartman@water.ca.gov) with questions",
          leafletOutput(outputId = 'smeltmap', height = 800)
        ))
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
  

  
  
   output$smeltmap <- renderLeaflet({
     if(is.null(input$Colorby)){
       col = smelt2$LifeStage
       pal <- colorFactor(
         palette = c("darkred", "yellow", "lightgreen", "orange"),
         domain = smelt2$LifeStage
       )
     } else {
  if(input$Colorby == "LifeStage"){
    col = smelt2$LifeStage
  pal <- colorFactor(
    palette = c("darkred", "yellow", "lightgreen", "orange"),
    domain = smelt2$LifeStage
  )
  } else {
    if(input$Colorby == "Release Type"){
      col = smelt2$ReleaseMethod
      pal <- colorFactor(
        palette = c("darkred", "yellow", "lightgreen", "orange", "blue", "purple"),
        domain = smelt2$ReleaseMethod
      )
    } else {
      if(input$Colorby == "Survey") {
        col = smelt2$Survey
        pal <- colorFactor(
          palette = c("darkred", "yellow", "lightgreen", "orange", "blue", "purple", "tan", "black"),
          domain = smelt2$Survey)
          
      } else {
        col = smelt2$wildcultured
        pal <- colorFactor(
          palette = c("darkred", "yellow"),
          domain = smelt2$wildcultured) 
      }
    }
  }
     }
 
   if(is.null(input$BroodYear)) Years = BYs else Years = input$BroodYear
    if(is.null(input$ReleaseEvent)) ReleaseE = Releases else ReleaseE = input$ReleaseEvent
    if(input$Timeslider == "No") months = c(1:12) else months = input$Month
    if(input$Timeslider == "No") years = c(min(smelt2$Year):max(smelt2$Year)) else years = input$Year
    smeltdat = filter(smelt2, BroodYear %in% Years, ReleaseEvent %in% ReleaseE, Month %in% months, Year %in% years) %>%
      mutate(Label = paste("FL=", ForkLength, "\n", SampleDate))
        # Make a map
        map <- leaflet() %>%
          addProviderTiles(providers$OpenStreetMap) %>%
          addPolygons(data = WW_Delta, weight = .8, color = "grey", opacity =1) %>%
           addLegend(data = smelt2, "bottomright", pal = pal, values = ~col,
                    title = input$Colorby,
                    opacity = 1)
        if(nrow(smeltdat) != 0) {
                  map %>%
         
          addCircleMarkers(data=smeltdat, lng = jitter(smeltdat$LongitudeStart, factor =5), lat = jitter(smeltdat$LatitudeStart, factor =2),
                           label = ~Label, radius =5, opacity =0, fillOpacity = 1,
                           fillColor = ~pal(col))
          } else {
          map
        }
         
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
