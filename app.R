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
          "This is a preliminary map of Delta Smelt catches. Data have not undergone quality controls.", br(),
          "No guarantees. In particular, larval smelt from brood year 2024 have not undergone genetic ID and some may be wakasagi", br(),
          "Please contact Rosemary Hartman (Rosemary.Hartman@water.ca.gov) with questions",
          leafletOutput(outputId = 'smeltmap', height = 600),
          "*Note that locations are not precise, points have been moved slightly to avoid overlapping symbols",
          plotOutput(outputId = "salvage")

        ))
    )


# Define server logic required to draw a map and bar graph
server <- function(input, output) {
  

  
  
   output$smeltmap <- renderLeaflet({
       if(is.null(input$BroodYear)) Years = BYs else Years = input$BroodYear
    if(is.null(input$ReleaseEvent)) ReleaseE = Releases else ReleaseE = input$ReleaseEvent
    if(input$Timeslider == "No") months = c(1:12) else months = input$Month
    if(input$Timeslider == "No") years = c(min(smelt2$Year):max(smelt2$Year)) else years = input$Year
    smeltdat = filter(smelt2, BroodYear %in% Years, ReleaseEvent %in% ReleaseE, Month %in% months, Year %in% years) %>%
      mutate(Label = paste("FL=", ForkLength, "\n", SampleDate))
    
    
    if(is.null(input$Colorby)){
      col = smeltdat$LifeStage
      pal <- colorFactor(
        palette = c("darkred", "yellow", "lightgreen", "orange"),
        levels =  sort(unique(smelt2$LifeStage))
      )
    } else {
      if(input$Colorby == "LifeStage"){
        col = smeltdat$LifeStage
        pal <- colorFactor(
          palette = c("darkred", "yellow", "lightgreen"),
          levels = sort(unique(smelt2$LifeStage))
        )
      } else {
        if(input$Colorby == "Release Type"){
          col = smeltdat$ReleaseMethod
          pal <- colorFactor(
            palette = c("darkred", "yellow", "lightgreen", "orange", "blue", "purple", "tan", "cyan", "black"),
            levels = sort(unique(smelt2$ReleaseMethod))
          )
        } else {
          if(input$Colorby == "Survey") {
            col = smeltdat$Survey
            pal <- colorFactor(
              palette = c("darkred", "yellow", "lightgreen", "orange", "blue", "purple", "tan", "cyan", "black","red", "darkgreen"),
              levels = sort(unique(smelt2$Survey)))
            
          } else {
            col = smeltdat$wildcultured
            pal <- colorFactor(
              palette = c("darkred", "yellow"),
              levels = c("Cultured", "Unmarked") )
          }
        }
      }
    }
    
    
    
    
         # Make a map
        map <- leaflet() %>%
          addProviderTiles(providers$OpenStreetMap) %>%
          addPolygons(data = WW_Delta, weight = .8, color = "grey", opacity =1) %>%
           addLegend(data = smelt2, "bottomright", pal = pal, values = ~col,
                    title = input$Colorby,
                    opacity = 1)
        if(nrow(smeltdat) != 0) {
                  map %>%
         
          addCircleMarkers(data=smeltdat, lng = jitter(smeltdat$LongitudeStart, factor =5), 
                           lat = jitter(smeltdat$LatitudeStart, factor =2),
                           label = ~Label, radius =5, opacity =1, weight =1,
                           fillOpacity = 1,
                           fillColor = ~pal(col), stroke = TRUE, color = "black")
          } else {
          map
        }
         
        
        
    })
   
   output$salvage <- renderPlot({

     
     if(is.null(input$BroodYear)) Years = BYs else Years = input$BroodYear
     if(is.null(input$ReleaseEvent)) ReleaseE = Releases else ReleaseE = input$ReleaseEvent
     if(input$Timeslider == "No") months = c(1:12) else months = input$Month
     if(input$Timeslider == "No") years = c(min(smelt2$Year):max(smelt2$Year)) else years = input$Year
     
     #filter data of interest
     smeltdatsal = filter(smelt2, 
                          BroodYear %in% Years, 
                       ReleaseEvent %in% ReleaseE, 
                       Month %in% months, 
                       Year %in% years) %>%
       mutate(Nfish = 1)
     
     #join with months and years so there is a entry for each month and year
     yearmonths = data.frame(Year = c(min(smelt2$Year):max(smelt2$Year))) %>%
       merge(data.frame(Month = 1:12))
     
     smeltdatsal2 = full_join(smeltdatsal, yearmonths) %>%
       mutate(Date = case_when(is.na(SampleDate)~ ymd(paste(Year, Month, 5)),
                               TRUE ~ SampleDate),
              shortdate = strftime(Date, "%Y-%m"),
              Nfish = case_when(is.na(Nfish) ~ 0,
                                TRUE ~ Nfish)) %>%
       filter(Date > ymd("2021-11-01"), Date < today())
     smeltdatave = group_by(smeltdatsal2, shortdate, LifeStage, Survey, wildcultured, ReleaseMethod) %>%
       summarize(Nfish = sum(Nfish, na.rm =T))
     
     #set up colors - there is probably a better way
     if(is.null(input$Colorby)){
       col = factor(smeltdatave$LifeStage, levels = c("Adult", "Juvenile", "Larva"))
     } else {
       if(input$Colorby == "LifeStage"){
         col = factor(smeltdatave$LifeStage, levels = c("Adult", "Juvenile", "Larva"))
       } else {
         if(input$Colorby == "Release Type"){
           col = smeltdatave$ReleaseMethod
           
         } else {
           if(input$Colorby == "Survey") {
             col = factor(smeltdatave$Survey, levels =c("CDFW 20-mm","CDFW Bay Study",  "CDFW SKT","CDFW SLS",        "CDFW Townet",    
                                                       "DJFMP","FCCL Broodstock", "Skinner","TFCF",    "USFWS Chipps",   
                                                        "USFWS EDSM"))
             
           } else {
             col = factor(smeltdatave$wildcultured, levels = c("Cultured", "Unmarked"))
           }
         }
       }
     }
     
     # Make a bar graph

   ggplot(smeltdatave, aes(x = shortdate, fill = col, y = Nfish))+ geom_col()+ylab("Number of Fish")+
  xlab("Year and Month")+ theme_bw()+scale_y_continuous(breaks = seq(0,nrow(smeltdatsal2), by =2))+
     scale_fill_manual(values = c("darkred", "yellow", "lightgreen", "orange", "blue", "purple", "tan", "cyan", "black", "red", "darkgreen"),
                       name = NULL)+
     ggtitle("Catch over time")+ theme(axis.text.x = element_text(angle = 90))
     
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
