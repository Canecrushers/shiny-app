library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(data.table)
library(DT)
library(shinythemes)
library(leaflet)
library(geojsonio)
library(sp)
library(scales)


features <- fread("./data/features.csv")
features$date <- ymd(features$date)

points <- fread("./data/points.csv")
mask <- fread("./data/mask.csv")
points = points %>% left_join(mask, by = c("x", "y"))
points = points %>% filter(sugarcane==TRUE,parcel_typ=="Lot Type Parcel") %>% select("x","y","lotplan","segpar")



ui <- navbarPage("Cane Crushers", theme = shinytheme("spacelab"),id= "nav",
                  tabPanel("Map View",
                           mainPanel(width=12, column(4,selectInput("Area", h3("Select a Region"),choices = c("Pindi Pindi","Yalboroo"), 
                                                          selected = "Pindi Pindi"),offset = 0),
                                     column(4,h3("Select a property lot"),offset = 2),
                                    column(leafletOutput("mapPlot",height = "520px"),width = 12))
                           
                  ),
                  tabPanel("Descriptive View",value = "desc",
                            sidebarLayout(
                              sidebarPanel(width = 4,
                                           
                                           br(),
                                           
                                           selectInput("Address", "Select Address"
                                                       ,choices = NULL
                                                       ,selected = NULL),

                                           
                                           br(),
                                           
                                           sliderInput("daterange", "Date Range",
                                                       min    = ymd("2016-12-22"),
                                                       max    = ymd("2019-08-09"),
                                                       value = c(ymd("2016-12-22"),ymd("2019-08-09")),
                                                       step = 10,timeFormat="%d %b %y",ticks = F),
                                           
                                           br()
                                           
                                           
                                           
                              ),
                              
                              
                              mainPanel(width = 8,
                                        
                                        
                                        tabsetPanel( type= "tabs",
                                                     tabPanel("Description",br(),
                                                              column(6,verbatimTextOutput("dollar_made"),align = "center"),
                                                              column(6,verbatimTextOutput("harvested_tonnes"),align = "center"),
                                                              column(6,verbatimTextOutput("N_value"),align = "center"),
                                                              column(6,verbatimTextOutput("water"),align = "center"),
                                                              column(12,plotlyOutput("harvestPlot",height = "250px")),
                                                              br(),
                                                              column(12,plotlyOutput("dollarPlot",height = "250px"))
                                                              ),
                                                     
                                                     
                                                     
                                                     tabPanel("Leaderboard",br(),tags$head(
                                                       tags$style(
                                                         HTML(
                                                           "#DataTables_Table_0_length {
                                                                float: right
                                                              }
                                                              #DataTables_Table_0_filter {
                                                                float: left
                                                              }
                                                              #DataTables_Table_0_paginate {
                                                              float: none;
                                                              text-align: center;
                                                              }
                                                              "
                                                         ))), DTOutput('leaderboard'))
                                                     
                                                     
                                                     
                                                     
                                        )
                              )
                            
                   )
                  
                  )
                
)

server <- function(input, output,session) {
  
  
  # Address Geojson
  
  points_data <- reactive({
    points_data <- geojson_read(paste0("./data/lots/Property_address_Queensland_",str_remove(input$Area," "),".geojson"),what= "sp")
    points_data <- subset(points_data,LOTPLAN %in% points$lotplan)
    points_data
  })
  
  # Lot to Address mapping
  
  lot_to_address <- reactive({
    lot_to_address <- data.frame(lotplan = points_data()$LOTPLAN, address = paste0("Lot ",points_data()$LOT,", ",points_data()$ADDRESS),stringsAsFactors = F)
    lot_to_address
  })
    
    
  # Dataframe containing features per date and lot
  
  timeline_summary <- reactive({
    feature_subset <- features %>% filter(date >= input$daterange[1],date <= input$daterange[2])
    feature_subset <- feature_subset %>% filter(lotplan %in% lot_to_address()$lotplan)
    feature_subset <- feature_subset %>% left_join(lot_to_address(),by ="lotplan")
    feature_subset <- feature_subset %>% group_by(address,date) %>% summarise(harvested_tonnes = sum(harvested_tonnes),sugar_made = sum(sugar_made),dollar_made=sum(dollar_made),min_N = mean(min_N,na.rm = T),max_N = mean(max_N,na.rm = T),leaf_water_content = mean(leaf_water_content,na.rm = T))
    feature_subset
  })
  
  # Subset containing features per lot
  
  lot_summary <- reactive({
    lot_summary <- timeline_summary() %>% group_by(address) %>% summarise(harvested_tonnes = sum(harvested_tonnes),sugar_made = sum(sugar_made),dollar_made=sum(dollar_made),min_N = mean(min_N,na.rm = T),max_N = mean(max_N,na.rm = T),leaf_water_content = mean(leaf_water_content,na.rm = T))
    lot_summary$timeline <- paste(format(input$daterange[1],"%d %b %y"), "-", format(input$daterange[2],"%d %b %y"))
    lot_summary %>% data.frame
  })
  
 
  # Lot boundaries for map
  
  polygon_data <- reactive({
    polygon_data <- geojson_read(paste0("./data/lots/Cadastral_data_QLD_CADASTRE_DCDB_",str_remove(input$Area," "),".geojson"),what= "sp")
    polygon_data <- subset(polygon_data, LOTPLAN %in% points_data()$LOTPLAN & is.na(FEAT_NAME)==T & LOTPLAN %in% points$lotplan & SEGPAR %in% points$segpar & is.na(LOTPLAN)==F & is.na(SEGPAR)==F)
    polygon_data
  })
  
  

  
  # Update Address list based on area
  
 observe({
    choices <- paste0("Lot ",points_data()$LOT,", ",points_data()$ADDRESS)
    updateSelectInput(session, "Address",
                      choices = choices,
                      selected = NULL)
   })
 
 
 # Create map plot
 
  output$mapPlot <- renderLeaflet({
      map <- leaflet(polygon_data()) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(layerId = ~LOTPLAN,fillOpacity = 1, weight = 2, smoothFactor = 0.5,opacity = 1.0,fillColor = "green",highlightOptions = highlightOptions(fillColor = "yellow"),label= paste0("Lot ",polygon_data()$LOT,", ",polygon_data()$ADDRESS))
      map
  })
  
  # Update selected address based on map click

  observeEvent(input$mapPlot_shape_click, {
    click <- input$mapPlot_shape_click
    address_filter <- subset(lot_to_address(),lotplan==click$id)
    selection = address_filter$address[1]

    
    updateSelectInput(session, "Address",
                      selected = selection)
    
  })

 
  # Subset containing features per lot for the selected lot
  
  descriptive_summary <- reactive({
   lot_summary() %>% filter(address == input$Address) %>% data.frame
  })

  # Subset containing features per date and lot for the selected lot
  
  descriptive_timeline <- reactive({
    descriptive_timeline <- timeline_summary() %>% filter(address == input$Address) %>% data.frame
    descriptive_timeline$date <- as.Date(descriptive_timeline$date,"%d-%m-%Y")
    descriptive_timeline
  })
  
  # Text outputs for description page
  
  output$dollar_made <- renderPrint({
    cat("Estimated Revenue: $",round(descriptive_summary()$dollar_made[1],2))
  })
  
  output$harvested_tonnes <- renderPrint({
  cat("Amount of Sugarcane Harvested:", round(descriptive_summary()$harvested_tonnes[1],2),"Tonnes")
  })
  
  output$N_value <- renderPrint({
    if(descriptive_summary()$min_N[1] < 110){usage = "Low fertilizer uasge"}
    else if(descriptive_summary()$min_N[1] < 120){usage = "Optimal fertilizer uasge"}
    else{usage = "Extreme fertilizer uasge"}
    cat("Nitrogen Content:", round(descriptive_summary()$min_N[1],0),"-",round(descriptive_summary()$max_N[1],0),"KgN/Ha,",usage)
  })

  
  output$water <- renderPrint({
    if(descriptive_summary()$leaf_water_content[1] < 0.62){content = "Water content in the cane lower than expected"}
    else{content = "Optimal water content in the cane"}
    cat(content)
  })
  
  # Plots for description page
  
  output$harvestPlot <- renderPlotly({
    plot = ggplot(data = descriptive_timeline(),aes(x=date,y=harvested_tonnes, group=1 )) + geom_point(colour = "blue") + geom_line(colour = "blue") + scale_x_date(date_breaks =  "100 days",date_labels = "%d-%b-%Y") + xlab("Date") + ylab("Sugarcane harvested (Tonnes)")
    ggplotly(plot)
  })
  
  output$dollarPlot <- renderPlotly({
    plot = ggplot(data = descriptive_timeline(),aes(x=date,y=dollar_made, group=1)) + geom_point(colour = "green") + geom_line(colour = "green") + scale_x_date(date_breaks =  "100 days",date_labels = "%d-%b-%Y") + scale_y_continuous(labels = dollar) + xlab("Date") + ylab("Estimated Revenue")
    ggplotly(plot)
  })
  
  # Leaderboard table
  
  output$leaderboard <- renderDT({
    leaderboard <- lot_summary() %>% select("address","harvested_tonnes","dollar_made") %>% data.frame
    leaderboard$rank <- rank(-leaderboard$dollar_made)
    leaderboard$harvested_tonnes <- round(leaderboard$harvested_tonnes,2)
    leaderboard$dollar_made <- round(leaderboard$dollar_made,2)
    colnames(leaderboard) = c('Address', "Sugarcane Harvested (Tonnes)", "Estimated Revenue","Rank")
    leaderboard <- leaderboard %>% select(4,1,2,3)
    datatable(leaderboard %>% arrange(Rank),rownames= FALSE,
              options = list(
                autoWidth = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              )) %>% formatStyle('Address',`text-align` = 'left') %>% formatStyle(
      'Address',
      target = 'row',
      backgroundColor = styleEqual(input$Address, '#FCF8E3'),fontWeight = styleEqual(input$Address, 'bold')) %>% 
      formatCurrency('Estimated Revenue',interval = 3,mark=',',currency = "$") %>% formatRound("Sugarcane Harvested (Tonnes)",interval = 3,mark=',')
  })
   
  observeEvent(input$mapPlot_shape_click, {
    updateNavbarPage(session, "nav", selected = "desc")
  })
  
 
}

shinyApp(ui = ui, server = server)
