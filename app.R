#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidytext)
library(leaflet)
library(leaflet.extras)
library(mapview)
library(highcharter)
library(xts)
library(viridis)
library(forecast)
library(prophet)
library(dygraphs)
library(webshot)
library(graphics)

# Define UI for application that draws a histogram
ui <- dashboardPage(
   
  # Select dashboard theme color
  skin = "blue",
  
  # Define Dashboard title
  dashboardHeader(
    title = "Crimes in Chicago"
  ),
   
  dashboardSidebar(
    sidebarMenu(
      
      menuItem(
        "Time Series",
        tabName = "timeseries",
        icon = icon("hourglass"),
        
        menuSubItem("Crimes and Arrests", tabName= "crimesandarrests"),
        menuSubItem("Forecast", tabName= "forecast")
      ),
      
      menuItem(
        "Visualization",
        tabName = "visualization",
        icon = icon("chart-bar"),
        
        menuSubItem("Crime Types by Year", tabName="crimetypesbyyear"),
        menuSubItem("Crime Locations by Year", tabName="crimelocations")
      ),

      menuItem(
        "Map",
        tabName = "map",
        icon = icon("map-pin")),
      
      menuItem(
        "Heatmap",
        tabName = "heatmap", 
        icon = icon("map"))
  )),
  
  dashboardBody(
    
    tabItems(

      tabItem(tabName = "crimesandarrests",
              
              fluidRow(
                
                box(
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 12,
                  highchartOutput("tsCrimeArrest")
                ),
                
                box(
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 12,
                  highchartOutput("tsArrests")
                )
            )
      ),
      
      tabItem(tabName = "forecast",

              fluidRow(
                
                box(
                  title = "Periods", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  sliderInput("per", label = "Select Period", min = 1, max = 6, step = 1, value = 3)
                ),
                
                box(
                  title = "Plot", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 12,
                  dygraphOutput("fcCrime")
                ),
                
                box(
                  title = "Download", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  radioButtons("fcFormat", "Document format", c("PNG"="png", "EPS"="eps", "PDF"="pdf"), inline = TRUE, selected = "png"),
                  downloadButton("fcDownload")
                )
              )
      ),
      
      tabItem(tabName = "crimetypesbyyear",
              
              fluidRow(
                
                box(
                  title = "Date", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  sliderInput("ctypeDate", label = "Select Year", min = 2001, max = 2016, step = 1, sep = '', value = c(2001,2016))
                ),
                
                box(
                  title = "Crime Type", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  height = 162,
                  selectInput("ctypeCrimeType", label= "Select Crime Type", choices = unique(cc$Primary.Type)) 
                ),
                
                box(
                  title = "Plot", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 12,
                  highchartOutput(outputId = "ctypeOutput")
                ),
                
                box(
                  title = "Top 20", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 12,
                  highchartOutput(outputId = "top20ctype")
                )
              )
            ),
      
      tabItem(tabName = "crimelocations",
              
              fluidRow(
                
                box(
                  title = "Date", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  sliderInput("locDate", label = "Select Year", min = 2001, max = 2016, step = 1, sep = '', value = c(2001,2016))
                ),
                
                box(
                  title = "Crime Type", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  height = 162,
                  selectInput("locLocation", label= "Select Location", choices = unique(cc$Location.Description)) 
                ),
                
                box(
                  title = "Plot", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 12,
                  highchartOutput(outputId = "locOutput")
                ),
                
                box(
                  title = "Top 20", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 12,
                  highchartOutput(outputId = "top20loc")
                )
              )
            ),
      
      tabItem(tabName = "map",
              
              fluidRow(
                
                column(width = 9,
                # Main plot
                box(
                  title = "Chicago Map", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = NULL,
                  height = NULL,
                  #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                  leafletOutput("map",height = "95vh")
                )),
                
                column(width = 3,
                box(
                  title = "Date", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = NULL,
                  #height = 142,
                  sliderInput("mapYear", label = "Select Year", min = 2001, max = 2016, step = 1, sep = '', value = c(2001,2016))
                ),
                
                box(
                  title = "Crime Type", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = NULL,
                  selectInput("mapCrimeType", label= "Select Crime Type", choices = unique(cc$Primary.Type), multiple = TRUE)
                ),
                
                box(
                  title = "Location", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = NULL,
                  selectInput("mapLocation", label= "Select Location", choices = unique(cc$Location.Description), multiple = TRUE)
                ),
                
                box(
                  title = "Download", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = NULL,
                  radioButtons("mapFormat", "Document format", c("PNG"="png", "EPS"="eps", "PDF"="pdf"), inline = TRUE, selected = "png"),
                  downloadButton("mapDownload")
                )
              )
            )
          ),
      
      tabItem(tabName = "heatmap",
              
              fluidRow(
                
                column(width = 9,
                       # Main plot
                       box(
                         title = "Chicago Heatmap", 
                         status = "danger", 
                         solidHeader = TRUE,
                         width = NULL,
                         height = NULL,
                         #tags$style(type = "text/css", "#heatmap {height: calc(100vh - 80px) !important;}"),
                         leafletOutput("heatmap",height = "95vh")
                       )),
                
                column(width = 3,
                       box(
                         title = "Date", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = NULL,
                         #height = 142,
                         sliderInput("heatmapYear", label = "Select Year", min = 2001, max = 2016, step = 1, sep = '', value = c(2001,2016))
                       ),
                       
                       box(
                         title = "Crime Type", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = NULL,
                         selectInput("heatmapCrimeType", label= "Select Crime Type", choices = unique(cc$Primary.Type), multiple = TRUE)
                       ),
                       
                       box(
                         title = "Location", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = NULL,
                         selectInput("heatmapLocation", label= "Select Location", choices = unique(cc$Location.Description), multiple = TRUE)
                       ),
                       
                       box(
                         title = "Download", 
                         status = "success", 
                         solidHeader = TRUE,
                         width = NULL,
                         radioButtons("heatmapFormat", "Document format", c("PNG"="png", "EPS"="eps", "PDF"="pdf"), inline = TRUE, selected = "png"),
                         downloadButton("heatmapDownload")
                       )
                    )
                 )
              ),
    
      tabItem(tabName = "data",
              h2("Dashboard tab content")
      )
    )
  )
)

    
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### Time Series ###
  Year12_16 <- cc[cc$Year2 %in% c("2012", "2013", "2014", "2015", "2016"),]
  
  groupByDate <- na.omit(Year12_16) %>% group_by(Date2) %>% summarise(Total = n())
  timeSeries <- xts(groupByDate$Total, order.by = groupByDate$Date2)
  
  arrestByDate <- na.omit(Year12_16[Year12_16$Arrest == "TRUE",]) %>% group_by(Date2) %>% summarise(Total = n())
  arrestTimeSeries <- xts(arrestByDate$Total, order.by = as.POSIXct(arrestByDate$Date2))
  
  output$tsCrimeArrest <- renderHighchart({
    
    hchart(timeSeries, name = "Crimes") %>%
      hc_exporting(enabled = TRUE, filename = "TS_Crimes_Arrest") %>%
      hc_add_series(arrestTimeSeries, name = "Arrests") %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text = "Time Series plot of Chicago Crimes") %>%
      hc_subtitle(text = "(2012 - 2016)") %>%
      hc_legend(enabled = TRUE, align = "center")
  })
  
  output$tsArrests <- renderHighchart({
    
    hchart(arrestTimeSeries, name = "Arrests") %>%
      hc_exporting(enabled = TRUE, filename = "TS_Arrests") %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text = "Time Series plot of Arrests in Chicago") %>%
      hc_subtitle(text = "(2012 - 2016)") %>%
      hc_legend(enabled = TRUE, align = "center")
  })
  
  output$fcCrime <- renderDygraph({
    
    df <- Year12_16 %>% group_by(Date2) %>% summarise(y = n()) %>% mutate(y = log(y))
    names(df) <- c("ds", "y")
    df$ds <- factor(df$ds)
    
    m <- prophet(df)
    future <- make_future_dataframe(m, 365*input$per)
    forecast <- predict(m, future)
    tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
    
    #plot(m, forecast, xlabel = "Year", ylabel = "Data")+ggtitle("Forecast of Crimes in Chicago")
  
    dyplot.prophet(m, forecast)
    
    #prophet_plot_components(m, forecast)
    
  })
  
  ### Draw Map ###
  output$map = renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      setView(lng = -87.623177, lat = 41.881832, zoom=11)
  })

  reactMap = reactive({
    cc %>%
      filter(Primary.Type %in% input$mapCrimeType &
               Location.Description %in% input$mapLocation &
               Year2 %in% cbind(input$mapYear[1],input$mapYear[2]))
  })

  observe({
    proxy = leafletProxy("map", data = reactMap()) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addCircleMarkers(clusterOptions = markerClusterOptions(),
                       lng =~ Longitude, lat =~ Latitude, radius = 5, group = 'Cluster',
                       popup =~ paste('<b><font color="Black">', 'Crime Information',
                                      '</font></b><br/>', 'Crime Type:', Primary.Type,'<br/>',
                                      'Date:', Date,'<br/>', #'Time:', Time,'<br/>',
                                      'Location:', Location.Description,'<br/>', 'Block:', Block, '<br/>', 'Arrest:', Arrest, '<br/>'))
  })

  fnMap <- reactive({paste(input$mapCrimeType,input$mapLocation,sep = ".")})
  doMap <- reactive({input$mapFormat})

  # Download current plot
  output$mapDownload <- downloadHandler(
    filename = fnMap,
    content = function(file) {

      mapshot(map$dat, file = file)
    }
  )
  
  ### Draw Heatmap ###
  output$heatmap = renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      setView(lng = -87.6105, lat = 41.8947, zoom=11)
  })
  
  reactHeatmap = reactive({
    cc %>%
      filter(Primary.Type %in% input$heatmapCrimeType &
               Location.Description %in% input$heatmapLocation &
               Year2 %in% cbind(input$heatmapYear[1],input$heatmapYear[2]))
  })
  
  observe({
    proxy = leafletProxy("heatmap", data = reactHeatmap) %>%
      removeWebGLHeatmap(layerId = "hm") %>%
      addWebGLHeatmap(layerId = "hm", data = reactHeatmap(),
                      lng =~ Longitude, lat =~ Latitude, size=120)
  })

  ### Draw Crimes by Year ###
  output$ctypeOutput <- renderHighchart({

    ctypeAnalysis <- cc[cc$Primary.Type == input$ctypeCrimeType,] %>% group_by(Year2) %>% summarise(Total = n()) %>% filter(as.numeric(levels(Year2))[Year2] >= input$ctypeDate[1] & as.numeric(levels(Year2))[Year2] <= input$ctypeDate[2])
    
    hchart(ctypeAnalysis %>% na.omit(), "column", hcaes(x = Year2, y = Total, color = Total)) %>%
      hc_exporting(enabled = TRUE, filename = paste(input$ctypeCrimeType, "by_Year", sep = "_")) %>%
      hc_title(text = paste("Crime Type by Year",input$ctypeCrimeType, sep = ": ")) %>%
      hc_subtitle(text = paste(input$ctypeDate[1], input$ctypeDate[2], sep = " - ")) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Crimes")) %>%
      hc_colorAxis(stops = color_stops(n = 10, colors = c("#d98880", "#85c1e9", "#82e0aa"))) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_legend(enabled = FALSE)
  })
  
  output$top20ctype <- renderHighchart({
    
    crimetypeAnalysis <- cc %>% group_by(Primary.Type) %>% summarise(Total = n()) %>% arrange(desc(Total))
    
    hchart(crimetypeAnalysis[1:20,], "column", hcaes(x = Primary.Type, y = Total, color = Total)) %>%
      hc_exporting(enabled = TRUE, filename = "Top_20_Crime_Types") %>%
      hc_title(text = "Top 20 Crime Types") %>%
      hc_subtitle(text = "(2001 - 2016)") %>%
      hc_xAxis(title = list(text = "Type"), labels = list(rotation = -90)) %>%
      hc_yAxis(title = list(text = "Crimes")) %>%
      hc_colorAxis(stops = color_stops(n = 10, colors = c("#d98880", "#85c1e9", "#82e0aa"))) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_legend(enabled = FALSE)
  })
  
  ### Draw Location by Year ###
  
  output$locOutput <- renderHighchart({
    
    locAnalysis <- cc[cc$Location.Description == input$locLocation,] %>% group_by(Year2) %>% summarise(Total = n()) %>% filter(as.numeric(levels(Year2))[Year2] >= input$locDate[1] & as.numeric(levels(Year2))[Year2] <= input$locDate[2])
    
    hchart(locAnalysis %>% na.omit(), "column", hcaes(x = Year2, y = Total, color = Total)) %>%
      hc_exporting(enabled = TRUE, filename = paste(input$locLocation, "by_Year", sep = "_")) %>%
      hc_title(text = paste("Crime Locations by Year",input$locLocation, sep = ": ")) %>%
      hc_subtitle(text = paste(input$locDate[1], input$locDate[2], sep = " - ")) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Crimes")) %>%
      hc_colorAxis(stops = color_stops(n = 10, colors = c("#d98880", "#85c1e9", "#82e0aa"))) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_legend(enabled = FALSE)
  })
  
  output$top20loc <- renderHighchart({
  
  locationAnalysis <- cc %>% group_by(Location.Description) %>% summarise(Total = n()) %>% arrange(desc(Total))

    hchart(locationAnalysis[1:20,], "column", hcaes(x = Location.Description, y = Total, color = Total)) %>%
      hc_exporting(enabled = TRUE, filename = "Top_20_Locations") %>%
      hc_title(text = "Top 20 Locations with most Crimes") %>%
      hc_subtitle(text = "(2001 - 2016)") %>%
      hc_xAxis(title = list(text = "Location"), labels = list(rotation = -90)) %>%
      hc_yAxis(title = list(text = "Crimes")) %>%
      hc_colorAxis(stops = color_stops(n = 10, colors = c("#d98880", "#85c1e9", "#82e0aa"))) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_legend(enabled = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

