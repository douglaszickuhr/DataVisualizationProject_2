# National College of Ireland
# Data Visualization Project
# Interactive Visualization 2
# Title: Analysis of Flights Delay in Brazil
# Author: Douglas Zikcuhr

# Loading libraries
library(lubridate)
library(tidyverse)
library(dplyr)
library(shiny)
library(data.table)
library(highcharter)
library(DT)

loadDelayFile <- function(file, 
                          colClasses = NULL){
  df <- fread(file = file,
              colClasses = colClasses)
  
  df$Day <- as.numeric(df$Day)
  df$Month <- factor(df$Month,levels(df$Month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
  df$WeekDay <- factor(df$WeekDay,levels(df$WeekDay)[c(4,2,6,7,5,1,3)])
  return(df)
}

loadDelayByTimeFile <- function(file,
                                colClasses = NULL){
  df <- fread(file = file,
              colClasses = colClasses)
  df$Day <- ymd(df$Day)
  
  return(df)
}

filterByDay <- function(df){
  df <- df %>% 
    group_by(Type, Flight.Type, Day) %>%
    summarise(AverageDelay = round(mean(AverageDelay, na.rm = T),2),
              TotalFlight = sum(TotalFlight)) %>%
    ungroup()
}

filterByMonth <- function(df){
  df %>%
    group_by(Type, Flight.Type, Month) %>%
    summarise(AverageDelay = round(mean(AverageDelay, na.rm = T),2),
              TotalFlight = sum(TotalFlight)) %>%
    ungroup()
}

filterByWeekDay <- function(df){
  df %>%
    group_by(Type, Flight.Type, WeekDay) %>%
    summarise(AverageDelay = round(mean(AverageDelay, na.rm = T),2),
              TotalFlight = sum(TotalFlight)) %>%
    ungroup()
}

averageDelay <- loadDelayFile("AverageDelay.csv",
                              colClasses = c("factor", "factor", "factor", "factor",
                                             "numeric", "numeric", "numeric", "numeric"))

delayByTime <- loadDelayByTimeFile("DelayByTime.csv",
                                   colClasses = c("factor","factor","character",
                                                  "numeric","numeric"))

returnDescription <- function(type){
  HTML(paste(img(src=paste0('http://www.tijoletarustica.com.br/img/',type,'.png'),
                 height = "15px"),type))
}

# Shiny User interface
ui <- fluidPage(
  
  # Title of panel
  titlePanel(paste("Brazilian Flights Delay Analysis from","31/12/2014","to","31/07/2017"), 
             windowTitle = "Data Visualization CA2 - Plot 1"),
  sidebarLayout(
    
    # The panel has the select input
    sidebarPanel(
      width = 3,
      
      # Well panel keep things tidy - Flight selection is the first
      wellPanel(h3("Flight Selection"),
                
                # The helptext output is used to give the user hints of how to use the dashboard
                helpText("Use these options to filter the records."),
                
                # Select Flight Type input
                checkboxGroupInput(inputId = "type",
                                   label = "Flight Type",
                                   choices = levels(averageDelay$Flight.Type),
                                   selected = levels(averageDelay$Flight.Type)
                )
      ),
      wellPanel(h3("Show the data"),
                # Action button to show
                actionButton(inputId = "updateButton", 
                             label = "Show Charts")
      ),
      wellPanel(h5("Built with",
                   tags$a(img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png",
                              height = "30px"),
                          hreg="https://shiny.rstudio.com/"),
                   "by",
                   tags$a(img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png",
                              height = "30px"),
                          href="https://www.rstudio.com"),
                   "."))
    ),
    
    # Output panel:
    mainPanel(
      
      # Tabset to create different tabs      
      tabsetPanel(
        
        #First tab for Map
        tabPanel("Hour",
                 tabsetPanel(
                   tabPanel(title = returnDescription("Departures"),
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Hint about how the map works.
                              #helpText("Click on the lines to see the route. Click on the red circle to see."),
                              #br(),
                              
                              # Outputting the leaflet map.
                              highchartOutput(outputId = "day_departure")
                            )
                   ),
                   tabPanel(title = returnDescription("Arrivals"),
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Hint about how the map works.
                              #helpText("Click on the lines to see the route. Click on the red circle to see."),
                              #br(),
                              
                              # Outputting the leaflet map.
                              highchartOutput(outputId = "day_arrival")
                            )
                   )
                 )
        ),
        #First tab for Map
        tabPanel("Day of Week",
                 tabsetPanel(
                   tabPanel(title = returnDescription("Departures"),
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Hint about how the map works.
                              #helpText("Click on the lines to see the route. Click on the red circle to see."),
                              #br(),
                              
                              # Outputting the leaflet map.
                              highchartOutput(outputId = "week_departure")
                            )
                   ),
                   tabPanel(title = returnDescription("Arrivals"),
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Hint about how the map works.
                              #helpText("Click on the lines to see the route. Click on the red circle to see."),
                              #br(),
                              
                              # Outputting the leaflet map.
                              highchartOutput(outputId = "week_arrival")
                            )
                   )
                 )
        ),
        
        #First tab for Map
        tabPanel("Month",
                 tabsetPanel(
                   tabPanel(title = returnDescription("Departures"),
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Hint about how the map works.
                              #helpText("Click on the lines to see the route. Click on the red circle to see."),
                              #br(),
                              
                              # Outputting the leaflet map.
                              highchartOutput(outputId = "month_departure")
                            )
                   ),
                   tabPanel(title = returnDescription("Arrivals"),
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Hint about how the map works.
                              #helpText("Click on the lines to see the route. Click on the red circle to see."),
                              #br(),
                              
                              # Outputting the leaflet map.
                              highchartOutput(outputId = "month_arrival")
                            )
                   )
                 )
        ),
        
        #First tab for Map
        tabPanel("Historical Data",
                 tabsetPanel(
                   tabPanel(title = returnDescription("Departures"),
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Hint about how the map works.
                              #helpText("Click on the lines to see the route. Click on the red circle to see."),
                              #br(),
                              
                              # Outputting the leaflet map.
                              highchartOutput(outputId = "historical_departure")
                            )
                   ),
                   tabPanel(title = returnDescription("Arrivals"),
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Hint about how the map works.
                              #helpText("Click on the lines to see the route. Click on the red circle to see."),
                              #br(),
                              
                              # Outputting the leaflet map.
                              highchartOutput(outputId = "historical_arrival")
                            )
                   )
                 )
        ),
        
        # Third tab - Showing the data and allowing the user to download it
        tabPanel("Data",
                 
                 # Well panel for tidying the visualization
                 wellPanel(
                   
                   #Description of the tab
                   h3("Detailed data"),
                   hr(),
                   
                   # A hint about the Data Table
                   helpText("It's possible to change the data that is being listed using the filter options."),
                   
                   # Outputting the table
                   dataTableOutput(outputId = "datatable"),
                   
                   # A hint about the Data Table
                   helpText("Click on the button to download the data"),
                   
                   # Option to download the correspondent data
                   downloadButton("downloadData", "Download")
                 )
        )
      ),
      
      # Listing the total of records found.
      uiOutput(outputId = "n"),
      
      h5(tags$a(img(src = "https://www.ncirl.ie/Portals/0/nciLogo.png", 
                    height = "30px"),
                href = "https://www.ncirl.ie"
      ),
      br(),
      tags$a("Student: Douglas Zickuhr",
             href="https://www.linkedin.com/in/douglas-zickuhr/"),
      br(),
      "Student Number: 17111781"),
      tags$a(h5("Data extracted from Kaggle"),
             href = "https://www.kaggle.com/ramirobentes/exploring-civil-aviation-in-brazil/data")
    )
  )
)

# Server - Shinny
server <- function(input, output, session) {
  
  # Take a reactive dependency on input$button, but not on any other inputs
  df1 <- eventReactive(input$updateButton, {
    req(input$type)
    df <- averageDelay %>%
      filter(Flight.Type %in% input$type)
    
    return(df)
  })
  
  df2  <- eventReactive(input$updateButton, {
    req(input$type)
    df <- delayByTime %>%
      filter(Flight.Type %in% input$type)
    
    return(df)
  })
  
  dayFiltered <- eventReactive(input$updateButton, {
    df1() %>% 
      group_by(Type, Flight.Type, Day) %>%
      summarise(AverageDelay = round(mean(AverageDelay, na.rm = T),2),
                TotalFlight = sum(TotalFlight)) %>%
      ungroup()
  })
  
  weekFiltered <- eventReactive(input$updateButton, {
    df1() %>%
      group_by(Type, Flight.Type, WeekDay) %>%
      summarise(AverageDelay = round(mean(AverageDelay, na.rm = T),2),
                TotalFlight = sum(TotalFlight)) %>%
      ungroup()
  }) 
  
  monthFiltered <- eventReactive(input$updateButton, {
    df1() %>%
      group_by(Type, Flight.Type, Month) %>%
      summarise(AverageDelay = round(mean(AverageDelay, na.rm = T),2),
                TotalFlight = sum(TotalFlight)) %>%
      ungroup()
  })
  
  dayPlot <- function(df, type = NULL){
    highchart() %>% 
      hc_add_series(df[df$Type == type,], 
                    type = 'scatter',
                    hcaes(x = factor(Day,levels=seq(1:31)), 
                          y = AverageDelay, 
                          group = Flight.Type,
                          size = TotalFlight)
      ) %>% 
      hc_tooltip(pointFormat = "<b>Type:</b> {point.Type} <br> 
                 <b>Average Delay:</b> {point.AverageDelay} <br> 
                 <b>Number of Flights:</b> {point.TotalFlight}") %>%
      hc_xAxis(title = list(text = "Day of Month"),
               type = 'category') %>%
      hc_yAxis(title = list(text = "Average Delay time in minutes")) %>% 
      hc_title(text = paste(type,"Average Delay time by Day of Month"),
               align = "center") %>%
      hc_subtitle(text = "Click on the circle to more information",
                  align = "center") %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_credits(enabled = TRUE,
                 text = "Source: Brazillian National Civil Aviation Agency",
                 style = list(fontSize = "10px"))
    
  }
  
  weekDayPlot <- function(df, type = NULL){
    chart <- highchart() %>%
      hc_xAxis(categories = df$WeekDay) %>%
      hc_add_series(df[df$Type == type,],
                    type = "column",
                    hcaes(x=WeekDay,
                          y=AverageDelay,
                          group=Flight.Type)) %>%
      hc_tooltip(pointFormat = "<b>Average Delay time:</b> {point.AverageDelay} <br>
                              <b>Number of Flights:</b> {point.TotalFlight}") %>%
      hc_yAxis(title = list(text = "Average Delay time in minutes")) %>% 
      hc_title(text = paste(type," Average Delay time by Week day"),
               align = "center") %>%
      hc_subtitle(text = "Click on the bar to more information",
                  align = "center") %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_credits(enabled = TRUE,
                 text = "Source: Brazillian National Civil Aviation Agency",
                 style = list(fontSize = "10px"))
    return(chart)
  }
  
  monthPlot <- function(df,type = NULL){
    chart <- highchart() %>%
      hc_add_series(df[df$Type==type,],
                    type = "line",
                    hcaes(x=Month,
                          y=AverageDelay,
                          group=Flight.Type)) %>%
      hc_tooltip(pointFormat = "<b>Number of Flights:</b> {point.TotalFlight}") %>%
      
      hc_xAxis(categories = df$Month) %>%
      hc_yAxis(title = list(text = "Average Delay time in minutes")) %>% 
      hc_title(text = paste(type," Average Delay time by Month"),
               align = "center") %>%
      hc_subtitle(text = "Click on the points to more information",
                  align = "center") %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_credits(enabled = TRUE,
                 text = "Source: Brazillian National Civil Aviation Agency",
                 style = list(fontSize = "10px"))
    return(chart)
  }
  
  historicalPlot <- function(df, type = NULL){
    print(head(df))
    
    chart <- highchart(type = "stock") %>%
      hc_add_series(df[df$Type == type,],
                    type = "line",
                    hcaes(x=Day,
                          y=AverageDelay,
                          group=Flight.Type)) %>%
      hc_rangeSelector(selected = 0) %>% 
      hc_title(text = paste(type,"Historic Average Delay"),
               align = "center") %>%
      hc_subtitle(text = "Click on the lines to more information",
                  align = "center") %>%
      hc_credits(enabled = TRUE,
                 text = "Source: Brazillian National Civil Aviation Agency",
                 style = list(fontSize = "10px")) %>%
      hc_legend(enabled = T,
                floating = T,
                verticalAlign = 'bottom',
                align = 'center') %>%
      hc_add_theme(hc_theme_google())
    return(chart)
  }
  
  output$day_departure <- renderHighchart(
    dayPlot(dayFiltered(), type = "Departure")
  )
  
  output$day_arrival <- renderHighchart(
    dayPlot(dayFiltered(), type = "Arrival")
  )
  
  output$week_departure <- renderHighchart(
    weekDayPlot(weekFiltered(),
                type = "Departure")
  )
  
  output$week_arrival <- renderHighchart(
    weekDayPlot(weekFiltered(),
                type = "Arrival")
  )
  
  output$month_departure <- renderHighchart(
    monthPlot(monthFiltered(),
              type = "Departure")
  )
  
  output$month_arrival <- renderHighchart(
    monthPlot(monthFiltered(),
              type = "Arrival")
  )
  
  output$historical_departure <- renderHighchart(
    historicalPlot(df2(),
                   type = "Departure")
  )
  
  output$historical_arrival <- renderHighchart(
    historicalPlot(df2(),
                   type = "Arrival")
  )
  
  output$datatable <- renderDataTable(
    df2()
  )
  
  # Rendering the option to download the file
  output$downloadData <- downloadHandler(
    
    # Function to return the filename
    filename = function() {
      paste("delay-in-brazil-by-day", ".csv", sep = "")
    },
    # Function to return the data
    content = function(file) {
      write.csv(df2(), file, row.names = FALSE)
    }
  )
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)