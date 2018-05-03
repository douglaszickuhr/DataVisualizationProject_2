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

# Function to load the Average Delay File by Hour, WeekDay, Day and Month
loadDelayFile <- function(file, 
                          colClasses = NULL){
  df <- fread(file = file,
              colClasses = colClasses)
  
  # Converting Day to Numeric for ordering purposes
  df$Day <- as.numeric(df$Day)
  
  # Converting Month and Weekday to factor
  df$Month <- factor(df$Month,levels(df$Month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
  df$WeekDay <- factor(df$WeekDay,levels(df$WeekDay)[c(4,2,6,7,5,1,3)])
  
  # Returning the dataframe
  return(df)
}

# Function to load the Average Delay Time and Total Flights by date
loadDelayByTimeFile <- function(file,
                                colClasses = NULL){
  # Reading the file using fread
  df <- fread(file = file,
              colClasses = colClasses)
  
  # Converting the variable Day to ymd format
  df$Day <- ymd(df$Day)
  
  # Retun the dataframe
  return(df)
}

# Function to load the average delay by cause
loadDelayTimeByCause <- function(file,
                                 colClasses = NULL){
  
  # Reading the file using fread
  df <- fread(file = file,
              colClasses = colClasses)
  
  # Removing variables which the delay is positive
  df %>%
    filter(AverageArrivalDelay > 0 & AverageDepartureDelay > 0) %>%
    arrange(desc(TotalFlight))
}

# Function to return the dataframe filtered by Day
filterByDay <- function(df){
  
  # Filterign using dplyr, grouped by Type, Flight Type and Day
  df <- df %>% 
    group_by(Type, Flight.Type, Day) %>%
    summarise(AverageDelay = round(mean(AverageDelay, na.rm = T),2),
              TotalFlight = sum(TotalFlight)) %>%
    ungroup()
}

# Function to return the dataframe filtered by Month
filterByMonth <- function(df){
  
  # Filterign using dplyr, grouped by Type, Flight Type and Month
  df %>%
    group_by(Type, Flight.Type, Month) %>%
    summarise(AverageDelay = round(mean(AverageDelay, na.rm = T),2),
              TotalFlight = sum(TotalFlight)) %>%
    ungroup()
}

# Function to return the dataframe filtered by Weekday
filterByWeekDay <- function(df){
  
  # Filterign using dplyr, grouped by Type, Flight Type and Month
  df %>%
    group_by(Type, Flight.Type, WeekDay) %>%
    summarise(AverageDelay = round(mean(AverageDelay, na.rm = T),2),
              TotalFlight = sum(TotalFlight)) %>%
    ungroup()
}

# Function to return the description plus image
returnDescription <- function(type){
  HTML(paste(img(src=paste0('http://www.tijoletarustica.com.br/img/',type,'.png'),
                 height = "15px"),type))
}

# Reading the Average Delay File
averageDelay <- loadDelayFile("AverageDelay.csv",
                              colClasses = c("factor", "factor", "factor", "factor",
                                             "numeric", "numeric", "numeric", "numeric"))

# Reading the Average Delay File by Time
delayByTime <- loadDelayByTimeFile("DelayByTime.csv",
                                   colClasses = c("factor","factor","character",
                                                  "numeric","numeric"))

# Reading the Average Delay File by Cause
delayByCause <- loadDelayTimeByCause("AverageDelayByCause.csv",
                                     colClasses = c("factor","numeric","numeric","integer"))

# Shiny User interface
ui <- fluidPage(
  
  # Title of panel
  titlePanel(paste("Brazilian Flights Delay Analysis"), 
             windowTitle = "Data Visualization Final Project - Dashboard 2"),
  helpText("The raw dataset contains over 2M flight observations from 31/Dec/2014 to 31/Jul/2017 "),
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
      # Well panel to cite Shiny and RStudio
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
        
        #First tab for Hour Line Plot 
        tabPanel("Hour",
                 tabsetPanel(
                   
                   # Panel to show the Departures
                   tabPanel(title = returnDescription("Departures"),
                            
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Outputting the Departures Hour Map
                              highchartOutput(outputId = "hour_departure")
                            )
                   ),
                   
                   # Panel to show the Arrivals
                   tabPanel(title = returnDescription("Arrivals"),
                            
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Outputting the Arrivals Hour Map
                              highchartOutput(outputId = "hour_arrival")
                            )
                   )
                 )
        ),
        
        #Second tab for Day Map
        tabPanel("Day",
                 tabsetPanel(
                   
                   # Panel to show the departure plot
                   tabPanel(title = returnDescription("Departures"),
                            
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Hint about how plot
                              helpText("The size of the circle is related to the number of flights."),
                              
                              # Outputting the Departures Day Plot.
                              highchartOutput(outputId = "day_departure")
                            )
                   ),
                   
                   # Panel to show the Arrivals Day plot
                   tabPanel(title = returnDescription("Arrivals"),
                            
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Hint about how plot
                              helpText("The size of the circle is related to the number of flights."),
                              
                              # Outputting the Arrivals Day Plot.
                              highchartOutput(outputId = "day_arrival")
                            )
                   )
                 )
        ),
        
        #Third tab for Weekday plot
        tabPanel("Day of Week",
                 tabsetPanel(
                   
                   # Panel to show the Departures plot
                   tabPanel(title = returnDescription("Departures"),
                            
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Outputting the Departures Weekday Plot
                              highchartOutput(outputId = "week_departure")
                            )
                   ),
                   
                   # Panel to show the Arrivals plot
                   tabPanel(title = returnDescription("Arrivals"),
                            # Well panel to organise the output
                            wellPanel(
                              
                               # Outputting the Arrivals Weekday Plot
                              highchartOutput(outputId = "week_arrival")
                            )
                   )
                 )
        ),
        
        #Fourth tab for Month Plot
        tabPanel("Month",
                 tabsetPanel(
                   
                   # Panel to show the Departures plot
                   tabPanel(title = returnDescription("Departures"),
                            # Well panel to organise the output
                            wellPanel(
                             
                              # Outputting the Departures Month Plot
                              highchartOutput(outputId = "month_departure")
                            )
                   ),
                   
                   # Panel to show the Arrivals plot
                   tabPanel(title = returnDescription("Arrivals"),
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Outputting the Arrivals Month Plot
                              highchartOutput(outputId = "month_arrival")
                            )
                   )
                 )
        ),
        
        #Fifth tab for Historical Data
        tabPanel("Historical Data",
                 tabsetPanel(
                   
                   # Panel to show the Departures plot
                   tabPanel(title = returnDescription("Departures"),
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Outputting the Departures Historical Plot
                              highchartOutput(outputId = "historical_departure")
                            )
                   ),
                   
                   # Panel to show the Arrivals plot
                   tabPanel(title = returnDescription("Arrivals"),
                            # Well panel to organise the output
                            wellPanel(
                              
                              # Outputting the Arrivals Historical Plot
                              highchartOutput(outputId = "historical_arrival")
                            )
                   )
                 )
        ),
        
        #Sixth tab - Delay by Cause
        tabPanel("Delay by Cause",
                 
                 # Well panel to organise the output
                 wellPanel(
                   
                   # Slider input to set the number of top records to show on the treemap
                   sliderInput(inputId = "treemap_size",
                               label = "Top Causes to plot on Treemap",
                               min = 1, 
                               max = 10, 
                               value = 5,
                               width = "30%"),
                   hr(),
                   
                   # Hint about the Treemap
                   helpText("The size of the rectangles are determined by the number of flights whilst the color is determined by the delay according to the legend on the right side."),
                   hr(),
                   
                   
                   # Outputting the Treemap
                   highchartOutput(outputId = "treemap")
                 )
        ),
        
        #Seventh tab - Heatmap
        tabPanel("HeatMap",
                 
                 # Well panel to organise the output
                 wellPanel(
                   
                   # Hint about how the map works.
                   helpText("The heatmap diagram has differente attributes and must be manually loaded."),
                   helpText("Press the button bellow to load the chart"),
                   
                   # Input to set the details for the heatmap
                   div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput(inputId = "heatmap_type",
                                                                                                   label = "Arrival/Departure",
                                                                                                   choices = c("Arrival","Departure"),
                                                                                                   selected = "Arrival",
                                                                                                   multiple = F)),
                   div(style="display: inline-block;vertical-align:top; width: 300px;",selectInput(inputId = "heatmap_flight_type",
                                                                                                   label = "Flight Type",
                                                                                                   choices = c("International","Domestic","Regional"),
                                                                                                   selected = "International",
                                                                                                   multiple = T)),
                   br(),
                   
                   # Button to update the heatmap
                   actionButton(inputId = "updateHeatMapButton",
                                label = "Load HeatMap"),
                   
                   hr(),
                   
                   # Well panel to organise the output
                   tabsetPanel(
                     tabPanel(title = "Week day by Hour",
                              # Well panel to organise the output
                              wellPanel(
                                
                                # Outputting heatmap for week day
                                highchartOutput(outputId = "heatmap_weekday")
                              )
                     ),
                     tabPanel(title = "Day by Month",
                              # Well panel to organise the output
                              wellPanel(
                                
                                # Outputting heatmap for day of month
                                highchartOutput(outputId = "heatmap_day_of_month")
                              )
                     )
                   )
                 )
        ),
        
        # Seventh tab - Showing the data and allowing the user to download it
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
      
      # Listing the details
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
  
  # Take a reactive dependency on input$type
  df1 <- eventReactive(input$type, {
    # Requesting input$type
    req(input$type)
    
    # Filtering averageDelay using the input$type
    df <- averageDelay %>%
      filter(Flight.Type %in% input$type)
    
    # Returning the dataframe
    return(df)
  })
  
  
  # Take a reactive dependency on input$type
  df2  <- eventReactive(input$type, {
    # Requiring input$type
    req(input$type)
    
    # Applying the filter based on the input$type
    df <- delayByTime %>%
      filter(Flight.Type %in% input$type)
    
    # Returning the filtered dataframe
    return(df)
  })
  
  
  # Take a reactive dependency on input$type
  hourFiltered <- eventReactive(input$type, {
    
    # Filtering the dataframe 1 (Based on the AverageDelay df)
    df1() %>%
      # Data from Hours 0,1 and 23 had to been removed due to big noisy in the source data
      mutate(AverageDelay = if_else((Flight.Type == "International" & 
                                       Hour %in% c("0","1","23") & 
                                       Type == "Departure"), 0,AverageDelay)) %>%
      group_by(Type, Flight.Type, Hour) %>%
      summarise(AverageDelay = round(mean(AverageDelay, na.rm = T),2),
                TotalFlight = sum(TotalFlight)) %>%
      mutate(Hour = as.numeric(Hour)) %>%
      arrange(Type,Flight.Type,Hour) %>%
      ungroup()
  })
  
  # Take a reactive dependency on input$type
  dayFiltered <- eventReactive(input$type, {
    
    # Filtering data frame 1 by Day
    df1() %>% 
      group_by(Type, Flight.Type, Day) %>%
      summarise(AverageDelay = round(mean(AverageDelay, na.rm = T),2),
                TotalFlight = sum(TotalFlight)) %>%
      ungroup()
  })
  
  # Take a reactive dependency on input$type
  weekFiltered <- eventReactive(input$type, {
    
    # Filtering data frame 1 by WeekDay
    df1() %>%
      group_by(Type, Flight.Type, WeekDay) %>%
      summarise(AverageDelay = round(mean(AverageDelay, na.rm = T),2),
                TotalFlight = sum(TotalFlight)) %>%
      ungroup()
  }) 
  
  # Take a reactive dependency on input$type
  monthFiltered <- eventReactive(input$type, {
    
    # Filtering data frame 1 by Month
    df1() %>%
      group_by(Type, Flight.Type, Month) %>%
      summarise(AverageDelay = round(mean(AverageDelay, na.rm = T),2),
                TotalFlight = sum(TotalFlight)) %>%
      ungroup()
  })
  
  # Take a reactive dependency on input$updateHeatMapButton
  weekDayHeatMapFiltered <- eventReactive(input$updateHeatMapButton,{
    
    # Requiring input$heatmap_type and input$heatmap_flight_type
    req(input$heatmap_type)
    req(input$heatmap_flight_type)
    
    # Filtering the data for the heatmap
    averageDelay %>%
      filter(Type == input$heatmap_type & 
               Flight.Type %in% input$heatmap_flight_type) %>%
      group_by(Type,WeekDay,Hour) %>%
      summarise(AverageDelay = mean(AverageDelay)) %>%
      ungroup() %>%
      mutate(Hour = as.numeric(Hour)) %>%
      arrange(WeekDay,Hour) %>%
      filter(Hour > 2)
  })
  
  # Take a reactive dependency on input$updateHeatMapButton
  dayOfMonthHeatMapFiltered <- eventReactive(input$updateHeatMapButton,{
    
    # Requiring input$heatmap_type and input$heatmap_flight_type
    req(input$heatmap_type)
    req(input$heatmap_flight_type)
    
    # Filtering the data for the heatmap
    averageDelay %>%
      filter(Type == input$heatmap_type & 
               Flight.Type %in% input$heatmap_flight_type) %>%
      group_by(Type,Month,Day) %>%
      summarise(AverageDelay = mean(AverageDelay)) %>%
      ungroup() %>%
      arrange(Month,Day)
  })
  
  # Function to generate the hourPlot
  hourPlot <- function(df,
                       type = NULL){
    
    # Using highchart library
    chart <- highchart() %>%
      hc_add_series(df[df$Type==type,],
                    type = "line",
                    hcaes(x=Hour,
                          y=AverageDelay,
                          group=Flight.Type)) %>%
      hc_tooltip(pointFormat = "<b>Number of Flights:</b> {point.TotalFlight} <br>
                                <b>Average Delay:</b> {point.AverageDelay}") %>%
      hc_xAxis(categories = df$Hour, 
               title = list(text = "Hour of Day")) %>%
      hc_yAxis(title = list(text = "Average Delay time in minutes")) %>% 
      hc_title(text = paste(type,"Average Delay time by Hour"),
               align = "center") %>%
      hc_subtitle(text = "Click on the points to more information",
                  align = "center") %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_credits(enabled = TRUE,
                 text = "Source: Brazillian National Civil Aviation Agency",
                 style = list(fontSize = "10px"))
    
    return(chart)
  }
  
  # Function to generate the dayPlot
  dayPlot <- function(df, type = NULL){
    
    # Using highchart library
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
  
  # Function to generate the weekDayPlot
  weekDayPlot <- function(df, type = NULL){
    
    # Using highchart library
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
  
  # Function to generate the monthPlot
  monthPlot <- function(df,type = NULL){
    
    # Highchart library
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
  
  # Function to generate the historicalPlot
  historicalPlot <- function(df, type = NULL){
    
    # It is using highchart library
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
  
  # Function to generate heatMap for Week
  heatMapWeek <- function(df){
    
    # Highcart library
    chart <- hchart(df, 
                    type = "heatmap", 
                    hcaes(x = WeekDay, 
                          y = factor(Hour), 
                          value = round(AverageDelay,2))) %>%
      hc_yAxis(title = list(text="Hour")) %>%
      hc_xAxis(title = list(text="Week day")) %>%
      hc_tooltip(pointFormat = "<b>Day of Week:</b> {point.WeekDay} <br>
                                <b>Hour:</b> {point.Hour} <br>
                                <b>Average Delay Time:</b> {point.AverageDelay} ") %>%
      hc_colorAxis(minColor = "#4144f4",
                   maxColor = "#f44b42") %>%
      hc_title(text = paste("Weekday by Hour Heatmap"),
               align = "center") %>%
      hc_subtitle(text = "Click on the map to more information",
                  align = "center") %>%
      hc_credits(enabled = TRUE,
                 text = "Source: Brazillian National Civil Aviation Agency",
                 style = list(fontSize = "10px")) %>%
      hc_legend(enabled = T,
                verticalAlign = 'top',
                layout = "vertical",
                margin = 0,
                symbolHeight = 220,
                y = 100,
                align = 'right') %>%
      hc_add_theme(hc_theme_google())
    
    return(chart)
  }
  
  # Function to generate the heatMap for Month
  heatMapMonth <- function(df){
    
    # Using highchart function
    chart <- hchart(df, 
                    type = "heatmap",
                    hcaes(x = factor(Day), 
                          y = Month, 
                          value = round(AverageDelay,2))) %>%
      hc_yAxis(reversed = TRUE,
               title = list(text="Month")) %>%
      hc_xAxis(title = list(text="Day of Month")) %>%
      hc_tooltip(pointFormat = "<b>Month:</b> {point.Month} <br>
                                <b>Day of Month:</b> {point.Day} <br>
                                <b>Average Delay Time:</b> {point.AverageDelay} ") %>%
      hc_colorAxis(minColor = "#4144f4",
                   maxColor = "#f44b42") %>%
      hc_title(text = paste("Day by Month Heatmap"),
               align = "center") %>%
      hc_subtitle(text = "Click on the map to more information",
                  align = "center") %>%
      hc_credits(enabled = TRUE,
                 text = "Source: Brazillian National Civil Aviation Agency",
                 style = list(fontSize = "10px")) %>%
      hc_legend(enabled = T,
                verticalAlign = 'top',
                layout = "vertical",
                margin = 0,
                symbolHeight = 220,
                y = 100,
                align = 'right') %>%
      hc_add_theme(hc_theme_google())
    
    return(chart)
  }
  
  # Function to generate the treeMap
  treeMap <- function(df){
    
    # Using highchart library
    chart <- hchart(head(df), 
                    "treemap", 
                    hcaes(x = Justification, 
                          value = TotalFlight, 
                          color = AverageArrivalDelay + AverageDepartureDelay)) %>%
      hc_colorAxis(minColor = "#4144f4",
                   maxColor = "#f44b42") %>%
      hc_title(text = paste("Delay by Causes"),
               align = "center") %>%
      hc_subtitle(text = "Click on the treemap to more information",
                  align = "center") %>%
      hc_tooltip(pointFormat = "<b>Number of Flights:</b> {point.TotalFlight} <br>
                                <b>Arrival Average Delay:</b> {point.AverageArrivalDelay} <br>
                                <b>Departure Average Delay:</b> {point.AverageDepartureDelay} ") %>%
      hc_credits(enabled = TRUE,
                 text = "Source: Brazillian National Civil Aviation Agency",
                 style = list(fontSize = "10px")) %>%
      hc_legend(enabled = T,
                verticalAlign = 'top',
                layout = "vertical",
                margin = 0,
                symbolHeight = 220,
                y = 100,
                align = 'right',
                title = list(text = "Average Delay")) %>%
      hc_add_theme(hc_theme_google())  
    
    return(chart)
  }
  
  # Rendering the Hour Departure plot
  output$hour_departure <- renderHighchart(
    hourPlot(hourFiltered(), type = "Departure")
  )
  
  # Rendering the Hour Arrival plot
  output$hour_arrival <- renderHighchart(
    hourPlot(hourFiltered(), type = "Arrival")
  )
  
  # Rendering the Day Departure plot
  output$day_departure <- renderHighchart(
    dayPlot(dayFiltered(), type = "Departure")
  )
  
  # Rendering the Day Arrival plot
  output$day_arrival <- renderHighchart(
    dayPlot(dayFiltered(), type = "Arrival")
  )
  
  # Rendering the Week Departure plot
  output$week_departure <- renderHighchart(
    weekDayPlot(weekFiltered(),
                type = "Departure")
  )
  
  # Rendering the Week Arrival plot
  output$week_arrival <- renderHighchart(
    weekDayPlot(weekFiltered(),
                type = "Arrival")
  )
  
  # Rendering the Month Departure plot
  output$month_departure <- renderHighchart(
    monthPlot(monthFiltered(),
              type = "Departure")
  )
  
  # Rendering the Month Arrival plot
  output$month_arrival <- renderHighchart(
    monthPlot(monthFiltered(),
              type = "Arrival")
  )
  
  # Rendering the Historical Departure plot
  output$historical_departure <- renderHighchart(
    historicalPlot(df2(),
                   type = "Departure")
  )
  
  # Rendering the Historical Arrival plot
  output$historical_arrival <- renderHighchart(
    historicalPlot(df2(),
                   type = "Arrival")
  )
  
  # Rendering the treeMap based on an observer event
  observeEvent(input$treemap_size,{
    output$treemap <- renderHighchart(
      treeMap(head(delayByCause,input$treemap_size)))
  })
  
  # Rendering the datatable
  output$datatable <- renderDataTable(
    df2()
  )
  
  # Rendering the heatmap for week day
  output$heatmap_weekday <- renderHighchart(
    heatMapWeek(df = weekDayHeatMapFiltered())
  )
  
  # Rendering the heatmap for day of month
  output$heatmap_day_of_month <- renderHighchart(
    heatMapMonth(df = dayOfMonthHeatMapFiltered())
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
