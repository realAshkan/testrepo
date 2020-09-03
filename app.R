library(tidyverse)
library(shiny)
library(leaflet)

df_original <- read_csv("./data/processed/2020-04-14-covid.csv")
pal <- colorFactor(c("firebrick", "steelblue"), c(FALSE, TRUE))
lng1 <- -125
lat1 <- 25
lng2 <- -68
lat2 <- 49

# UI --------------------
ui <- fluidPage(
    title = "Tracking the Spread of COVID-19",
    titlePanel("Tracking the Spread of COVID-19 by County"),
    fluidRow(
        column(12,
               mainPanel(leafletOutput("map"))
        )
    ),
    hr(),
    fluidRow(
        column(4,        
               sliderInput("date_select", 
                           "Select Mapping Date",
                           min = min(df_original$date),
                           max = max(df_original$date),
                           value = max(df_original$date),
                           animate = TRUE)
        ),
        column(4,
               radioButtons("color_by",
                            "Color Markers By Policy",
                            choices = list("Stay At Home Order" = "stay_at_home",
                                           "State of Emergency" = "state_of_emergency",
                                           "K-12 Schools Closed" = "schools_closed",
                                           "Non-essential Businesses Closed" = "non_essentials_closed"))
        ),
        column(4,
               radioButtons("size_by",
                            "Size Markers By Value",
                            choices = list("Total Confirmed Cases" = "confirmed_cases",
                                           "Total Confirmed Cases per 100k People" = "confirmed_cases_per_100k",
                                           "New Cases in Last Week" = "new_cases_week",
                                           "New Cases in Last Week per 100k People" = "new_cases_week_per_100k",
                                           "Total Deaths" = "deaths",
                                           "Deaths per 100k People" = "deaths_per_100k",
                                           "Deaths in Last Week" = "new_deaths_week",
                                           "Deaths in Last Week per 100k People" = "new_deaths_week_per_100k"), 
                            selected = "new_cases_week_per_100k")
        )
    ),
    hr(),
    fluidRow(
        column(12,
               p("Data from ", 
                 a("Johns Hopkins", 
                   href = "https://github.com/CSSEGISandData/COVID-19", 
                   target = "_blank"),
                 " and ", 
                 a("Boston University", 
                   href = "https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit?usp=sharing", 
                   target = "_blank"))
        )
    )
)

# Server --------------------
server <- function(input, output) {
    
    df <- reactive({
        # This is the same code we used to filter to the latest date in last week's lesson!
        tmp <- df_original %>%
            filter(new_cases_week_per_100k > 0) %>%
            filter(date == input$date_select)
        
        return(tmp)
    })
    
    output$map <- renderLeaflet({
        
        leaflet() %>%
            addTiles() %>%
            fitBounds(lng1, lat1, lng2, lat2) %>%
            addLegend("bottomright", 
                      pal = pal, 
                      values = c(FALSE, TRUE),
                      title = input$color_by,
                      opacity = 1)
        
        
    })
    
    observe({
        
        leafletProxy("map", data = df()) %>%
            clearMarkers() %>%
            addCircleMarkers(radius = ~sqrt(get(input$size_by)),
                             stroke = FALSE,
                             fillOpacity = 0.5,
                             color = ~pal(get(input$color_by)),
                             popup = ~paste0("<b>", region, "</b><br/>",
                                             "Total confirmed cases to this date: ", confirmed_cases, "<br/>",
                                             "Per 100k people: ", confirmed_cases_per_100k, "<br/><br/>",
                                             "Total confirmed deaths to this date: ", deaths, "<br/>",
                                             "Per 100k people: ", deaths_per_100k, "<br/><br/>",
                                             "Cases in the preceding week: ", new_cases_week, "<br/>",
                                             "Per 100k people: ", new_cases_week_per_100k, "<br/><br/>",
                                             "Deaths in the preceding week: ", new_deaths_week, "<br/>",
                                             "Per 100k people: ", new_deaths_week_per_100k, "<br/><br/>",
                                             "Stay at home in place on this date: ", stay_at_home))
    })
    
}

shinyApp(ui = ui, server = server)