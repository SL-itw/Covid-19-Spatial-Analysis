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
library(tigris) 
library(leaflet)


ui <- dashboardPage(
    dashboardHeader(title = "Covid-19 In NYC"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            infoBox(title = "Case Rate",
                    subtitle = "per 100,000",
                    10*2
                    ),
            infoBoxOutput("approvalBox")
            
        ),
        
        fluidRow(
            column(width = 9, 
                   box(
                        width = NULL, 
                        solidHeader = TRUE,
                        leafletOutput("plot1", height = 500)
                        )
                   )
            ),
        
            
        )
    )


server <- function(input, output) {
    
    #plot 1
        #data
    # Data
    `%notin%` = Negate(`%in%`)
    
    covCen<- read_csv("./data/covid_census.csv")
    #nys shape file
    nycshape4 <- zctas(cb = T, starts_with = c("100","101","102 ","103","104","107","110","112","113", "114", "116"))
    
 
    #joining file with population data
    zipsmap <- geo_join(nycshape4, 
                        covCen, 
                        by_sp = "GEOID10", 
                        by_df = "GEOID",
                        how = "left")
    
   
    #plot
    output$plot1 <- renderLeaflet({
      #  data <- histdata[seq_len(input$slider)]
        pal <- colorNumeric(
            palette = "Blues",
            domain = zipsmap@data$positive)
        # create labels for zipcodes
        labels <- 
            paste0(
                "Zip Code: ",
                zipsmap@data$GEOID10, "<br/>",
                "Positive Cases: ",
                round(zipsmap@data$positive, digits = 0)) %>%
            lapply(htmltools::HTML)
        
        zipsmap %>% leaflet %>% 
            # add base map
            addProviderTiles("CartoDB") %>% 
            # add zip codes
            addPolygons(fillColor = ~pal(positive),
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(weight = 2,
                                                     color = "#666",
                                                     dashArray = "",
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        label = labels) %>%
            # add legend
            addLegend(pal = pal, 
                      values = ~positive, 
                      opacity = 0.7, 
                      title = htmltools::HTML("Case Rate <br/>per 100k"),
                      position = "bottomright")
        
    })
}

shinyApp(ui, server)
