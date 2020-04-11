#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidycensus)
library(magrittr)
library(sf)
library(leaflet.extras)
library(htmlwidgets)
library(htmltools)
library(shiny)
library(shinydashboard)
library(tigris) 
library(leaflet)
library(tidyverse)
options(tigris_use_cache = TRUE)


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
        
        fluidRow(
          column(width = 9, 
                 box(
                   width = NULL, 
                   solidHeader = TRUE,
                   leafletOutput("plot2", height = 500)
                 ),
                 box(
                   width = NULL, 
                   solidHeader = TRUE,
                   leafletOutput("plot3", height = 500)
                 ),
                 box(
                   width = NULL, 
                   solidHeader = TRUE,
                   leafletOutput("plot4", height = 500)
                 )
          )
        ),
        
            
        )
    )


server <- function(input, output) {
    
   
   #################################### Data
   # NYC SHAPE FILE
    zips = c(10453, 10457, 10460,10458, 10467, 10468,10451, 10452, 10456,10454, 
             10455, 10459, 10474,10463, 10471,10466, 10469, 10470, 10475,10461, 
             10462,10464, 10465, 10472, 10473,11212, 11213, 11216, 11233, 11238, 
             11209, 11214, 11228,11204, 11218, 11219, 11230,11234, 11236, 11239,
             11223, 11224, 11229, 11235,11201, 11205, 11215, 11217, 11231,11203, 
             11210, 11225, 11226,11207, 11208,11211, 11222,11220, 11232,11206, 
             11221, 11237,10026, 10027, 10030, 10037, 10039,10001, 10011, 10018, 
             10019, 10020, 10036,10029, 10035,10010, 10016, 10017, 10022,10012, 
             10013, 10014,10004, 10005, 10006, 10007, 10038, 10280,10002, 10003, 
             10009,10021, 10028, 10044, 10065, 10075, 10128,10023, 10024, 10025,
             10031, 10032, 10033, 10034, 10040,11361, 11362, 11363, 11364,11354, 
             11355, 11356, 11357, 11358, 11359, 11360,11365, 11366, 11367,11412, 
             11423, 11432, 11433, 11434, 11435, 11436,11101, 11102, 11103, 11104, 
             11105, 11106,11374, 11375, 11379, 11385,11691, 11692, 11693, 11694, 
             11695, 11697,11004, 11005, 11411, 11413, 11422, 11426, 11427, 11428, 
             11429,11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421,11368, 
             11369, 11370, 11372, 11373, 11377, 11378,10302, 10303, 10310,10306, 
             10307, 10308, 10309, 10312,10301, 10304, 10305,10314) %>% 
      tibble() %>% 
      rename(zips = ".") %>% 
      mutate(zips = as.character(zips))
    
       nycshape <- zctas(cb = T, starts_with = c(zips$zips))
    
    #Census data
    covCen<- read_csv("./Covid-19_NYC/covid_census.csv")
    
    
    
    #joining file with population data
    zipsmap <- geo_join(nycshape, 
                        covCen, 
                        by_sp = "GEOID10", 
                        by_df = "GEOID",
                        how = "inner")
  
    
   
    #plot1 population case rate
    output$plot1 <- renderLeaflet({
      #  data <- histdata[seq_len(input$slider)]
        pal <- colorNumeric(
            palette = "Blues",
            domain = zipsmap@data$case_rate)
        # create labels for zipcodes
        labels <- 
            paste0(
                "Zip Code: ",
                zipsmap@data$GEOID10, "<br/>",
                "Case Rate: ",
                round(zipsmap@data$case_rate, digits = 0)) %>%
            lapply(htmltools::HTML)
        
        zipsmap %>% leaflet %>% 
            # add base map
            addProviderTiles("CartoDB") %>% 
            # add zip codes
            addPolygons(fillColor = ~pal(case_rate),
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
                      values = ~case_rate, 
                      opacity = 0.7, 
                      title = htmltools::HTML("Case Rate <br/>per 100k"),
                      position = "bottomright")
        
      
        
        
        
    })
    
  #################### Plot 2 
    
    #pallets
            
            #percent non hispanic black
    pal_BLack <- colorNumeric(
      palette = c("#EDEDED", "#FF94C0", "#FF2C54"), 0:100,
      domain = covCen$nhblack_pct)
    
            #percent hispanic
    pal_Hisp<- colorNumeric(c("#EDEDED", "#FF94C0", "#FF2C54"),0:100,
                            domain = covCen$hispanic_pct)
    
            #case rate
    pal_rate <- colorNumeric(
      palette = c("#EDEDED", "#94C6E7", "#4CB1DF"),
      domain = zipsmap$case_rate
    )
    
    #labels
    
    raceEthlabs <- lapply(seq(nrow(zipsmap)), function(i) {
      paste0(
        "Non Hispanic Black: %", 
        round(zipsmap@data[i, "nhblack_pct"], 0), "<br>",
        "Hispanic: %", 
        round(zipsmap@data[i, "hispanic_pct"], 0), "<br>",
        "Case rate: ", round(zipsmap@data[i, "case_rate"], 3)
      ) 
    })
    
        #plotting race ethnicity
    output$plot2 <- renderLeaflet({
      zipsmap %>% 
        leaflet(
          width = "100%",
          options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.5)
        ) %>% 
        # add base map
        addProviderTiles("CartoDB") %>% 
        # add zip codes
        addPolygons(group = "Non Hispanic Black",
                    fillColor = ~pal_BLack(x = nhblack_pct),
                    fillOpacity = 0.5,
                    stroke = F,
                    smoothFactor = 0.2) %>%
        addPolygons(group = "Hispanic/Lantinx",
                    fillColor = ~pal_Hisp(x = hispanic_pct),
                    fillOpacity = 0.5,
                    stroke = F,
                    smoothFactor = 0.2) %>%
        addPolygons(group = "Case Rate",
                    fillColor = ~pal_rate(case_rate),
                    fillOpacity = 0.5,
                    stroke = F,
                    smoothFactor = 0.2) %>% 
        
        addLayersControl(
          baseGroups = c("Non Hispanic Black", "Hispanic/Lantinx"),
          options = layersControlOptions(collapsed = FALSE),
          position = "topright"
        ) %>% 
        
        htmlwidgets::onRender("
    function(el, x) {
      this.on('baselayerchange', function(e) {
        e.layer.bringToBack();
      })
       
    }
   
  "
        ) %>% 
        
        addPolygons(
          label = lapply(raceEthlabs, htmltools::HTML),
          labelOptions = labelOptions(textsize = "12px"),
          fillColor = NA,
          fillOpacity = 0,
          color = "gray",
          weight = 1,
          opacity = 1,
          highlightOptions = highlightOptions(weight = 2)) %>% 
        
        addResetMapButton() %>% 
        
        addFullscreenControl() %>% 
        
        suspendScroll(sleepNote = F, sleepOpacity = 1) %>% 
        
        addControl(
            
          html = "<img src = 'https://sl4269.github.io/zipsmap_race_caserate.svg' width = '128' height = '128'>",
          position = "topright",
          className = "legend-bivar"
        )
    })
    
    ###########################plot 3
    #pallets
    
    #percent age over 65
    pal_pctageover65 <- colorNumeric(
      palette = c("#EDEDED", "#FF94C0", "#FF2C54"), 0:100,
      domain = covCen$age65_over_pct)
    
    #male over 65
    pal_maleover65<- colorNumeric(
      palette = c("#EDEDED", "#FF94C0", "#FF2C54"),
      domain = covCen$male_65over)
    
    #percent age over 65
    pal_femaleover65 <- colorNumeric(
      palette = c("#EDEDED", "#FF94C0", "#FF2C54"),
      domain = covCen$female_65over)
    
    #case rate
    pal_rate <- colorNumeric(
      palette = c("#EDEDED", "#94C6E7", "#4CB1DF"),
      domain = zipsmap$case_rate
    )
    
    agelabs <- lapply(seq(nrow(zipsmap)), function(i) {
      paste0(
        "Age Above 65: %", 
        round(zipsmap@data[i, "age65_over_pct"], 0), "<br>",
        "Male Above 65: ", 
        round(zipsmap@data[i, "male_65over"], 0), "<br>",
        "Female Above 65: ", 
        round(zipsmap@data[i, "female_65over"], 0), "<br>",
        "Case rate: ", round(zipsmap@data[i, "case_rate"], 3)
      ) 
    })
    
    #plotting age 
    output$plot3 <- renderLeaflet({
      zipsmap %>% 
        leaflet(
          width = "100%",
          options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.5)
        ) %>% 
        # add base map
        addProviderTiles("CartoDB") %>% 
        # add zip codes
        addPolygons(group = "Age Above 65 pct",
                    fillColor = ~pal_pctageover65(x = age65_over_pct),
                    fillOpacity = 0.5,
                    stroke = F,
                    smoothFactor = 0.2) %>%
        addPolygons(group = "Male Age Above 65",
                    fillColor = ~pal_maleover65(x = male_65over),
                    fillOpacity = 0.5,
                    stroke = F,
                    smoothFactor = 0.2) %>%
        addPolygons(group = "Female Age Above 65",
                    fillColor = ~pal_femaleover65(x = female_65over),
                    fillOpacity = 0.5,
                    stroke = F,
                    smoothFactor = 0.2) %>%
        addPolygons(group = "Case Rate",
                    fillColor = ~pal_rate(case_rate),
                    fillOpacity = 0.5,
                    stroke = F,
                    smoothFactor = 0.2) %>% 
        
        addLayersControl(
          baseGroups = c("Age Above 65 pct", "Male Age Above 65","Female Age Above 65"),
          options = layersControlOptions(collapsed = FALSE),
          position = "topright"
        ) %>% 
        
        htmlwidgets::onRender("
    function(el, x) {
      this.on('baselayerchange', function(e) {
        e.layer.bringToBack();
      })
       
    }
   
  "
        ) %>% 
        
        addPolygons(
          label = lapply(agelabs, htmltools::HTML),
          labelOptions = labelOptions(textsize = "12px"),
          fillColor = NA,
          fillOpacity = 0,
          color = "gray",
          weight = 1,
          opacity = 1,
          highlightOptions = highlightOptions(weight = 2)) %>% 
        
        addResetMapButton() %>% 
        
        addFullscreenControl() %>% 
        
        suspendScroll(sleepNote = F, sleepOpacity = 1) %>% 
        
        addControl(
          
          html = "<img src = 'https://sl4269.github.io/zipsmap_age_caserate.svg' width = '128' height = '128'>",
          position = "topright",
          className = "legend-bivar"
        )
    })
    
    
    ###########################plot 4
    #pallets
    
    #percent rented
    pal_pctrent <- colorNumeric(
      palette = c("#EDEDED", "#FF94C0", "#FF2C54"), 0:100,
      domain = covCen$rent_pct)
    
    #house average houshold size
    pal_aveHHS<- colorNumeric(
      palette = c("#EDEDED", "#FF94C0", "#FF2C54"),
      domain = covCen$hhsize_average)
    
    #renting houshold size
    pal_rentedHHS <- colorNumeric(
      palette = c("#EDEDED", "#FF94C0", "#FF2C54"),
      domain = covCen$hhsize_rented)
    
    #owning houshold size
    pal_ownedHHS <- colorNumeric(
      palette = c("#EDEDED", "#FF94C0", "#FF2C54"),
      domain = covCen$hhsize_owned)
    
    #case rate
    pal_rate <- colorNumeric(
      palette = c("#EDEDED", "#94C6E7", "#4CB1DF"),
      domain = zipsmap$case_rate
    )
    
   housinglabs <- lapply(seq(nrow(zipsmap)), function(i) {
      paste0(
        "Rent: %", 
        round(zipsmap@data[i, "rent_pct"], 0), "<br>",
        "House Hold Avg Size: ", 
        round(zipsmap@data[i, "hhsize_average"], 0), "<br>",
        "House Hold Size (Rent): ", 
        round(zipsmap@data[i, "hhsize_rented"], 0), "<br>",
        "House Hold Size (Owned): ", 
        round(zipsmap@data[i, "hhsize_owned"], 0), "<br>",
        "Case rate: ", round(zipsmap@data[i, "case_rate"], 3)
      ) 
    })
    
    #plotting age 
    output$plot4 <- renderLeaflet({
      zipsmap %>% 
        leaflet(
          width = "100%",
          options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.5)
        ) %>% 
        # add base map
        addProviderTiles("CartoDB") %>% 
        # add zip codes
        addPolygons(group = "Rent Percent",
                    fillColor = ~pal_pctrent(x = rent_pct),
                    fillOpacity = 0.5,
                    stroke = F,
                    smoothFactor = 0.2) %>%
        addPolygons(group = "Average Household Size",
                    fillColor = ~pal_aveHHS(x = hhsize_average),
                    fillOpacity = 0.5,
                    stroke = F,
                    smoothFactor = 0.2) %>%
        addPolygons(group = "Houshold Size of renters",
                    fillColor = ~pal_rentedHHS(x = hhsize_rented),
                    fillOpacity = 0.5,
                    stroke = F,
                    smoothFactor = 0.2) %>%
        addPolygons(group = "Houshold Size of owners",
                    fillColor = ~pal_ownedHHS(x = hhsize_owned),
                    fillOpacity = 0.5,
                    stroke = F,
                    smoothFactor = 0.2) %>%
        addPolygons(group = "Case Rate",
                    fillColor = ~pal_rate(case_rate),
                    fillOpacity = 0.5,
                    stroke = F,
                    smoothFactor = 0.2) %>% 
        
        addLayersControl(
          baseGroups = c("Rent Percent", "Average Household Size","Houshold Size of renters","Houshold Size of owners"),
          options = layersControlOptions(collapsed = FALSE),
          position = "topright"
        ) %>% 
        
        htmlwidgets::onRender("
    function(el, x) {
      this.on('baselayerchange', function(e) {
        e.layer.bringToBack();
      })
       
    }
   
  "
        ) %>% 
        
        addPolygons(
          label = lapply(housinglabs, htmltools::HTML),
          labelOptions = labelOptions(textsize = "12px"),
          fillColor = NA,
          fillOpacity = 0,
          color = "gray",
          weight = 1,
          opacity = 1,
          highlightOptions = highlightOptions(weight = 2)) %>% 
        
        addResetMapButton() %>% 
        
        addFullscreenControl() %>% 
        
        suspendScroll(sleepNote = F, sleepOpacity = 1) %>% 
        
        addControl(
          
          html = "<img src = 'https://sl4269.github.io/zipsmap_housing_caserate.svg' width = '128' height = '128'>",
          position = "topright",
          className = "legend-bivar"
        )
    })
}

shinyApp(ui, server)

runApp("Covid-19_NYC")
