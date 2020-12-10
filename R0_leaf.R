library(dplyr)
library(tidycovid19)
library(lubridate)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(flipTime)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(shinycssloaders)
library(sf)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(tigris)
library(spdplyr)
library("ggdendro")
library("reshape2")
library(scales)
library(shinyalert)
library(shinybusy)
rm(list = ls())


latest = download_merged_data(silent = TRUE, cached = TRUE)
load("r0_predictions.rda")
world = readOGR(dsn="world", layer="World_Countries__Generalized_")

ui <- fluidPage(theme = shinytheme("sandstone"),
                
                titlePanel("COVID-19 R Estimator"),
                hr(),
                
                
                
                navbarPage("",
                           
                           tabPanel("Map",
                                    
                                    mainPanel(
                                      
                                      dateInput("date_end",
                                                label = "End of two week period to estimate R:",
                                                max(latest$date)),
                                      
                                      p("Click on the map to see the country-specific estimated R confidence intervals."),
                                      
                                      leafletOutput("view") %>% withSpinner(color="#a52a2a"),
                                      
                                      width = 12)),#End of Map tabpanel
                           
                           
                           
                           tabPanel("Assumptions",
                                    fluidPage(
                                      fluidRow(
                                        
                                        p(HTML("<p> This visualisation plots the raw number of cases in a selected country and calculates the R number for that period using the methods described in <a href = 'https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/1472-6947-12-147'>Obadia et al, BMC Medical Informatics and Decision Making, 2012</a>.<p> The method relies on an estimate of the Generation Time of the disease; this is the time from becoming infected with COVID-19 to the time of generating a secondary case. The estimated generation time distribution and its parameters have been taken from <a href = 'https://onlinelibrary.wiley.com/doi/full/10.1111/biom.13325'>Yuhao et al Biometrics, 2020</a>. The values can be changed by clicking the 'show extra options' button.<p> The R0 package allows for different methods to calculate the R value. We use the Sequential Bayes method which also provides a 95% confidence interval. Other methods can be selected in the extra options.<p> If there are large number of zero cases, or the date range is too large/small, the estimate may fail and an R0 number will not be shown.<p> Be aware that most of these methods have hidden assumptions (e.g. that the date range shows a period of exponential growth). If you are changing the method, we would recommend reading the above papers first to avoid mistaken readings."))
                                        
                                      )
                                    )
                                    
                           ) #End of tabPanels
                 )#End of navbar
                
                
) #End of UI

server <- function(input, output) {
  
  
  
  
  output$view <- renderLeaflet({
    
    data_use = latest %>% 
      group_by(country) %>% 
      mutate(cum_cases = ecdc_cases,
             cases = c(cum_cases[1], diff(ecdc_cases))) %>% 
      ungroup() %>% 
      dplyr::select(country,date,cum_cases,cases,population) %>% 
      filter(date >= input$date_end - 14, date <= input$date_end) %>% 
      na.omit()
    
    data_use$country[which(data_use$country == "Czechia")] <- "Czech Republic"
    r0_predictions$country[which(r0_predictions$country == "Czechia")] <- "Czech Republic"
    
    # COVID generation time
    estR0 = r0_predictions %>% 
      group_by(country) %>% 
      dplyr::mutate(n_rows = length(country) - 1) %>% 
      do( data.frame(., date = seq.Date(Sys.Date() - min(.$n_rows), Sys.Date(),  by = 1))) %>% 
      dplyr::filter(date == input$date_end)
    
    estR0 = estR0 %>% mutate(R_est = signif(pred, 3))
      
    
    df3 <- geo_join(world, estR0,"COUNTRY", "country")
    
    df3 <- df3 %>% dplyr::mutate(pop = paste0("Estimated R0: ",df3$R_est))
    
    pal <- colorNumeric(palette = c("white","gray","darkred"), domain = df3$R_est)
    
    popup_sb <- df3$pop
    
    leaflet() %>%
      addTiles()  %>% setView(-7.5959, 0, zoom = 1) %>%
      addPolygons(data = df3, fillColor = ~pal(df3$R_est), layerId= ~COUNTRY,
                  fillOpacity = 0.8,
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=popup_sb,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = df3$R_est, title = "R0 Estimates", opacity = 0.7) %>%
      leaflet.extras::addResetMapButton() %>% 
      leaflet.extras::addSearchOSM(options = searchOptions(collapsed = T,zoom = 9,hideMarkerOnCollapse = T, moveToLocation = FALSE,
                                                           autoCollapse =T))
    
    
    
  })
  
  
  observeEvent(input$view_shape_click,{#Plotting R plots for each region after clicking on the map


    output$plot2 <- renderPlotly({


      current_country <- input$view_shape_click$id
      date_max <- input$date_end
      
      
      latest_filter <- latest %>% 
        dplyr::filter(country == current_country) %>% 
        dplyr::mutate(cum_cases = ecdc_cases,
                      cases = c(cum_cases[1], diff(ecdc_cases))) %>%
        dplyr::select(date, cases, population) %>%
        dplyr::filter(date >= date_max - 14, date <= date_max) %>%
        na.omit()
      
      estR0 = r0_predictions %>%
        dplyr::filter(country == current_country) 
      
      n_dates <- seq.Date(Sys.Date() - nrow(estR0) + 1, Sys.Date(),  by = 1)
      
      estR0 = estR0 %>% 
        dplyr::mutate(date = n_dates) %>% 
        dplyr::filter(date == date_max)
      
      
      
      p = ggplot(data = latest_filter, aes(x = date, y = cases)) + 
        geom_point() + 
        labs(x = 'Date',
             y = 'Cases',
             title = paste('Cases in',current_country, 'from', 
                           format(input$date_end - 14, '%d-%b'), 'to',
                           format(input$date_end, '%d-%b'))) + 
        theme_bw() + 
        geom_smooth(se = FALSE)
      
      ggp <- ggplot_build(p)
      yrange = ggp$layout$panel_params[[1]]$y.range
      xrange = ggp$layout$panel_params[[1]]$x.range
      
      # Add the annotation
      a <- list(
        x = ggp$layout$panel_scales_x[[1]]$range$range[1],
        y = ggp$layout$panel_scales_y[[1]]$range$range[2],
        xref = "x",
        yref = "y",
        xanchor = 'left',
        showarrow = FALSE,
        font = list(size = 20)
      )
      
      #if(nrow(estR0) == 0 | any(data_use$cases < 10)) {
      if(nrow(estR0) == 0) {
        a$text = "R0 not estimated (bad case values or date range)"
        a$font = list(size = 14)
      } else {
        #if(input$R_method == "SB") {
        R_est = signif(estR0$pred, 3)
        R_low = signif(estR0$low, 3)
        R_high = signif(estR0$upp, 3)
        #} else {
        
        a$text = paste0("Estimated R = ", R_est,
                        ",  10-90 Quantile Interval: (", R_low,', ',
                        R_high, ')')
      }
      
      ggplotly(p) %>% layout(annotations = a)  


    })


  }) #End of Observation

  observeEvent(input$view_shape_click,{

    showModal(modalDialog(
      title = "",
      size = "l",
      footer = actionButton("close", "Close"),
      plotlyOutput("plot2") %>% withSpinner(color="#a52a2a") ))

  })


  observeEvent(input$close, { #Removing modal and erasing previous plot

    output$plot2 <- NULL
    removeModal()

  })

  
}

# Run the application
shinyApp(ui = ui, server = server)