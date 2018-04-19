#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

  createDataSetLink <- function(data, val) {
    sprintf('<a href="http://data.ctdata.org/visualization/%s" target="_blank">%s</a>', data, val)
  }
  
  createSourceLink <- function(url, val) {
    sprintf('<a href="%s" target="_blank">%s</a>', url, val)
  }
  
# Define server logic
shinyServer(function(input, output, session) {

    observeEvent(input$do0a, {
        updateTabsetPanel(session, "tabs",
                          selected = "home")
    })
    observeEvent(input$do0b, {
        updateTabsetPanel(session, "tabs",
                          selected = "home")
    })
    observeEvent(input$do0c, {
        updateTabsetPanel(session, "tabs",
                          selected = "home")
    })
    observeEvent(input$do0d, {
        updateTabsetPanel(session, "tabs",
                          selected = "home")
    })    
    observeEvent(input$do0e, {
        updateTabsetPanel(session, "tabs",
                          selected = "home")
    })    
    observeEvent(input$do0f, {
        updateTabsetPanel(session, "tabs",
                          selected = "home")
    })      
    observeEvent(input$do1, {
        updateTabsetPanel(session, "tabs",
                          selected = "indicators")
    })

    observeEvent(input$do2, {
        updateTabsetPanel(session, "tabs",
                          selected = "datasets")
    })
    observeEvent(input$do3, {
        updateTabsetPanel(session, "tabs",
                          selected = "resources")      
    })
    observeEvent(input$do4, {
        updateTabsetPanel(session, "tabs",
                          selected = "profiles")      
    }) 
    observeEvent(input$do5, {
        updateTabsetPanel(session, "tabs",
                          selected = "products")      
    })     
    observeEvent(input$do6, {
        updateTabsetPanel(session, "tabs",
                          selected = "news")      
    })
    
   output$indicator_table <- renderDataTable({
     my_table <- clean
     my_table <- my_table %>% 
       mutate(Indicator = ifelse(is.na(Link), Indicator, createSourceLink(Link, Indicator))) %>% 
       select(-Link)
     my_table
   }, #selection = 'single', #able to select only one row at a time
      escape=FALSE,
      filter='top', 
      rownames=F,   
      options=list(columnDefs = list(list(width = '15%', targets = c(0, 1))), 
                   initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
                       "}"),
                     searchHighlight = TRUE, 
                     scrollX=TRUE
      )
   )
   
   output$dataset_table <- renderDataTable({
     my_table <- dataset_table
     my_table$`Data Set` <- createDataSetLink(my_table$name, my_table$title)
     my_table$name <- NULL
     my_table$title <- NULL
     my_table <- my_table %>% select(Source, `Data Set`, Geography, Dimensions, `Measure Type`, `Priority Problem`, `Key Terms`)
     return(my_table)
   }, escape = FALSE, 
      filter='top', 
      rownames=F,
      options = list(columnDefs = list(list(Width=c("25%"), targets=list(1))), #set secific width of column 2
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
                       "}"),
                     searchHighlight = TRUE, 
                     scrollX=TRUE
                     )
   )
  
    output$result <- renderText({
      paste("You chose", input$select)
    })
})


