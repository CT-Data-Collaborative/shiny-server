#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# Define server logic
shinyServer(function(input, output, session) {
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
    
   output$indicator_table <- renderDataTable({
     indicator_csv       
   }, filter='top', rownames=F, 
      options = list(autoWidth = TRUE, columnDefs = list(list(width = '200px', targets = "_all")))
   )
   
    output$result <- renderText({
      paste("You chose", input$select)
    })
   
  # # observe({ 
  #   url <- ("http://cpes.ctdata.org.s3-website-us-east-1.amazonaws.com")
  # # })
  # output$frame <- renderUI({
  #   my_test <- tags$iframe(src=url) #, height=600, width=535
  #   print(my_test)
  #   my_test
  # })
   
   
   
   
   
})


