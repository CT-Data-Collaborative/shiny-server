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
    observeEvent(input$do4, {
        updateTabsetPanel(session, "tabs",
                          selected = "profiles")      
    })    
    
   output$indicator_table <- renderDataTable({
     indicator_csv       
   }, filter='top', rownames=F, 
      options = list(autoWidth = TRUE, columnDefs = list(list(width = '200px', targets = "_all")))
   )
   
    output$result <- renderText({
      paste("You chose", input$select)
    })
   
  # output$outputButton <- downloadHandler(
  #   filename <- "About-the-SEOW.pdf", 
  #   content <- function(file) {
  #     file.copy("https://github.com/CT-Data-Collaborative/dataset-tools/raw/master/cpes/www/About-the-SEOW.pdf", "About-the-SEOW.pdf")
  #     library(RCurl)
  #     x <- getURL("https://raw.github.com/CT-Data-Collaborative/dataset-tools/raw/master/cpes/www/About-the-SEOW.pdf")
  #     y <- download.file(x, "About-the-SEOW.pdf", method = "curl")
  #     download.file("https://www.rstudio.com/products/shiny/download-server/", "/home/jdaly/Downloads/test.pdf")
  #   }
  # )
  
  
#   output$downloadData <- downloadHandler(
# 
#     # This function returns a string which tells the client
#     # browser what name to use when saving the file.
#     filename = function() {
# 		  paste(input$dataset, input$filetype, sep = ".")
# 	  },
# 
#     # This function should write data to a file given to it by
#     # the argument 'file'.
#     content = function(file) {
#       sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
# 
#       # Write to a file specified by the 'file' argument
#       write.table(datasetInput(), file, sep = sep,
#         row.names = FALSE)
#     }
#   )
  
  
  
  
})


