
library(shiny)
library(shinyBS)
library(shinyLP)
library(shinydashboard)
library(plotly)
library(RCurl)
library(tidyr)
library(DT)
library(ggplot2)
library(datapkg)
library(ggthemes)
library(stringr)
library(shinythemes)
library(knitr)
library(kableExtra)

#source('./scripts/read_in_data.R')
indicator_csv <- read.csv("indicators_for_table.csv", stringsAsFactors = F, header=T, check.names=F)


sources <- unique(indicator_csv$Source)

# Define UI for application
shinyUI(
  fluidPage(
    list(tags$head(HTML('<link rel="icon", href="connect.png", type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'", titlePanel(title="", windowTitle="CPES Dashboard")),
    navbarPage(id="tabs",
      title=div(""),
      inverse = F, # for diff color view
      theme = shinytheme("lumen"),

      tabPanel(title = "CPES Dashboard", value="cpesdashboard", icon = icon("home"),
        fluidRow(
          column(12, panel_div(class_type = "primary",
                              panel_title = "Center for Prevention Evaluation and Statistics (CPES) ",
                              content = "The CPES is responsible for designing and implementing data collection and management; 
                                         disseminating and utilizing epidemiological data for decisionmaking; and providing 
                                         technical assistance and training on evaluation-related tasks and topics."))
        ), 
        tabItem("navigation",
                fluidRow(
                  box(width=4,
                      align="center",
                      icon("bar-chart", "fa-5x"),
                      h1("Indicators"),
                      h5("Search through indicators by Substance, Source, Gender and More"),
                      actionButton(inputId = "do1", 
                                   label = "View Indicators", 
                                   icon = icon("arrow-circle-right"),
                                   style = "color: black; 
                                            background-color: #87CEFF; 
                                            position: relative; 
                                            text-align:center;
                                            border-radius: 6px;
                                            border-width: 2px")
                  ),
                  box(width=4, align="center",
                      icon("table", "fa-5x"),
                      h1("Data Sets"),
                      h5("Search through data sets"),
                      actionButton(inputId = "do2", label = "View Data Sets", icon = icon("arrow-circle-right"), 
                                   style = "color: black; 
                                            background-color: #87CEFF; 
                                            position: relative; 
                                            text-align:center;
                                            border-radius: 6px;
                                            border-width: 2px")
                  ),
                  box(width=4, align="center",
                      icon("info-circle", "fa-5x"),
                      h1("Resources"),
                      h5("Explore additional resources like reports and fact sheets."),
                      actionButton(inputId = "do3", label = "View Resources", icon = icon("arrow-circle-right"),
                                   style = "color: black; 
                                            background-color: #87CEFF; 
                                            position: relative; 
                                            text-align:center;
                                            border-radius: 6px;
                                            border-width: 2px")
                  )
                )
        ), 
        tags$footer(HTML(paste("Powered by the Connecticut Data Collaborative", "Copyright © 2017", sep="<br/>")), 
                    align = "center", 
                    style = "position:absolute; /* sits at bottom */
                             bottom:0;
                             width:96%;
                             height:60px;   /* Height of the footer */
                             color: white;
                             padding: 10px;
                             background-color: #3c8dbc;
                             z-index: 1000;")

      ),
      tabPanel(title = "Indicators", value="indicators", 
        fluidRow(
          column(12
                 #panel_div(class_type = "primary",
                      #         panel_title = "Directions",
                      #         content = "")
                 )
        ),
        dashboardPage(
          dashboardHeader(disable=T),
          dashboardSidebar(width=250),
          dashboardBody(
            dataTableOutput("indicator_table")
          )
        )
      ),
      tabPanel(title = "Data Sets", value="datasets", 
        dashboardPage(
          dashboardHeader(disable=T),
          dashboardSidebar(
            tags$head(
              tags$style("ol {columns: 2;
                              -webkit-columns: 2;
                              -moz-columns: 2;},
                          li {list-style-type: none;
                              /*width: 100px;*/
                              overflow-x: auto; /* change to hidden if that's what you want */
                              float: left;
                              margin-right: 20px;},
                          #region_text{color: black;
                                       font-size: 20px;
                                       font-style: bold;}",
                          HTML(".skin-blue .main-sidebar {
                                      color: #000000;
                                      background-color: #ffffff;}",
                               ".selectize-input {
                                      color: #000000;
                                      font-size: 14px;}
                               .selectize-dropdown {
                                      font-size: 14px; }"
                          )
              ),
              tags$script(HTML('$(document).ready(function() {
                                            $("header").find("nav").append(\'<div class="myClass"> Text Here </div>\');
                                            })
                                            '))
           ),
            sidebarPanel(
              conditionalPanel(
               condition="input.tabselected==2",
                fluidRow(
                  column(12, div(style="padding: 0px 0px",
                      sidebarMenu(
                        selectInput("select",
                        label = HTML('<h3 style="color:black;">Select Source</h3>'),
                        choices = sources)
                        )
                      )
                   )
                )
              ),
              width=300,
                fluidRow(
                  column(12, div(style="padding: 0px 0px",
                      sidebarMenu(
                        selectInput("select", 
                        label = HTML('<h4>Source</h4>'),
                        choices = sources, selected="YRBS")
                        )
                      )
                   )
                )
            ),  
            textOutput("result")
          ), #end of side bar
          dashboardBody(
            tags$header(HTML(paste("RAW DATA", "Explore all our datasets in raw format", sep="<br/>")), 
                    align = "center", 
                    style = "width:100%;
                             height:60px;   /* Height of the header */
                             color: black;
                             padding: 10px;
                             background-color: gray;
                             z-index: 1000;")
        #position:absolute; /* sits at bottom */
        #/*bottom:0;*/
          )
        )               
      ),
      tabPanel(title = "Resources", value="resources", 
        fluidRow(
          column(12
                 #panel_div(class_type = "primary",
                  #             panel_title = "Directions",
                  #             content = "")
                 )
        ),
        dashboardPage(
          dashboardHeader(disable=T),
          dashboardSidebar(disable=T),
          dashboardBody()
        )               
      )               
    )
  )
)
