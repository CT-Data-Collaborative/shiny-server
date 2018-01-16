
library(shiny)
library(shinyBS)
library(shinyLP)
library(shinydashboard)
# library(plotly)
# library(RCurl)
# library(tidyr)
# library(DT)
# library(ggplot2)
# library(datapkg)
# library(ggthemes)
# library(stringr)
library(shinythemes)
# library(knitr)
# library(kableExtra)

#source('./scripts/read_in_data.R')
#indicator_csv <- read.csv("indicators_for_table.csv", stringsAsFactors = F, header=T, check.names=F)

#sources <- unique(indicator_csv$Source)

# Define UI for application
shinyUI(
  fluidPage(
    title = "Connecticut SEOW Data Portal",
      # tags$head(
      #   #tags$link(href = "style.css", rel = "stylesheet"),
      #   #tags$link(rel = "shortcut icon", type="image/x-icon", href="https://raw.githubusercontent.com/CT-Data-Collaborative/shiny-server/master/images/favicon.ico")
      # ),
      # div(id = "header",
      #   div(id = "title",
      #     "Connecticut SEOW Data Portal" 
      #   ),
      #   div(id = "subtitle",
      #     HTML("<br>Bringing together Connecticut’s epidemiological data in support of a comprehensive <br>
      #                         public health approach to substance abuse prevention and health promotion.")
      #   )
      # ),
      navbarPage(id="tabs",
        title=div(""),
        #inverse = F, # for diff color view
        #theme = shinytheme("lumen"),
        tabPanel(
           # div(icon("home"), "Home"),
            fluidRow(
              column(12, panel_div(class_type = "primary",
                              panel_title = "",
                              content = "The SEOW Data Portal, developed with support from the Department of Mental Health 
                              and Addiction Services (DMHAS), is a collaborative effort of the UCONN Health Center for 
                              Prevention Evaluation and Statistics (CPES), the State Epidemiological Outcomes Workgroup (SEOW) 
                              and the Connecticut Data Collaborative."))
            )
        #     tabItem("navigation",
        #         fluidRow(
        #           box(width=3,
        #               align="center",
        #               icon("bar-chart", "fa-5x"),
        #               h1("Indicators"),
        #               h5("Search through indicators by Substance, Source, Gender and More"),
        #               actionButton(inputId = "do1", 
        #                            label = "View Indicators", 
        #                            icon = icon("arrow-circle-right"),
        #                            style = "color: black; 
        #                                     background-color: #87CEFF; 
        #                                     position: relative; 
        #                                     text-align:center;
        #                                     border-radius: 6px;
        #                                     border-width: 2px")
        #           ),
        #           box(width=3, align="center",
        #               icon("table", "fa-5x"),
        #               h1("Data Sets"),
        #               h5("Search through data sets"),
        #               actionButton(inputId = "do2", label = "View Data Sets", icon = icon("arrow-circle-right"), 
        #                            style = "color: black; 
        #                                     background-color: #87CEFF; 
        #                                     position: relative; 
        #                                     text-align:center;
        #                                     border-radius: 6px;
        #                                     border-width: 2px")
        #           ),
        #           box(width=3, align="center",
        #               icon("info-circle", "fa-5x"),
        #               h1("Resources"),
        #               h5("Explore additional resources like reports and fact sheets."),
        #               actionButton(inputId = "do3", label = "View Resources", icon = icon("arrow-circle-right"),
        #                            style = "color: black; 
        #                                     background-color: #87CEFF; 
        #                                     position: relative; 
        #                                     text-align:center;
        #                                     border-radius: 6px;
        #                                     border-width: 2px")
        #           ), 
        #           box(width=3, align="center",
        #               icon("user-md", "fa-5x"),
        #               h1("Epidemiological Profiles"),
        #               h5("Explore statewide profiles"),
        #               actionButton(inputId = "do4", label = "View Profiles", icon = icon("arrow-circle-right"),
        #                            style = "color: black; 
        #                                     background-color: #87CEFF; 
        #                                     position: relative; 
        #                                     text-align:center;
        #                                     border-radius: 6px;
        #                                     border-width: 2px")
        #           )
        #         ),
        #         fluidRow(
        #           box(width=12,
        #             HTML('<br>'),
        #             HTML('<hr>'),
        #             h3("Partners"),
        #             align="center"
        #             #img(src="./www/LOGOtransparentcolor.png", width=100), 
        #             #img(src="./www/uconn-health-wordmark-stacked-blue.png", width=100), 
        #             #img(src="./www/ctdata-logo.png", width=100)
        #           )
        #         )
        # ), 
        # tags$footer(HTML(paste("Powered by the Connecticut Data Collaborative", "Copyright © 2018", sep="<br/>")), 
        #             align = "center", 
        #             style = "position:absolute; /* sits at bottom */
        #                      width:96%;
        #                      height:60px;   /* Height of the footer */
        #                      color: white;
        #                      padding: 10px;
        #                      background-color: #3c8dbc;
        #                      z-index: 1000;") #bottom:0;
      )
      # tabPanel(value="indicators",
      #   div(icon("bar-chart"), "Indicators"),  
      #   dashboardPage(
      #     dashboardHeader(disable=T),
      #     dashboardSidebar(width=250),
      #     dashboardBody(
      #      # dataTableOutput("indicator_table")
      #     )
      #   ),
      #   tags$footer(HTML(paste("Powered by the Connecticut Data Collaborative", "Copyright © 2018", sep="<br/>")), 
      #               align = "center", 
      #               style = "position:absolute; /* sits at bottom */
      #                        width:96%;
      #                        height:60px;   /* Height of the footer */
      #                        color: white;
      #                        padding: 10px;
      #                        background-color: #3c8dbc;
      #                        z-index: 1000;") #bottom:0;
      # ),
      # tabPanel(
      # div(icon("table"), "Data Sets"), value="datasets", 
      #   dashboardPage(
      #     dashboardHeader(disable=T),
      #     dashboardSidebar(
      #       tags$head(
      #         tags$style("ol {columns: 2;
      #                         -webkit-columns: 2;
      #                         -moz-columns: 2;},
      #                     li {list-style-type: none;
      #                         /*width: 100px;*/
      #                         overflow-x: auto; /* change to hidden if that's what you want */
      #                         float: left;
      #                         margin-right: 20px;},
      #                     #region_text{color: black;
      #                                  font-size: 20px;
      #                                  font-style: bold;}",
      #                     HTML(".skin-blue .main-sidebar {
      #                                 color: #000000;
      #                                 background-color: #ffffff;}",
      #                          ".selectize-input {
      #                                 color: #000000;
      #                                 font-size: 14px;}
      #                          .selectize-dropdown {
      #                                 font-size: 14px; }"
      #                     )
      #         ),
      #         tags$script(HTML('$(document).ready(function() {
      #                                       $("header").find("nav").append(\'<div class="myClass"> Text Here </div>\');
      #                                       })
      #                                       '))
      #      ),
      #       sidebarPanel(
      #         conditionalPanel(
      #          condition="input.tabselected==2",
      #           fluidRow(
      #             column(12, div(style="padding: 0px 0px",
      #                 sidebarMenu(
      #                   #selectInput("select",
      #                  # label = HTML('<h3 style="color:black;">Select Source</h3>'),
      #                   #choices = sources)
      #                   )
      #                 )
      #              )
      #           )
      #         ),
      #         width=300,
      #           fluidRow(
      #             column(12, div(style="padding: 0px 0px",
      #                 sidebarMenu(
      #                   #selectInput("select", 
      #                   #label = HTML('<h4>Source</h4>'),
      #                   #choices = sources, selected="YRBS")
      #                   )
      #                 )
      #              )
      #           )
      #       ),  
      #       textOutput("result")
      #     ), #end of side bar
      #     dashboardBody(
      #       tags$header(HTML(paste("RAW DATA", "Explore all our datasets in raw format", sep="<br/>")), 
      #               align = "center", 
      #               style = "width:100%;
      #                        position:absolute; /* sits at bottom */
      #                        height:60px;   /* Height of the header */
      #                        color: black;
      #                        padding: 10px;
      #                        background-color: gray;
      #                        z-index: 1000;")
      #   #
      #   #/*bottom:0;*/
      #     )
      #   ),
      #   tags$footer(HTML(paste("Powered by the Connecticut Data Collaborative", "Copyright © 2018", sep="<br/>")), 
      #               align = "center", 
      #               style = "position:absolute; /* sits at bottom */
      #                        width:96%;
      #                        height:60px;   /* Height of the footer */
      #                        color: white;
      #                        padding: 10px;
      #                        background-color: #3c8dbc;
      #                        z-index: 1000;") #bottom:0;
      # )
    # tabPanel(
    #   div(icon("info-circle"), "Resources"), value="resources", 
    #     fluidRow(
    #     div(id = "resourcepage",
    #       div(id = "urls",
    #              a("Learn about the SEOW", href="https://github.com/CT-Data-Collaborative/dataset-tools/raw/master/cpes/www/About-the-SEOW.pdf"), 
    #              HTML("<br>"),
    #              "The State Epidemiological Outcomes Workgroup (SEOW) is a collaborative group of State
    #              agency representatives and key stakeholders committed to the identification, sharing and use of
    #              data to improve substance abuse prevention and mental health promotion, and behavioral health in
    #              general.", 
    #              HTML("<hr>"),
    #              HTML("SEOW Membership Roster"),
    #              HTML("<hr>"),
    #              a("Learn more about UCONN Health Department of Community Medicine and Healthcare", href="https://health.uconn.edu/community-medicine/programs/health-services-research-unit/"),
    #              HTML("<br>"),                 
    #              "The mission of the UCONN Health Department of Community Medicine and Healthcare is to provide education, research, 
    #              and service to the University and the State of Connecticut and the global public health community. ", 
    #              HTML("<hr>"), 
    #              a("Learn more about DMHAS Prevention and Health Promotion Division", href="http://www.ct.gov/dmhas/cwp/view.asp?a=2912&Q=335152&dmhasNav=| "),
    #              HTML("<br>"),
    #              "The Department of Mental Health and Addiction Services' (DMHAS) prevention system is designed to promote the overall 
    #              health and wellness of individuals and communities by 
    #              preventing or delaying substance use.",
    #              HTML("<hr>"),
    #              a("Connecticut Data Collaborative", href="http://ctdata.org/"),
    #              HTML("<br>"),
    #              "Striving for informed decision-making across Connecticut, we empower an ecosystem of data users by democratizing 
    #              access to public data and building data literacy.",
    #              HTML("<hr>"),
    #              a("CT Open Data Portal", href="https://data.ct.gov/"),
    #              HTML("<hr>"),
    #              a("NSDUH", href="https://www.samhsa.gov/data/population-data-nsduh/reports?tab=38"),
    #              HTML("<hr>"),
    #              a("Connecticut Crash Data Repository", href="https://www.ctcrash.uconn.edu/"),
    #              HTML("<hr>"),
    #              a("CT State Data Center", href="https://ctsdc.uconn.edu/connecticut_census_data/"),
    #              HTML("<hr>"),
    #              a("Drug Enforcement Administration website", href="https://www.dea.gov/resource-center/statistics.shtml"),
    #              HTML("<hr>"),
    #              a("CT Suicide Prevention website", href="http://www.preventsuicidect.org/"),
    #              HTML("<hr>")
    #       )
    #     )
    #     ),
    #     # dashboardPage(
    #     #   dashboardHeader(disable=T),
    #     #   dashboardSidebar(disable=T),
    #     #   dashboardBody(disable=T)
    #     # ),
    #     tags$footer(HTML(paste("Powered by the Connecticut Data Collaborative", "Copyright © 2018", sep="<br/>")), 
    #                 align = "center", 
    #                 style = "position:absolute; /* sits at bottom */
    #                          width:96%;
    #                          height:60px;   /* Height of the footer */
    #                          color: white;
    #                          padding: 10px;
    #                          background-color: #3c8dbc;
    #                          z-index: 1000;") #bottom:0;
    #   ) 
    # tabPanel(
    #   div(icon("user-md"), "Epidemiological Profiles"), value="profiles", 
    #     fluidRow(
    #              HTML("Statewide Epidemiological Profiles by Substance"), HTML("<br>"),
    #              a("Alcohol", href="https://github.com/CT-Data-Collaborative/dataset-tools/raw/master/cpes/www/2017%20Alcohol%20Profile.docx"), HTML("<br>"),
    #              a("Cocaine", href="https://github.com/CT-Data-Collaborative/dataset-tools/raw/master/cpes/www/2017%20Cocaine%20Profile.docx"), HTML("<br>"),
    #              a("Heroin", href="https://github.com/CT-Data-Collaborative/dataset-tools/raw/master/cpes/www/2017%20Heroin%20Profile.docx"), HTML("<br>"),
    #              a("Marijuana", href="https://github.com/CT-Data-Collaborative/dataset-tools/raw/master/cpes/www/2017%20Marijuana%20Profile.docx"), HTML("<br>"),
    #              a("Opioid", href="https://github.com/CT-Data-Collaborative/dataset-tools/raw/master/cpes/www/2017%20Opioid%20Profile.docx"), HTML("<br>"),
    #              a("Prescription Drug", href="https://github.com/CT-Data-Collaborative/dataset-tools/raw/master/cpes/www/2017%20Prescription%20Drug%20Profile.docx"), HTML("<br>"),
    #              a("Tobacco and ENDS", href="https://github.com/CT-Data-Collaborative/dataset-tools/raw/master/cpes/www/2017%20Tobacco%20and%20ENDS%20Profile.docx"),
    #              HTML("<hr>"),
    #              HTML("Comprehensive Statewide Epidemiological Profile"),
    #              HTML("<hr>"),
    #              HTML("Indicator definitions"),
    #              HTML("<hr>"),
    #              HTML("Statewide Mental Health Epidemiological Profiles"),
    #              HTML("<hr>")
    #     ),
    #       tags$footer(HTML(paste("Powered by the Connecticut Data Collaborative", "Copyright © 2018", sep="<br/>")), 
    #                 align = "center", 
    #                 style = "position:absolute; /* sits at bottom */
    #                          width:96%;
    #                          height:60px;   /* Height of the footer */
    #                          color: white;
    #                          padding: 10px;
    #                          background-color: #3c8dbc;
    #                          z-index: 1000;") #bottom:0;
    # 
    #   )
    )
  ) 
)

