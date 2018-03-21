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

source('./scripts/read_in_data.R')

# Define UI for application
shinyUI(
  fluidPage(
    title = "Connecticut SEOW Data Portal",
      tags$head(
        tags$link(href = "style.css", rel = "stylesheet"),
        tags$link(rel = "shortcut icon", type="image/x-icon", href="https://raw.githubusercontent.com/CT-Data-Collaborative/shiny-server/master/images/favicon.ico")
      ),
      div(id = "header",
        div(id = "title",
          "Connecticut SEOW Data Portal" 
        ),
        div(id = "subtitle",
          HTML("<br>Bringing together Connecticut’s epidemiological data in support of a comprehensive <br>
                              public health approach to substance abuse prevention and health promotion.")
        )
      ),
      navbarPage(id="tabs",
        title=div(""),
        inverse = F, # for diff color view
        theme = shinytheme("lumen"),
        tabPanel(
            div(icon("home"), "Home"),
            fluidRow(
              column(12, panel_div(class_type = "primary",
                              panel_title = "",
                              content = "The SEOW Data Portal, developed with support from the Department of Mental Health 
                              and Addiction Services (DMHAS), is a collaborative effort of the UCONN Health Center for 
                              Prevention Evaluation and Statistics (CPES), the State Epidemiological Outcomes Workgroup (SEOW) 
                              and the Connecticut Data Collaborative."))
            ), 
            tabItem("navigation",
                fluidRow(
                  box(width=3,
                      align="center",
                      icon("bar-chart", "fa-5x"),
                      h1("Indicators"),
                      h5("Search through indicators by Source, Geography Level, Priority Problem and More"),
                      actionButton(inputId = "do1", 
                                   label = "View Indicators", 
                                   icon = icon("arrow-circle-right"),
                                   style = "color: white; 
                                            background-color: #0e005f; 
                                            position: relative; 
                                            text-align:center;
                                            border-radius: 6px;
                                            border-width: 2px")
                  ),
                  box(width=3, align="center",
                      icon("table", "fa-5x"),
                      h1("Data Sets"),
                      h5("Search through data sets by Source, Geography Level, Priority Problem and More"),
                      actionButton(inputId = "do2", label = "View Data Sets", icon = icon("arrow-circle-right"), 
                                   style = "color: white; 
                                            background-color: #0e005f; 
                                            position: relative; 
                                            text-align:center;
                                            border-radius: 6px;
                                            border-width: 2px")
                  ),
                  box(width=3, align="center",
                      icon("info-circle", "fa-5x"),
                      h1("Resources"),
                      h5("Explore additional resources like reports and fact sheets."),
                      actionButton(inputId = "do3", label = "View Resources", icon = icon("arrow-circle-right"),
                                   style = "color: white; 
                                            background-color: #0e005f; 
                                            position: relative; 
                                            text-align:center;
                                            border-radius: 6px;
                                            border-width: 2px")
                  ), 
                  box(width=3, align="center",
                      icon("user-md", "fa-5x"),
                      h1("Epidemiological Profiles"),
                      h5("Explore statewide profiles"),
                      actionButton(inputId = "do4", label = "View Profiles", icon = icon("arrow-circle-right"),
                                   style = "color: white; 
                                            background-color: #0e005f; 
                                            position: relative; 
                                            text-align:center;
                                            border-radius: 6px;
                                            border-width: 2px")
                  )
                ),
                fluidRow(
                  box(width=12,
                    HTML('<br>'),
                    HTML('<hr>'),
                    h3("Partners"),
                    align="center",
                    img(src='CPES logo final 022818.PNG', width=100),
                    HTML("<a href='http://www.ct.gov/dmhas/cwp/view.asp?a=2912&Q=335152&dmhasNav=%7C' target='_blank'><img border='0' alt='dmhas' src='LOGOtransparentcolor.png' width='100'></a>"),
                    HTML("<a href='https://health.uconn.edu/community-medicine/programs/health-services-research-unit/' target='_blank'><img border='0' alt='uconn' src='uconn-health-wordmark-stacked-blue.png' width='100'></a>"),
                    HTML("<a href='http://ctdata.org/' target='_blank'><img border='0' alt='ctdata' src='ctdata-logo.png' width='100'></a>")
                  )
                )
        ), 
        tags$footer(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                      HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                           "Copyright © 2018 CT Data Collaborative", 
                           sep="<br/>")), 
                      align = "center"
          )
      ),
      tabPanel(value="indicators",
        div(icon("bar-chart"), "Indicators"),  
        dashboardPage(
          dashboardHeader(disable=T),
          dashboardSidebar(disable=T),
          dashboardBody(
                fluidRow(
                  column(12, 
                         style = "font-size:80%",
                         dataTableOutput(outputId = "indicator_table") 
                  )
                )
          )
        ),
        tags$footer(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                      HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                           "Copyright © 2018 CT Data Collaborative", 
                           sep="<br/>")), 
                      align = "center"
          )
      ),
      tabPanel(
      div(icon("table"), "Data Sets"), value="datasets", 
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
            fluidRow(
              column(12,
                     style = "font-size:80%", 
                     dataTableOutput("dataset_table")
              )
            )
          )
        ),
        tags$footer(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                      HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                           "Copyright © 2018 CT Data Collaborative", 
                           sep="<br/>")), 
                      align = "center"
          )
      ),
    tabPanel(
      div(icon("info-circle"), "Resources"), value="resources", id = "resourcepage",
        dashboardPage(
          dashboardHeader(disable=T),
          dashboardSidebar(disable=T),
          dashboardBody(   
            fluidRow(
              column(9, offset = 1,
                      HTML("<b><a href='https://s3.amazonaws.com/cpes-ctdata/reports/About+the+SEOW.doc' target='_blank'><font color='#0e005f'><b>
                            The State Epidemiological Outcomes Workgroup (SEOW)</b></font></a><br></b>
                            <ul>The SEOW is a collaborative group of State
                            agency representatives and key stakeholders committed to the identification, sharing and use of
                            data to improve substance abuse prevention and mental health promotion, and behavioral health in
                            general. </ul>")
              )
            ),
            fluidRow(
              column(9, offset = 1,
                HTML("<b><a href='https://health.uconn.edu/community-medicine/programs/health-services-research-unit/' target='_blank'><font color='#0e005f'><b>
                            UCONN Health Department of Community Medicine and Healthcare</b></font></a><br></b>
                            <ul>The department's mission is to provide education, research, 
                            and service to the University and the State of Connecticut and the global public health community.</ul>")
              )
            ),
            fluidRow(
              column(9, offset = 1,
                HTML("<b><a href='http://www.ct.gov/dmhas/cwp/view.asp?a=2912&Q=335152&dmhasNav=%7C' target='_blank'><font color='#0e005f'><b>
                            The Department of Mental Health and Addiction Services (DMHAS)</b></font></a><br></b>
                            <ul>The DMHAS prevention system is designed to promote the overall 
                            health and wellness of individuals and communities by 
                            preventing or delaying substance use.</ul>")
              )
            ),
            fluidRow(
              column(9, offset = 1,
                HTML("<b><a href='http://ctdata.org/' target='_blank'><font color='#0e005f'><b>
                            Connecticut Data Collaborative</b></font></a><br></b>
                            <ul>Striving for informed decision-making across Connecticut, we empower an ecosystem of data users by democratizing 
                            access to public data and building data literacy.</ul>")
              )
            ),
            fluidRow(
              column(9, offset = 1,
                HTML("<b>SEOW Membership Roster</b><br>")
              )
            ),  
            fluidRow(
              column(9, offset = 1,
                HTML("<b><a href='https://data.ct.gov/' target='_blank'><font color='#0e005f'><b>
                            CT Open Data Portal</b></font></a><br></b>
                            <ul></ul>")
              )
            ),            
            fluidRow(
              column(9, offset = 1,
                HTML("<b><a href='https://www.samhsa.gov/data/population-data-nsduh/reports?tab=38' target='_blank'><font color='#0e005f'><b>
                            National Survey on Drug Use and Health (NSDUH)</b></font></a><br></b>
                            <ul></ul>")
              )
            ),
            fluidRow(
              column(9, offset = 1,
                HTML("<b><a href='https://www.ctcrash.uconn.edu/' target='_blank'><font color='#0e005f'><b>
                            Connecticut Crash Data Repository</b></font></a><br></b>
                            <ul></ul>")
              )
            ), 
            fluidRow(
              column(9, offset = 1,
                HTML("<b><a href='https://ctsdc.uconn.edu/connecticut_census_data/' target='_blank'><font color='#0e005f'><b>
                            CT State Data Center</b></font></a><br></b>
                            <ul></ul>")
              )
            ), 
            fluidRow(
              column(9, offset = 1,
                HTML("<b><a href='https://www.dea.gov/resource-center/statistics.shtml' target='_blank'><font color='#0e005f'><b>
                            Drug Enforcement Administration website</b></font></a><br></b>
                            <ul></ul>")
              )
            ), 
            fluidRow(
              column(9, offset = 1,
                HTML("<b><a href='http://www.preventsuicidect.org/' target='_blank'><font color='#0e005f'><b>
                            CT Suicide Prevention website</b></font></a><br></b>
                            <ul></ul>")
              )
            ) 
         )
        ),
        tags$footer(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                      HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                           "Copyright © 2018 CT Data Collaborative", 
                           sep="<br/>")), 
                      align = "center"
          )
      ), 
    tabPanel(
      div(icon("user-md"), "Epidemiological Profiles"), 
      value="profiles", 
        dashboardPage(
          dashboardHeader(disable=T),
          dashboardSidebar(disable=T),
          dashboardBody(   
            fluidRow(
              column(9, offset=1,
                HTML("<b>Statewide Epidemiological Profiles by Substance</b><hr>"),
                HTML("<button class = 'btn btn-primary' style='color:#0e005f;background-color:white;border:3pt'>
                    <a href='https://s3.amazonaws.com/cpes-ctdata/reports/2017+Alcohol+Profile.docx'><font color='#0e005f'>Alcohol</font></a>
                    </button>
                    <button class = 'btn btn-primary' style='color:#0e005f;background-color:white;border:3pt'>
                    <a href='https://s3.amazonaws.com/cpes-ctdata/reports/2017+Cocaine+Profile.docx'><font color='#0e005f'>Cocaine</font></a>
                    </button>
                    <button class = 'btn btn-primary' style='color:#0e005f;background-color:white;border:3pt'>
                    <a href='https://s3.amazonaws.com/cpes-ctdata/reports/2017+Heroin+Profile.docx'><font color='#0e005f'>Heroin</font></a>
                    </button>
                    <button class = 'btn btn-primary' style='color:#0e005f;background-color:white;border:3pt'>
                    <a href='https://s3.amazonaws.com/cpes-ctdata/reports/2017+Marijuana+Profile.docx'><font color='#0e005f'>Marijuana</font></a>
                    </button>
                    <button class = 'btn btn-primary' style='color:#0e005f;background-color:white;border:3pt'>
                    <a href='https://s3.amazonaws.com/cpes-ctdata/reports/2017+Opioid+Profile.docx'><font color='#0e005f'>Opioid</font></a>
                    </button>
                    <button class = 'btn btn-primary' style='color:#0e005f;background-color:white;border:3pt'>
                    <a href='https://s3.amazonaws.com/cpes-ctdata/reports/2017+Prescription+Drug+Profile.docx'><font color='#0e005f'>Prescription Drug</font></a>
                    </button>
                    <button class = 'btn btn-primary' style='color:#0e005f;background-color:white;border:3pt'>
                    <a href='https://s3.amazonaws.com/cpes-ctdata/reports/2017+Tobacco+and+ENDS+Profile.docx'><font color='#0e005f'>Tobacco and ENDS</font></a>
                    </button>"))
            ),    
            HTML("<hr>"),
        fluidRow(
          column(9,  offset=1,
                 HTML("<b>Comprehensive Statewide Epidemiological Profile</b><hr>"),
                 HTML("<b>Statewide Mental Health Epidemiological Profiles</b><hr>"), 
                 HTML("<b>Indicator definitions</b><hr>"))
        ),
        tags$footer(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                      HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                           "Copyright © 2018 CT Data Collaborative", 
                           sep="<br/>")), 
                      align = "center"
          )

      )
    )
  ) 
  )
  )
)
