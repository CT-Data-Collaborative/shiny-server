
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny) 
library(shinyBS)
library(shinyLP)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(RCurl)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(shinythemes)
library(knitr)
library(kableExtra)
library(rgeos)
library(maptools)
library(dplyr)
library(rgdal)
library(censusr)
library(RColorBrewer)
library(formattable)
library(DT)
library(shinyWidgets)


##Read in data
#######HEALTH####################################################################################################################################
source('./scripts/read_in_data.R')

#using this to add ctdata logo at top right
navbarPageWithText <- function(..., text) {
  navbar <- navbarPage(...)
  textEl <- tags$p(class = "navbar-text", text)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], textEl)
  navbar
}


# Define UI for application
shinyUI(
  fluidPage(
    title = "CONNECT Dashboard",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "shortcut icon", type="image/x-icon", href="https://raw.githubusercontent.com/CT-Data-Collaborative/shiny-server/master/images/favicon.ico"), 
      #Google analytics script
      includeScript("google-analytics.js"), 
      tags$style(".navbar-default  {
                    min-height:80px !important;
                    font-size: 20px;
                    background-color: #272869 !important;
                  }", 
                 ".navbar .navbar-text {
                    float: right;
                  }",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ), 
      tags$style(type="text/css", "body {padding-top: 70px;}")
    ),
    div(style="padding: 1px 0px; width: '100%'", titlePanel(title="", windowTitle="CONNECT Dashboard")),
    navbarPageWithText( #custom function
      position = "fixed-top",
      inverse=T, 
      text = HTML("<a href='http://ctdata.org/' target='_blank'><img border='0' alt='ctdata' src='logo2.png' width='115' height='50'></a>"),
      title=div(img(src="CONNECTing.png", height = "60px", style = "padding:0px 0px", align="left")),
      tabPanel(
        title = h4(style="text-align:center", "Home", icon("home")), 
            HTML("<h1>A Dashboard of Data Indicators About Your Community.</h1><p>Use this dashboard to explore data by Department of Children and Families (DCF) region and education data by district.</p><hr size = '50'>"),
        fluidRow(
          column(10, offset = 1, 
                 panel_div(class_type = "primary", 
                              panel_title = "About CONNECT",
                              content = HTML("<p>The Connecticut Network of Care Transformation (CONNECT) is a statewide initiative to create 
                              a partnership between families, state agencies, and service providers at the local, regional and state 
                              levels. That enhanced partnership supports children, youth, and families in accessing the services they 
                              need in a timely and effective manner through an integrated network of care.</p><p><b>CONNECT engages 
                              partners from each of the following child-serving sectors:</b></p><ul><li>Behavioral Health<li>Child Welfare
                              <li>Early Childhood<li>Juvenile Justice<li>Substance Abuse<li>Education<li>Advocacy groups<li>Families 
                              and children served</ul>")
                 )
          ),
          column(10, offset = 1, panel_div("info", 
                              "About Connecticut Data Collaborative", 
                              content = HTML("<p>Striving for informed decision-making across Connecticut, we empower an ecosystem of data users by democratizing 
                              access to public data and building data literacy. Visit <a href='http://ctdata.org/' target='_blank'> ctdata.org</a> for more info!</p>"))), 
          column(10, offset = 1, panel_div("primary", 
                              "Need Help? Email Us!",
                              content = HTML( "Ask a question or provide feedback to the <a href='mailto:info@ctdata.org?Subject=CONNECT%20Dashboard' target='_top'>Connecticut Data Collaborative</a>")))
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
        title = h4(style="text-align:center", "Dashboard", icon("cog")), 
       # fluidRow(
          # column(10, offset = 1, 
          #        bsCollapsePanel("Click Here for Directions", 
          #                        HTML('<ul><li>Use the pull down menu on the left to select the Region or statewide data you wish to explore.
          #                              <li>Click through the category tabs along the top to explore the various categories.
          #                              <li>Use the Education tab to select data by District.
          #                              <li>Town/District level data is available by clicking on the <font size="4" color="dodgerblue">Explore the Data</font> link.</li><ul/>'),
          #                        style="primary"
          #        )
          # ),
      #  ),
           dashboardPage(
             dashboardHeader(disable=T),
             dashboardSidebar(
               width=350, 
               tags$head(
                 tags$style(" ol {columns: 2;
                                 -webkit-columns: 3;
                                 -moz-columns: 3;},
                            li {list-style-type: none;
                                /*width: 100px;*/
                                overflow-x: auto; /* change to hidden if that's what you want */
                                float: left;
                                margin-right: 20px;
                                margin-left: 20px;},
                           #region_text{color: black;
                                        font-size: 20px;
                                        font-style: bold;},
                           #region_list {-moz-column-gap: 20;
                                       -moz-column-count: 3;
                                       -webkit-column-count: 3;
                                       -webkit-column-gap: 20;
                                       column-count: 3;
                                       column-gap: 20;
                                       font-size: 8px;}",
                           ".skin-blue .main-sidebar {
                                       color: #000000;
                                       background-color: #ffffff;}",
                                ".selectize-input {
                                       color: #000000;
                                       font-size: 14px;}
                                .selectize-dropdown {
                                       font-size: 14px; }", 
                          "#ct_text {display:inline}", 
                          "#reg_text {display:inline}",
                          "#acs_year {display:inline}", "#acs_year2 {display:inline}",
                          "#suicide_year1 {display:inline}", "#button {display:inline}",
                          ".small-box.bg-navy { background-color: #272869 !important; color: white !important; }", 
                          ".bg-navy {background-color: #272869 !important; }")
               ),
               conditionalPanel(
                 style = "position:fixed;",
                 condition="input.tabselected==1 || input.tabselected==2 || input.tabselected==3 || 
                           input.tabselected==4 || input.tabselected==5 || input.tabselected==6",
                 sidebarMenu(
                   width=300, 
                   style="padding: 0px 0px",
                   selectInput("select",
                     label = HTML('<h3 style="color:black;">Select Region</h3>'),
                     choices = list("Statewide", "Southwest Region", 
                                  "South Central Region", "Eastern Region", 
                                  "North Central Region", "Western Region", 
                                  "Central Region"), 
                     selected = "Statewide"
                   )
                 ),
                 column(width = 12, 
                   plotOutput("gg_regions", height = 150), 
                   uiOutput("region_text"),
                   uiOutput("region_list")
                 )
               ), 
               conditionalPanel(
                 style = "position:fixed;",                
                 condition="input.tabselected==7",
                 sidebarMenu(
                   width=350,
                   style="padding: 0px 0px",
                   selectizeInput("select_edu",
                     label = HTML('<h3 style="color:black;">Select One or More Districts</h3><br><p style="color:black;">Calculated percents 
                                   resulting from counts of less than 5 students have been suppressed. <br>Be aware that as the number of districts 
                                   selected increases, visibility/usability of these charts may decrease.</p>'),
                     choices = district_list, selected = "Hartford School District", multiple=TRUE, 
                     options = list(placeholder = "Start typing in a District", 
                                    'plugins' = list('remove_button'), #adding remove button to options in selectize
                                    'create' = TRUE,
                                    'persist' = FALSE)
                   )
                 )
               )
             ), #end of dashboardSidebar
             dashboardBody(
               tabsetPanel(
              tabPanel("Demographics", value = 5,
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Population by Age and Race/Ethnicity - ", 
                                                HTML('<a href="http://data.ctdata.org/visualization/population-by-age-by-town" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                  column(8,
                    box(width="100%",
                        plotlyOutput("DPlot_age_race", width="100%")
                    )
                  ),
                  column(4,
                    box(width="100%",
                      selectInput("race", 
                                   label = HTML('<h4 style="color:black;">Select Race/Ethnicity:</h4>'),
                                   choices = list("All", 
                                                  "White Alone",
                                                  "White Alone Not Hispanic or Latino",
                                                  "Hispanic or Latino",
                                                  "Black or African American Alone",
                                                  "Some Other Race Alone",
                                                  "Asian Alone",
                                                  "Two or More Races",
                                                  "American Indian and Alaska Native Alone",
                                                  "Native Hawaiian and Other Pacific Islander"), selected = "All"), 
                      size = "default"
                    ), 
                      fluidRow(width=4,
                               infoBox(value= textOutput("race_sel_text1"),
                                       subtitle = textOutput("race_sel_value1"),
                                       title = "",
                                       icon = shiny::icon("hashtag"), color = "navy", width = NULL,
                                       href = NULL, fill = FALSE)),
                      fluidRow(width=4,
                               infoBox(value= textOutput("race_sel_text2"), 
                                       subtitle = textOutput("race_sel_value2"),
                                       title = "",
                                       icon = shiny::icon("percent"), color = "navy", width = NULL,
                                       href = NULL, fill = FALSE))

                  ), 
                  collapsible=T
                )
                ),  
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Population by Age and Gender - ", 
                                                HTML('<a href="http://data.ctdata.org/visualization/population-by-age-by-town" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                        plotlyOutput("DPlot_age", width="100%"),
                        collapsible = T
                  )
                ),
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Population by Race/Ethnicity and Gender - ", 
                                                HTML('<a href="http://data.ctdata.org/visualization/population-by-age-by-town" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                        plotlyOutput("DPlot_race", width="100%"),
                        collapsible = T
                  )
                ),
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Single-Parent Families - ", 
                                                HTML('<a href="http://data.ctdata.org/visualization/single-parent-families" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                    conditionalPanel(
                      condition="input.select=='Statewide'",
                      column(9,
                             box(width="100%",
                                 h4('Connecticut Single-Parent Families: ', style="display:inline", textOutput("acs_year")), HTML("<br><br>"),
                                 valueBox(value = textOutput("spf_value"), subtitle = textOutput("spf_moe"), icon = NULL, color = "navy", width = 4, href = NULL),                                 
                                 valueBox(value = textOutput("spf_valueP"), subtitle = textOutput("spf_moeP"), icon = NULL, color = "navy", width = 4, href = NULL),
                                 HTML('<h5 align="right" style="color:grey;">Source: U.S. Census<br>accessed via ctdata.org</h5>')
                             )
                      ), collapsible=T
                    ), 
                    conditionalPanel(
                      condition="input.select!='Statewide'",
                      column(9,
                             box(width="100%",
                                 plotlyOutput("DPlot_spf", width="100%")
                             )
                      ),
                      column(3,
                             box(width="100%",
                                 h4("Single-Parent Families in Region"),
                                 HTML('<h3><span id="spf_valueR" class="shiny-text-output"></span>&nbsp;<span id="spf_moeR"
                                       style="color:grey;font-size:0.67em;"
                                       class="shiny-text-output"></span></h3>')
                             ),
                             box(width="100%",
                                 h4("Percent of all Families in Region"),
                                 HTML('<h3><span id="spf_valuePR" class="shiny-text-output"></span>&nbsp;<span id="spf_moePR"
                                       style="color:grey;font-size:0.67em;"
                                       class="shiny-text-output"></span></h3>')
                             )
                      ), collapsible=T
                    ), collapsible=T
                      
                  )
                ),
                fluidRow(
                  box(width=12, 
                      title = tagList(shiny::icon("bar-chart"), "Median Household Income - ", 
                                      HTML('<a href="http://data.ctdata.org/visualization/median-household-income-by-town" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                    conditionalPanel(
                      condition="input.select=='Statewide'",
                      column(9,
                             box(width="100%",
                                 h4('Connecticut Median Household Income: ', style="display:inline", textOutput("acs_year2")), HTML("<br><br>"),
                                 valueBox(value = textOutput("mhi_value"), subtitle = textOutput("mhi_moe"), icon = NULL, color = "navy", width = 4, href = NULL),                                 
                                 HTML('<h5 align="right" style="color:grey;">Source: U.S. Census<br>accessed via ctdata.org</h5>')
                             )
                      ), collapsible=T
                    ), 
                    conditionalPanel(
                      condition="input.select!='Statewide'",
                      column(9,
                             box(width="100%",
                                 plotlyOutput("Dplot_mhi", width="100%")
                             )
                      ),
                      column(3,
                             box(width="100%",
                                 HTML('<h4 style="color:black;"> Max Median Household Income</h4>'),
                                 h3(textOutput("max_mhi_town")),
                                 HTML('<h3><span id="max_mhi_value" class="shiny-text-output"></span>&nbsp;<span id="max_mhi_moe" 
                                       style="color:grey;font-size:0.67em;" 
                                       class="shiny-text-output"></span></h3>')
                             ),
                             box(width="100%",
                                 HTML('<h4 style="color:black;">Min Median Household Income</h4>'),
                                 h3(textOutput("min_mhi_town")),
                                 HTML('<h3><span id="min_mhi_value" class="shiny-text-output"></span>&nbsp;<span id="min_mhi_moe"
                                       style="color:grey;font-size:0.67em;"
                                       class="shiny-text-output"></span></h3>')
                             )
                      ), collapsible=T
                    ), collapsible=T
                  )
                ), 
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Poverty Status by Age Range and Race/Ethnicity - ", 
                                                HTML('<a href="http://data.ctdata.org/visualization/poverty-status-by-town" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                        plotlyOutput("DPlot_pov", width="100%"),
                        collapsible = T
                  )
                )
              ),                 
                 tabPanel("Health", value = 1,
                   fluidRow(
                     box(width=12, title = tagList(shiny::icon("bar-chart"), "Mortality Rates - ", 
                                                   HTML('<a href="http://data.ctdata.org/visualization/fetal-and-infant-mortality---5-year-aggregations-by-town" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                         plotlyOutput("HPlot1", width="100%"),
                        collapsible = T
                  )
                ),
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Mortality Rates by Race/Ethnicity - ", 
                                                HTML('<a href="http://data.ctdata.org/visualization/fetal-and-infant-mortality---5-year-aggregations-by-town" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                      plotlyOutput("HPlot2", width="100%"), 
                      collapsible = T
                  )
                ),
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("info-circle"), "Suicide Rates - ", 
                                                HTML('<a href="http://data.ctdata.org/visualization/suicide" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                    conditionalPanel(
                      condition="input.select=='Statewide'",
                      column(12,
                             box(width="100%",
                                 h4('Connecticut Suicide Statistics: ', style="display:inline", textOutput("suicide_year1")), HTML("<br><br>"),
                                 valueBox(value = textOutput("suicide_cr"), subtitle = "Crude Rate (per 100,000)", icon = NULL, color = "navy", width = 4, href = NULL),                                 
                                 valueBox(value = textOutput("suicide_aamr"), subtitle = "AAMR (per 100,000)", icon = NULL, color = "navy", width = 4, href = NULL),
                                 valueBox(value = textOutput("suicide_value1"), subtitle = "Total Suicides", icon = NULL, color = "navy", width = 4, href = NULL),
                                 HTML('<h5 align="right" style="color:grey;">Source: Department of Public Health<br>accessed via ctdata.org</h5>')
                             ), 
                             fluidRow(
                               tabBox(width=6,
                                 tabPanel("Crude Rate", 
                                          div(HTML("Crude death rate is a measure of the number of deaths in a population scaled to the size of that 
                                                  population per unit time. The rate is calculated by dividing the number of deaths in a population 
                                                  in a year by the midyear resident population."), 
                                              HTML("<br>Source: Department of Public Health")))
                               ),
                               tabBox(width=6,
                                 tabPanel("Age-Adjusted Mortality Rate (AAMR)", 
                                          div(HTML("Age-adjusted mortality rates are rates where the effect of differing age
                                                  distributions between the groups has been removed. Age-adjusted rates are computed by applying age-specific 
                                                  rates in a population of interest to a standardized age distribution, in order to 
                                                  eliminate differences in observed rates that result from age differences in population composition."), 
                                              HTML("<br>Source: Department of Public Health")))
                               )  
                             )
                      )
                    ), 
                    conditionalPanel(
                      condition="input.select!='Statewide'",
                      column(8,
                             box(width="100%",
                                 radioButtons("rates", label = NULL, inline = TRUE, 
                                          c("Crude Rate" = "Crude Rate (per 100,000)", "AAMR" = "AAMR (per 100,000)"), 
                                         selected = "Crude Rate (per 100,000)"),                                 
                                 plotlyOutput("HPlot3", width="100%")
                             )
                      ),
                      column(4,
                             tabBox(width="100%",
                               tabPanel("Crude Rate", 
                                        div(HTML("Crude death rate is a measure of the number of deaths in a population scaled to the size of that 
                                                  population per unit time. The rate is calculated by dividing the number of deaths in a population 
                                                  in a year by the midyear resident population."), 
                                              HTML("<br>Source: Department of Public Health")))
                             ),
                             tabBox(width="100%",
                               tabPanel("Age-Adjusted Mortality Rate (AAMR)", 
                                        div(HTML("Age-adjusted mortality rates are rates where the effect of differing age
                                                  distributions between the groups has been removed. Age-adjusted rates are computed by applying age-specific 
                                                  rates in a population of interest to a standardized age distribution, in order to 
                                                  eliminate differences in observed rates that result from age differences in population composition."), 
                                              HTML("<br>Source: Department of Public Health")))
                             )                             
                      ), collapsible=T
                    ), collapsible=T
                      
                  )
                ),                
                fluidRow(
                  box(width= 12, 
                      title = tagList(shiny::icon("table"), "Mental Health and Substance Abuse Treatment Admissions", paste0("(FY ", max_year_SA_regions, ")"), " - ", 
                      HTML('<a href="http://data.ctdata.org/visualization/mental-health-and-substance-abuse-treatment-admissions" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                    fluidRow(  
                      box(width = 9, 
                          tableOutput("MHSATable")
                       ),                               
                       tabBox(width=3, id = "tabset6",
                           tabPanel("Metadata", div(HTML("These values represent unduplicated client counts (each client counted once) within each admission type.")))
                       )                   
                    ),collapsible = T
                  )
                ), 
                fluidRow(
                  box(width=12, 
                      title = tagList(shiny::icon("table"), "Substance Abuse Treatment Admissions by Drug Type", paste0("(FY ", max_year_SA_regions, ")"), " - ", 
                      HTML('<a href="http://data.ctdata.org/visualization/substance-treatment-admissions-by-drug-type" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                    conditionalPanel(
                      condition="input.select=='Statewide'",
                      fluidRow(
                             box(width = 9, 
                                 tableOutput("SATable")
                             ), 
                             box(width=3,
                                 h4('Total Admissions: ', style="display:inline", textOutput("ct_text")), 
                                 HTML('<h3><span id="sa_ct_value" class="shiny-text-output"></span></h3>'),
                                 HTML('<h5 align="right" style="color:grey;">Source: CT DMHAS <br> accessed via ctdata.org</h5>')
                             ), 
                             tabBox(width=3, id = "tabset6",
                               tabPanel("Metadata", div(HTML("These values represent duplicated admission counts (clients may be admitted more than once during the year) for a given drug 
                                        type. The 'Other' category includes: Other Stimulants, Methamphetamines, Tobacco, Other Sedatives or Hypnotics, Amphetamines, Over-the-Counter, 
                                                             Tranquilizers, Cocaine/Crack, Other, Barbiturates, Non-Prescriptive Methadone, Inhalants, and Hallucinogens: LSD, DMS, STP, etc")))
                             )
                      ) 
                    ), collapsible=T,
                    conditionalPanel(
                      condition="input.select!='Statewide'",
                      fluidRow(
                             box(width=9,
                                 tableOutput("SARegionTable")
                             ),
                             box(width=3,
                                 h4('Total Admissions: ', style="display:inline", textOutput("reg_text")), 
                                 HTML('<h3><span id="sa_region_value" class="shiny-text-output"></span></h3>'),
                                 HTML('<h5 align="right" style="color:grey;">Source: CT DMHAS <br> accessed via ctdata.org</h5>')
                             ),
                             tabBox(width=3, id = "tabset6",
                               tabPanel("Metadata", div(HTML("These values represent duplicated admission counts (clients may be admitted more than once during the year) for a given drug 
                                        type. The 'Other' category includes: Other Stimulants, Methamphetamines, Tobacco, Other Sedatives or Hypnotics, Amphetamines, Over-the-Counter, 
                                                             Tranquilizers, Cocaine/Crack, Other, Barbiturates, Non-Prescriptive Methadone, Inhalants, and Hallucinogens: LSD, DMS, STP, etc")))
                             ) 
                      ), collapsible = T
                    )
                  ) 
                ), collapsible=T 
              ),
              tabPanel("Early Childhood", value = 2,
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Birth To Three Services, Annual - ", 
                                                HTML('<a href="http://data.ctdata.org/visualization/birth-to-three-annual-data" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                         plotlyOutput("ECPlot1", width="100%"), 
                         collapsible = T
                  )
                ), 
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Birth To Three Services, Cohort - ", 
                                                HTML('<a href="http://data.ctdata.org/visualization/birth-to-three-birth-cohort-data" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                          plotlyOutput("ECPlot2", width="100%"), 
                          collapsible = T
                  )
                ),
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Birth To Three Services, % Cohort - ", 
                                                HTML('<a href="http://data.ctdata.org/visualization/birth-to-three-birth-cohort-data" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                         plotlyOutput("ECPlot3", width="100%"),
                         collapsible = T
                  ) 
                )
              ),
              tabPanel("Juvenile Justice", value = 3, 
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Selected Crimes - ", 
                                                HTML('<a href="http://data.ctdata.org/visualization/juvenile-arrests" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                    box(width=8,plotlyOutput("JJPlot1", width="100%")
                    ),
                    tabBox(width=4, id = "tabset3", height = "100px",
                           tabPanel("Selected Crimes", div(HTML("<b>Disorderly Conduct</b> — Breach of the peace.  <br>
                                                          <b>Drugs</b> — Offenses relating to narcotic drugs. <br>
                                                          <b>Larceny</b> — The unlawful taking of property from the possession of another.  <br>
                                                          <b>Other</b> — All other violations of state or local laws, except traffic violations.<br>
                                                          <b>Simple Assault</b> — Assaults which are not of an aggravated type."))
                           )
                    ), collapsible=T
                  ), 
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Rate per 100,000 Persons - ", 
                                                HTML('<a href="http://data.ctdata.org/visualization/juvenile-arrests" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                    box(width=8, plotlyOutput("JJPlot2", width="100%")
                    ),                    
                    tabBox(width=4,id = "tabset4", height = "100px",
                          tabPanel("Metadata", "The denominators for these rates are derived from 5-Year ACS population figures. As such, the rate of arrests is given with a Margin of Error calculated from the Margin of Error present in the population data.")
                    ), collapsible=T
                  )
                )  
              ),
              tabPanel("Child Welfare", value = 4,
                fluidRow(
                  box(width=12, title =  tagList(shiny::icon("info-circle"), "Children in Placement"),
                    column(9,
                    tabsetPanel(
                      tabPanel("Age", value = 1,
                        box(width="200%",
                          title =  tagList(shiny::icon("table"), 
                                           HTML('<a href="http://data.ctdata.org/visualization/children-in-placement-by-age" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                          tableOutput("CWTable")
                        )
                      ),
                      tabPanel("Gender", value=2,
                        box(width="100%",
                          title = tagList(shiny::icon("bar-chart"), 
                                          HTML('<a href="http://data.ctdata.org/visualization/children-in-placement-by-gender" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                          plotlyOutput("CW_gender", width="100%")
                        ) 
                      ), 
                      tabPanel("Race/Ethnicity", value=3,
                        box(width="100%",
                          title = tagList(shiny::icon("bar-chart"), 
                                          HTML('<a href="http://data.ctdata.org/visualization/children-in-placement-by-race-and-ethnicity" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                          plotlyOutput("CW_race", width="100%")
                        ) 
                      )
                    ) 
                  ),
                  column(3,
                    box(width="100%",
                      radioButtons("rd", 
                                   label = HTML('<h4 style="color:black;">Select Location:</h4>'),
                                   choices = list("In State", "Out of State"),
                                   selected = "In State"
                      ), 
                      size = "default"
                    ), 
                    tabBox(width="100%", height = "100px",
                           tabPanel("Statewide Values", div(HTML("<font size='2' color='grey'>Statewide totals include those treated 
                                                                  in both 'Region 0' and 'Other', therefore Regions 1-6 will not add up to the Statewide totals. 
                                                                  The 'Other' category includes all cases that are not being served by a Regional DCF Office. </font> "))
                           )
                    ), collapsible=T
                  ), collapsible=T
                  )
                ),
                fluidRow(
                    box(width=12, title = tagList(shiny::icon("info-circle"), "Disengaged Youth - ", HTML('<a href="http://data.ctdata.org/visualization/disengaged-youth" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                         #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                    infoBox(title= "Female", value = textOutput("dy_value_f"), subtitle = textOutput("dy_moe_f"),
                            icon = shiny::icon("female"), color = "red", width = 4,
                            href = NULL, fill = FALSE),
                    infoBox(title= "Male", value = textOutput("dy_value_m"), subtitle = textOutput("dy_moe_m"),
                            icon = shiny::icon("male"), color = "blue", width = 4,
                            href = NULL, fill = FALSE),
                    infoBox(title= "Total", value = textOutput("dy_value_t"), subtitle = textOutput("dy_moe_t"),
                            icon = shiny::icon("users"), color = "black", width = 4,
                            href = NULL, fill = FALSE),
                    HTML("<font color='grey'>Source: U.S. Census<br>accessed via ctdata.org</font>")
                         ), collapsible=T
                )#, 
                # fluidRow(
                #   box(width=12, 
                #     title = tagList(shiny::icon("bar-chart"), "Child Abuse and Neglect - ", 
                #                                 HTML('<a href="http://data.ctdata.org/visualization/child-abuse-and-neglect" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')
                #     ),
                #     column(8,
                #       box(width="100%",
                #         plotlyOutput("CWPlot_neglect", width="100%")
                #       )
                #     )
                #   )
                # )
              ),
              tabPanel("Behavioral Health", value = 6,
                fluidRow(
                  box(width=12,title =  tagList(shiny::icon("bar-chart"), "Children in Need of Treatment by Age and Race/Ethnicity"),
                       plotlyOutput("BHPlot1"),
                       collapsible = T
                  )
                ),
                fluidRow(
                  box(width=12, title =  tagList(shiny::icon("table"), "Potential Treatment Needs by Age Range and Race/Ethnicity"),
                    box(width=8,
                        tableOutput("BHTable"), offset=0, align = "left"
                    ),
                    tabBox(width = 4,                                         
                      id = "tabset5", height = "100px",
                      tabPanel("Labels", div(HTML("<b>Pop</b> — Population: Number of children in CT according to 2010 Decennial Census.<br>
                                                   <b>Tx</b> — Estimated number of individuals needing treatment for a serious emotional disturbance (10% of population).<br>
                                                   <b>Life</b> — Estimated number who have experienced a mental health disorder in their lifetime (20% of population).")))
                    ), collapsible=T
                  )
                )  
              ),              
              tabPanel("Education", value = 7,
               fluidRow(
                   box(width=12,title =  tagList(shiny::icon("bar-chart"), "Educational Need", max_year_edu, "-", HTML('<a href="http://data.ctdata.org/visualization/educational-need" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                       plotlyOutput("EPlot1"),
                       collapsible = T
                   ),
                   box(width=12,title =  tagList(shiny::icon("bar-chart"), "Suspension Rate by Race/Ethnicity", max_year_edu2, "-", HTML('<a href="http://data.ctdata.org/visualization/suspension-rate-by-race" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                       plotlyOutput("EPlot2"),
                       collapsible = T
                   ),
                   box(width=12,title =  tagList(shiny::icon("bar-chart"), "Sanctions by Type", max_year_edu3, "-", HTML('<a href="http://data.ctdata.org/visualization/sanctions" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                       plotlyOutput("EPlot3"),
                       collapsible = T
                   ), 
                   box(width=12, title = span(shiny::icon("bar-chart"),"Incidents by Type", max_year_edu4, "-", dropdownButton(label = "Explore the Data", 
                                    HTML('<a href="http://data.ctdata.org/visualization/bullying" target="_blank"><font color="dodgerblue">Bullying</font></a>'),                                                                                                                               
                                    HTML('<a href="http://data.ctdata.org/visualization/incidents" target="_blank"><font color="dodgerblue">Incidents</font></a>'),
                                    circle = FALSE, status = "success", icon = NULL)),
                       plotlyOutput("EPlot4"),
                       collapsible = T),
                   box(width=12,title =  tagList(shiny::icon("bar-chart"), "Four-Year Graduation Rates by Race/Ethnicity", max_year_edu5, "-", HTML('<a href="http://data.ctdata.org/visualization/four-year-grad-rates-by-race-ethnicity" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                       plotlyOutput("EPlot5"),
                       collapsible = T
                   ), 
                   box(width=12,title =  tagList(shiny::icon("bar-chart"), "Chronic Absenteeism by Race/Ethnicity", max_year_edu6, "-", HTML('<a href="http://data.ctdata.org/visualization/chronic-absenteeism-by-race-ethnicity" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                       plotlyOutput("EPlot6"),
                       collapsible = T
                   ), 
                   box(width=12,title =  tagList(shiny::icon("bar-chart"), "Student Enrollment by Race/Ethnicity", max_year_edu6, "-", HTML('<a href="http://data.ctdata.org/visualization/student-enrollment-by-race-ethnicity" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                       plotlyOutput("EPlot7"),
                       collapsible = T
                   ),
                   box(width=12,title =  tagList(shiny::icon("bar-chart"), "Kindergarten Entrance Inventory by Domain and Level", max_year_kei, "-", HTML('<a href="http://data.ctdata.org/visualization/kindergarten-entrance-inventory-results" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                            plotlyOutput("kei"),
                            collapsible = T
                  ), collapsible = T
                )
              ),
              id = "tabselected"
            )
          )
        ), #end of dashboardPage
        tags$footer(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
          HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                     "Copyright © 2018 CT Data Collaborative", 
                     sep="<br/>")), 
          align = "center"
        )
      ),
      tabPanel(
        title = h4(style="text-align:center", "Additional Resources", icon("info-circle")), 
        fluidRow(
          column(10, offset=1, 
                 panel_div(class_type = "primary", 
                           panel_title = "Data Sources",
                           content = HTML("<b>Connecticut Public Data</b> - <a href='https://data.ct.gov/' target='_blank'> www.data.ct.gov</a><br>
                                      <ul>Pursuant to Executive Order 39, executive branch agencies must publish open public data to the state's Open Data portal.</ul><hr>
                                      <b>Individual School Level Data</b> - <a href='http://edsight.ct.gov/SASPortal/main.do' target='_blank'> www.edsight.ct.gov</a><br>
                                      <ul>School level data from the State Department of Education.</ul><hr>
                                      <b>Judicial Branch Public Data</b> - <a href='https://www.jud.ct.gov/Statistics/' target='_blank'> www.jud.ct.gov</a><br>
                                      <ul><p>Judicial Branch Statistics and Reports</ul>")
                 ),                  
                 panel_div(class_type = "info", 
                           panel_title = "Data Processing",
                           content = HTML("<ul>Click <a href='https://docs.google.com/spreadsheets/d/1qDh0Vg6RWLerTAzlF2A1xoyUGWxCB4Cq_rppfnl6Vek/edit?usp=sharing' target='_blank'> here</a> 
                                          to see when data is added to the Dashboard!<ul/>")
                 ), 
                 panel_div(class_type = "primary", 
                           panel_title = "Additional Resources",
                           content = HTML("<b>The Child Health and Development Institute of Connecticut</b> - <a href='https://www.chdi.org/' target='_blank'> www.chdi.org</a><hr>
                                      <b>Network of Care Analysis</b>
                                      <ul><li><a href = 'https://s3.amazonaws.com/connect-ctdata/reports/Updated2017+NOC+Overview-pdf.pdf' target='_blank'>Network of Care Introduction and Methods</a></ul>
                                      <ul><li><a href='https://s3.amazonaws.com/connect-ctdata/reports/CCMC_AAP+Final+Report+10_24_17.pdf' target='_blank'>Assessment of the System of Mental Health Care for Children: a Focus on Pediatric Primary Care</a></ul>
                                      <ul><li><a href='https://s3.amazonaws.com/connect-ctdata/reports/Brief+Report+SSM.pdf' target='_blank'>System Support Mapping and Schools Pilot Project</a></ul>
                                      <ul><li><a href='https://s3.amazonaws.com/connect-ctdata/reports/Collaboration+survey+Overall+Summary+and+All+Reports.pdf' target='_blank'>Collaboration Survey - Overall Summary and All Reports</a></ul>
                                      <ul><li><a href='https://s3.amazonaws.com/connect-ctdata/reports/2017+Community+Conversations+Summary+FINAL+9+27+2017.pdf' target='_blank'>2017 Community Conversations Summary</a></ul>")                 )                  
          ) 
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
) # end of shiny                        