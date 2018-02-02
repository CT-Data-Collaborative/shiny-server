
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

##Read in data
#######HEALTH#####################################################################################################################################
source('./scripts/read_in_data.R')

# Define UI for application
shinyUI(
  fluidPage(
      title = "CONNECT Dashboard",
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$link(rel = "shortcut icon", type="image/x-icon", href="https://raw.githubusercontent.com/CT-Data-Collaborative/shiny-server/master/images/favicon.ico")
      ),
    div(style="padding: 1px 0px; width: '100%'", titlePanel(title="", windowTitle="CONNECT Dashboard")),
    navbarPage(title=div(img(src="transparent_dots.png"), "CONNECT Dashboard"), inverse = F, # for diff color view
      theme = shinytheme("lumen"),
      tabPanel(
        "Home", 
        icon = icon("home"),
        HTML("<h1>A Dashboard of Data Indicators About Your Community.</h1><p>Use this dashboard to explore data by Department of Children and Families (DCF) region and education data by district.</p><hr>"),
        #wells(content = "Use this dashboard to explore DCF data by region and educational data by district.", size = "default"),
        fluidRow(
          column(12, panel_div(class_type = "primary", 
                              panel_title = "About CONNECT",
                              content = HTML("<p>The Connecticut Network of Care Transformation (CONNECT) is a statewide initiative to create 
                              a partnership between families, state agencies, and service providers at the local, regional and state 
                              levels. That enhanced partnership supports children, youth, and families in accessing the services they 
                              need in a timely and effective manner through an integrated network of care.</p><p><b>CONNECT engages 
                              partners from each of the following child-serving sectors:</b></p><ul><li>Behavioral Health<li>Child Welfare
                              <li>Early Childhood<li>Juvenile Justice<li>Substance Abuse<li>Education<li>Advocacy groups<li>Families 
                              and children served</ul>"))),
          column(12, panel_div("info", 
                              "About Connecticut Data Collaborative", 
                              content = HTML("<p>Striving for informed decision-making across Connecticut, we empower an ecosystem of data users by democratizing 
                              access to public data and building data literacy. Visit <a href='http://ctdata.org/' target='_blank'> ctdata.org</a> for more info!</p>"))), 
          column(12, panel_div("primary", 
                              "Need Help? Email Us!",
                              "Ask a question or provide feedback to the <a href='mailto:info@ctdata.org?Subject=CONNECT%20Dashboard' target='_top'>Connecticut Data Collaborative</a>"))
        ),  
        #### FAVICON TAGS SECTION ####
        tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
        #### JAVASCRIPT TAGS SECTION #### - ENABLE WHEN READY
        #tags$head(tags$script(src='pl.js')),
        #   # bsModal("modalExample", "Instructional Video", "tabBut", size = "large" ,
        #   #         p("Additional text and widgets can be added in these modal boxes. Video plays in chrome browser"),
        #   #         iframe(width = "560", height = "315", url_link = "https://www.youtube.com/embed/0fKg7e37bQE")
        #   #         )
        # 
        # )
                tags$footer(
                      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                      HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                           "Copyright © 2018 CT Data Collaborative", 
                           sep="<br/>")), 
                      align = "center"
               )
      ),
      tabPanel(
        "Dashboard", 
        icon = icon("cog"),
        fluidRow(
          column(12, panel_div(class_type = "primary", 
                               panel_title = "Directions",
                               content = HTML('<ul><li>Use the pull down menu on the left to select the Region or statewide data you wish to explore.
                                          <li>Click through the category tabs along the top to explore the various categories.
                                          <li>Use the Education tab to select data by District.
                                          <li>Town/District level data is available by clicking on the <font size="4" color="dodgerblue">Explore the Data</font> link.</li><ul/>')))
        ),
        dashboardPage(
          dashboardHeader(disable = TRUE),
          dashboardSidebar(
            tags$head(
              tags$style("ol {columns: 2;
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
            width = 350,
            sidebarPanel(
              width=300,
              conditionalPanel(
                condition="input.tabselected==1 || input.tabselected==2 || input.tabselected==3 || 
                           input.tabselected==4 || input.tabselected==5 || input.tabselected==6",
                fluidRow(
                  column(12, div(style="padding: 0px 0px", 
                      sidebarMenu(
                        selectInput("select",
                        label = HTML('<h3 style="color:black;">Select Region</h3>'),
                        choices = list("Statewide", "Southwest Region","South Central Region",
                                    "Eastern Region","North Central Region","Western Region","Central Region"), 
                        selected = "Statewide")
                        )
                      )
                    )
                ), 
                plotOutput("gg_regions", height = 200), 
                uiOutput("region_text"),
                uiOutput("region_list")
              ), 
              conditionalPanel(
                condition="input.tabselected==7",
                fluidRow(
                  column(12, div(style="padding: 0px 0px",
                      sidebarMenu(
                        selectizeInput("select_edu",
                        label = HTML('<h3 style="color:black;">Select One or More Districts</h3>'),
                        choices = district_list, selected = "Hartford School District", multiple=TRUE, 
                        options = list(placeholder = "Start typing in a District", 
                                       'plugins' = list('remove_button'), #adding remove button to options in selectize
                                       'create' = TRUE,
                                       'persist' = FALSE)
                        )
                      )
                    )
                  )
                ),
                uiOutput("edu_text")
              )
            ) 
          ), #end of side bar
          dashboardBody(
            tags$head(
              #tags$style(type='text/css',".nav-tabs {font-size: 10px} ")
              #tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
              # tags$style("#eey_moe {color:grey;
              #                       font-size: 0.67em;}, 
              #             #mhi_moe {color:grey;
              #                       font-size: 0.67em;}") 
            ),
            tabsetPanel(
              tabPanel("Health", value = 1,
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Mortality Rates - ", HTML('<a href="http://data.ctdata.org/visualization/fetal-and-infant-mortality---5-year-aggregations-by-town?v=table&f={%22Town%22:%20%22Hartford%22,%20%22Variable%22:%20%22Fetal%20Mortality%22,%20%22Race%22:%20%22All%22,%20%22Year%22:%20%222010-2014%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                        plotlyOutput("HPlot1", width="100%"),
                        collapsible = T
                  )
                ),
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Mortality Rates by Race/Ethnicity - ", HTML('<a href="http://data.ctdata.org/visualization/fetal-and-infant-mortality---5-year-aggregations-by-town?v=table&f={%22Town%22:%20%22Hartford%22,%20%22Variable%22:%20%22Fetal%20Mortality%22,%20%22Race%22:%20%22All%22,%20%22Year%22:%20%222010-2014%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                      plotlyOutput("HPlot2", width="100%"), 
                      collapsible = T
                  )
                )
              ),
              tabPanel("Early Childhood", value = 2,
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Birth To Three Services, Annual - ", HTML('<a href="http://data.ctdata.org/visualization/birth-to-three-annual-data?v=table&f={%22Town%22:%20%22Connecticut%22,%20%22Variable%22:%20%22Birth%20to%20Three%20Indicators%22,%20%22Indicator%22:%20%22Total%20Eligible%22,%20%22Measure%20Type%22:%20%22Number%22,%20%22Year%22:%20%222016%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                         plotlyOutput("ECPlot1", width="100%"), 
                         collapsible = T
                  )
                ), 
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Birth To Three Services, Cohort - ", HTML('<a href="http://data.ctdata.org/visualization/birth-to-three-birth-cohort-data?v=table&f={%22Town%22:%20%22Connecticut%22,%20%22Variable%22:%20%22Birth%20to%20Three%20Indicators%22,%20%22Indicator%22:%20%22Total%20Eligible%22,%20%22Year%22:%20%222013%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                          plotlyOutput("ECPlot2", width="100%"), 
                          collapsible = T
                  )
                ),
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Birth To Three Services, % Cohort - ", HTML('<a href="http://data.ctdata.org/visualization/birth-to-three-birth-cohort-data?v=table&f={%22Town%22:%20%22Connecticut%22,%20%22Variable%22:%20%22Birth%20to%20Three%20Indicators%22,%20%22Indicator%22:%20%22Total%20Eligible%22,%20%22Year%22:%20%222013%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                         plotlyOutput("ECPlot3", width="100%"),
                         collapsible = T
                  ) 
                )
              ),
              tabPanel("Juvenile Justice", value = 3, 
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Selected Crimes - ", HTML('<a href="http://data.ctdata.org/visualization/juvenile-arrests?v=table&f={%22Town%22:%20%22Connecticut%22,%20%22Variable%22:%20[%22Juvenile%20Arrests%22,%20%22Margins%20of%20Error%22],%20%22Crime%22:%20%22Total%22,%20%22Age%20Range%22:%20%22Total%22,%20%22Year%22:%20%222015%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                    box(width=8,plotlyOutput("JJPlot1", width="100%")
                    ),
                    tabBox(width=4, id = "tabset3", height = "100px",
                           tabPanel("Metadata", div(HTML("<b>Disorderly Conduct</b> — Breach of the peace.  <br>
                                                          <b>Drugs</b> — Offenses relating to narcotic drugs. <br>
                                                          <b>Larceny</b> — The unlawful taking of property from the possession of another.  <br>
                                                          <b>Other</b> — All other violations of state or local laws, except traffic violations.<br>
                                                          <b>Simple Assault</b> — Assaults which are not of an aggravated type."))
                           )
                    ), collapsible=T
                  ), 
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Rate per 100,000 Persons - ", HTML('<a href="http://data.ctdata.org/visualization/juvenile-arrests?v=table&f={%22Town%22:%20%22Connecticut%22,%20%22Variable%22:%20[%22Juvenile%20Arrests%22,%20%22Margins%20of%20Error%22],%20%22Crime%22:%20%22Total%22,%20%22Age%20Range%22:%20%22Total%22,%20%22Year%22:%20%222015%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
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
                        box(width="100%",
                          title =  tagList(shiny::icon("table"), HTML('<a href="http://data.ctdata.org/visualization/children-in-placement-by-age?v=table&f={%22Variable%22:%20%22Children%20in%20Placement%22,%20%22Region%22:%20%22Region%201:%20Southwest%22,%20%22Measure%20Type%22:%20%22Number%22,%20%22Year%22:%20%22SFY%202013-2014%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                          tableOutput("CWTable")
                        )
                      ),
                      tabPanel("Gender", value=2,
                        box(width="100%",
                          title = tagList(shiny::icon("bar-chart"), HTML('<a href="http://data.ctdata.org/visualization/children-in-placement-by-gender?v=table&f={%22Variable%22:%20%22Children%20in%20Placement%22,%20%22Region%22:%20%22Region%201:%20Southwest%22,%20%22Measure%20Type%22:%20%22Number%22,%20%22Year%22:%20%22SFY%202013-2014%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                          plotlyOutput("CW_gender", width="100%")
                        ) 
                      ), 
                      tabPanel("Race/Ethnicity", value=3,
                        box(width="100%",
                          title = tagList(shiny::icon("bar-chart"), HTML('<a href="http://data.ctdata.org/visualization/children-in-placement-by-race-and-ethnicity?v=table&f={%22Variable%22:%20%22Children%20in%20Placement%22,%20%22Region%22:%20%22Region%201:%20Southwest%22,%20%22Measure%20Type%22:%20%22Number%22,%20%22Year%22:%20%22SFY%202013-2014%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
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
                    )
                  ), collapsible=T
                  )
                ),
                fluidRow(
                    box(width=12, title = tagList(shiny::icon("info-circle"), "Employed or Enrolled Youth - ", HTML('<a href="http://data.ctdata.org/visualization/employed-or-enrolled-youth?v=table&f={%22Town%22:%20%22Connecticut%22,%20%22Variable%22:%20[%22Employed%20or%20Enrolled%20Youth%22,%20%22Margins%20of%20Error%22],%20%22Measure%20Type%22:%20%22Percent%22,%20%22Year%22:%20%222012-2016%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                         #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                    infoBox(title= "Female", value = textOutput("eey_value_f"), subtitle = textOutput("eey_moe_f"),
                            icon = shiny::icon("female"), color = "red", width = 4,
                            href = NULL, fill = FALSE),
                    infoBox(title= "Male", value = textOutput("eey_value_m"), subtitle = textOutput("eey_moe_m"),
                            icon = shiny::icon("male"), color = "blue", width = 4,
                            href = NULL, fill = FALSE),
                    infoBox(title= "Total", value = textOutput("eey_value_t"), subtitle = textOutput("eey_moe_t"),
                            icon = shiny::icon("users"), color = "black", width = 4,
                            href = NULL, fill = FALSE)
                         ), collapsible=T
                )
              ),
              tabPanel("Demographics", value = 5,
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Population by Age and Race/Ethnicity - ", HTML('<a href="http://data.ctdata.org/visualization/population-by-age-by-town?v=table&f={%22Town%22:%20%22Connecticut%22,%20%22Variable%22:%20[%22Population%22,%20%22Margins%20of%20Error%22],%20%22Race/Ethnicity%22:%20%22All%22,%20%22Age%20Cohort%22:%20%22Total%22,%20%22Year%22:%20%222012-2016%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                  column(9,
                    box(width="100%",
                        plotlyOutput("DPlot_age_race", width="100%")
                    )
                  ),
                  column(3,
                    box(width="100%",
                      selectInput("race", 
                                   label = HTML('<h4 style="color:black;">Select Race/Ethnicity:</h4>'),
                                   choices = list("Native Hawaiian and Other Pacific Islander", 
                                                  "American Indian and Alaska Native Alone",
                                                  "Two or More Races",
                                                  "Asian Alone",
                                                  "Some Other Race Alone",
                                                  "Black or African American Alone",
                                                  "Hispanic or Latino",
                                                  "White Alone Not Hispanic or Latino",
                                                  "White Alone", 
                                                  "All"), selected = "All"), 
                      size = "default"
                    )
                  ), collapsible=T
                )
                ),  
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Population by Age and Gender - ", HTML('<a href="http://data.ctdata.org/visualization/population-by-age-by-town?v=table&f={%22Town%22:%20%22Connecticut%22,%20%22Variable%22:%20[%22Population%22,%20%22Margins%20of%20Error%22],%20%22Race/Ethnicity%22:%20%22All%22,%20%22Age%20Cohort%22:%20%22Total%22,%20%22Year%22:%20%222012-2016%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                        plotlyOutput("DPlot_age", width="100%"),
                        collapsible = T
                  )
                ),
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Population by Race/Ethnicity and Gender - ", HTML('<a href="http://data.ctdata.org/visualization/population-by-age-by-town?v=table&f={%22Town%22:%20%22Connecticut%22,%20%22Variable%22:%20[%22Population%22,%20%22Margins%20of%20Error%22],%20%22Race/Ethnicity%22:%20%22All%22,%20%22Age%20Cohort%22:%20%22Total%22,%20%22Year%22:%20%222012-2016%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                        plotlyOutput("DPlot_race", width="100%"),
                        collapsible = T
                  )
                ),
                fluidRow(
                  box(width=12, title = tagList(shiny::icon("bar-chart"), "Median Household Income - ", HTML('<a href="http://data.ctdata.org/visualization/educational-need?v=table&f={%22Variable%22:%20%22Indicator%20of%20Educational%20Need%22,%20%22Measure%20Type%22:%20%22Percent%22,%20%22Indicator%20of%20Educational%20Need%22:%20[%22Special%20Education%22,%20%22English%20Language%20Learner%22,%20%22Eligible%20for%20Free%20or%20Reduced%20Price%20Lunch%22],%20%22District%22:%20%22Hartford%20School%20District%22,%20%22Year%22:%20[%222014-2015%22,%20%222015-2016%22,%20%222016-2017%22]}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                  column(9,
                    box(width="100%",
                        plotlyOutput("Dplot_mhi", width="100%")
                    )
                  ),
                  column(3,
                    box(width="100%",
                      h3(textOutput("mhi_text")),
                      HTML('<h4 style="color:black;"> Median Household Income</h4>'),
                      HTML('<h3><span id="mhi_value" class="shiny-text-output"></span>&nbsp;<span id="mhi_moe" 
                           style="color:grey;font-size:0.67em;" 
                           class="shiny-text-output"></span></h3>')
                    )
                  ), collapsible=T
                )
                ) 
              ),
              tabPanel("Behavioral Health", value = 6,
                fluidRow(
                  box(width=12,title =  tagList(shiny::icon("bar-chart"), "Children in Need of Treatment by Age and Race"),
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
                # column(12,
                   box(width=12,title =  tagList(shiny::icon("bar-chart"), "Educational Need", max_year_edu, HTML('<a href="http://data.ctdata.org/visualization/educational-need?v=table&f={%22Variable%22:%20%22Indicator%20of%20Educational%20Need%22,%20%22Measure%20Type%22:%20%22Percent%22,%20%22Indicator%20of%20Educational%20Need%22:%20[%22Special%20Education%22,%20%22English%20Language%20Learner%22,%20%22Eligible%20for%20Free%20or%20Reduced%20Price%20Lunch%22],%20%22District%22:%20%22Hartford%20School%20District%22,%20%22Year%22:%20[%222014-2015%22,%20%222015-2016%22,%20%222016-2017%22]}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                       plotlyOutput("EPlot1"),
                       collapsible = T
                   ),
                   box(width=12,title =  tagList(shiny::icon("bar-chart"), "Suspension Rate by Race", max_year_edu2, HTML('<a href="http://data.ctdata.org/visualization/suspension-rate-by-race?v=table&f={%22Variable%22:%20%22Suspensions%22,%20%22Measure%20Type%22:%20%22Percent%22,%20%22District%22:%20%22Connecticut%22,%20%22Year%22:%20%222015-2016%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                       plotlyOutput("EPlot2"),
                       collapsible = T, collapsed = T
                   ),
                   box(width=12,title =  tagList(shiny::icon("bar-chart"), "Sanctions by Type", max_year_edu3, HTML('<a href="http://data.ctdata.org/visualization/sanctions?v=table&f={%22Variable%22:%20%22Sanctions%22,%20%22Measure%20Type%22:%20%22Number%22,%20%22District%22:%20%22Connecticut%22,%20%22Year%22:%20%222015-2016%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                       plotlyOutput("EPlot3"),
                       collapsible = T, collapsed = T
                   ), 
                   box(width=12,title =  tagList(shiny::icon("bar-chart"), "Incidents by Type", max_year_edu4, HTML('<a href="http://data.ctdata.org/visualization/incidents?v=table&f={%22Variable%22:%20%22Incidents%22,%20%22Measure%20Type%22:%20%22Number%22,%20%22District%22:%20%22Connecticut%22,%20%22Year%22:%20%222015-2016%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                       plotlyOutput("EPlot4"),
                       collapsible = T, collapsed = T
                   )
                   # box(width=12,title =  tagList(shiny::icon("bar-chart"), "Kindergarten Entrance Inventory", max_year_kei, HTML('<a href="http://data.ctdata.org/visualization/kindergarten-entrance-inventory-results?v=table&f={%22Variable%22:%20%22Kindergarten%20Entrance%20Inventory%20Results%22,%20%22Measure%20Type%22:%20%22Percent%22,%20%22District%22:%20%22Connecticut%22,%20%22Year%22:%20%222016-2017%22}" target="_blank"><font color="dodgerblue">Explore the Data</font></a>')),
                   #     uiOutput("kei_plots"),
                   #     collapsible = T, collapsed = T
                   # )
                # )
                )
              ),
              id = "tabselected"
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
        "Additional Resources", 
        icon = icon("info-circle"),
        fluidRow(
          column(12, 
                 panel_div(class_type = "primary", 
                           panel_title = "Further Reading",
                           content = "<b>The Child Health and Development Institute of Connecticut</b> - <a href='https://www.chdi.org/' target='_blank'> www.chdi.org</a><hr>
                                      <b>Network of Care Analysis:</b>"
                 ), 
                 panel_div(class_type = "info", 
                           panel_title = "Data Sources",
                           content = "<b>Connecticut Public Data</b> - <a href='https://data.ct.gov/' target='_blank'> www.data.ct.gov</a><br>
                                      <ul>Pursuant to Executive Order 39, executive branch agencies must publish open public data to the state's Open Data portal.</ul><hr>
                                      <b>Individual School Level Data</b> - <a href='http://edsight.ct.gov/SASPortal/main.do' target='_blank'> www.edsight.ct.gov</a><br>
                                      <ul>School level data from the State Department of Education.</ul><hr>
                                      <b>Judicial Branch Public Data</b> - <a href='https://www.jud.ct.gov/Statistics/' target='_blank'> www.jud.ct.gov</a><br>
                                      <ul><p>Judicial Branch Statistics and Reports</ul>"
                 ), 
                 panel_div(class_type = "primary", 
                           panel_title = "Data Processing",
                           content = "<ul>
                                      <li>Click <a href='https://docs.google.com/spreadsheets/d/1qDh0Vg6RWLerTAzlF2A1xoyUGWxCB4Cq_rppfnl6Vek/edit?usp=sharing' target='_blank'> here</a> 
                                          to see when data is added to the Dashboard!
                                      <ul/>"
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
      )
    )
  )   
) # end of shiny                        