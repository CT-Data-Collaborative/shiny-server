library(shiny)
library(shinyBS)
library(shinyLP)
library(shinyjs)
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
    title = "Connecticut SEOW Prevention Data Portal",
      tags$head(
        tags$link(href = "style.css", rel = "stylesheet"),
        tags$link(rel = "shortcut icon", type="image/x-icon", src = 'favicon.ico'), 
        tags$style(HTML(" .navbar-default  {
                       font-size: 1.2em !important;
                       border: 0 !important; 
                       -webkit-box-shadow: none; 
                       box-shadow: none;
                       background-color: #0e005f !important;
                       color: #fff !important;
                   }", 
                   ".navbar .navbar-text {
                       float: right;
                   }", 
                   ".navbar-nav li a:hover, .navbar-nav > .active > a {
                             color: black !important;
                             background-color:white !important;
                             background-image: none !important;
                   }", 
                   "body {padding-top: 30px;}")) 
      ),
        div(style="padding: 1px 0px; width: '100%'", titlePanel(title="", windowTitle="Connecticut SEOW Prevention Data Portal")),

      div(id = "header",
          div(id = "title", "Connecticut SEOW Prevention Data Portal", width="100"), 
              #img(src = "combined-logos.png", width = "150")),

# <a href='https://health.uconn.edu/community-medicine/programs/health-services-research-unit/center-for-prevention-evaluation-and-statistics-cpes/' target='_blank'>
#                    <img border='1' alt='cpes' title='Center for Prevention Evaluation and Statistics' src='CPES logo final 022818.PNG' width='70'><a href='http://www.ct.gov/dmhas/cwp/view.asp?a=2912&Q=335152&dmhasNav=%7C' target='_blank'>
#                    <img border='1' alt='dmhas' title = 'Department of Mental Health and Addiction Services' src='dmhas-logo.jpg'  width='70'></a><a href='https://health.uconn.edu/community-medicine/programs/health-services-research-unit/' target='_blank'>
#                    <img border='1' alt='uconn' title = 'UConn Health' src='uconn-health-wordmark-stacked-blue.png' width='70'></a><a href='http://ctdata.org/'target='_blank'>
#                    <img border='1' alt='ctdata' title = 'Connecticut Data Collaborative' src='logo2.png' width='70'></a>"))),
        div(id = "subtitle",
          HTML("Bringing together Connecticut’s epidemiological data in support of a comprehensive 
                              public health approach to substance abuse prevention and health promotion.")
        )
      ),
      navbarPage(id="tabs",
        position = "fixed-top",
        title="",
        inverse = T, # for diff color view
        theme = shinytheme("lumen"),
        tabPanel(value = "home",
            div(icon("home"), "Home"),
            tabItem("navigation",
                fluidRow(
                        box(width=4, status = "success", solidHeader = TRUE,
                          align="center",
                          icon("bar-chart", "fa-5x"),
                          h1("Indicators"),
                          h5("Search through indicators by Geography, Dimensions and More"),
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
                      box(width=4, status = "success", solidHeader = TRUE,
                          align="center",
                          icon("table", "fa-5x"),
                          h1("Data Sets"),
                          h5("Search through data sets by Source, Geography, Dimensions and More"),
                          actionButton(inputId = "do2", label = "View Data Sets", icon = icon("arrow-circle-right"), 
                                       style = "color: white; 
                                                background-color: #0e005f; 
                                                position: relative; 
                                                text-align:center;
                                                border-radius: 6px;
                                                border-width: 2px")
                      ),
                      box(width=4, status = "success", solidHeader = TRUE,
                          align="center",
                          icon("info-circle", "fa-5x"),
                          h1("Resources"),
                          h5("Explore Connecticut’s prevention partners and data resources."),
                          actionButton(inputId = "do3", label = "View Resources", icon = icon("arrow-circle-right"),
                                       style = "color: white; 
                                                background-color: #0e005f; 
                                                position: relative; 
                                                text-align:center;
                                                border-radius: 6px;
                                                border-width: 2px")
                     )    
              ), 
              fluidRow(
                  box(width=4, status = "success", solidHeader = TRUE,
                          align="center",                      
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
                  ),                 
                  box(width=4,  status = "success", solidHeader = TRUE,
                          align="center",                      
                      icon("line-chart", "fa-5x"),
                      h1("Products"),
                      h5("Explore additional products such as charts, maps, and tables"),
                      actionButton(inputId = "do5", label = "View Products", icon = icon("arrow-circle-right"),
                                   style = "color: white; 
                                            background-color: #0e005f; 
                                            position: relative; 
                                            text-align:center;
                                            border-radius: 6px;
                                            border-width: 2px")
                  ),
                  box(width=4, status = "success", solidHeader = TRUE,
                          align="center",                      
                      icon("bullhorn", "fa-5x"),
                      h1("News"),
                      h5("Stay up to date with what’s new and other happenings!"),
                      actionButton(inputId = "do6", label = "View News", icon = icon("arrow-circle-right"),
                                   style = "color: white; 
                                            background-color: #0e005f; 
                                            position: relative; 
                                            text-align:center;
                                            border-radius: 6px;
                                            border-width: 2px")
                  )
                ),
                fluidRow(
                  box(width=12, align = "center",
                    HTML("<h3 style='background-color:#86c658; padding: 15px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px
                              0 rgba(0, 0, 0, 0.19);'>Partners</h3>"),
                    # HTML("<p text-align: left;>The Connecticut SEOW Prevention Data Portal, developed with support from 
                    #            the Department of Mental Health and Addiction Services (DMHAS), is a collaborative effort of the 
                    #            UConn Health Center for Prevention Evaluation and Statistics (CPES), the State Epidemiological 
                    #            Outcomes Workgroup (SEOW) and the Connecticut Data Collaborative.</p>"),
                    HTML("<a href='https://health.uconn.edu/community-medicine/programs/health-services-research-unit/center-for-prevention-evaluation-and-statistics-cpes/' 
                         target='_blank'><img border='0' alt='cpes' title='Center for Prevention Evaluation and Statistics' src='CPES logo final 022818.PNG' width='125'></a>"),
                    HTML("<a href='http://www.ct.gov/dmhas/cwp/view.asp?a=2912&Q=335152&dmhasNav=%7C' 
                         target='_blank'><img border='0' alt='dmhas' title = 'Department of Mental Health and Addiction Services' src='LOGOtransparentcolor.png' width='125'></a>"),
                    HTML("<a href='https://health.uconn.edu/community-medicine/programs/health-services-research-unit/' 
                         target='_blank'><img border='0' alt='uconn' title = 'UConn Health' src='uconn-health-wordmark-stacked-blue.png' width='125'></a>"),
                    HTML("<a href='http://ctdata.org/' 
                         target='_blank'><img border='0' alt='ctdata' title = 'Connecticut Data Collaborative' src='ctdata-logo.png' width='125'></a>")
                )
                )
        ), 
        tags$footer(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                      HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                           "Copyright © 2018 CT Data Collaborative", 
                           sep="<br/>")), 
                      align = "center", 
                      position = "absolute"
          )
      ),
      tabPanel(value="indicators",
        div(icon("bar-chart"), "Indicators"),  
        div(style="display:inline-block;width:100%;text-align: right;",
                actionButton(inputId = "do0a", 
                                       label = "Back Home", 
                                       icon = icon("arrow-circle-left"),
                                       style = "color: white; 
                                                background-color: #0e005f; 
                                                position: relative; 
                                                text-align:center;
                                                border-radius: 6px;
                                                border-width: 2px;
                                                ")
            ),
        div(DT::dataTableOutput("indicator_table"), style = "font-size: 90%; width: 100%"),
        tags$footer(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                      HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                           "Copyright © 2018 CT Data Collaborative", 
                           sep="<br/>")), 
                      align = "center", 
                      position = "absolute"
          )
      ),
      tabPanel(
        div(icon("table"), "Data Sets"), value="datasets",
            div(style="display:inline-block;width:100%;text-align: right;",
                actionButton(inputId = "do0b", 
                                       label = "Back Home", 
                                       icon = icon("arrow-circle-left"),
                                       style = "color: white; 
                                                background-color: #0e005f; 
                                                position: relative; 
                                                text-align:center;
                                                border-radius: 6px;
                                                border-width: 2px;
                                                ")
            ),
        div(DT::dataTableOutput("dataset_table"), style = "font-size: 90%; width: 100%"),
        tags$footer(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                      HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                           "Copyright © 2018 CT Data Collaborative", 
                           sep="<br/>")), 
                      align = "center", 
                      position = "absolute"
          )
      ),
      tabPanel(
        div(icon("info-circle"), "Resources"), value="resources", id = "resourcepage",
        dashboardPage(
          dashboardHeader(disable=T),
          dashboardSidebar(disable=T),
          dashboardBody(
            div(style="display:inline-block;width:100%;text-align: right;",
                actionButton(inputId = "do0c", 
                                       label = "Back Home", 
                                       icon = icon("arrow-circle-left"),
                                       style = "color: white; 
                                                background-color: #0e005f; 
                                                position: relative; 
                                                text-align:center;
                                                border-radius: 6px;
                                                border-width: 2px;
                                                ")
            ),            
            fluidRow(
              box(width=12, title = tagList(icon("handshake"), "Prevention Partners"), status = "primary", solidHeader = TRUE,
                column(7, offset = 2, 
                  HTML("<b><a href='https://health.uconn.edu/community-medicine/programs/health-services-research-unit/center-for-prevention-evaluation-and-statistics-cpes/' target='_blank'><font color='#0e005f'><b>
                            Center for Prevention Evaluation and Statistics (CPES)</b></font></a><br></b>
                            <font color='black'><ul style = 'line-height: 1.6;'>CPES is a DMHAS-funded Resource Link, staffed by staff from UConn Health to support the Prevention 
                           and Health Promotion (PHP) Division in its efforts through the identification, collection, analysis, 
                           interpretation and dissemination of data pertaining to substance abuse prevention, mental health, 
                           and health disparities.</ul></font>"), 
                 HTML("<b><a href='http://www.preventsuicidect.org/' target='_blank'><font color='#0e005f'><b>
                            Connecticut Suicide Advisory Board (CTSAB)</b></font></a><br></b>
                            <font color='black'><ul style = 'line-height: 1.6;'>The Connecticut Suicide Advisory Board (CTSAB) is the single, legislatively mandated, state-level suicide 
                           advisory board that addresses suicide prevention and response across the lifespan. </ul></font>"), 
                 HTML("<b><a href='http://www.ct.gov/dmhas/cwp/view.asp?a=2912&Q=335152&dmhasNav=%7C' target='_blank'><font color='#0e005f'><b>
                            Department of Mental Health and Addiction Services (DMHAS)</b></font></a><br></b>
                            <font color='black'><ul style = 'line-height: 1.6;'>The DMHAS prevention system is designed to promote the overall health and wellness of individuals and 
                            communities by preventing or delaying substance use and promoting mental health.</ul></font>"), 
                 HTML("<b><a href='https://health.uconn.edu/community-medicine/programs/health-services-research-unit/' target='_blank'><font color='#0e005f'><b>
                            UConn Health Department of Community Medicine and Healthcare</b></font></a><br></b>
                            <font color='black'><ul style = 'line-height: 1.6;'>The department's mission is to provide education, research, and service to the University and the State of Connecticut 
                            and the global public health community. </ul></font>"), 
                 HTML("<b><a href='https://www.samhsa.gov/about-us/who-we-are/offices-centers/csap' target='_blank'><font color='#0e005f'><b>
                            Substance Abuse and Mental Health Services Administration (SAMHSA), Center for Substance Abuse Prevention (CSAP)</b></font></a><br></b>
                            <font color='black'><ul style = 'line-height: 1.6;'>SAMHSA, under the U.S. Department of Health and Human Services, leads public health efforts to advance the behavioral health 
                            of the nation. SAMHSA's mission is to reduce the impact of substance abuse and mental illness on America's communities. <br> 
                            The mission of the CSAP, within SAMHSA, is to improve behavioral health through evidence-based prevention approaches. 
                     The CSAP works with federal, state, public, and private organizations to develop comprehensive prevention systems by providing 
                     national leadership in the development of policies, programs, and services and promoting effective substance abuse prevention 
                     practices that enable states, communities, and other organizations to apply prevention knowledge effectively.</ul></font>"), 
                 HTML("<b><a href='https://health.uconn.edu/community-medicine/programs/health-services-research-unit/center-for-prevention-evaluation-and-statistics-cpes/the-seow/' target='_blank'><font color='#0e005f'><b>
                            The State Epidemiological Outcomes Workgroup (SEOW)</b></font></a><br></b>
                            <font color='black'><ul style = 'line-height: 1.6;'>The SEOW is a collaborative group of State
                            agency representatives and key stakeholders committed to the identification, sharing and use of
                            data to improve substance abuse prevention and mental health promotion, and behavioral health in
                            general. Read more <a href='https://s3.amazonaws.com/cpes-ctdata/reports/About+the+SEOW.doc' 
                            target='_blank'><font color='#0e005f'><u>here</u></font></a>                          
                           </ul></font>"), 
                 HTML("<font color='#0e005f'><b>SEOW Member Organizations</b></font>
                            <font color='black'><ul style='list-style: none; line-height: 1.6;'>
                            <li>AIDS CT
                            <li>Board of Pardons and Parole
                            <li>Child Health and Development Institute (CHDI)
                            <li>Connecticut Hospital Association (CHA)
                            <li>Connecticut Transportation Safety Research Center
                            <li>Connecticut Youth Services Organization
                            <li>Court Support Services Division, Judicial Branch (CSSD)
                            <li>Department of Children and Families (DCF)
                            <li>Department of Consumer Protection (DCP)
                            <li>Department of Corrections (DOC)
                            <li>Department of Mental Health and Addiction Services (DMHAS)
                            <li>Department of Public Health (DPH)
                            <li>Department of Transportation (DOT)
                            <li>Office of the Child Advocate
                            <li>Office of Early Childhood
                            <li>Office of Policy and Management (OPM)
                            <li>Southeastern Regional Action Council (SERAC)
                            <li>UConn Health
                            <li>Yale School of Medicine</ul></font>")
                ), collapsible = T, collapsed = T
              )
            ),            
            fluidRow(
              box(width=12, title = tagList(icon("handshake"), "Data Resources"), status = "primary", solidHeader = TRUE,
                column(7, offset = 2, 
                HTML("<b><a href='https://www.ctcrash.uconn.edu/' target='_blank'><font color='#0e005f'><b>
                            Connecticut Crash Data Repository (CTCDR)</b></font></a><br></b>
                            <font color='black'><ul style = 'line-height: 1.6;'>The Connecticut Crash Data Repository (CTCDR), 
                     maintained by UConn, is a web tool designed to provide access to select crash information collected by 
                     state and local police. This data repository enables users to query, analyze and print/export the data 
                     for research and informational purposes. The CTCDR is comprised of crash data from two separate sources; 
                     The Department of Public Safety (DPS) and The Connecticut Department of Transportation (CTDOT).</ul></font>"),
                HTML("<b><a href='http://ctdata.org/' target='_blank'><font color='#0e005f'><b>
                            Connecticut Data Collaborative</b></font></a><br></b>
                            <font color='black'><ul style = 'line-height: 1.6;'>Striving for informed decision-making across Connecticut, 
                     the CT Data Collaborative empowers an ecosystem of data users by democratizing access to public data and building data 
                     literacy. In addition to making data available, and staffing the Data Academy, CTDC manages content and functionality 
                     of CPES’ SEOW Prevention Data Portal.</ul></font>"), 
                HTML("<b><a href='https://data.ct.gov/' target='_blank'><font color='#0e005f'><b>
                            Connecticut Open Data Initiative and Portal</b></font></a><br></b>
                            <font color='black'><ul style = 'line-height: 1.6;'>The Open Data Initiative and Portal, 
                     formed as result of Governor Malloy’s Executive Order 39, provides open access to state agency data 
                     in its rawest form, as well as analysis and dissemination of the State's enterprise information assets.</ul></font>"), 
                HTML("<b><a href='https://ctsdc.uconn.edu/connecticut_census_data/' target='_blank'><font color='#0e005f'><b>
                            Connecticut State Data Center (CTSDC)</b></font></a><br></b>
                            <font color='black'><ul style = 'line-height: 1.6;'>The Connecticut State Data Center (CTSDC) is the State’s lead agency 
                            in the U.S. Census Bureau’s State Data Center Program that makes census data available locally to the public through a 
                            network of state agencies, universities, libraries, and regional and local governments. The CTSDC, a collaboration between 
                            the <a href='https://lib.uconn.edu/' target='_blank'><font color='#0e005f'><u>University of Connecticut Libraries,</u></font></a>
                            the UConn <a href='https://geography.uconn.edu/' target='_blank'><font color='#0e005f'><u>Department of Geography,</u></font></a>
                            and the <a href='http://www.ct.gov/OPM/site/default.asp' target='_blank'><font color='#0e005f'><u>Office of Policy and Management,</u></font></a> 
                            serves as the state’s official liaison to the <a href='https://www.census.gov/' target='_blank'><font 
                            color='#0e005f'><u>U.S. Census Bureau</u></font></a> and manages a public portal to support data access and visualization.</ul></font>"), 
                HTML("<b><a href='https://www.dea.gov/resource-center/statistics.shtml' target='_blank'><font color='#0e005f'><b>
                            Drug Enforcement Administration (DEA)</b></font></a><br></b>
                            <font color='black'><ul style = 'line-height: 1.6;'>The mission of the DEA is to enforce U.S. controlled substances 
                     laws and regulations and bring to the criminal and civil justice system those involved in the growing, manufacture, or 
                     distribution of controlled substances appearing in or destined for illicit traffic in the United States. The DEA also 
                     has a non-enforcement prevention role aimed at reducing the availability of illicit controlled substances on the domestic 
                     and international markets. DEA supply, trade, and enforcement data and its annual Drug Threat Assessment reports, are 
                     important data assets for monitoring and surveillance of illicit substance trends and identification of emerging issues.</ul></font>"), 
                HTML("<b><a href='https://www.samhsa.gov/data/population-data-nsduh/reports?tab=38' target='_blank'><font color='#0e005f'><b>
                            National Survey on Drug Use and Health (NSDUH)</b></font></a><br></b>
                            <font color='black'><ul style = 'line-height: 1.6;'>The National Survey on Drug Use and Health (NSDUH), Directed by SAMHSA 
                     and conducted by RTI International, is an annual household survey, conducted nationally since 1971, which  provides up-to-date 
                     information on tobacco, alcohol, and drug use, mental health and other health-related issues in the United States. Information from 
                     NSDUH, available at the national and state levels, is used to support prevention and treatment programs, monitor substance use trends, 
                     estimate the need for treatment and inform public health policy.</ul></font>")
                ), collapsible = T, collapsed = T
              )
            ), 
            fluidRow(
              box(width=12, title = tagList(shiny::icon("fa-handshake"), "Other Resources"), status = "primary", solidHeader = TRUE,
                column(7, offset = 2), collapsible = T, collapsed = T)
            )
         )
        ),
        tags$footer(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                      HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                           "Copyright © 2018 CT Data Collaborative", 
                           sep="<br/>")), 
                      align = "center", position = "absolute"
          )
      ), 
    tabPanel(
      div(icon("user-md"), "Epidemiological Profiles"), 
      value="profiles", 
        dashboardPage(
          dashboardHeader(disable=T),
          dashboardSidebar(disable=T),
          dashboardBody(
            div(style="display:inline-block;width:100%;text-align: right;",
                actionButton(inputId = "do0d", 
                                       label = "Back Home", 
                                       icon = icon("arrow-circle-left"),
                                       style = "color: white; 
                                                background-color: #0e005f; 
                                                position: relative; 
                                                text-align:center;
                                                border-radius: 6px;
                                                border-width: 2px;
                                                ")
            ),
            fluidRow(
              box(width=12, title = tagList(shiny::icon("fa-handshake"), "Statewide Epidemiological Profiles by Substance"), status = "primary", solidHeader = TRUE,
                column(12, align = "center", 
                         tags$a(div(HTML("<p>Alcohol</p>"), style="display: inline-block; margin:10px;",
                               img(src="alcohol.jpg", width=125)), 
                               href = "https://s3.amazonaws.com/cpes-ctdata/reports/Alcohol+Profile.pdf"),
                         tags$a(div(HTML("<p>Cocaine</p>"), style="display: inline-block; margin:10px;",
                               img(src="cocaine.jpg", width=125)),
                               href = "https://s3.amazonaws.com/cpes-ctdata/reports/Cocaine+Profile.pdf"), 
                         tags$a(div(HTML("<p>Heroin</p>"), style="display: inline-block; margin:10px;",
                               img(src="heroin.jpg", width=125)),
                               href = "https://s3.amazonaws.com/cpes-ctdata/reports/Heroin+Profile.pdf"),
                         tags$a(div(HTML("<p>Marijuana</p>"), style="display: inline-block; margin:10px;",
                               img(src="marijuana.jpg", width=125)),
                               href = "https://s3.amazonaws.com/cpes-ctdata/reports/Marijuana+Profile.pdf"),
                         tags$a(div(HTML("<p>Prescription Drugs</p>"), style="display: inline-block; margin:10px;",
                               img(src="pres_drugs.jpg", width=125)),
                               href = "https://s3.amazonaws.com/cpes-ctdata/reports/Prescription+Drugs+Profile.pdf")
                  ), collapsible = T)
            ),
            fluidRow(
              box(width=12, title = tagList(shiny::icon("fa-handshake"), "Comprehensive Statewide Epidemiological Profile"), status = "primary", solidHeader = TRUE,
                column(7, offset = 2), collapsible = T)
            ),
            fluidRow(
              box(width=12, title = tagList(shiny::icon("fa-handshake"), "Statewide Mental Health Epidemiological Profiles"), status = "primary", solidHeader = TRUE,
                column(7, offset = 2), collapsible = T)
            ),
            fluidRow(
              box(width=12, title = tagList(shiny::icon("fa-handshake"), "Indicator definitions"), status = "primary", solidHeader = TRUE,
                column(7, offset = 2), collapsible = T)
            )              
          )
    ),          
        tags$footer(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                      HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                           "Copyright © 2018 CT Data Collaborative", 
                           sep="<br/>")), 
                      align = "center", 
                      position = "absolute"
        )
  ), 
  tabPanel(
        div(icon("line-chart"), "Products"), 
        value="products",
        dashboardPage(
          dashboardHeader(disable=T),
          dashboardSidebar(disable=T),
          dashboardBody(  
            div(style="display:inline-block;width:100%;text-align: right;",
                actionButton(inputId = "do0e", 
                                       label = "Back Home", 
                                       icon = icon("arrow-circle-left"),
                                       style = "color: white; 
                                                background-color: #0e005f; 
                                                position: relative; 
                                                text-align:center;
                                                border-radius: 6px;
                                                border-width: 2px;
                                                ")
            ),  
            fluidRow(
              box(width=12, title = tagList(shiny::icon("fa-handshake"), "Products Coming Soon"), status = "primary", solidHeader = TRUE,
                column(7, offset = 2), collapsible = T)
            )            
          )
        ),
        tags$footer(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                      HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                           "Copyright © 2018 CT Data Collaborative", 
                           sep="<br/>")), 
                      align = "center", 
                      position = "absolute"
          )
      ),
  tabPanel(
    div(icon("bullhorn"), "News"), 
        value="news",
        dashboardPage(
          dashboardHeader(disable=T),
          dashboardSidebar(disable=T),
          dashboardBody( 
            div(style="display:inline-block;width:100%;text-align: right;",
                actionButton(inputId = "do0f", 
                                       label = "Back Home", 
                                       icon = icon("arrow-circle-left"),
                                       style = "color: white; 
                                                background-color: #0e005f; 
                                                position: relative; 
                                                text-align:center;
                                                border-radius: 6px;
                                                border-width: 2px;
                                                ")
            ),  
            fluidRow(
              box(width=12, title = tagList(shiny::icon("fa-handshake"), "News Coming Soon"), status = "primary", solidHeader = TRUE,
                column(7, offset = 2), collapsible = T)
            ) 
          )
        ),
        tags$footer(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                      HTML(paste("Connecticut Data Collaborative is a Project of InformCT, Inc.", 
                           "Copyright © 2018 CT Data Collaborative", 
                           sep="<br/>")), 
                      align = "center", 
                      position = "absolute"
          )
  )
  )
  )
)
