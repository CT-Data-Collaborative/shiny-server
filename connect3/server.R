#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic
shinyServer(function(input, output, session) {
  cols <- c("1" = "darkblue")
    region_map_reactive <- reactive({
		if(input$select=="Statewide") {
        ggplot() + geom_polygon(data = dcf_regions_CT, aes(x = long, y = lat, group = group, fill = Value, col="yellow"), color = "black") + 
        scale_fill_distiller(guide=FALSE, direction=1) + scale_colour_manual(values = cols) +  theme(rect = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
        labs(x="", y="")
		} else if (input$select=="Southwest Region") {
        ggplot() + geom_polygon(data = dcf_regions_1, aes(x = long, y = lat, group = group, fill = Value), color = "black") + 
        scale_fill_distiller(guide=FALSE, direction=1) + theme(rect = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
        labs(x="", y="")
		} else if (input$select=="South Central Region") {
        ggplot() + geom_polygon(data = dcf_regions_2, aes(x = long, y = lat, group = group, fill = Value), color = "black") + 
        scale_fill_distiller(guide=FALSE, direction=1) + theme(rect = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
        labs(x="", y="")
		} else if (input$select=="Eastern Region") {
        ggplot() + geom_polygon(data = dcf_regions_3, aes(x = long, y = lat, group = group, fill = Value), color = "black") + 
        scale_fill_distiller(guide=FALSE, direction=1) + theme(rect = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
        labs(x="", y="")
		} else if (input$select=="North Central Region") {
        ggplot() + geom_polygon(data = dcf_regions_4, aes(x = long, y = lat, group = group, fill = Value), color = "black") + 
        scale_fill_distiller(guide=FALSE, direction=1) + theme(rect = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
        labs(x="", y="")
		} else if (input$select=="Western Region") {
        ggplot() + geom_polygon(data = dcf_regions_5, aes(x = long, y = lat, group = group, fill = Value), color = "black") + 
        scale_fill_distiller(guide=FALSE, direction=1) + theme(rect = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
        labs(x="", y="")
		} else if (input$select=="Central Region") {
        ggplot() + geom_polygon(data = dcf_regions_6, aes(x = long, y = lat, group = group, fill = Value), color = "black") + 
        scale_fill_distiller(guide=FALSE, direction=1) + theme(rect = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
        labs(x="", y="")
		  }
	})

  output$gg_regions <- renderPlot({region_map_reactive()}, height = 200, width = 300)  
  ###########################
  region_text_reactive <- reactive({
    if(input$select=="Statewide") {
      paste(" ")
    } else {
      HTML(paste("<font color=\"#000000\"><b>", "Towns in:", input$select, "</b></font>"))
    }
  })
  output$region_text <- renderUI({region_text_reactive()})
  ###########################
  region_list_reactive <- reactive({
    if(input$select=="Southwest Region") {
            HTML(paste("<font color=\"#000000\">", "<ol><li>Bridgeport</li><li>Darien</li><li>Easton</li><li>Fairfield</li><li>Greenwich</li><li>Monroe</li><li>New Canaan</li><li>Norwalk</li><li>Stamford</li><li>Stratford</li><li>Trumbull</li><li>Weston</li><li>Westport</li><li>Wilton</li></ol>", "</font>"))
    } else if (input$select=="South Central Region") {
            HTML(paste("<font color=\"#000000\">", "<ol><li>Ansonia</li><li>Bethany</li><li>Branford</li><li>Derby</li><li>East Haven</li><li>Hamden</li><li>Milford</li><li>New Haven </li><li>North Branford</li><li>North Haven</li><li>Orange</li><li>Seymour</li><li>Shelton</li><li>West Haven</li><li>Woodbridge</li></ol>", "</font>"))
    } else if (input$select=="Eastern Region") {
            HTML(paste("<font color=\"#000000\">", "<ol><li>Ashford</li><li>Bozrah </li><li>Brooklyn </li><li>Canterbury </li><li>Chaplin </li><li>Chester </li><li>Clinton </li><li>Colchester </li><li>Columbia </li><li>Coventry </li><li>Cromwell </li><li>Deep River </li><li>Durham </li><li>East Haddam </li><li>East Hampton </li><li>East Lyme </li><li>Eastford </li><li>Essex </li><li>Franklin </li><li>Griswold </li><li>Groton </li><li>Guilford </li><li>Haddam </li><li>Hampton </li><li>Killingly </li><li>Killingworth </li><li>Lebanon </li><li>Ledyard </li><li>Lisbon </li><li>Lyme </li><li>Madison </li><li>Mansfield </li><li>Middlefield  </li><li>Middletown </li><li>Montville </li><li>New London</li><li>North Stonington </li><li>Norwich </li><li>Old Lyme </li><li>Old Saybrook </li><li>Plainfield </li><li>Pomfret </li><li>Portland </li><li>Preston </li><li>Putnam </li><li>Salem </li><li>Scotland </li><li>Sprague </li><li>Sterling </li><li>Stonington </li><li>Thompson </li><li>Union </li><li>Voluntown </li><li>Waterford </li><li>Westbrook </li><li>Willington </li><li>Windham </li><li>Woodstock</li></ol>", "</font>"))
    } else if (input$select=="North Central Region") {
            HTML(paste("<font color=\"#000000\">", "<ol><li>Andover</li><li>Bloomfield</li><li>Bolton</li><li>East Granby</li><li>East Hartford</li><li>East Windsor</li><li>Ellington</li><li>Enfield</li><li>Glastonbury</li><li>Granby</li><li>Hartford</li><li>Hebron</li><li>Manchester</li><li>Marlborough</li><li>Somers</li><li>South Windsor</li><li>Stafford</li><li>Suffield</li><li>Tolland</li><li>Vernon</li><li>West Hartford</li><li>Windsor</li><li>Windsor Locks</li></ol>", "</font>"))
    } else if (input$select=="Western Region") {
            HTML(paste("<font color=\"#000000\">", "<ol><li>Barkhamsted</li><li>Beacon Falls</li><li>Bethel</li><li>Bethlehem</li><li>Bridgewater</li><li>Brookfield</li><li>Canaan</li><li> Cheshire</li><li>Colebrook</li><li>Cornwall</li><li>Danbury</li><li>Goshen</li><li>Hartland</li><li>Harwinton</li><li>Kent</li><li> Litchfield</li><li>Middlebury</li><li>Morris</li><li>Naugatuck</li><li>New Fairfield</li><li>New Hartford</li><li>New Milford</li><li> Newtown</li><li>Norfolk</li><li>North Canaan</li><li>Oxford</li><li>Prospect</li><li>Redding</li><li>Ridgefield</li><li>Roxbury</li><li> Salisbury</li><li>Sharon</li><li>Sherman</li><li>Southbury</li><li>Thomaston</li><li>Torrington</li><li>Warren</li><li>Washington</li><li> Waterbury</li><li>Watertown</li><li>Winchester</li><li>Wolcott</li><li>Woodbury</li></ol>", "</font>"))
    } else if (input$select=="Central Region") {
            HTML(paste("<font color=\"#000000\">", "<ol><li>Avon</li><li>Berlin</li><li>Bristol</li><li>Burlington</li><li>Canton</li><li>Farmington</li><li>Meriden</li><li>New Britain</li><li> Newington</li><li>Plainville</li><li>Plymouth</li><li>Rocky Hill</li><li>Simsbury</li><li>Southington</li><li>Wallingford</li><li> Wethersfield</li></ol>", "</font>"))
    }
  })
  ###########################
  output$region_list <- renderUI({region_list_reactive()})
  ###########################
  output$HPlot1 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    h_plot1 <- health_regions[health_regions$`Measure Type` == "Rate" & health_regions$Region == selected & 
                               health_regions$Year == max_year_health_regions,]
    h_plot1 <- unique(h_plot1 %>%
                       group_by(Type) %>% 
                       mutate(avg_Value = round(mean(Value), 1)) %>% 
                       select(-Town, -Value, -FIPS)) %>% 
                       rename(Value = avg_Value)
    h_plot1 <- spread(h_plot1, Type, Value)
    m <- list(b=50) # l = left; r = right; t = top; b = bottom
    hplot1 <- plot_ly(h_plot1, x = "Fetal", y = ~Fetal, name = 'Fetal', type = 'bar', 
                     hoverinfo = 'text', text = ~paste0(Fetal), textposition = 'auto') %>%
             add_trace(x = "Infant", y = ~Infant, name = 'Infant', text = ~paste0(Infant)) %>%
             add_trace(x = "Neonatal", y = ~Neonatal, name = 'Neonatal', text = ~paste0(Neonatal)) %>%
             add_trace(x = "Postneonatal", y = ~Postneonatal, name = 'Postneonatal', text = ~paste0(Postneonatal)) %>%
     layout(margin=m, title = paste(paste0(unique(h_plot1$Region), ","), "All Races,", max_year_health_regions, sep = " "),
         #xaxis = list(title = "Type"),
         yaxis = list(title = "Rate"), 
         showlegend = FALSE, 
         annotations = list(x = 1, y = -0.15, text = "Source: Connecticut Department of Public Health", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")))
     hplot1
  })
  ###########################
  health_rates_regions$Total.Births <- as.numeric(health_rates_regions$Total.Births)
  
  output$HPlot2 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    h_plot2 <- health_rates_regions[health_rates_regions$Region == selected & health_rates_regions$RR_YR == max_year_health_rates_regions &
                                      health_rates_regions$Type %in% c("Fetal", "Infant") & health_rates_regions$Race != "All",]

    if (selected == "Statewide") {
    h_plot2 <- unique(h_plot2 %>%
      select(Race, Type, Death.Rate, Region))      
    h_plot2 <- spread(h_plot2, Type, Death.Rate)
    } else {
    h_plot2 <- h_plot2 %>%
      group_by(Region, Race, Type) %>%
      mutate(Total_Births = sum(Total.Births),  #per race
             Total_Deaths = sum(Value))  #per race/type
    
    h_plot2$Death_Rate <- round(h_plot2$Total_Deaths*1000/h_plot2$Total_Births,2)
    
    h_plot2 <- unique(h_plot2 %>% 
      select(Race, Type, Death_Rate, Region))
    h_plot2 <- spread(h_plot2, Type, Death_Rate)
      
    }
    h_plot2$Fetal <- as.numeric(h_plot2$Fetal)
    h_plot2$Infant <- as.numeric(h_plot2$Infant)
    m <- list(b=50) # l = left; r = right; t = top; b = bottom
    hplot2 <- plot_ly(h_plot2, x = ~Race, y = ~Fetal, name = 'Fetal', type = 'bar', 
                     hoverinfo = 'text', text = ~paste0(Fetal), textposition = 'auto') %>%
             add_trace(y = ~Infant, name = 'Infant', text = ~paste0(Infant)) %>%
    layout(margin=m, title = paste(paste0(unique(h_plot2$Region), ","), 
                                    max_year_health_rates_regions, sep = " "),
         xaxis = list(title = ""),
         yaxis = list(title = "Rate"), 
         legend = list(x = 0.8, y = 0.85), 
         annotations = list(x = 1, y = -0.15, text = "Source: Connecticut Department of Public Health", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")))
     hplot2
  })  
  ###########################
  output$ECPlot1 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    ec_plot1 <- b23_regions[b23_regions$Region == selected & b23_regions$Year == max_year_b23_regions,]
    ec_plot1 <- unique(ec_plot1 %>%
                       group_by(Indicator) %>% 
                       mutate(tot_Value = round(sum(Value), 0)) %>% 
                       select(-Town, -Value, -FIPS)) %>% 
                       rename(Value = tot_Value)
    ec_plot1 <- spread(ec_plot1, Indicator, Value)
    m <- list(b=80) # l = left; r = right; t = top; b = bottom
    x <- list(tickangle = 0)
    ecplot1 <- plot_ly(ec_plot1, x = "Evaluations", y = ~Evaluations, name = 'Evaluations', type = 'bar', 
                     hoverinfo = 'text', text = ~paste0(Evaluations), textposition = 'auto') %>%
             add_trace(x = "Exited to<br>Early Childhood<br>Special Education", 
                       y = ~`Exited to Early Childhood Special Education`, 
                       name = 'Exited to<br>Early<br>Childhood<br>Special<br>Education', 
                       text = ~paste0(`Exited to Early Childhood Special Education`)) %>%
             add_trace(x = "Individual<br>Family Service<br>Plans", 
                       y = ~`Individual Family Service Plans`, name = 'Individual<br>Family<br>Service<br>Plans', 
                       text = ~paste0(`Individual Family Service Plans`)) %>%
             add_trace(x = "Referrals", y = ~Referrals, name = 'Referrals', text = ~paste0(Referrals)) %>%
             add_trace(x = "Total Eligible", y = ~`Total Eligible`, name = 'Total Eligible', text = ~paste0(`Total Eligible`)) %>%
             add_trace(x = "Total Served", y = ~`Total Served`, name = 'Total Served', text = ~paste0(`Total Served`)) %>%
     layout(margin=m, title = paste(paste0(unique(ec_plot1$Region), ","), max_year_b23_regions, sep = " "),
         xaxis = x, #list(title = "Indicator"),
         yaxis = list(title = "Number"), 
         showlegend = FALSE, 
         annotations = list(x = 1, y = -0.25, text = "Source: Connecticut Office of Early Childhood", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")))
     ecplot1
  })    
  ###########################
  # ec_reactive2 <- reactive({
  #   selected<- input$select
  #   b23c_regions$`Total Births` <- as.numeric(b23c_regions$`Total Births`)
  #   ec_plot2 <- b23c_regions[b23c_regions$Region == selected & b23c_regions$Year == max_year_b23c_regions 
  #                            & b23c_regions$`Measure Type` == "Number",]
  #   ec_plot2 <- unique(ec_plot2 %>% 
  #                        group_by(Indicator) %>% 
  #                        mutate(tot_Value = round(sum(Value), 0), 
  #                               total_Births = sum(`Total Births`), 
  #                               `% Cohort` = round((tot_Value / total_Births)*100, 1)) %>% 
  #                        select(-`% Cohort`, -Value, -`Total Births`, -FIPS, -Year, -`Measure Type`, -Variable, -Town, -total_Births))
  # 
  #   ec_plot2 <- spread(ec_plot2, Indicator, tot_Value)
  #   return(ec_plot2)
  # })

  output$ECPlot2 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    b23c_regions$`Total Births` <- as.numeric(b23c_regions$`Total Births`)
    ec_plot2 <- b23c_regions[b23c_regions$Region == selected & b23c_regions$Year == max_year_b23c_regions 
                             & b23c_regions$`Measure Type` == "Number",]
    ec_plot2 <- unique(ec_plot2 %>% 
                         group_by(Indicator) %>% 
                         mutate(tot_Value = round(sum(Value), 0), 
                                total_Births = sum(`Total Births`), 
                                `% Cohort` = round((tot_Value / total_Births)*100, 1)) %>% 
                         select(-`% Cohort`, -Value, -`Total Births`, -FIPS, -Year, -`Measure Type`, -Variable, -Town, -total_Births))

    ec_plot2 <- spread(ec_plot2, Indicator, tot_Value)
    m <- list(b=80) # l = left; r = right; t = top; b = bottom
    x <- list(tickangle = 0)
    ecplot2 <- plot_ly(ec_plot2, x = "Evaluations", y = ~Evaluations, name = 'Evaluations', type = 'bar', 
                     hoverinfo = 'text', text = ~paste0(Evaluations), textposition = 'auto') %>%
             add_trace(x = "Exited to<br>Early Childhood<br>Special Education", 
                       y = ~`Exited to Early Childhood Special Education`, 
                       name = 'Exited to Early<br>Childhood<br>Special<br>Education', 
                       text = ~paste0(`Exited to Early Childhood Special Education`)) %>%
             add_trace(x = "Individual Family<br>Service Plans", 
                       y = ~`Individual Family Service Plans`, name = 'Individual Family<br>Service<br>Plans', 
                       text = ~paste0(`Individual Family Service Plans`)) %>%
             add_trace(x = "Referrals", y = ~Referrals, name = 'Referrals', text = ~paste0(Referrals)) %>%
             add_trace(x = "Total Eligible", y = ~`Total Eligible`, name = 'Total Eligible', text = ~paste0(`Total Eligible`)) %>%
             add_trace(x = "Total Served", y = ~`Total Served`, name = 'Total Served', text = ~paste0(`Total Served`)) %>%
     layout(margin=m, title = paste(paste0(unique(ec_plot2$Region), ","), max_year_b23c_regions, "Birth Cohort", sep = " "),
         xaxis = x, #list(title = "Indicator"),
         yaxis = list(title = "Number"), 
         showlegend = FALSE, 
         annotations = list(x = 1, y = -0.25, text = "Source: Connecticut Office of Early Childhood", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")))
     ecplot2    
  })
  ###########################
  # ec_reactive3 <- reactive({
  #   selected<- input$select
  #   b23c_regions$`Total Births` <- as.numeric(b23c_regions$`Total Births`)
  #   ec_plot3 <- b23c_regions[b23c_regions$Region == selected & b23c_regions$Year == max_year_b23c_regions 
  #                            & b23c_regions$`Measure Type` == "Number",]
  #   ec_plot3 <- unique(ec_plot3 %>% 
  #                        group_by(Indicator) %>% 
  #                        mutate(tot_Value = round(sum(Value), 0), 
  #                               total_Births = sum(`Total Births`), 
  #                               `% Cohort` = round((tot_Value / total_Births)*100, 1)) %>% 
  #                        select(-tot_Value, -Value, -`Total Births`, -FIPS, -Year, -`Measure Type`, -Variable, -Town, -total_Births))
  # 
  #   ec_plot3 <- spread(ec_plot3, Indicator, `% Cohort`)
  #   return(ec_plot3)
  # })
  
  output$ECPlot3 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
        selected<- input$select
    b23c_regions$`Total Births` <- as.numeric(b23c_regions$`Total Births`)
    ec_plot3 <- b23c_regions[b23c_regions$Region == selected & b23c_regions$Year == max_year_b23c_regions 
                             & b23c_regions$`Measure Type` == "Number",]
    ec_plot3 <- unique(ec_plot3 %>% 
                         group_by(Indicator) %>% 
                         mutate(tot_Value = round(sum(Value), 0), 
                                total_Births = sum(`Total Births`), 
                                `% Cohort` = round((tot_Value / total_Births)*100, 1)) %>% 
                         select(-tot_Value, -Value, -`Total Births`, -FIPS, -Year, -`Measure Type`, -Variable, -Town, -total_Births))

    ec_plot3 <- spread(ec_plot3, Indicator, `% Cohort`)
    m <- list(b=80) # l = left; r = right; t = top; b = bottom
    x <- list(tickangle = 0)
    ecplot3 <- plot_ly(ec_plot3, x = "Evaluations", y = ~Evaluations, name = 'Evaluations', type = 'bar', 
                     hoverinfo = 'text', text = ~paste0(Evaluations, '%'), textposition = 'auto') %>%
             add_trace(x = "Exited to<br>Early Childhood<br>Special Education", 
                       y = ~`Exited to Early Childhood Special Education`, 
                       name = 'Exited to<br>Early<br>Childhood<br>Special<br>Education', 
                       text = ~paste0(`Exited to Early Childhood Special Education`, '%')) %>%
             add_trace(x = "Individual<br>Family Service<br>Plans", 
                       y = ~`Individual Family Service Plans`, name = 'Individual<br>Family<br>Service<br>Plans', 
                       text = ~paste0(`Individual Family Service Plans`, '%')) %>%
             add_trace(x = "Referrals", y = ~Referrals, name = 'Referrals', text = ~paste0(Referrals, '%')) %>%
             add_trace(x = "Total Eligible", y = ~`Total Eligible`, name = 'Total Eligible', text = ~paste0(`Total Eligible`, '%')) %>%
             add_trace(x = "Total Served", y = ~`Total Served`, name = 'Total Served', text = ~paste0(`Total Served`, '%')) %>%
     layout(margin=m, title = paste(paste0(unique(ec_plot3$Region), ","), "% of", max_year_b23c_regions, "Birth Cohort", sep = " "),
         xaxis = x, #list(title = "Indicator"),
         yaxis = list(title = "Percent"), 
         showlegend = FALSE, 
         annotations = list(x = 1, y = -0.25, text = "Source: Connecticut Office of Early Childhood", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")))
     ecplot3 
  })
  ###########################  
  jj_reactive <- reactive({
    selected<- input$select
    jj_plot1 <- jj_regions[jj_regions$Year == max_year_jj_regions & jj_regions$Region == selected & 
                               jj_regions$Variable == "Juvenile Arrests" & jj_regions$`Measure Type` == "Number" &
                               jj_regions$Crime != "Total" & jj_regions$`Age Range` != "Total",]
    jj_plot1 <- unique(jj_plot1 %>% 
                         group_by(`Age Range`, Crime) %>% 
                         mutate(tot_Value = sum(Value)) %>% 
                         select(-Town, -Value, -FIPS, -Year, -Variable, -`Measure Type`))
    jj_plot1 <- spread(jj_plot1, `Age Range`, tot_Value)    
    jj_plot1 <- jj_plot1[jj_plot1$Crime %in% c("Drugs", "Other", "Disorderly Conduct", "Larceny", "Other Assault (Simple)"),]
    
    return(jj_plot1)
  })

  output$JJPlot1 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
        selected<- input$select
    jj_plot1 <- jj_regions[jj_regions$Year == max_year_jj_regions & jj_regions$Region == selected & 
                               jj_regions$Variable == "Juvenile Arrests" & jj_regions$`Measure Type` == "Number" &
                               jj_regions$Crime != "Total" & jj_regions$`Age Range` != "Total",]
    jj_plot1 <- unique(jj_plot1 %>% 
                         group_by(`Age Range`, Crime) %>% 
                         mutate(tot_Value = sum(Value)) %>% 
                         select(-Town, -Value, -FIPS, -Year, -Variable, -`Measure Type`))
    jj_plot1 <- spread(jj_plot1, `Age Range`, tot_Value)    
    jj_plot1 <- jj_plot1[jj_plot1$Crime %in% c("Drugs", "Other", "Disorderly Conduct", "Larceny", "Other Assault (Simple)"),]
    
    m <- list(b=110) # l = left; r = right; t = top; b = bottom
    x <- list(tickangle = 0)
    jjplot1 <- plot_ly(jj_plot1, x = ~str_wrap(Crime, width=15), y = ~`0 to 9 years`, name = '0 to 9 years', type = 'bar', 
                     hoverinfo = 'text', text = ~paste0(`0 to 9 years`), textposition = 'auto') %>%
             add_trace( y = ~`10 to 14 years`, name = '10 to 14 years', text = ~paste0(`10 to 14 years`)) %>%
             add_trace( y = ~`15 to 17 years`, name = '15 to 17 years', text = ~paste0(`15 to 17 years`)) %>%
     layout(margin=m, title = paste(paste0(unique(jj_plot1$Region), ","), max_year_jj_regions, sep = " "),
         xaxis = list(title = ""),
         yaxis = list(title = "Number"), 
         barmode = 'group', legend = list(orientation = 'h', x = 0.2, y = -0.15),
         annotations = list(x = 1, y = -0.35, text = "Source: Connecticut Uniform Crime Report", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")))
     jjplot1    
  })
  ###########################  
  # jj_reactive2 <- reactive({

  #   return(jj_plot2)
  # })
  output$JJPlot2 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    jj_plot2 <- jj_regions[jj_regions$Year == max_year_jj_regions & jj_regions$Region == selected & 
                             jj_regions$Variable == "Juvenile Arrests" & 
                             jj_regions$`Measure Type` == "Rate (per 100,000)" & 
                             jj_regions$Crime == "Total",]
    jj_plot2 <- unique(jj_plot2 %>% 
                         group_by(`Age Range`, Crime) %>% 
                         mutate(avg_Value = round(mean(Value), 1)) %>% 
                         select(-Town, -Value, -FIPS))
    jj_plot2 <- spread(jj_plot2, `Age Range`, avg_Value)
    m <- list(b=90) # l = left; r = right; t = top; b = bottom
    x <- list(tickangle = 0)
    jjplot2 <- plot_ly(jj_plot2, x = "0 to 9 years", y = ~`0 to 9 years`, name = '0 to 9 years', type = 'bar', 
                     hoverinfo = 'text', text = ~paste0(`0 to 9 years`), textposition = 'auto') %>%
               add_trace(x = "10 to 14 years", y = ~`10 to 14 years`, name = '10 to 14 years', text = ~paste0(`10 to 14 years`)) %>%
               add_trace(x = "15 to 17 years", y = ~`15 to 17 years`, name = '15 to 17 years', text = ~paste0(`15 to 17 years`)) %>%
               add_trace(x = "Total", y = ~`Total`, name = 'Total', text = ~paste0(Total)) %>%
     layout(margin=m, title = paste(paste0(unique(jj_plot2$Region), ","), max_year_jj_regions, sep = " "),
         xaxis = x, #list(title = "Indicator"),
         yaxis = list(title = "Rate per 100,000 Persons"), 
         showlegend = FALSE, 
         annotations = list(x = 1, y = -0.2, text = "Source: Connecticut Uniform Crime Report", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")))
     jjplot2 
  })
  ###########################  
  cols <- c("0 to 3 Years", "4 to 6 Years", "7 to 12 Years", "13 to 17 Years", "18 Years and Over", "Total")
  cw_total[cols] <- sapply(cw_total[cols],as.numeric)

   output$CWTable <- renderTable({
      placement <- input$rd
      selected<- input$select
    if(input$select=="Statewide") {
      if(input$rd == "In State") {
       table <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,]
       table <- table %>% select(-Region, -`Location of Placement`)
      } else {       
       table  <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,]
       table <- table %>% select(-Region, -`Location of Placement`)
        }
    } else if (input$select=="Southwest Region") {
      if(input$rd == "In State") {
       table <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,] 
       table <- table %>% select(-Region, -`Location of Placement`)
      } else {
       table <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,] 
       table <- table %>% select(-Region, -`Location of Placement`)
      }
    } else if (input$select=="South Central Region") {
      if(input$rd == "In State") {
        table <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,]
        table <- table %>% select(-Region, -`Location of Placement`)
      } else {
        table <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,] 
        table <- table %>% select(-Region, -`Location of Placement`)
      }
    } else if (input$select=="Eastern Region") {
      if(input$rd == "In State") {
        table <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,] 
        table <- table %>% select(-Region, -`Location of Placement`)
      } else {
        table <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,] 
        table <- table %>% select(-Region, -`Location of Placement`)
      }
    } else if (input$select=="North Central Region") {
      if(input$rd == "In State") {
        table <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,] 
        table <- table %>% select(-Region, -`Location of Placement`)
      } else {
        table <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,] 
        table <- table %>% select(-Region, -`Location of Placement`)
      }
    } else if (input$select=="Western Region") {
      if(input$rd == "In State") {
        table <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,]
        table <- table %>% select(-Region, -`Location of Placement`)
      } else {
        table <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,] 
        table <- table %>% select(-Region, -`Location of Placement`)
      }
    } else if (input$select=="Central Region") {
      if(input$rd == "In State") {
        table <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,] 
        table <- table %>% select(-Region, -`Location of Placement`)
      } else {
        table <- cw_total[cw_total$Region == selected & cw_total$`Location of Placement` == placement,] 
        table <- table %>% select(-Region, -`Location of Placement`)
      }
    }
  }, caption = "Source: CT Dept of Children and Families, accessed via data.ct.gov", striped=T, hover=T, condensed=T, responsive=T, spacing="xs")
  ########################### 

  output$CW_gender <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    placement <- input$rd
    cw_plot1 <- cw_gender_total[cw_gender_total$Region == selected & 
                             cw_gender_total$`Location of Placement` == placement & cw_gender_total$Gender != "Total" &
                             cw_gender_total$`Type of Placement` != "Total",]
    cw_plot1 <- spread(cw_plot1, Gender, Value)
    cw_plot1$`Type of Placement` <- gsub(" Placement", "", cw_plot1$`Type of Placement`)
    m <- list(l=220, b=110) # l = left; r = right; t = top; b = bottom
    cwplot1 <- plot_ly(cw_plot1, y = ~str_wrap(`Type of Placement`, width=40), x = ~Female, name = 'Female', type = 'bar', 
                     hoverinfo = 'text', text = ~paste0(Female), textposition = 'auto', marker = list(color = 'rgba(222,45,38,0.8)')) %>%
             add_trace( x = ~Male, name = 'Male', text = ~paste0(Male), marker = list(color = 'rgb(49,130,189)')) %>%
     layout(margin=m, title = paste(paste0(unique(cw_plot1$Region), ","), max_year_cw_gender, "-", placement, sep = " "),
         xaxis = list(title = "Number", tickangle = 0),
         yaxis = list(title = "Type of Placement"), 
         barmode = 'group', legend = list(x = -0.2, y = -0.1, orientation = 'h'),
         annotations = list(x = 1, y = -0.35, text = "Source: CT Dept of Children and Families, accessed via data.ct.gov", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")))
     cwplot1 
  })
  ########################### 

  output$CW_race <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    placement <- input$rd
    
    cw_plot2 <- cw_backfill[cw_backfill$Region == selected & 
                             cw_backfill$`Location of Placement` == placement & cw_backfill$`Race/Ethnicity` != "Total" &
                             cw_backfill$`Type of Placement` != "Total",]

    cw_plot2 <- spread(cw_plot2, `Race/Ethnicity`, Value)
    cw_plot2$`Type of Placement` <- gsub(" Placement", "", cw_plot2$`Type of Placement`)

    m <- list(l=220, b=110) # l = left; r = right; t = top; b = bottom
    cwplot2 <- plot_ly(cw_plot2, y = ~str_wrap(`Type of Placement`, width=40), x = ~Other, name = 'Other', type = 'bar', 
                     hoverinfo = 'text', text = ~paste0(Other), textposition = 'auto', marker = list(color='#d62728')) %>%
             add_trace( x = ~Black, name = 'Black or African American', text = ~paste0(Black), marker = list(color='#2ca02c')) %>%
             add_trace( x = ~Hispanic, name = 'Hispanic or Latino', text = ~paste0(Hispanic), marker = list(color = '#ff7f0e')) %>%
             add_trace( x = ~White, name = 'White', text = ~paste0(White), marker = list(color = '#1f77b4')) %>%
     layout(margin=m, title = paste(paste0(unique(cw_plot2$Region), ","), max_year_cw_race, "-", 
                                    placement, sep = " "),
         xaxis = list(title = "Number", tickangle = 0),
         yaxis = list(title = "Type of Placement"), 
         barmode = 'group', legend = list(x = -0.2, y = -0.39, traceorder='reversed'),
         annotations = list(x = 1, y = -0.4, text = "Source: CT Dept of Children and Families, accessed via data.ct.gov", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")))
     cwplot2    
  })
  ###########################  
  output$eey_text <- renderText({
     input$select
  })  
  ########################### 
  output$eey_value_f <- renderText({
    selected<- input$select
    cw_plot3 <- cw_eey_regions[cw_eey_regions$Region == selected & cw_eey_regions$`Measure Type` == "Number",]
    cw_plot3 <- spread(cw_plot3, Variable, Value)
    
    cw_plot3_calc <- unique(cw_plot3 %>% 
      group_by(Gender) %>% 
      mutate(tot_Value = sum(`Employed or Enrolled Youth`), 
             tot_moe = aggregate_moe(`Margins of Error`)) %>% 
      select(Gender, `Measure Type`, Region, tot_Value, tot_moe))
    
    cw_plot3_calc <- cw_plot3_calc %>% 
       gather(variable, value, -(Region:Gender)) %>%
       unite(temp, Gender, variable) %>%
       spread(temp, value)
    cw_plot3_calc$Female_tot_Value <- round(cw_plot3_calc$Female_tot_Value, 0)
    value <- format(unique(cw_plot3_calc$Female_tot_Value), big.mark=",", scientific=FALSE) 
  })   
  ########################### 
  output$eey_value_m <- renderText({
    selected<- input$select
    cw_plot3 <- cw_eey_regions[cw_eey_regions$Region == selected & cw_eey_regions$`Measure Type` == "Number",]
    cw_plot3 <- spread(cw_plot3, Variable, Value)
    
    cw_plot3_calc <- unique(cw_plot3 %>% 
      group_by(Gender) %>% 
      mutate(tot_Value = sum(`Employed or Enrolled Youth`), 
             tot_moe = aggregate_moe(`Margins of Error`)) %>% 
      select(Gender, `Measure Type`, Region, tot_Value, tot_moe))
    
    cw_plot3_calc <- cw_plot3_calc %>% 
       gather(variable, value, -(Region:Gender)) %>%
       unite(temp, Gender, variable) %>%
       spread(temp, value)
    cw_plot3_calc$Male_tot_Value <- round(cw_plot3_calc$Male_tot_Value, 0)
    value <- format(unique(cw_plot3_calc$Male_tot_Value), big.mark=",", scientific=FALSE) 
  })   
  ########################### 
  output$eey_value_t <- renderText({
    selected<- input$select
    cw_plot3 <- cw_eey_regions[cw_eey_regions$Region == selected & cw_eey_regions$`Measure Type` == "Number",]
    cw_plot3 <- spread(cw_plot3, Variable, Value)
    
    cw_plot3_calc <- unique(cw_plot3 %>% 
      group_by(Gender) %>% 
      mutate(tot_Value = sum(`Employed or Enrolled Youth`), 
             tot_moe = aggregate_moe(`Margins of Error`)) %>% 
      select(Gender, `Measure Type`, Region, tot_Value, tot_moe))
    
    cw_plot3_calc <- cw_plot3_calc %>% 
       gather(variable, value, -(Region:Gender)) %>%
       unite(temp, Gender, variable) %>%
       spread(temp, value)
    cw_plot3_calc$Total_tot_Value <- round(cw_plot3_calc$Total_tot_Value, 0)
    value <- format(unique(cw_plot3_calc$Total_tot_Value), big.mark=",", scientific=FALSE) 
  })    
  ########################### 
  output$eey_moe_f <- renderText({
    selected<- input$select
    cw_plot3 <- cw_eey_regions[cw_eey_regions$Region == selected & cw_eey_regions$`Measure Type` == "Number",]
    cw_plot3 <- spread(cw_plot3, Variable, Value)
    
    cw_plot3_calc <- unique(cw_plot3 %>% 
      group_by(Gender) %>% 
      mutate(tot_Value = sum(`Employed or Enrolled Youth`), 
             tot_moe = aggregate_moe(`Margins of Error`)) %>% 
      select(Gender, `Measure Type`, Region, tot_Value, tot_moe))
    
    cw_plot3_calc <- cw_plot3_calc %>% 
       gather(variable, value, -(Region:Gender)) %>%
       unite(temp, Gender, variable) %>%
       spread(temp, value)
    cw_plot3_calc$Female_tot_moe <- round(cw_plot3_calc$Female_tot_moe, 0)
    moe <- format(unique(cw_plot3_calc$Female_tot_moe), big.mark=",", scientific=FALSE) 
    paste("+/-", moe, sep = " ")
  })     
  ########################### 
  output$eey_moe_m <- renderText({
    selected<- input$select
    cw_plot3 <- cw_eey_regions[cw_eey_regions$Region == selected & cw_eey_regions$`Measure Type` == "Number",]
    cw_plot3 <- spread(cw_plot3, Variable, Value)
    
    cw_plot3_calc <- unique(cw_plot3 %>% 
      group_by(Gender) %>% 
      mutate(tot_Value = sum(`Employed or Enrolled Youth`), 
             tot_moe = aggregate_moe(`Margins of Error`)) %>% 
      select(Gender, `Measure Type`, Region, tot_Value, tot_moe))
    
    cw_plot3_calc <- cw_plot3_calc %>% 
       gather(variable, value, -(Region:Gender)) %>%
       unite(temp, Gender, variable) %>%
       spread(temp, value)
    cw_plot3_calc$Male_tot_moe <- round(cw_plot3_calc$Male_tot_moe, 0)
    moe <- format(unique(cw_plot3_calc$Male_tot_moe), big.mark=",", scientific=FALSE) 
    paste("+/-", moe, sep = " ")
  })  
  ########################### 
  output$eey_moe_t <- renderText({
    selected<- input$select
    cw_plot3 <- cw_eey_regions[cw_eey_regions$Region == selected & cw_eey_regions$`Measure Type` == "Number",]
    cw_plot3 <- spread(cw_plot3, Variable, Value)
    
    cw_plot3_calc <- unique(cw_plot3 %>% 
      group_by(Gender) %>% 
      mutate(tot_Value = sum(`Employed or Enrolled Youth`), 
             tot_moe = aggregate_moe(`Margins of Error`)) %>% 
      select(Gender, `Measure Type`, Region, tot_Value, tot_moe))
    
    cw_plot3_calc <- cw_plot3_calc %>% 
       gather(variable, value, -(Region:Gender)) %>%
       unite(temp, Gender, variable) %>%
       spread(temp, value)
    cw_plot3_calc$Total_tot_moe <- round(cw_plot3_calc$Total_tot_moe, 0)
    moe <- format(unique(cw_plot3_calc$Total_tot_moe), big.mark=",", scientific=FALSE) 
    paste("+/-", moe, sep = " ")
  }) 
  ########################### 

  output$DPlot_age_race <- renderPlotly({
      shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    race_sel <- input$race
    d_plot_age_race <- pop_by_age_race_regions[pop_by_age_race_regions$Region == selected 
                                               & pop_by_age_race_regions$`Race/Ethnicity` == race_sel,]

    d_plot_age_race <- spread(d_plot_age_race, Variable, Value)
    options(scipen = 9999)
    d_plot_age_race_calc <- unique(d_plot_age_race %>%
      group_by(`Age Cohort`, `Race/Ethnicity`, Region) %>%
      mutate(Sum_Pop = sum(Population)) %>%
      select(`Age Cohort`, `Race/Ethnicity`, Sum_Pop, Region) %>% 
      rename(Race = `Race/Ethnicity`, Age = `Age Cohort`))
    xState <- list(tickangle = 0, title = "Population", 
              tickmode='array', 
              tickvals = c(-600000, -500000, -400000, -300000, -200000, -100000, 0, 100000, 200000, 300000, 400000, 500000, 600000), 
              ticktext = c('600000', '500000', '400000', '300000', '200000', '100000', '0', '100000', '200000', '300000', '400000', '500000', '600000'))
    xRegion <- list(tickangle = 0, title = "Population", 
               tickmode='array', 
               tickvals = c(-100000, -80000, -60000, -40000, -20000, 0, 20000, 40000, 60000, 80000, 100000), 
               ticktext = c('100000', '80000', '60000', '40000', '20000', '0', '20000', '40000', '60000', '80000', '100000'))
    y <- list(tickangle = 0, title = "Age Range")
    if (input$select == "Statewide") {
      xx = xState
    } else {
      xx = xRegion
    }
    d_plot_age_race_calc$Age <- factor(d_plot_age_race_calc$Age, levels = c("0 to 4 years",
                                                                            "5 to 9 years",
                                                                            "10 to 14 years",
                                                                            "15 to 19 years",
                                                                            "20 to 24 years",
                                                                            "25 to 29 years",
                                                                            "30 to 34 years",
                                                                            "35 to 44 years",
                                                                            "45 to 54 years",
                                                                            "55 to 64 years",
                                                                            "65 to 74 years",
                                                                            "75 to 84 years",
                                                                            "85 years and over"))
    

    d_plot_age_race_calc <- spread(d_plot_age_race_calc, Age, Sum_Pop)
    m <- list(b=100, r=50) # l = left; r = right; t = top; b = bottom
    dplotagerace <- plot_ly(d_plot_age_race_calc, x="0 to 4", y = ~`0 to 4 years`, type = 'bar',
                        hoverinfo = 'name+text', textposition = 'auto',
                        name = '0 to 4 years', text = ~paste0(`0 to 4 years`), marker = list(color = 'rgb(49,130,189)')) %>%
                    add_trace(x="5 to 9", y=~`5 to 9 years`, name = '5 to 9 years',  text = ~paste0(`5 to 9 years`)) %>%
                    add_trace(x="10 to 14", y=~`10 to 14 years`, name = '10 to 14 years',  text = ~paste0(`10 to 14 years`)) %>%
                    add_trace(x="15 to 19", y=~`15 to 19 years`, name = '15 to 19 years',  text = ~paste0(`15 to 19 years`)) %>%
                    add_trace(x="20 to 24", y=~`20 to 24 years`, name = '20 to 24 years',  text = ~paste0(`20 to 24 years`)) %>%
                    add_trace(x="25 to 29", y=~`25 to 29 years`, name = '25 to 29 years',  text = ~paste0(`25 to 29 years`)) %>%
                    add_trace(x="30 to 34", y=~`30 to 34 years`, name = '30 to 34 years',  text = ~paste0(`30 to 34 years`)) %>%
                    add_trace(x="35 to 44", y=~`35 to 44 years`, name = '35 to 44 years',  text = ~paste0(`35 to 44 years`)) %>%
                    add_trace(x="45 to 54", y=~`45 to 54 years`, name = '45 to 54 years',  text = ~paste0(`45 to 54 years`)) %>%
                    add_trace(x="55 to 64", y=~`55 to 64 years`, name = '55 to 64 years',  text = ~paste0(`55 to 64 years`)) %>%
                    add_trace(x="65 to 74", y=~`65 to 74 years`, name = '65 to 74 years',  text = ~paste0(`65 to 74 years`)) %>%
                    add_trace(x="75 to 84", y=~`75 to 84 years`, name = '75 to 84 years',  text = ~paste0(`75 to 84 years`)) %>%
                    add_trace(x="85 and over", y=~`85 years and over`, name = '85 years and over',  text = ~paste0(`85 years and over`)) %>%
                layout(margin = m, showlegend = FALSE,
                       title = paste(paste0(unique(d_plot_age_race_calc$Region), ","), max_year_pop_regions, sep = " "),          
                       xaxis = list(title = "Age Group", tickangle = 30),
                       yaxis = list(title = "Number"), 
                       annotations = list(x = 1, y = -0.3, text = "Source: U.S. Census", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")),
                       barmode = 'group',
                       xaxis = xx, 
                       yaxis = y)

    dplotagerace
  })
  ###########################  

  output$DPlot_age <- renderPlotly({
      shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    d_plot_age <- pop_by_age_gender_regions[pop_by_age_gender_regions$Region == selected & pop_by_age_gender_regions$`Age Cohort` != "Total",]
    d_plot_age <- spread(d_plot_age, Variable, Value)
    
    d_plot_age_calc <- d_plot_age %>%
      group_by(`Age Cohort`, Gender, Region) %>%
      mutate(Sum_Pop = sum(Population),
             MOEsq = (`Margins of Error`)^2,
             MOEsqAgg = sum(MOEsq),
             MOEsqAggsq = sqrt(MOEsqAgg)) %>%
      select(`Age Cohort`, Gender, Sum_Pop, MOEsqAggsq, Region)
    
    d_plot_age_calc$Population <- with(d_plot_age_calc, ifelse(Gender == "Male", -Sum_Pop, Sum_Pop))

    d_plot_age_calc_plot <- unique(d_plot_age_calc %>% 
      select(`Age Cohort`, Region, Gender, Population)) %>% 
      spread(Gender, Population) %>% 
      rename(Age = `Age Cohort`) %>% 
      mutate(abs_m_pop = abs(Male))
    
    d_plot_age_calc_plot$Age <- factor(d_plot_age_calc_plot$Age, levels = c("0 to 4 years",
                                                                            "5 to 9 years",
                                                                            "10 to 14 years",
                                                                            "15 to 19 years",
                                                                            "20 to 24 years",
                                                                            "25 to 29 years",
                                                                            "30 to 34 years",
                                                                            "35 to 44 years",
                                                                            "45 to 54 years",
                                                                            "55 to 64 years",
                                                                            "65 to 74 years",
                                                                            "75 to 84 years",
                                                                            "85 years and over"))
    
    m <- list(l=120, b=100) # l = left; r = right; t = top; b = bottom
    xState <- list(tickangle = 0, title = "Population", 
              tickmode='array', 
              tickvals = c(-600000, -500000, -400000, -300000, -200000, -100000, 0, 100000, 200000, 300000, 400000, 500000, 600000), 
              ticktext = c('600000', '500000', '400000', '300000', '200000', '100000', '0', '100000', '200000', '300000', '400000', '500000', '600000'))
    xRegion <- list(tickangle = 0, title = "Population", 
               tickmode='array', 
               tickvals = c(-100000, -80000, -60000, -40000, -20000, 0, 20000, 40000, 60000, 80000, 100000), 
               ticktext = c('100000', '80000', '60000', '40000', '20000', '0', '20000', '40000', '60000', '80000', '100000'))
    y <- list(tickangle = 0, title = "Age Range")
    
    if (input$select == "Statewide") {
      xx = xState
    } else {
      xx = xRegion
    }
    dplotage <- plot_ly(d_plot_age_calc_plot, x = ~Female, y = ~Age, type = 'bar',
                        hoverinfo = 'name+text',textposition = 'auto',
                        orientation = 'h', name = 'Female', text = ~paste0(Female),
                        marker = list(color = 'rgba(222,45,38,0.8)', 
                                                  width = 3)) %>%
                add_trace(x=~Male, name = 'Male',  text = ~paste0(abs_m_pop),                      
                          marker = list(color = 'rgb(49,130,189)', 
                                                  width = 3)) %>%
                layout(margin = m, legend = list(traceorder='reversed', orientation = 'h', x = 0.1, y = -0.1),
                       title = paste(paste0(unique(d_plot_age_calc_plot$Region), ","),
                                     max_year_pop_regions,  
                                     sep = " "),
                       annotations = list(x = 1, y = -0.3, text = "Source: U.S. Census", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")),
                       barmode = 'overlay',
                       xaxis = xx, 
                       yaxis = y)

    dplotage
	})
  
  ###########################  

  output$DPlot_race <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    d_plot_race <- pop_by_race_gender_regions[pop_by_race_gender_regions$Region == selected & pop_by_race_gender_regions$`Race/Ethnicity` != "All",]
    d_plot_race <- spread(d_plot_race, Variable, Value)
    
    d_plot_race_calc <- d_plot_race %>%
      group_by(`Race/Ethnicity`, Gender, Region) %>%
      mutate(Sum_Pop = sum(Population),
             MOEsq = (`Margins of Error`)^2,
             MOEsqAgg = sum(MOEsq),
             MOEsqAggsq = sqrt(MOEsqAgg)) %>%
      select(`Race/Ethnicity`, Gender, Sum_Pop, MOEsqAggsq, Region)
    
    d_plot_race_calc$Population <- with(d_plot_race_calc, ifelse(Gender == "Male", -Sum_Pop, Sum_Pop))

    d_plot_race_calc_plot <- unique(d_plot_race_calc %>% 
      select(`Race/Ethnicity`, Region, Gender, Population)) %>% 
      spread(Gender, Population) %>% 
      rename(Race = `Race/Ethnicity`) %>% 
      mutate(abs_m_pop = abs(Male))
    
    d_plot_race_calc_plot$Race <- factor(d_plot_race_calc_plot$Race, levels = c("Native Hawaiian and Other Pacific Islander", 
                                                              "American Indian and Alaska Native Alone",
                                                              "Two or More Races",
                                                              "Asian Alone",
                                                              "Some Other Race Alone",
                                                              "Black or African American Alone",
                                                              "Hispanic or Latino",
                                                              "White Alone Not Hispanic or Latino",
                                                              "White Alone"))
    
    m <- list(l=250, b=100) # l = left; r = right; t = top; b = bottom
    xState <- list(tickangle = 0, 
                   title = "Population", 
                   tickmode='array', 
                   tickvals = c( -1250000, -1000000, -750000, -500000, -250000, 0, 
                                250000, 500000, 750000, 1000000, 1250000), 
                   ticktext = c('1250000', '1000000', '750000', '500000', '250000', '0', 
                                '250000', '500000', '750000', '1000000', '1250000'))
    xRegion <- list(tickangle = 0, title = "Population", 
               tickmode='array', 
               tickvals = c(-300000, -250000, -200000, -150000, -100000, -50000, 0, 
                            50000, 100000, 150000, 200000, 250000, 300000), 
               ticktext = c('300000', '250000', '200000', '150000', '100000', '50000', '0', 
                            '50000', '100000', '150000', '200000', '250000', '300000'))
    yy <- list(tickangle = 0, 
              title = "Race/Ethnicity", 
              categoryorder = 'array', 
              categoryarray = c("Native Hawaiian and Other Pacific Islander", 
                                "American Indian and Alaska Native Alone", 
                                "Two or More Races", 
                                "Asian Alone", 
                                "Some Other Race Alone", 
                                "Black or African American Alone", 
                                "Hispanic or Latino", 
                                "White Alone Not Hispanic or Latino", 
                                "White Alone"))
    
    if (input$select == "Statewide") {
      xx = xState
    } else {
      xx = xRegion
    }
    dplotrace <- plot_ly(d_plot_race_calc_plot, x = ~Female, y = ~Race, type = 'bar',
                        hoverinfo = 'name+text',textposition = 'auto',
                        orientation = 'h', name = 'Female', text = ~paste0(Female),
                        marker = list(color = 'rgba(222,45,38,0.8)', 
                                                  width = 3)) %>%
                add_trace(x=~Male, name = 'Male',  text = ~paste0(abs_m_pop),                      
                          marker = list(color = 'rgb(49,130,189)', 
                                                  width = 3)) %>%
                layout(margin = m,
                       legend = list(traceorder='reversed', orientation = 'h', x = 0, y = -0.1),
                       title = paste(paste0(unique(d_plot_race_calc_plot$Region), ","),
                                     max_year_pop_regions,  
                                     sep = " "),
                       annotations = list(x = 1, y = -0.3, text = "Source: U.S. Census", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")),
                       barmode = 'overlay',
                       xaxis = xx, 
                       yaxis = yy)

    dplotrace
	})
  ###########################   

  output$Dplot_mhi <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    
  if (selected == "Southwest Region") {
    marker_select = list(color = c('rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 
                                   '#1f77b4', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)'))
  } else if (selected == "South Central Region") {
    marker_select = list(color = c('rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', '#1f77b4', 
                                   'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)')) 
  } else if (selected == "Eastern Region") {
    marker_select = list(color = c('rgba(204,204,204,1)', '#1f77b4', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 
                                   'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)'))     
  } else if (selected == "North Central Region") {
    marker_select = list(color = c('rgba(204,204,204,1)', 'rgba(204,204,204,1)', '#1f77b4', 'rgba(204,204,204,1)', 
                                   'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)'))  
  } else if (selected == "Western Region") {
    marker_select = list(color = c('rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 
                                   'rgba(204,204,204,1)', 'rgba(204,204,204,1)', '#1f77b4'))
  } else if (selected == "Central Region") {
    marker_select = list(color = c('#1f77b4', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 
                                   'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)')) 
  } else { #Statewide
    marker_select = list(color = c('rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 
                                   'rgba(204,204,204,1)', '#1f77b4', 'rgba(204,204,204,1)')) 
  }
    
    total_mhi <- mhi_df_regions[mhi_df_regions$Variable == "Median Household Income",]
    total_n <- total_mhi %>% 
      count(Region)
    total_mhi <- merge(total_mhi, total_n, by = "Region")
    total_mhi <- unique(total_mhi %>% 
      group_by(Region) %>% 
      mutate(tot_mhi = sum(Value), 
             final_mhi = tot_mhi / n) %>% 
      select(Region, n, final_mhi))
      
    total_moe <- mhi_df_regions[mhi_df_regions$Variable == "Margins of Error",]
    total_moe <- unique(total_moe %>% 
      group_by(Region) %>% 
      mutate(final_moe = aggregate_moe(Value)) %>% 
      select(final_moe, Region))
    
    mhi_df_regions_calc <- merge(total_mhi, total_moe, by = "Region")
    mhi_df_regions_calc$final_mhi <- round(mhi_df_regions_calc$final_mhi, 0)
    mhi_df_regions_calc$final_moe <- round(mhi_df_regions_calc$final_moe, 0)
    mhi_df_regions_calc$Region <- factor(mhi_df_regions_calc$Region, 
                                         levels = c("Statewide", 
                                                    "Southwest Region",      #1
                                                    "South Central Region",  #2
                                                    "Eastern Region",        #3
                                                    "North Central Region",  #4
                                                    "Western Region",        #5
                                                    "Central Region"))
    m <- list(b=110, r=60) # l = left; r = right; t = top; b = bottom
    dplot3 <- plot_ly(mhi_df_regions_calc, x = ~Region, y = ~final_mhi, 
                     name = 'Median Household Income', type = 'bar', 
                               marker = marker_select,
                     hoverinfo = 'text', text = ~paste(paste0("$", final_mhi), "+/-", final_moe, sep = " "), 
                     error_y = ~list(array = final_moe, color = '#000000')) %>%
     layout(margin=m, title = paste(selected, "vs. All Regions,", max_year_mhi_regions, sep = " "),
         xaxis = list(title = "", tickangle = 30),
         yaxis = list(title = "Income ($)"), 
         barmode = 'group', legend = list(x = 0.7, y = 0.95),
         annotations = list(x = 1, y = -0.35, text = "Source: U.S. Census", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")))
     dplot3    
  })
  ###########################  
  output$mhi_text <- renderText({
     input$select
  })
  ########################### 
  output$mhi_value <- renderText({
     selected<- input$select
     total_mhi <- mhi_df_regions[mhi_df_regions$Variable == "Median Household Income",]
    total_n <- total_mhi %>% 
      count(Region)
    total_mhi <- merge(total_mhi, total_n, by = "Region")
    total_mhi <- unique(total_mhi %>% 
      group_by(Region) %>% 
      mutate(tot_mhi = sum(Value), 
             final_mhi = tot_mhi / n) %>% 
      select(Region, n, final_mhi))
      
    total_moe <- mhi_df_regions[mhi_df_regions$Variable == "Margins of Error",]
    total_moe <- unique(total_moe %>% 
      group_by(Region) %>% 
      mutate(final_moe = aggregate_moe(Value)) %>% 
      select(final_moe, Region))
    
    mhi_df_regions_calc <- merge(total_mhi, total_moe, by = "Region")
    mhi_df_regions_calc$final_mhi <- round(mhi_df_regions_calc$final_mhi, 0)
    mhi_df_regions_calc$final_moe <- round(mhi_df_regions_calc$final_moe, 0)
    mhi_df_regions_calc$Region <- factor(mhi_df_regions_calc$Region, 
                                         levels = c("Statewide", 
                                                    "Southwest Region",      #1
                                                    "South Central Region",  #2
                                                    "Eastern Region",        #3
                                                    "North Central Region",  #4
                                                    "Western Region",        #5
                                                    "Central Region"))
     mhi_value_df <- mhi_df_regions_calc[mhi_df_regions_calc$Region == selected,]
     value <- format(unique(mhi_value_df$final_mhi), big.mark=",", scientific=FALSE) 
     paste0("$", value)
  })
  ########################### 
  output$mhi_moe <- renderText({
     selected<- input$select
          total_mhi <- mhi_df_regions[mhi_df_regions$Variable == "Median Household Income",]
    total_n <- total_mhi %>% 
      count(Region)
    total_mhi <- merge(total_mhi, total_n, by = "Region")
    total_mhi <- unique(total_mhi %>% 
      group_by(Region) %>% 
      mutate(tot_mhi = sum(Value), 
             final_mhi = tot_mhi / n) %>% 
      select(Region, n, final_mhi))
      
    total_moe <- mhi_df_regions[mhi_df_regions$Variable == "Margins of Error",]
    total_moe <- unique(total_moe %>% 
      group_by(Region) %>% 
      mutate(final_moe = aggregate_moe(Value)) %>% 
      select(final_moe, Region))
    mhi_df_regions_calc <- merge(total_mhi, total_moe, by = "Region")
    mhi_df_regions_calc$final_mhi <- round(mhi_df_regions_calc$final_mhi, 0)
    mhi_df_regions_calc$final_moe <- round(mhi_df_regions_calc$final_moe, 0)
    mhi_df_regions_calc$Region <- factor(mhi_df_regions_calc$Region, 
                                         levels = c("Statewide", 
                                                    "Southwest Region",      #1
                                                    "South Central Region",  #2
                                                    "Eastern Region",        #3
                                                    "North Central Region",  #4
                                                    "Western Region",        #5
                                                    "Central Region"))
     mhi_value_df <- mhi_df_regions_calc[mhi_df_regions_calc$Region == selected,]
     moe <- format(unique(mhi_value_df$final_moe), big.mark=",", scientific=FALSE) 
     paste0("+/-", moe)
  })  
  ###########################
  cols <- c("White", "Black", "Hispanic", "Other", "Total")
  bh_plot1[cols] <- sapply(bh_plot1[cols],as.numeric)

  output$BHPlot1 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    bh_plot_race <- bh_plot1[bh_plot1$Region == selected,]
    bh_plot_race <- bh_plot_race[bh_plot_race$`Age Range` != "0 to 21",]
    m <- list(b=110) # l = left; r = right; t = top; b = bottom
    x <- list(tickangle = 0)
    
    bh_plot_race$`Age Range` <- factor(bh_plot_race$`Age Range`, levels = c("0 to 5", "6 to 9", "10 to 15", "16 to 19", "20 to 21"))
    bh_plot_race <- arrange(bh_plot_race, `Age Range`)
    bhplotrace <- plot_ly(bh_plot_race, x = ~`Age Range`, y = ~White, name = 'White', type = 'bar', 
                     hoverinfo = 'text', text = ~paste0(White), textposition = 'auto') %>%
                   add_trace( y = ~Hispanic, name = 'Hispanic or Latino', text = ~paste0(Hispanic)) %>%

             add_trace( y = ~Black, name = 'Black or African American', text = ~paste0(Black)) %>%
             add_trace( y = ~Other, name = 'Other', text = ~paste0(Other)) %>%
     layout(margin=m, title = paste(paste0(unique(bh_plot_race$Region), ","), "2010", sep = " "),
         xaxis = list(title = ""),
         yaxis = list(title = "Number"), 
         barmode = 'group', legend = list(orientation = 'h', x = 0.2, y = -0.15),
         annotations = list(x = 1, y = -0.35, text = "Source: US Census; Calculations by CONNECT project", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")))
     bhplotrace    
  })
  ###########################  
   output$BHTable <- function() {
    if(input$select=="Statewide") {
      bh_CT %>%
       knitr::kable("html", row.names=NA) %>%
       kable_styling(c("striped", "hover", "condensed", "responsive", "xs"), full_width = T) %>%
       group_rows("0 - 5", 1, 3) %>%
       group_rows("6 - 9", 4, 6) %>%
       group_rows("10 - 15", 7, 9) %>%
       group_rows("16 - 19", 10, 12) %>%
       group_rows("20 - 21", 13, 15) %>%
       group_rows("0 - 21", 16, 18) %>%
       add_footnote(c("Source: US Census; Calculations by CONNECT project"), notation = "alphabet")
       #scroll_box(height = "400px")
    } else if (input$select=="Southwest Region") {
      bh_r1 %>%
       knitr::kable("html") %>%
       kable_styling(c("striped", "hover", "condensed", "responsive"), full_width = T ) %>%
       group_rows("0 - 5", 1, 3) %>%
       group_rows("6 - 9", 4, 6) %>%
       group_rows("10 - 15", 7, 9) %>%
       group_rows("16 - 19", 10, 12) %>%
       group_rows("20 - 21", 13, 15) %>%
       group_rows("0 - 21", 16, 18) %>%
       add_footnote(c("Source: US Census; Calculations by CONNECT project"), notation = "alphabet") 
       # scroll_box(height = "400px")
    } else if (input$select=="South Central Region") {
        bh_r2 %>%
       knitr::kable("html") %>%
       kable_styling(c("striped", "hover", "condensed", "responsive"), full_width = T) %>%
       group_rows("0 - 5", 1, 3) %>%
       group_rows("6 - 9", 4, 6) %>% 
       group_rows("10 - 15", 7, 9) %>% 
       group_rows("16 - 19", 10, 12) %>% 
       group_rows("20 - 21", 13, 15) %>% 
       group_rows("0 - 21", 16, 18) %>%
       add_footnote(c("Source: US Census; Calculations by CONNECT project"), notation = "alphabet") 
       # scroll_box(height = "400px")
    } else if (input$select=="Eastern Region") {
        bh_r3 %>%
       knitr::kable("html") %>%
       kable_styling(c("striped", "hover", "condensed", "responsive"), full_width = T) %>%
       group_rows("0 - 5", 1, 3) %>%
       group_rows("6 - 9", 4, 6) %>% 
       group_rows("10 - 15", 7, 9) %>% 
       group_rows("16 - 19", 10, 12) %>% 
       group_rows("20 - 21", 13, 15) %>% 
       group_rows("0 - 21", 16, 18) %>%
       add_footnote(c("Source: US Census; Calculations by CONNECT project"), notation = "alphabet") 
       # scroll_box(height = "400px")
    } else if (input$select=="North Central Region") {
        bh_r4 %>%
       knitr::kable("html") %>%
       kable_styling(c("striped", "hover", "condensed", "responsive"), full_width = T) %>%
       group_rows("0 - 5", 1, 3) %>%
       group_rows("6 - 9", 4, 6) %>% 
       group_rows("10 - 15", 7, 9) %>% 
       group_rows("16 - 19", 10, 12) %>% 
       group_rows("20 - 21", 13, 15) %>% 
       group_rows("0 - 21", 16, 18) %>%
       add_footnote(c("Source: US Census; Calculations by CONNECT project"), notation = "alphabet") 
       # scroll_box(height = "400px")
    } else if (input$select=="Western Region") {
        bh_r5 %>%
       knitr::kable("html") %>%
       kable_styling(c("striped", "hover", "condensed", "responsive"), full_width = T) %>%
       group_rows("0 - 5", 1, 3) %>%
       group_rows("6 - 9", 4, 6) %>% 
       group_rows("10 - 15", 7, 9) %>% 
       group_rows("16 - 19", 10, 12) %>% 
       group_rows("20 - 21", 13, 15) %>% 
       group_rows("0 - 21", 16, 18) %>%
       add_footnote(c("Source: US Census; Calculations by CONNECT project"), notation = "alphabet") 
       # scroll_box(height = "400px")
    } else if (input$select=="Central Region") {
       bh_r6 %>%
       knitr::kable("html") %>%
       kable_styling(c("striped", "hover", "condensed", "responsive"), full_width = T) %>%
       group_rows("0 - 5", 1, 3) %>%
       group_rows("6 - 9", 4, 6) %>% 
       group_rows("10 - 15", 7, 9) %>% 
       group_rows("16 - 19", 10, 12) %>% 
       group_rows("20 - 21", 13, 15) %>% 
       group_rows("0 - 21", 16, 18) %>%
       add_footnote(c("Source: US Census; Calculations by CONNECT project"), notation = "alphabet") 
       # scroll_box(height = "400px")
    }
   }
  ###########################  
  output$edu_text <- renderUI({
    HTML(paste("<font color=\"#000000\"><b>", "<p>&nbsp;</p>", " - Calculated percents resulting from counts of less than 5 students have been suppressed.", "<p>&nbsp;</p>", " - Be aware that as the number of districts selected increases, visibility/usability of this chart may decrease.", "</b></font>"))
  })
  ########################### 
  
dd <- unique(edu$District)
# dd.col <- cm.colors(length(dd))
# set.seed(001)
# dd.col.random <- sample(dd.col)
# #names(dd.col.random)  <- dd
# 
# brewerplot <- function (palette) {
#   p + scale_fill_brewer(palette = palette) 
# }
# brewerplot ("Set1")
  
  
  values = list("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462",
                "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f",
                "#e6194b", "#3cb44b", "#ffe119", "#0082c8", "#f58231")
  
    #scale_fill_brewer palette Set3

  # , "#911eb4", 
  #               "#46f0f0", "#f032e6", "#d2f53c", "#fabebe", "#008080", "#e6beff", 
  #               "#aa6e28", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", 
  #               "#000080", "#808080", "#FFFFFF", "#000000", "#a6cee3", "#1f78b4")
  #   
  #scale_fill_brewer palette Set1
  # values = list("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", 
  #               "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99", "#b15928")
  
  dups <- list(values)[rep(1,12)] #<- repeats it 17 times to create list of 204
  dups2 <- do.call(c, unlist(dups, recursive=FALSE))
  set.seed(007)
  dd.col.random <- sample(dups2)
  names(dd.col.random)  <- dd #ensures both charts have same colors assigned to same districts

   output$EPlot1 <- renderPlotly({
     shiny::validate(
       need(input$select_edu != "", "Please select a District to populate the chart")
     )
     selected<- input$select_edu
     edu_plot <- edu[edu$`Measure Type` == "Percent" & edu$District %in% selected & 
                       edu$Value != -9999.0 &
                       edu$Year == max_year_edu,]
     m <- list(b=100)
         e_plot1 <- ggplot(data=edu_plot, 
                           aes(x=`Indicator of Educational Need`, y=Value, fill=District))+
         geom_bar(stat="identity", position = "dodge") + 
         #geom_text(aes(label=paste(Value, "%")), size=3, position = position_dodge(width=1)) + 
         theme_bw() +
         xlab("Indicator of Educational Need") + ylab("Percent") +
         scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
         scale_fill_manual(values = dd.col.random)+
         theme(axis.text = element_text(size=8), 
 			         plot.title = element_text(size=10, face="bold"),
               axis.title = element_text(size=10), 
 			         plot.margin = unit(c(0.1,0.1,1,0.1), "cm")) +
         labs(caption = "Source: Connecticut State Department of Education")
         e_plot1 <- ggplotly(e_plot1)
         e_plot1 <- e_plot1 %>% layout(margin=m, annotations = list(x = 1.2, y = -0.3, text = "Source: Connecticut State Department of Education", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")))
         e_plot1
   })
  ###########################  
  output$EPlot2 <- renderPlotly({
    shiny::validate(
      need(input$select_edu != "", "Please select a District to populate the chart")
    )    
    selected<- input$select_edu
    edu2_plot <- edu2[edu2$`Measure Type` == "Percent" & edu2$District %in% selected & 
                        edu2$Value != -6666 & edu2$Value != -9999 & edu2$Year == max_year_edu2,]
    shiny::validate(
      need(nrow(edu2_plot) != 0, "No data are available for your selection, try selecting another District")
    )
      x <- list(
        ticklen = 5,
        tickwidth = 1,
        tickfont = list(size = 10)
      )
      y <- list(
        ticklen = 15,
        tickwidth = 1
      )      
      m <- list(l=100, r=20, b=80, t=40) # l = left; r = right; t = top; b = bottom
        e_plot2 <- ggplot(data=edu2_plot, aes(x=`Race/Ethnicity`, y=Value, fill=District)) +
        geom_bar(stat="identity", position = "dodge") + ylab("Percent") +
        #geom_text(aes(x=`Race/Ethnicity`, y=Value, label=paste(Value, "%")), vjust=-0.4, size=3, position = position_dodge(width=1)) + 
          theme_bw() + 
        scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
        scale_fill_manual(values = dd.col.random)+ 
        theme(axis.text = element_text(size=8),
			        plot.title = element_text(size=10, face="bold"), 
              axis.title=element_text(size=10), 
 			        plot.margin = unit(c(0.1,0.1,1,0.1), "cm")) +     
        labs(caption = "Source: Connecticut State Department of Education")
        e_plot2 <- ggplotly(e_plot2)
        e_plot2 <- e_plot2 %>% layout(
         margin=m, annotations = list(x = 1.2, y = -0.3, text = "Source: Connecticut State Department of Education", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")),
         barmode = 'group',
         xaxis = x, yaxis = y, 
         bargap = 0.3,
         legend = list(x = 100, y = 0.9, font = list(size=10)),
         xaxis = x, yaxis = y)
        e_plot2        
    })
  ###########################  

  output$EPlot3 <- renderPlotly({
    shiny::validate(
      need(input$select_edu != "", "Please select a District to populate the chart")
    )    
    selected<- input$select_edu
    edu3_plot <- edu3[edu3$District %in% selected & edu3$Value != -6666 & edu3$Value != -9999 & 
                        edu3$Year == max_year_edu3,]
    shiny::validate(
      need(nrow(edu3_plot) != 0, "No data are available for your selection, try selecting another District")
    )
      x <- list(
        ticklen = 5,
        tickwidth = 1,
        tickfont = list(size = 10)
      )
      y <- list(
        ticklen = 15,
        tickwidth = 1
      )      
      m <- list(l=100, r=20, b=80, t=40) # l = left; r = right; t = top; b = bottom
        e_plot3 <- ggplot(data=edu3_plot, aes(x=`Sanction Type`, y=Value, fill=District)) +
        geom_bar(stat="identity", position = "dodge") + ylab("Number") +
        #geom_text(aes(x=`Sanction Type`, y=Value, label=paste(Value)), vjust=-0.4, size=3, 
                #  position = position_dodge(width=1)) + 
     theme_bw() + 
        scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
        scale_fill_manual(values = dd.col.random)+ 
        theme(axis.text = element_text(size=8),
			        plot.title = element_text(size=10, face="bold"), 
              axis.title=element_text(size=10), 
 			        plot.margin = unit(c(0.1,0.1,1,0.1), "cm")) +     
        labs(caption = "Source: Connecticut State Department of Education")
        e_plot3 <- ggplotly(e_plot3)
        e_plot3 <- e_plot3 %>% layout(
         margin=m, annotations = list(x = 1.2, y = -0.3, text = "Source: Connecticut State Department of Education", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")),
         barmode = 'group',
         xaxis = x, yaxis = y, 
         bargap = 0.3,
         legend = list(x = 100, y = 0.9, font = list(size=10)),
         xaxis = x, yaxis = y)
        e_plot3        
    })
  ########################### 

  output$EPlot4 <- renderPlotly({
    shiny::validate(
      need(input$select_edu != "", "Please select a District to populate the chart")
    )    
    selected<- input$select_edu
    edu4_plot <- edu4[edu4$District %in% selected & edu4$Value != -6666 & edu4$Value != -9999 & 
                        edu4$Year == max_year_edu4,]
    shiny::validate(
      need(nrow(edu4_plot) != 0, "No data are available for your selection, try selecting another District")
    )
      x <- list(
        ticklen = 5,
        tickwidth = 1,
        tickfont = list(size = 10)
      )
      y <- list(
        ticklen = 15,
        tickwidth = 1
      )      
      m <- list(l=100, r=20, b=80, t=40) # l = left; r = right; t = top; b = bottom
        e_plot4 <- ggplot(data=edu4_plot, aes(x=`Incident Type`, y=Value, fill=District)) +
        geom_bar(stat="identity", position = "dodge") + ylab("Number") +
        #geom_text(aes(x=`Incident Type`, y=Value, label=paste(Value)), vjust=-0.4, size=3, 
       #           position = position_dodge(width=1)) + 
          theme_bw() + 
        scale_x_discrete(labels = function(x) str_wrap(x, width=10)) +
        scale_fill_manual(values = dd.col.random)+ 
        theme(axis.text = element_text(size=8),
			        plot.title = element_text(size=10, face="bold"), 
              axis.title=element_text(size=10), 
 			        plot.margin = unit(c(0.1,0.1,1,0.1), "cm")) +     
        labs(caption = "Source: Connecticut State Department of Education")
        e_plot4 <- ggplotly(e_plot4)
        e_plot4 <- e_plot4 %>% layout(
         margin=m, annotations = list(x = 1.2, y = -0.3, text = "Source: Connecticut State Department of Education", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")),
         barmode = 'group',
         xaxis = x, yaxis = y, 
         bargap = 0.3,
         legend = list(x = 100, y = 0.9, font = list(size=10)),
         xaxis = x, yaxis = y)
        e_plot4        
    })   
  ###########################
   # edu5_plot <- kei[kei$FixedDistrict %in% selected & kei$`Level 1` != -9999 & !is.na(kei$`Level 1`) & kei$Year == max_year_kei,]
   # edu5_plot$`Level 1` <- round(edu5_plot$`Level 1`, 0)
   # edu5_plot$`Level 2` <- round(edu5_plot$`Level 2`, 0)
   # edu5_plot$`Level 3` <- round(edu5_plot$`Level 3`, 0)
   # #Create list of plots
   # for (i in 1:length(selected)) {
   #   df <- edu5_plot[edu5_plot$FixedDistrict == selected[i],]
   #   plotname <- plot_ly(df,
   #                       y = ~str_wrap(`Domain`, width=15),
   #                       x = ~`Level 1`,
   #                       name = 'Level 1',
   #                       type = 'bar',
   #                       hoverinfo = 'text',
   #                       text = ~paste0(`Level 1`, '%'),
   #                       textposition = 'auto') %>%
   #              add_trace(x = ~`Level 2`, name = 'Level 2', text = ~paste0(`Level 2`, '%')) %>%
   #              add_trace(x = ~`Level 3`, name = 'Level 3', text = ~paste0(`Level 3`, '%')) %>%
   #              layout(margin=m,
   #                     #title = paste(unique(df$FixedDistrict), max_year_kei, sep = " "),
   #                     xaxis = list(title = "Percentage of Students"),
   #                     yaxis = list(title = "Skill Domain"),
   #                     barmode = 'stack', legend = list(orientation = 'h', x = 0.2, y = -0.25, traceorder='normal'),
   #                     annotations = list(list(x = 1, y = -0.35, text = "Source: Connecticut State Department of Education",
   #                                 showarrow = F, xref='paper', yref='paper',
   #                                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
   #                                 font=list(size=15, color="grey")),
   #                                 list(x = 0.4, y = 1.1, text = selected[i],
   #                                 showarrow = F, xref='paper', yref='paper',
   #                                 xanchor='right', yanchor='auto', xshift=0, yshift=0))
   #             )
   #        assign(paste0("kei_plot", i), plotname)
   # 
   # }
   # #Combine plots for subplot
   # 
   # do.call("<-",list(get_kei_plot_only[1], "parameter_value"))
   # 
   # get_kei_plot_only <- grep("^kei_plot", lists, value=T)
   # 
   # 
   # eval(parse(text=paste("df$", get_kei_plot_only, sep = "")))
   # 
   # thisName = get_kei_plot_only[i]
   # #plotList[[thisName]] = plot_ly(name=thisName, evaluate=TRUE)
   # 
   # 
   # eval(get_kei_plot_only) %>% 
   #   subplot(nrows = NROW(.), shareX = TRUE)
   # 
   # eval(get_kei_plot_only)

   # lists <- ls()[sapply(mget(ls(), .GlobalEnv), is.list)]
   # get_kei_plot_only <- list(grep("^kei_plot", lists, value=T) )        
   #          
   # economics_long %>%
   #   split(.$variable) %>%
   #   lapply(function(d) plot_ly(d, x = ~date, y = ~value)) %>%
   #   subplot(nrows = NROW(.), shareX = TRUE)
   # 
   # 
   # 
   # 
   # 
   # 
   # library(plotly)
   # plotList = list()
   # 
   # for (i in 1:length(get_kei_plot_only)){
   #   plot = get_kei_plot_only[i]
   #   sbp <- 
   # }
   # 
   # sbp = subplot(kei_plot1, kei_plot2)
   # print(sbp)
   #   
   #          thisName = get_kei_plot_only[i]
   #     plotList[[thisName]] = plot_ly(name=thisName, evaluate=TRUE)
   # 
   
})