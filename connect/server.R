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
  
  observeEvent(input$toTop, {
    js$toTop();
 })
  
  cols <- c("1" = "darkblue")
  region_map_reactive <- reactive({
    data <- dcf_regions_CT
    selected <- input$select
    if (input$select != "Statewide") {
      data <- data %>% mutate(Value = ifelse(grepl(paste0("^", selected), Region), 1, 0)) #get exact match
    } else {
      data <- data %>% mutate(Value = 1)
    }
  })
  
  output$gg_regions <- renderPlot({
    ggplot() + geom_polygon(data = region_map_reactive(), aes(x = long, y = lat, group = group, fill = Value, col="yellow"), color = "black") + 
    scale_fill_distiller(guide=FALSE, direction=1) + scale_colour_manual(values = cols) +  theme(rect = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), 
    axis.text.y = element_blank()) +
    labs(x="", y="")
  }, height = 150, width = 225)
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
            HTML(paste("<font color=\"#000000\" size = 1px;>", "<ol style='list-style: none;'><li>Bridgeport</li><li>Darien</li><li>Easton</li><li>Fairfield</li><li>Greenwich</li><li>Monroe</li><li>New Canaan</li><li>Norwalk</li><li>Stamford</li><li>Stratford</li><li>Trumbull</li><li>Weston</li><li>Westport</li><li>Wilton</li></ol>", "</font>"))
    } else if (input$select=="South Central Region") {
            HTML(paste("<font color=\"#000000\" size = 1px;>", "<ol style='list-style: none;'><li>Ansonia</li><li>Bethany</li><li>Branford</li><li>Derby</li><li>East Haven</li><li>Hamden</li><li>Milford</li><li>New Haven </li><li>North Branford</li><li>North Haven</li><li>Orange</li><li>Seymour</li><li>Shelton</li><li>West Haven</li><li>Woodbridge</li></ol>", "</font>"))
    } else if (input$select=="Eastern Region") {
            HTML(paste("<font color=\"#000000\" size = 1px;>", "<ol style='list-style: none;'><li>Ashford</li><li>Bozrah </li><li>Brooklyn </li><li>Canterbury </li><li>Chaplin </li><li>Chester </li><li>Clinton </li><li>Colchester </li><li>Columbia </li><li>Coventry </li><li>Cromwell </li><li>Deep River </li><li>Durham </li><li>East Haddam </li><li>East Hampton </li><li>East Lyme </li><li>Eastford </li><li>Essex </li><li>Franklin </li><li>Griswold </li><li>Groton </li><li>Guilford </li><li>Haddam </li><li>Hampton </li><li>Killingly </li><li>Killingworth </li><li>Lebanon </li><li>Ledyard </li><li>Lisbon </li><li>Lyme </li><li>Madison </li><li>Mansfield </li><li>Middlefield  </li><li>Middletown </li><li>Montville </li><li>New London</li><li>North Stonington </li><li>Norwich </li><li>Old Lyme </li><li>Old Saybrook </li><li>Plainfield </li><li>Pomfret </li><li>Portland </li><li>Preston </li><li>Putnam </li><li>Salem </li><li>Scotland </li><li>Sprague </li><li>Sterling </li><li>Stonington </li><li>Thompson </li><li>Union </li><li>Voluntown </li><li>Waterford </li><li>Westbrook </li><li>Willington </li><li>Windham </li><li>Woodstock</li></ol>", "</font>"))
    } else if (input$select=="North Central Region") {
            HTML(paste("<font color=\"#000000\" size = 1px;>", "<ol style='list-style: none;'><li>Andover</li><li>Bloomfield</li><li>Bolton</li><li>East Granby</li><li>East Hartford</li><li>East Windsor</li><li>Ellington</li><li>Enfield</li><li>Glastonbury</li><li>Granby</li><li>Hartford</li><li>Hebron</li><li>Manchester</li><li>Marlborough</li><li>Somers</li><li>South Windsor</li><li>Stafford</li><li>Suffield</li><li>Tolland</li><li>Vernon</li><li>West Hartford</li><li>Windsor</li><li>Windsor Locks</li></ol>", "</font>"))
    } else if (input$select=="Western Region") {
            HTML(paste("<font color=\"#000000\" size = 1px;>", "<ol style='list-style: none;'><li>Barkhamsted</li><li>Beacon Falls</li><li>Bethel</li><li>Bethlehem</li><li>Bridgewater</li><li>Brookfield</li><li>Canaan</li><li> Cheshire</li><li>Colebrook</li><li>Cornwall</li><li>Danbury</li><li>Goshen</li><li>Hartland</li><li>Harwinton</li><li>Kent</li><li> Litchfield</li><li>Middlebury</li><li>Morris</li><li>Naugatuck</li><li>New Fairfield</li><li>New Hartford</li><li>New Milford</li><li> Newtown</li><li>Norfolk</li><li>North Canaan</li><li>Oxford</li><li>Prospect</li><li>Redding</li><li>Ridgefield</li><li>Roxbury</li><li> Salisbury</li><li>Sharon</li><li>Sherman</li><li>Southbury</li><li>Thomaston</li><li>Torrington</li><li>Warren</li><li>Washington</li><li> Waterbury</li><li>Watertown</li><li>Winchester</li><li>Wolcott</li><li>Woodbury</li></ol>", "</font>"))
    } else if (input$select=="Central Region") {
            HTML(paste("<font color=\"#000000\" size = 1px;>", "<ol style='list-style: none;'><li>Avon</li><li>Berlin</li><li>Bristol</li><li>Burlington</li><li>Canton</li><li>Farmington</li><li>Meriden</li><li>New Britain</li><li> Newington</li><li>Plainville</li><li>Plymouth</li><li>Rocky Hill</li><li>Simsbury</li><li>Southington</li><li>Wallingford</li><li> Wethersfield</li></ol>", "</font>"))
    }
  })
  ###########################
  output$region_list <- renderUI({region_list_reactive()})
  ###########################
  health_reactive <- reactive({
    selected<- input$select
    h_plot1 <- health_regions
    h_plot1 <- subset(h_plot1, `Measure Type` == "Rate" & Region == selected & Year == max_year_health_regions)
    h_plot1 <- unique(h_plot1 %>%
                     group_by(Type) %>% 
                     mutate(avg_Value = round(mean(Value), 1)) %>% 
                     select(-Town, -Value, -FIPS)) %>% 
                     rename(Value = avg_Value)
  })
  
  output$HPlot1 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
        #color palette = "Paired"
# [1] "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C"
# [7] "#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"
    selected<- input$select
    hplot1 <- ggplot(health_reactive(), aes(x=Type, y=Value, fill = Type, text=sprintf("%s<br>%s", Type, Value))) +
              geom_bar(stat="identity", position = "dodge") + xlab ("") + ylab("Rate per 1000 births") + theme_minimal() +
              theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), axis.line.x = element_line())+
              scale_fill_brewer(palette="Paired") +
              scale_y_continuous(expand = c(0,0), breaks = NULL) #remove space around plot
              hplot1 <- ggplotly(hplot1, tooltip="text")
              hplot1 <- hplot1 %>% 
                layout(margin=list(t=30, b=60, l=40), 
                       title = paste(paste0(selected, ","), "All Races,", max_year_health_regions, sep = " "),
                       annotations = list(x = 1, y = -0.15, 
                                          text = HTML("Source: Connecticut Department of Public Health, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"),
                                          showarrow = F, 
                                          xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                          font=list(size=15, color="grey", align="right")
                       ),                       
                       barmode = 'group',
                       xaxis = list(tickfont = list(size = 12)), 
                       bargap = 0.3,
                       showlegend = FALSE
                )
              hplot1
  })
  ###########################
  health_rates_regions$Total.Births <- as.numeric(health_rates_regions$Total.Births)
  
  health_rates_reactive <- reactive({
    selected<- input$select
    h_plot2 <- health_rates_regions
    h_plot2 <- subset(h_plot2, Region == selected & RR_YR == max_year_health_rates_regions &
                                      Type %in% c("Fetal", "Infant") & Race != "All")
    if (selected != "Statewide") {
    h_plot2 <- h_plot2 %>%
      group_by(Region, Race, Type) %>%
      mutate(Total_Births = sum(Total.Births),  #per race
             Total_Deaths = sum(Value))  #per race/type
    h_plot2$Death.Rate <- round(h_plot2$Total_Deaths*1000/h_plot2$Total_Births,2)
    h_plot2 <- unique(h_plot2 %>% 
      select(Race, Type, Death.Rate, Region))
    } else {
    h_plot2 <- unique(h_plot2 %>%
      select(Race, Type, Death.Rate, Region)) 
    }
  })
  
  output$HPlot2 <- renderPlotly({
    selected<- input$select
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    hplot2 <- ggplot(health_rates_reactive(), aes(x=Race, y=Death.Rate, fill = Type, text=sprintf("%s<br>%s<br>%s", Type, Race, Death.Rate))) +
              geom_bar(stat="identity", position = "dodge") + xlab ("") + ylab("Rate per 1000 births") + theme_minimal() +
              theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), axis.line.x = element_line(), plot.title = element_text(size=8))+
              scale_fill_brewer(palette="Paired") +
              scale_y_continuous(expand = c(0,0), breaks = NULL) #remove space around plot
              hplot2 <- ggplotly(hplot2, tooltip="text")
              hplot2 <- hplot2 %>% 
                layout(margin=list(t=30, b=60, l=40), 
                       title = paste(paste0(selected, ","), max_year_health_rates_regions, sep = " "),
                       annotations = list(x = 1, y = -0.15, 
                                          text = HTML("Source: Connecticut Department of Public Health, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"),
                                          showarrow = F, 
                                          xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                          font=list(size=15, color="grey", align="right")
                       ),                       
                       barmode = 'group',
                       xaxis = list(tickfont = list(size = 12)), 
                       bargap = 0.3
                )
              hplot2      
  })  
  ###########################
  output$sa_ct_value <- renderText({
    selected = input$select
    sa_ct <- sa_regions[sa_regions$Region == selected,]
    sa_ct$Total_Admc <- format(sa_ct$Total_Adm, big.mark=",", scientific=FALSE)
    sa_ct_num <- unique(sa_ct$Total_Admc)
    SFY <- unique(sa_ct$SFY)
    paste("SFY", paste0(SFY-1, "-", SFY), ":", sa_ct_num, sep = " ")
  })
  ###########################
  sa_regions$Value <- as.numeric(sa_regions$Value)
  output$HPlot3 <- renderPlotly({
    selected<- input$select
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )   
    hplot3 <- sa_regions %>% filter(Substance != "Total", Region == selected, !is.na(Total_Adm), Total_Adm != 0)
    hplot3 <- ggplot(hplot3, aes(y=Town, x=Substance, group= Substance, text=sprintf("%s<br>%s", Town, paste(Total_Adm, Substance, "Admissions", sep = " ")))) +
                  geom_point(aes(size = Value, color = Substance)) + ylab ("Town") + xlab("Substance") + theme_minimal() +
              theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), axis.line.x = element_line(), plot.title = element_text(size=8)) +
              scale_x_discrete(labels = function(x) str_wrap(x, width=10)) 
              scale_y_continuous(expand = c(0,0), breaks = NULL) #remove space around plot
              hplot3 <- ggplotly(hplot3, tooltip="text")
              hplot3 <- hplot3 %>% 
                layout(margin=list(t=30, b=120, l=90), 
                       title = paste(paste0(selected, ","), "SFY", paste0(max_year_SA_regions-1, "-", max_year_SA_regions), sep = " "),
                       annotations = list(x = 1, y = -0.27, 
                                          text = HTML("Source: Connecticut DMHAS, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"),
                                          showarrow = F, 
                                          xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                          font=list(size=15, color="grey", align="right")
                       ),                       
                       barmode = 'group',
                       xaxis = list(tickfont = list(size = 12)), 
                       bargap = 0.3, showlegend = FALSE
                )
              hplot3   
  })  
  ###########################

  mhsa_table_reactive <- reactive({
    selected <- input$select
    table <- sa_mh_regions_long
    table <- subset(sa_mh_regions_long, Region == selected)
    table <- table %>% 
      select(-Region, -Year)
  })
  
   output$MHSATable <- renderTable({
      selected<- input$select
      mhsa_table_reactive()
   }, digits = 0, caption = "Source: CT DMHAS, accessed via ctdata.org", 
     striped=T, hover=T, condensed=T, responsive=T, spacing="s", align = 'lccc', width = "100%")
  ###########################
  ecplot1_reactive <- reactive({
    selected<- input$select
    ec_plot1 <- b23_regions
    ec_plot1 <- subset(ec_plot1, Region == selected & Year == max_year_b23_regions)
    ec_plot1 <- unique(ec_plot1 %>%
                       group_by(Indicator) %>% 
                       mutate(tot_Value = round(sum(Value), 0)) %>% 
                       select(-Town, -Value, -FIPS, -`Measure Period`, -`Measure Type`, -Variable, -Year)) %>% 
                       rename(Value = tot_Value)
    
    ec_plot1$ValueC <- format(ec_plot1$Value, big.mark=",", scientific=FALSE) 
    ec_plot1
  })
  
  output$ECPlot1 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    ecplot1 <- ggplot(ecplot1_reactive(), aes(x=Indicator, y=Value, fill = Indicator, text=sprintf("%s<br>%s", Indicator, ValueC))) +
              geom_bar(stat="identity", position = "dodge") + 
              xlab ("") + ylab("Number") + theme_minimal() +
              theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), axis.line.x = element_line(), plot.title = element_text(size=8))+
              scale_fill_brewer(palette="Paired") +
              scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
              scale_y_continuous(expand = c(0,0), breaks = NULL) #remove space around plot
              ecplot1 <- ggplotly(ecplot1, tooltip="text", textposition = 'auto')
              ecplot1 <- ecplot1 %>% 
                layout(margin=list(t=30, b=70, l=50), 
                       title = paste(paste0(selected, ","), max_year_b23_regions, sep = " "),
                       annotations = list(x = 1, y = -0.2, 
                                          text = HTML("Source: Connecticut Office of Early Childhood, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"),
                                          showarrow = F, 
                                          xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                          font=list(size=15, color="grey", align="right")
                       ),                       
                       barmode = 'group',
                       xaxis = list(tickfont = list(size = 12)), 
                       bargap = 0.3, showlegend = FALSE
                )
              ecplot1         
  })    
  ###########################
  ecplot2_reactive <- reactive({
    selected<- input$select
    b23c_regions$`Total Births` <- as.numeric(b23c_regions$`Total Births`)
    ec_plot2 <- b23c_regions
    ec_plot2 <- subset(ec_plot2, Region == selected & Year == max_year_b23c_regions & `Measure Type` == "Number")
    ec_plot2 <- unique(ec_plot2 %>% 
                         group_by(Indicator) %>% 
                         mutate(tot_Value = round(sum(Value), 0), 
                                total_Births = sum(`Total Births`), 
                                `% Cohort` = round((tot_Value / total_Births)*100, 1)) %>% 
                         select(-`% Cohort`, -Value, -`Total Births`, -FIPS, -Year, -`Measure Type`, -Variable, -Town, -total_Births))

    ec_plot2$ValueC <- format(ec_plot2$tot_Value, big.mark=",", scientific=FALSE) 
    ec_plot2
  })
  
  output$ECPlot2 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    

    selected<- input$select
    ecplot2 <- ggplot(ecplot2_reactive(), aes(x=Indicator, y=tot_Value, fill = Indicator, text=sprintf("%s<br>%s", Indicator, ValueC))) +
              geom_bar(stat="identity", position = "dodge") + 
              xlab ("") + ylab("Number") + theme_minimal() +
              theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), axis.line.x = element_line(), plot.title = element_text(size=8))+
              scale_fill_brewer(palette="Paired") +
              scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
              scale_y_continuous(expand = c(0,0), breaks = NULL) #remove space around plot
              ecplot2 <- ggplotly(ecplot2, tooltip="text", textposition = 'auto')
              ecplot2 <- ecplot2 %>% 
                layout(margin=list(t=30, b=70, l=50), 
                       title = paste(paste0(selected, ","), max_year_b23c_regions, "Birth Cohort", sep = " "),
                       annotations = list(x = 1, y = -0.2, 
                                          text = HTML("Source: Connecticut Office of Early Childhood, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"),
                                          showarrow = F, 
                                          xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                          font=list(size=15, color="grey", align="right")
                       ),                       
                       barmode = 'group',
                       xaxis = list(tickfont = list(size = 12)), 
                       bargap = 0.3, showlegend = FALSE
                )
              ecplot2 
  })
  ###########################
  ecplot3_reactive <- reactive({
    selected<- input$select
    b23c_regions$`Total Births` <- as.numeric(b23c_regions$`Total Births`)
    ec_plot3 <- b23c_regions
    ec_plot3 <- subset(ec_plot3, Region == selected & Year == max_year_b23c_regions & `Measure Type` == "Number")
    ec_plot3 <- unique(ec_plot3 %>% 
                         group_by(Indicator) %>% 
                         mutate(tot_Value = round(sum(Value), 0), 
                                total_Births = sum(`Total Births`), 
                                `% Cohort` = round((tot_Value / total_Births)*100, 1)) %>% 
                         select(-tot_Value, -Value, -`Total Births`, -FIPS, -Year, -`Measure Type`, -Variable, -Town, -total_Births))
    ec_plot3
  })
  
  
  output$ECPlot3 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    ecplot3 <- ggplot(ecplot3_reactive(), aes(x=Indicator, y=`% Cohort`, fill = Indicator, text=sprintf("%s<br>%s", Indicator, paste0(`% Cohort`, "%")))) +
              geom_bar(stat="identity", position = "dodge") + 
              xlab ("") + ylab("Number") + theme_minimal() +
              theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), axis.line.x = element_line(), plot.title = element_text(size=8))+
              scale_fill_brewer(palette="Paired") +
              scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
              scale_y_continuous(expand = c(0,0), breaks = NULL) #remove space around plot
              ecplot3 <- ggplotly(ecplot3, tooltip="text", textposition = 'auto')
              ecplot3 <- ecplot3 %>% 
                layout(margin=list(t=30, b=70, l=50), 
                      title = paste(paste0(selected, ","), "% of", max_year_b23c_regions, "Birth Cohort", sep = " "),
                       annotations = list(x = 1, y = -0.2, 
                                          text = HTML("Source: Connecticut Office of Early Childhood, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"),
                                          showarrow = F, 
                                          xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                          font=list(size=15, color="grey", align="right")
                       ),                       
                       barmode = 'group',
                       xaxis = list(tickfont = list(size = 12)), 
                       bargap = 0.3, showlegend = FALSE
                )
              ecplot3
  })
  ###########################  
  jj_reactive1 <- reactive({
    selected<- input$select
    jj_plot1 <- jj_regions
    jj_plot1 <- subset(jj_plot1, Year == max_year_jj_regions & Region == selected & 
                               Variable == "Juvenile Arrests" & `Measure Type` == "Number" &
                               Crime != "Total" & `Age Range` != "Total")
    jj_plot1 <- unique(jj_plot1 %>% 
                         group_by(`Age Range`, Crime) %>% 
                         mutate(tot_Value = sum(Value)) %>% 
                         select(-Town, -Value, -FIPS, -Year, -Variable, -`Measure Type`))
    jj_plot1 <- jj_plot1[jj_plot1$Crime %in% c("Drugs", "Other", "Disorderly Conduct", "Larceny", "Other Assault (Simple)"),]
    jj_plot1
  })
  
  output$JJPlot1 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    jjplot1 <- ggplot(jj_reactive1(), aes(x=Crime, y=tot_Value, fill = `Age Range`, text=sprintf("%s<br>%s<br>%s", Crime, `Age Range`, tot_Value))) +
              geom_bar(stat="identity", position = "dodge") + 
              xlab ("") + ylab("Number") + theme_minimal() +
              theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), axis.line.x = element_line(), plot.title = element_text(size=8))+
              scale_fill_brewer(palette="Paired") +
              scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
              scale_y_continuous(expand = c(0,0), breaks = NULL) #remove space around plot
              jjplot1 <- ggplotly(jjplot1, tooltip="text", textposition = 'auto')
              jjplot1 <- jjplot1 %>% 
                layout(margin=list(t=30, b=90, l=50), 
                      title = paste(paste0(selected, ","), max_year_jj_regions, sep = " "),
                       annotations = list(x = 1, y = -0.3, 
                                          text = HTML("Source: Connecticut Uniform Crime Report, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"),
                                          showarrow = F, 
                                          xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                          font=list(size=15, color="grey", align="right")
                       ),                       
                       barmode = 'group',
                       xaxis = list(tickfont = list(size = 12)), 
                       bargap = 0.3, legend = list(orientation = 'h', x = 0, y = -0.1)
                )
              jjplot1
  })
  ###########################  
  jj_reactive2 <- reactive({
    selected<- input$select
    jj_plot2 <- jj_regions
    jj_plot2 <- subset(jj_plot2, Year == max_year_jj_regions & Region == selected & Variable == "Juvenile Arrests" & 
                             `Measure Type` == "Rate (per 100,000)" & Crime == "Total")
    jj_plot2 <- unique(jj_plot2 %>% 
                         group_by(`Age Range`, Crime) %>% 
                         mutate(avg_Value = round(mean(Value), 1)) %>% 
                         select(-Town, -Value, -FIPS))
    jj_plot2
  })
  output$JJPlot2 <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    jjplot2 <- ggplot(jj_reactive2(), aes(x=`Age Range`, y=avg_Value, fill = `Age Range`, text=sprintf("%s<br>%s", `Age Range`, avg_Value))) +
              geom_bar(stat="identity", position = "dodge") + 
              xlab ("") + ylab("Rate per 100,000 Persons") + theme_minimal() +
              theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), axis.line.x = element_line(), plot.title = element_text(size=8))+
              scale_fill_brewer(palette="Paired") +
              scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
              scale_y_continuous(expand = c(0,0), breaks = NULL) #remove space around plot
              jjplot2 <- ggplotly(jjplot2, tooltip="text", textposition = 'auto')
              jjplot2 <- jjplot2 %>% 
                layout(margin=list(t=30, b=90, l=50), 
                      title = paste(paste0(selected, ","), max_year_jj_regions, sep = " "),
                       annotations = list(x = 1, y = -0.3, 
                                          text = HTML("Source: Connecticut Uniform Crime Report, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"),
                                          showarrow = F, 
                                          xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                          font=list(size=15, color="grey", align="right")
                       ),                       
                       barmode = 'group',
                       xaxis = list(tickfont = list(size = 12)), 
                       bargap = 0.3, legend = list(orientation = 'h', x = 0, y = -0.1)
                )
              jjplot2
  })
  ###########################  
  cols <- c("0 to 3 Years", "4 to 6 Years", "7 to 12 Years", "13 to 17 Years", "18 Years and Over", "Total")
  cw_age_final[cols] <- sapply(cw_age_final[cols],as.numeric)
  
  cw_table_reactive <- reactive({
    placement <- input$rd
    selected <- input$select
    table <- cw_age_final
    table <- subset(table, Region == selected & `Location of Placement` == placement)
    table <- table %>% 
      select(-Region, -`Location of Placement`)
  })
  
   output$CWTable <- renderTable({
      placement <- input$rd
      selected<- input$select
      cw_table_reactive()
   }, digits = 0, caption = "Source: CT Dept of Children and Families, accessed via data.ct.gov", 
     striped=T, hover=T, condensed=T, responsive=T, spacing="xs", width= "auto")
  ########################### 
  cwgender_reactive <- reactive ({
    selected<- input$select
    placement <- input$rd
    cw_plot1 <- cw_gender_final
    cw_plot1 <- subset(cw_plot1, Region == selected & `Location of Placement` == placement & `Type of Placement` != "Total")
    cw_plot1$`Type of Placement` <- gsub(" Placement", "", cw_plot1$`Type of Placement`)
    cw_plot1 <- gather(cw_plot1, Gender, Value, 4:6, factor_key = FALSE)
    cw_plot1 <- filter(cw_plot1, Gender != "Total")
    cw_plot1
  }) 
  
  output$CW_gender <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    placement <- input$rd
    cwplot1 <- ggplot(cwgender_reactive(), aes(x=`Type of Placement`, y=Value, fill = Gender, text=sprintf("%s<br>%s<br>%s", Gender, `Type of Placement`, Value))) +
              geom_bar(stat="identity", position = "dodge") + 
              xlab ("Type of Placement") + ylab("Number") + theme_minimal() +
              theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), axis.line.x = element_line(), 
                    plot.title = element_text(size=8)) +
              scale_fill_manual(values = c('#F84740', '#3182bd')) +
              scale_x_discrete(labels = function(x) str_wrap(x, width=40)) + coord_flip() +
              scale_y_continuous(expand = c(0,0), breaks = NULL) #remove space around plot
              cwplot1 <- ggplotly(cwplot1, tooltip="text", textposition = 'auto')
              cwplot1 <- cwplot1 %>% 
                layout(margin=list(l=220, b=82, t=30), 
                      title = paste(paste0(selected, ","), max_year_cw_gender, "-", placement, sep = " "),
                       annotations = list(x = 1, y = -0.27, 
                                          text = HTML("Source: CT Dept of Children and Families, accessed via <a href='https://data.ct.gov/' target='_blank'>data.ct.gov</a>"),
                                          showarrow = F, 
                                          xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                          font=list(size=15, color="grey", align="right")
                       ),                       
                       barmode = 'group',
                       xaxis = list(tickfont = list(size = 12)), 
                       bargap = 0.3, legend = list(x = 1, y = 1)
                )
              cwplot1
  })
  ########################### 
  cwrace_reactive <- reactive ({
    selected<- input$select
    placement <- input$rd
    cwplot2 <- cw_race_final
    cwplot2 <- subset(cwplot2, Region == selected & `Location of Placement` == placement & `Type of Placement` != "Total")
    cwplot2$`Type of Placement` <- gsub(" Placement", "", cwplot2$`Type of Placement`)
    cwplot2 <- gather(cwplot2, `Race/Ethnicity`, Value, 4:8, factor_key = FALSE)
    cwplot2 <- filter(cwplot2, `Race/Ethnicity` != "Total")
    cwplot2
  })
  
  output$CW_race <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    placement <- input$rd
    cwplot2 <- ggplot(cwrace_reactive(), aes(x=`Type of Placement`, y=Value, fill = `Race/Ethnicity`, text=sprintf("%s<br>%s<br>%s", `Race/Ethnicity`, `Type of Placement`, Value))) +
              geom_bar(stat="identity", position = "dodge") + 
              xlab ("Type of Placement") + ylab("Number") + theme_minimal() +
              theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), axis.line.x = element_line(), plot.title = element_text(size=8))+
              scale_fill_brewer(palette="Paired") +
              scale_x_discrete(labels = function(x) str_wrap(x, width=40)) + coord_flip() +
              scale_y_continuous(expand = c(0,0), breaks = NULL) #remove space around plot
              cwplot2 <- ggplotly(cwplot2, tooltip="text", textposition = 'auto')
              cwplot2 <- cwplot2 %>% 
                layout(margin=list(l=220, b=82, t=30, r=130), 
                      title = paste(paste0(selected, ","), max_year_cw_race, "-", placement, sep = " "),
                       annotations = list(x = 1, y = -0.27, 
                                          text = HTML("Source: CT Dept of Children and Families, accessed via <a href='https://data.ct.gov/' target='_blank'>data.ct.gov</a>"),
                                          showarrow = F, 
                                          xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                          font=list(size=15, color="grey", align="right")
                       ),                       
                       barmode = 'group',
                       xaxis = list(tickfont = list(size = 12)), 
                       bargap = 0.3, legend = list(x = 1, y = 1)
                )
              cwplot2
  })
  ###########################  
  output$eey_text <- renderText({
     input$select
  })  
  ########################### 
  output$dy_value_f <- renderText({
    selected<- input$select
    cw_plot3 <- cw_dy_regions_calc2[cw_dy_regions_calc2$Region == selected & cw_dy_regions_calc2$Gender == "Female",]
    value <- paste0(cw_plot3$`Percent Disengaged`, "%")
  })   
  ########################### 
  output$dy_moe_f <- renderText({
    selected<- input$select
    cw_plot3 <- cw_dy_regions_calc2[cw_dy_regions_calc2$Region == selected & cw_dy_regions_calc2$Gender == "Female",]
    moe <- cw_plot3$`Percent Disengaged MOE`
    paste("+/-", moe, sep = " ")
  })  
  ########################### 
  output$dy_value_m <- renderText({
    selected<- input$select
    cw_plot3 <- cw_dy_regions_calc2[cw_dy_regions_calc2$Region == selected & cw_dy_regions_calc2$Gender == "Male",]
    value <- paste0(cw_plot3$`Percent Disengaged`, "%")
  })   
  ########################### 
  output$dy_moe_m <- renderText({
    selected<- input$select
    cw_plot3 <- cw_dy_regions_calc2[cw_dy_regions_calc2$Region == selected & cw_dy_regions_calc2$Gender == "Male",]
    moe <- cw_plot3$`Percent Disengaged MOE`
    paste("+/-", moe, sep = " ")
  })  
  ########################### 
  output$dy_value_t <- renderText({
    selected<- input$select
    cw_plot3 <- cw_dy_regions_calc2[cw_dy_regions_calc2$Region == selected & cw_dy_regions_calc2$Gender == "Total",]
    value <- paste0(cw_plot3$`Percent Disengaged`, "%")
  })   
  ########################### 
  output$dy_moe_t <- renderText({
    selected<- input$select
    cw_plot3 <- cw_dy_regions_calc2[cw_dy_regions_calc2$Region == selected & cw_dy_regions_calc2$Gender == "Total",]
    moe <- cw_plot3$`Percent Disengaged MOE`
    paste("+/-", moe, sep = " ")
  })  
  ########################### 
  cwneglect_reactive <- reactive ({
    selected<- input$select
    town_sel <- input$town_select
    cwplot5 <- cw_can_regions
    cwplot5 <- subset(cwplot5, Region == selected & Town == town_sel & Value != -9999 & Category != "All")
    cwplot5
  })
  
  cw_plot5 <- ggplot(cwplot5, aes(Category, Value, text=sprintf("%s<br>%s<br>%s<br>%s<br>%s", Town, Region, Category, `Allegation Type`, Value))) + 
    geom_bar(aes(fill = `Allegation Type`),
                width = 0.4, position = position_dodge(width=0.5), stat="identity")
  
  cw_plot5 <- ggplotly(cw_plot5, tooltip = "text")
  
  
  output$CWPlot_neglect <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    cw_plot5 <- ggplot(cwneglect_reactive(), aes(x=`Type of Placement`, y=Value, fill = `Race/Ethnicity`, text=sprintf("%s<br>%s<br>%s", `Race/Ethnicity`, `Type of Placement`, Value))) +
              geom_bar(stat="identity", position = "dodge") + 
              xlab ("Type of Placement") + ylab("Number") + theme_minimal() +
              theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), axis.line.x = element_line(), plot.title = element_text(size=8))+
              scale_fill_brewer(palette="Paired") +
              scale_x_discrete(labels = function(x) str_wrap(x, width=40)) + coord_flip() +
              scale_y_continuous(expand = c(0,0), breaks = NULL) #remove space around plot
              cwplot2 <- ggplotly(cwplot2, tooltip="text", textposition = 'auto')
              cwplot2 <- cwplot2 %>% 
                layout(margin=list(l=220, b=82, t=30, r=130), 
                      title = paste(paste0(selected, ","), max_year_cw_race, "-", placement, sep = " "),
                       annotations = list(x = 1, y = -0.27, 
                                          text = HTML("Source: CT Dept of Children and Families, accessed via <a href='https://data.ct.gov/' target='_blank'>data.ct.gov</a>"),
                                          showarrow = F, 
                                          xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                          font=list(size=15, color="grey", align="right")
                       ),                       
                       barmode = 'group',
                       xaxis = list(tickfont = list(size = 12)), 
                       bargap = 0.3, legend = list(x = 1, y = 1)
                )
              cwplot2
  })
  
  
  
  
  
  
  
  ########################### 
  output$race_sel_text1 <- renderText({
    if (input$race == "All" & input$select == "Statewide") {
      paste0("Total Population in CT: ") 
    } else if (input$race != "All" & input$select == "Statewide") {
      paste0("Total Population of Race in CT: ") 
    } else if (input$race == "All" & input$select != "Statewide") {
      paste0("Total Population in Region: ") 
    } else if (input$race != "All" & input$select != "Statewide") {
      paste0("Total Population of Race in Region: ") 
    }
  })
  ########################### 
  output$race_sel_text2 <- renderText({
    if (input$race == "All" & input$select == "Statewide") {
      paste0("Percent Population in CT: ") 
    } else if (input$race != "All" & input$select == "Statewide") {
      paste0("Percent Race in CT: ") 
    } else if (input$race == "All" & input$select != "Statewide") {
      paste0("Percent of CT Population in Region: ") 
    } else if (input$race != "All" & input$select != "Statewide") {
      paste0("Percent Race in Region: ") 
    }
  })
  ########################### 
  output$race_sel_value1 <- renderText({
    selected<- input$select
    race_sel <- input$race
    d_plot_age_race <- subset(pop_by_age_race_regions, Region == selected & `Race/Ethnicity` == race_sel & Variable == "Population" & `Age Cohort` == "Total")
    d_plot_age_race <- d_plot_age_race %>% summarise(Total_Value = sum(Value))
    d_plot_age_race$Total_Valuec <- format(d_plot_age_race$Total_Value, big.mark = ",", scientific = FALSE)
    pop_num <- d_plot_age_race$Total_Valuec
  })
  ###########################  
  output$race_sel_value2 <- renderText({
    selected<- input$select
    race_sel <- input$race
    race_region <- subset(pop_by_age_race_regions, Region == selected & `Race/Ethnicity` == race_sel & Variable == "Population" & `Age Cohort` == "Total")
    race_region <- unique(race_region %>% mutate(Total_Value = sum(Value)) %>% select(-Town, -Value, -FIPS))
    total_region <- subset(pop_by_age_race_regions, Region == selected & `Race/Ethnicity` == "All" & Variable == "Population" & `Age Cohort` == "Total")
    total_region <- unique(total_region %>% mutate(Total_Value = sum(Value)) %>% select(-Town, -Value, -FIPS))
    race_state <- subset(pop_by_age_race_regions, Region == "Statewide" & `Race/Ethnicity` == race_sel & Variable == "Population" & `Age Cohort` == "Total")
    total_state <- subset(pop_by_age_race_regions, Region == "Statewide" & `Race/Ethnicity` == "All" & Variable == "Population" & `Age Cohort` == "Total")
    total_region_state <- merge(total_region, total_state, by = c("Race/Ethnicity", "Age Cohort", "Measure Type", "Variable", "Gender", "Year"))
    total_region_state$Percent <- round((total_region_state$Total_Value / total_region_state$Value)*100,2)
    total_race_region <- merge(race_region, total_region, by = c("Region", "Age Cohort", "Measure Type", "Variable", "Gender", "Year"))
    total_race_region$Percent <- round((total_race_region$Total_Value.x / total_race_region$Total_Value.y)*100,2)
    total_race_state <- merge(race_state, total_state, by = c("Town", "Region", "Age Cohort", "Measure Type", "Variable", "Gender", "Year")) 
    total_race_state$Percent <- round((total_race_state$Value.x / total_race_state$Value.y)*100,2)
    
    if (input$race == "All" & input$select == "Statewide") { #num = all people in CT, denom = all people in CT (100%)
      "100%"
    } else if (input$race != "All" & input$select == "Statewide") { #num = all white in CT, denom = all people in CT
      paste0(total_race_state$Percent, "%")
    } else if (input$race == "All" & input$select != "Statewide") { #num = all people in region 1, denom = all people in CT
      paste0(total_region_state$Percent, "%")
    } else if (input$race != "All" & input$select != "Statewide") { #num = all white in region 1, denom = all people in region 1
      paste0(total_race_region$Percent, "%")
    }
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
    d_plot_age_race_calc$Sum_PopC <- format(d_plot_age_race_calc$Sum_Pop, big.mark=",", scientific=FALSE)
    d_plot_age_race_calc <- d_plot_age_race_calc %>% 
       gather(Sum_Pop, Sum_PopC, -c(Age, Race, Region)) %>%
       unite(temp, Age, Sum_Pop) %>%
       spread(temp, Sum_PopC)
    cols <- c("0 to 4 years_Sum_Pop", "5 to 9 years_Sum_Pop",         
              "10 to 14 years_Sum_Pop", "15 to 19 years_Sum_Pop",     
              "20 to 24 years_Sum_Pop", "25 to 29 years_Sum_Pop",     
              "30 to 34 years_Sum_Pop", "35 to 44 years_Sum_Pop",     
              "45 to 54 years_Sum_Pop", "55 to 64 years_Sum_Pop",     
              "65 to 74 years_Sum_Pop", "75 to 84 years_Sum_Pop", 
              "85 years and over_Sum_Pop")
    d_plot_age_race_calc[cols] <- sapply(d_plot_age_race_calc[cols],as.numeric)
    m <- list(b=100, r=50) # l = left; r = right; t = top; b = bottom
    ax <- list(
      type = "category",
      categoryorder = "array",
      categoryarray = c("0 to 4 years_Sum_Pop", "5 to 9 years_Sum_Pop",         
                  "10 to 14 years_Sum_Pop", "15 to 19 years_Sum_Pop",     
                  "20 to 24 years_Sum_Pop", "25 to 29 years_Sum_Pop",     
                  "30 to 34 years_Sum_Pop", "35 to 44 years_Sum_Pop",     
                  "45 to 54 years_Sum_Pop", "55 to 64 years_Sum_Pop",     
                  "65 to 74 years_Sum_Pop", "75 to 84 years_Sum_Pop", 
                  "85 years and over_Sum_Pop"),
      showgrid = TRUE,
      showline = TRUE,
      autorange = TRUE,
      showticklabels = TRUE,
      ticks = "outside",
      tickangle = 0
)
    dplotagerace <- plot_ly(d_plot_age_race_calc, x="0 to 4", y = ~`0 to 4 years_Sum_Pop`, type = 'bar',
                        hoverinfo = 'name+text', textposition = 'auto',
                        name = '0 to 4 years', text = ~paste0(`0 to 4 years_Sum_PopC`), marker = list(color = '#A6CEE3')) %>%
                    add_trace(x="5 to 9", y=~`5 to 9 years_Sum_Pop`, name = '5 to 9 years',  text = ~paste0(`5 to 9 years_Sum_PopC`)) %>%
                    add_trace(x="10 to 14", y=~`10 to 14 years_Sum_Pop`, name = '10 to 14 years',  text = ~paste0(`10 to 14 years_Sum_PopC`)) %>%
                    add_trace(x="15 to 19", y=~`15 to 19 years_Sum_Pop`, name = '15 to 19 years',  text = ~paste0(`15 to 19 years_Sum_PopC`)) %>%
                    add_trace(x="20 to 24", y=~`20 to 24 years_Sum_Pop`, name = '20 to 24 years',  text = ~paste0(`20 to 24 years_Sum_PopC`)) %>%
                    add_trace(x="25 to 29", y=~`25 to 29 years_Sum_Pop`, name = '25 to 29 years',  text = ~paste0(`25 to 29 years_Sum_PopC`)) %>%
                    add_trace(x="30 to 34", y=~`30 to 34 years_Sum_Pop`, name = '30 to 34 years',  text = ~paste0(`30 to 34 years_Sum_PopC`)) %>%
                    add_trace(x="35 to 44", y=~`35 to 44 years_Sum_Pop`, name = '35 to 44 years',  text = ~paste0(`35 to 44 years_Sum_PopC`)) %>%
                    add_trace(x="45 to 54", y=~`45 to 54 years_Sum_Pop`, name = '45 to 54 years',  text = ~paste0(`45 to 54 years_Sum_PopC`)) %>%
                    add_trace(x="55 to 64", y=~`55 to 64 years_Sum_Pop`, name = '55 to 64 years',  text = ~paste0(`55 to 64 years_Sum_PopC`)) %>%
                    add_trace(x="65 to 74", y=~`65 to 74 years_Sum_Pop`, name = '65 to 74 years',  text = ~paste0(`65 to 74 years_Sum_PopC`)) %>%
                    add_trace(x="75 to 84", y=~`75 to 84 years_Sum_Pop`, name = '75 to 84 years',  text = ~paste0(`75 to 84 years_Sum_PopC`)) %>%
                    add_trace(x="85 and over", y=~`85 years and over_Sum_Pop`, name = '85 years and over',  
                              text = ~paste0(`85 years and over_Sum_PopC`)) %>%
                layout(margin = m, showlegend = FALSE,
                       title = paste(paste0(unique(d_plot_age_race_calc$Region), ","), max_year_pop_regions, sep = " "),          
                       xaxis = list(title = "Age Group", tickangle = 30, 
      type = "category",
      categoryorder = "array",
      categoryarray = c("0 to 4 years_Sum_Pop", "5 to 9 years_Sum_Pop",         
                  "10 to 14 years_Sum_Pop", "15 to 19 years_Sum_Pop",     
                  "20 to 24 years_Sum_Pop", "25 to 29 years_Sum_Pop",     
                  "30 to 34 years_Sum_Pop", "35 to 44 years_Sum_Pop",     
                  "45 to 54 years_Sum_Pop", "55 to 64 years_Sum_Pop",     
                  "65 to 74 years_Sum_Pop", "75 to 84 years_Sum_Pop", 
                  "85 years and over_Sum_Pop"),
      autorange = TRUE),
                       yaxis = list(title = "Number"), 
                       annotations = list(x = 1, y = -0.3, text = HTML("Source: U.S. Census, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"), 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")),
                       barmode = 'group')

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
    d_plot_age_calc_plot$FemaleC <- format(d_plot_age_calc_plot$Female, big.mark=",", scientific=FALSE)
    d_plot_age_calc_plot$abs_m_popC <- format(d_plot_age_calc_plot$abs_m_pop, big.mark=",", scientific=FALSE)
    m <- list(l=120, b=100) # l = left; r = right; t = top; b = bottom
    xState <- list(tickangle = 0, title = "Population", 
              tickmode='array', 
              tickvals = c(-600000, -500000, -400000, -300000, -200000, -100000, 0, 100000, 200000, 300000, 400000, 500000, 600000), 
              ticktext = c('600,000', '500,000', '400,000', '300,000', '200,000', '100,000', '0', '100,000', '200,000', '300,000', '400,000', '500,000', '600,000'))
    xRegion <- list(tickangle = 0, title = "Population", 
               tickmode='array', 
               tickvals = c(-100000, -80000, -60000, -40000, -20000, 0, 20000, 40000, 60000, 80000, 100000), 
               ticktext = c('100,000', '80,000', '60,000', '40,000', '20,000', '0', '20,000', '40,000', '60,000', '80,000', '100,000'))
    y <- list(tickangle = 0, title = "Age Range")
    
    if (input$select == "Statewide") {
      xx = xState
    } else {
      xx = xRegion
    }
    dplotage <- plot_ly(d_plot_age_calc_plot, x = ~Female, y = ~Age, type = 'bar',
                        hoverinfo = 'name+text',textposition = 'auto',
                        orientation = 'h', name = 'Female', text = ~paste0(FemaleC),
                        marker = list(color = 'rgba(222,45,38,0.8)', 
                                                  width = 3)) %>%
                add_trace(x=~Male, name = 'Male',  text = ~paste0(abs_m_popC),                      
                          marker = list(color = 'rgb(49,130,189)', 
                                                  width = 3)) %>%
                layout(margin = m, legend = list(traceorder='reversed', orientation = 'h', x = 0.1, y = -0.1),
                       title = paste(paste0(unique(d_plot_age_calc_plot$Region), ","),
                                     max_year_pop_regions,  
                                     sep = " "),
                       annotations = list(x = 1, y = -0.3, text = HTML("Source: U.S. Census, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"), 
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
    d_plot_race_calc_plot$FemaleC <- format(d_plot_race_calc_plot$Female, big.mark=",", scientific=FALSE)
    d_plot_race_calc_plot$abs_m_popC <- format(d_plot_race_calc_plot$abs_m_pop, big.mark=",", scientific=FALSE)
    m <- list(l=250, b=100) # l = left; r = right; t = top; b = bottom
    xState <- list(tickangle = 0, 
                   title = "Population", 
                   tickmode='array', 
                   tickvals = c( -1250000, -1000000, -750000, -500000, -250000, 0, 
                                250000, 500000, 750000, 1000000, 1250000), 
                   ticktext = c('1,250,000', '1,000,000', '750,000', '500,000', '250,000', '0', 
                                '250,000', '500,000', '750,000', '1,000,000', '1,250,000'))
    xRegion <- list(tickangle = 0, title = "Population", 
               tickmode='array', 
               tickvals = c(-300000, -250000, -200000, -150000, -100000, -50000, 0, 
                            50000, 100000, 150000, 200000, 250000, 300000), 
               ticktext = c('300,000', '250,000', '200,000', '150,000', '100,000', '50,000', '0', 
                            '50,000', '100,000', '150,000', '200,000', '250,000', '300,000'))
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
                        orientation = 'h', name = 'Female', text = ~paste0(FemaleC),
                        marker = list(color = 'rgba(222,45,38,0.8)', 
                                                  width = 3)) %>%
                add_trace(x=~Male, name = 'Male',  text = ~paste0(abs_m_popC),                      
                          marker = list(color = 'rgb(49,130,189)', 
                                                  width = 3)) %>%
                layout(margin = m,
                       legend = list(traceorder='reversed', orientation = 'h', x = 0, y = -0.1),
                       title = paste(paste0(unique(d_plot_race_calc_plot$Region), ","),
                                     max_year_pop_regions,  
                                     sep = " "),
                       annotations = list(x = 1, y = -0.3, text = HTML("Source: U.S. Census, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"), 
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
    mhi_df_towns <- mhi_df_regions[mhi_df_regions$Region == selected,]
    mhi_df_towns <- spread(mhi_df_towns, Variable, Value)
    mhi_df_towns$Town <- factor(mhi_df_towns$Town, levels = unique(mhi_df_towns$Town)[order(mhi_df_towns$`Median Household Income`, decreasing = TRUE)])
    mhi_df_towns$`Median Household IncomeC` <- format(mhi_df_towns$`Median Household Income`, big.mark = ",", scientific=FALSE)
    mhi_df_towns$`Margins of ErrorC` <- format(mhi_df_towns$`Margins of Error`, big.mark = ",", scientific=FALSE)

    dplot3 <- plot_ly(mhi_df_towns, x = ~Town, y = ~`Median Household Income`, 
                     name = 'Median Household Income', type = 'scatter', mode = 'markers',
                     hoverinfo = 'text', text = ~paste(paste0(Town, ":"), paste0("$", `Median Household IncomeC`), "+/-", `Margins of ErrorC`, sep = " "), 
                     error_y = ~list(array = `Margins of Error`, color = '#000000')) %>%
     layout(margin=list(b=100, r=45),
            title = paste(selected, max_year_mhi_regions, sep = ", "),
         xaxis = list(title = "Towns", tickangle = 45),
         yaxis = list(title = "Median Household Income ($)"), 
         barmode = 'group', 
         legend = list(x = 0.7, y = 0.95),
         annotations = list(x = 1, y = -0.35, text = HTML("Source: U.S. Census, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"), 
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
    mhi_df_towns <- mhi_df_regions[mhi_df_regions$Region == selected,]
    mhi_df_towns <- spread(mhi_df_towns, Variable, Value)
    max_valueC <- format(mhi_df_towns$`Median Household Income`, big.mark=",", scientific=FALSE) 
    paste0("$", max_valueC)
  }) 
  ########################### 
  output$mhi_moe <- renderText({
    selected<- input$select
    mhi_df_towns <- mhi_df_regions[mhi_df_regions$Region == selected,]
    mhi_df_towns <- spread(mhi_df_towns, Variable, Value)
    max_moeC <- format(mhi_df_towns$`Margins of Error`, big.mark=",", scientific=FALSE) 
    paste0("+/-", max_moeC)
  })  
  ########################### 
  output$max_mhi_town <- renderText({
    selected<- input$select
    mhi_df_towns <- mhi_df_regions[mhi_df_regions$Region == selected,]
    mhi_df_towns <- spread(mhi_df_towns, Variable, Value)
    max_value <- max(mhi_df_towns$`Median Household Income`)
    max_mhi_df_towns <- mhi_df_towns[mhi_df_towns$`Median Household Income` == max_value,]
    max_town <- unique(max_mhi_df_towns$Town)
    max_town
  })
  ########################### 
  output$max_mhi_value <- renderText({
    selected<- input$select
    mhi_df_towns <- mhi_df_regions[mhi_df_regions$Region == selected,]
    mhi_df_towns <- spread(mhi_df_towns, Variable, Value)
    max_value <- max(mhi_df_towns$`Median Household Income`)
    max_mhi_df_towns <- mhi_df_towns[mhi_df_towns$`Median Household Income` == max_value,]
    max_town <- unique(max_mhi_df_towns$Town)
    max_valueC <- format(max_value, big.mark=",", scientific=FALSE) 
    paste0("$", max_valueC)
  }) 
  ########################### 
  output$max_mhi_moe <- renderText({
    selected<- input$select
    mhi_df_towns <- mhi_df_regions[mhi_df_regions$Region == selected,]
    mhi_df_towns <- spread(mhi_df_towns, Variable, Value)
    max_value <- max(mhi_df_towns$`Median Household Income`)
    max_mhi_df_towns <- mhi_df_towns[mhi_df_towns$`Median Household Income` == max_value,]
    max_moe <- unique(max_mhi_df_towns$`Margins of Error`)
    max_moeC <- format(max_moe, big.mark=",", scientific=FALSE) 
    paste0("+/-", max_moeC)
  })    
  ########################### 
  output$min_mhi_town <- renderText({
    selected<- input$select
    mhi_df_towns <- mhi_df_regions[mhi_df_regions$Region == selected,]
    mhi_df_towns <- spread(mhi_df_towns, Variable, Value)
    min_value <- min(mhi_df_towns$`Median Household Income`)
    min_mhi_df_towns <- mhi_df_towns[mhi_df_towns$`Median Household Income` == min_value,]
    min_town <- unique(min_mhi_df_towns$Town)
    min_town
  })
  ########################### 
  output$min_mhi_value <- renderText({
    selected<- input$select
    mhi_df_towns <- mhi_df_regions[mhi_df_regions$Region == selected,]
    mhi_df_towns <- spread(mhi_df_towns, Variable, Value)
    min_value <- min(mhi_df_towns$`Median Household Income`)
    min_mhi_df_towns <- mhi_df_towns[mhi_df_towns$`Median Household Income` == min_value,]
    min_town <- unique(min_mhi_df_towns$Town)
    min_valueC <- format(min_value, big.mark=",", scientific=FALSE) 
    paste0("$", min_valueC)
  }) 
  ########################### 
  output$min_mhi_moe <- renderText({
    selected<- input$select
    mhi_df_towns <- mhi_df_regions[mhi_df_regions$Region == selected,]
    mhi_df_towns <- spread(mhi_df_towns, Variable, Value)
    min_value <- min(mhi_df_towns$`Median Household Income`)
    min_mhi_df_towns <- mhi_df_towns[mhi_df_towns$`Median Household Income` == min_value,]
    min_town <- unique(min_mhi_df_towns$Town)
    min_moe <- unique(min_mhi_df_towns$`Margins of Error`)
    min_moeC <- format(min_moe, big.mark=",", scientific=FALSE) 
    paste0("+/-", min_moeC)
  })   
 
  ########################### 
  output$mhi_moe <- renderText({
     selected<- input$select
    total_mhi <- mhi_df_regions[mhi_df_regions$Variable == "Median Household Income",]
    total_n <- total_mhi %>% 
      count(Region)
    total_mhi <- merge(total_mhi, total_n, by = "Region")
    total_mhi_calc <- unique(total_mhi %>% 
      group_by(Region) %>% 
      mutate(tot_mhi = sum(Value), 
             final_mhi = tot_mhi / n, 
             sigma = sd(Value), 
             sd_mhi = sigma/sqrt(n), 
             ci_mhi = 2.01*sd_mhi) %>% #tvalue n-1<35
        select(Region, final_mhi, ci_mhi) %>% 
        filter(Region != "Statewide")) 
    
    test  <- total_mhi %>% 
      group_by(Region) %>% 
      mutate(tot_mhi = sum(Value), 
             final_mhi = tot_mhi / n, 
             sigma = sd(Value), 
             sd_mhi = sigma/sqrt(n), 
             ci_mhi = 2.01*sd_mhi) %>% #tvalue n-1<35
        filter(Region != "Statewide")
    
    total_mhi_calc <- as.data.frame(total_mhi_calc)  
    #bring in Statewide MOE
    total_moe <- mhi_df_regions[mhi_df_regions$Region == "Statewide",]
    total_moe <- spread(total_moe, Variable, Value)
    total_moe <- total_moe %>% 
      select(Region, `Median Household Income`, `Margins of Error`) %>% 
      rename(final_mhi = `Median Household Income`, ci_mhi = `Margins of Error`)
  
    total_mhi_calc <- rbind(total_mhi_calc, total_moe)
    total_mhi_calc$final_mhi <- round(total_mhi_calc$final_mhi, 0)
    total_mhi_calc$ci_mhi <- round(total_mhi_calc$ci_mhi, 0)
    total_mhi_calc$Region <- factor(total_mhi_calc$Region, 
                                         levels = c("Statewide", 
                                                    "Southwest Region",      #1
                                                    "South Central Region",  #2
                                                    "Eastern Region",        #3
                                                    "North Central Region",  #4
                                                    "Western Region",        #5
                                                    "Central Region"))       #6
     mhi_value_df <- total_mhi_calc[total_mhi_calc$Region == selected,]
     moe <- format(unique(mhi_value_df$ci_mhi), big.mark=",", scientific=FALSE) 
     paste0("+/-", moe)
  })  
  ###########################
  output$DPlot_pov <- renderPlotly({
    shiny::validate(
      need(input$select != "", "Please select a Region to populate the chart")
    )    
    selected<- input$select
    d_plot_pov <- pov_df_regions
    d_plot_pov <- subset(d_plot_pov, Region == selected)
    
    dplotpov <- ggplot(d_plot_pov, aes(fill = Age, text=sprintf("%s<br>%s<br>%s", `Age`, `Race/Ethnicity`, paste0(`Percent Below Poverty Level`, "%")))) + 
      geom_bar(position = 'dodge', stat='identity') +
      xlab ("Percent Below Poverty Level") + ylab("Race/Ethnicity") + theme_minimal() + 
      aes(x = `Race/Ethnicity`, y=`Percent Below Poverty Level`) + 
      facet_grid(~Age, scales = "free_y", space = "free") + 
      coord_flip() +
      theme(legend.position="none",  panel.grid.major.y = element_blank(),  plot.title = element_text(size=8), 
            panel.spacing.x = unit(1E-5, "lines")) +
      scale_fill_brewer(palette="Paired") +
      scale_y_continuous(breaks=seq(0,40,5))
    dplotpov <- ggplotly(dplotpov, tooltip = "text", textposition = "auto")
    dplotpov <- dplotpov %>%
      layout(margin=list(l=260, t=50, b=90), 
      title = paste(paste0(selected, ","), unique(d_plot_pov$Year), sep = " "),
      annotations = list(x = 1, y = -0.2, text = HTML("Source: U.S. Census, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"), 
                         showarrow = F, 
                         xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,
                         font=list(size=15, color="grey", align="right")
                    )
      )
    dplotpov  
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
    bh_plot_race$WhiteC <- format(bh_plot_race$White, big.mark=",", scientific=FALSE)
    bh_plot_race$BlackC <- format(bh_plot_race$Black, big.mark=",", scientific=FALSE)
    bh_plot_race$HispanicC <- format(bh_plot_race$Hispanic, big.mark=",", scientific=FALSE)
    bh_plot_race$OtherC <- format(bh_plot_race$Other, big.mark=",", scientific=FALSE)

    bhplotrace <- plot_ly(bh_plot_race, x = ~`Age Range`, y = ~White, name = 'White', type = 'bar', 
                     hoverinfo = 'text', text = ~paste0(WhiteC), textposition = 'auto', 
                     marker = list(color="#A6CEE3")) %>%
                   add_trace( y = ~Hispanic, name = 'Hispanic or Latino', text = ~paste0(HispanicC), 
                     marker = list(color="#1F78B4")) %>%
             add_trace( y = ~Black, name = 'Black or African American', text = ~paste0(BlackC), 
                     marker = list(color="#B2DF8A")) %>%
             add_trace( y = ~Other, name = 'Other', text = ~paste0(OtherC), 
                     marker = list(color="#33A02C")) %>%
     layout(margin=m, title = paste(paste0(unique(bh_plot_race$Region), ","), "2010", sep = " "),
         xaxis = list(title = ""),
         yaxis = list(title = "Number"), 
         barmode = 'group', legend = list(orientation = 'h', x = 0.2, y = -0.15),
         annotations = list(x = 1, y = -0.35, text = HTML("Source: US Census, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>; Calculations by CONNECT project"), 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")))
     bhplotrace    
  })
  ###########################  
   output$BHTable <- function() {
    if(input$select=="Statewide") {
      bh_CT[is.na(bh_CT)]<-""
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
      bh_r1[is.na(bh_r1)]<-""
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
      bh_r2[is.na(bh_r2)]<-""
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
      bh_r3[is.na(bh_r3)]<-""
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
      bh_r4[is.na(bh_r4)]<-""
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
      bh_r5[is.na(bh_r5)]<-""
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
      bh_r6[is.na(bh_r6)]<-""
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
  
  dd <- unique(edu$District)
  values = list("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462",
                "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f",
                "#e6194b", "#3cb44b", "#ffe119", "#0082c8", "#f58231")
  dups <- list(values)[rep(1,12)] #<- repeats it 17 times to create list of 204
  dups2 <- do.call(c, unlist(dups, recursive=FALSE))
  set.seed(007)
  dd.col.random <- sample(dups2)
  names(dd.col.random)  <- dd #ensures both charts have same colors assigned to same districts
  
  eplot1_reactive <- reactive ({
    selected<- input$select_edu
    edu_plot <- edu[edu$`Measure Type` == "Percent" & edu$District %in% selected & 
                       edu$Value != -9999.0 &
                       edu$Year == max_year_edu,]
  })

  output$EPlot1 <- renderPlotly({
    shiny::validate(
      need(input$select_edu != "", "Please select a District to populate the chart")
    )
    shiny::validate(
      need(nrow(eplot1_reactive()) != 0, "No data are available for your selection, try selecting another District")
    )    
    e_plot1 <- ggplot(eplot1_reactive(), 
                      aes(`Indicator of Educational Need`, y=Value, fill=District, 
                          text=sprintf("%s<br>%s<br>%s", District, `Indicator of Educational Need`, paste0(Value, "%"))))+
         geom_bar(stat="identity", position = "dodge") + 
         theme_bw() +
         xlab("Indicator of Educational Need") + ylab("Percent") +
         scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
         scale_fill_manual(values = dd.col.random)+
         theme(axis.text = element_text(size=8), 
 			         plot.title = element_text(size=10, face="bold"),
               axis.title = element_text(size=10), 
 			         plot.margin = unit(c(0.1,0.1,1,0.1), "cm")) 
         e_plot1 <- ggplotly(e_plot1, tooltip="text")
         e_plot1 <- e_plot1 %>% 
           layout(margin=list(l=40, b=90), 
                  annotations = list(x = 1.2, y = -0.3, text = HTML("Source: Connecticut State Department of Education, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"), 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=15, color="grey")))
         e_plot1
   })
  ###########################  
  eplot2_reactive <- reactive ({
    selected<- input$select_edu
    edu2_plot <- edu2[edu2$`Measure Type` == "Percent" & edu2$District %in% selected & 
                        edu2$Value != -6666 & edu2$Value != -9999 & edu2$Year == max_year_edu2,]
  })
  output$EPlot2 <- renderPlotly({
    shiny::validate(
      need(input$select_edu != "", "Please select a District to populate the chart")
    )    
    shiny::validate(
      need(nrow(eplot2_reactive()) != 0, "No data are available for your selection, try selecting another District")
    )
    e_plot2 <- ggplot(eplot2_reactive(), aes(x=`Race/Ethnicity`, y=Value, fill=District,
                                          text=sprintf("%s<br>%s<br>%s", District, `Race/Ethnicity`, paste0(Value, "%")))) +
      geom_bar(stat="identity", position = "dodge") + ylab("Percent") +
      theme_bw() + 
      scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
      scale_fill_manual(values = dd.col.random)+ 
      theme(axis.text = element_text(size=8),
			      plot.title = element_text(size=10, face="bold"), 
            axis.title=element_text(size=10), 
 			      plot.margin = unit(c(0.1,0.1,1,0.1), "cm")) 
      e_plot2 <- ggplotly(e_plot2, tooltip="text")
      e_plot2 <- e_plot2 %>% layout(
        margin=list(l=40, b=90), 
        annotations = list(x = 1.2, y = -0.3, text = HTML("Source: Connecticut State Department of Education, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"), 
        showarrow = F, xref='paper', yref='paper', 
        xanchor='right', yanchor='auto', xshift=0, yshift=0,
        font=list(size=15, color="grey")),
        barmode = 'group',
        xaxis = list(tickfont = list(size = 10)), 
        bargap = 0.3,
        legend = list(x = 100, y = 0.9, font = list(size=10)))
      e_plot2        
    })
  ###########################  
  output$EPlot3 <- renderPlotly({
    selected<- input$select_edu
    edu3_plot <- edu3[edu3$District %in% selected & edu3$Value != -6666 & edu3$Value != -9999 & 
                        edu3$Year == max_year_edu3,]
    edu3_plot$Valuec <- format(edu3_plot$Value, big.mark = ",", scientific = FALSE)
    
    shiny::validate(
      need(input$select_edu != "", "Please select a District to populate the chart")
    )    
    shiny::validate(
      need(nrow(edu3_plot) != 0, "No data are available for your selection, try selecting another District")
    )
    e_plot3 <- ggplot(edu3_plot, aes(x=`Sanction Type`, y=Value, fill=District,
                                          text=sprintf("%s<br>%s<br>%s", District, `Sanction Type`, paste0(Valuec)))) +
      geom_bar(stat="identity", position = "dodge") + ylab("Number") +
      theme_bw() + 
      scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
      scale_fill_manual(values = dd.col.random)+ 
      theme(axis.text = element_text(size=8),
			      plot.title = element_text(size=10, face="bold"), 
            axis.title=element_text(size=10), 
 			      plot.margin = unit(c(0.1,0.1,1,0.1), "cm")) 
      e_plot3 <- ggplotly(e_plot3, tooltip="text")
      e_plot3 <- e_plot3 %>% layout(
        margin=list(l=50, b=90), 
        annotations = list(x = 1.2, y = -0.3, text = HTML("Source: Connecticut State Department of Education, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"), 
        showarrow = F, xref='paper', yref='paper', 
        xanchor='right', yanchor='auto', xshift=0, yshift=0,
        font=list(size=15, color="grey")),
        barmode = 'group',
        xaxis = list(tickfont = list(size = 10)),
        bargap = 0.3,
        legend = list(x = 100, y = 0.9, font = list(size=10)))
      e_plot3        
    })
  ########################### 
  output$EPlot4 <- renderPlotly({
    selected<- input$select_edu
    edu4_plot <- edu4[edu4$District %in% selected & edu4$Value != -6666 & edu4$Value != -9999 & 
                        edu4$Year == max_year_edu4,]
    edu4_plot$Valuec <- format(edu4_plot$Value, big.mark = ",", scientific = FALSE)
    
    shiny::validate(
      need(input$select_edu != "", "Please select a District to populate the chart")
    )    
    shiny::validate(
      need(nrow(edu4_plot) != 0, "No data are available for your selection, try selecting another District")
    )
    e_plot4 <- ggplot(edu4_plot, aes(x=`Incident Type`, y=Value, fill=District,
                                          text=sprintf("%s<br>%s<br>%s", District, `Incident Type`, paste0(Valuec)))) +
      geom_bar(stat="identity", position = "dodge") + ylab("Number") +
      theme_bw() + 
      scale_x_discrete(labels = function(x) str_wrap(x, width=10)) +
      scale_fill_manual(values = dd.col.random)+ 
      theme(axis.text = element_text(size=8),
			      plot.title = element_text(size=10, face="bold"), 
            axis.title=element_text(size=10), 
 			      plot.margin = unit(c(0.1,0.1,1,0.1), "cm")) 
      e_plot4 <- ggplotly(e_plot4, tooltip="text")
      e_plot4 <- e_plot4 %>% layout(
        margin=list(l=50, b=90), 
        annotations = list(x = 1.2, y = -0.3, text = HTML("Source: Connecticut State Department of Education, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"), 
        showarrow = F, xref='paper', yref='paper', 
        xanchor='right', yanchor='auto', xshift=0, yshift=0,
        font=list(size=15, color="grey")),
        barmode = 'group',
        xaxis = list(tickfont = list(size = 10)),
        bargap = 0.3,
        legend = list(x = 100, y = 0.9, font = list(size=10)))
      e_plot4        
    })   
  ########################### 
  output$EPlot5 <- renderPlotly({
    selected <- input$select_edu
    edu5_plot <- edu5[edu5$District %in% selected & edu5$Value != -6666 & edu5$Value != -9999 & 
                        edu5$Year == max_year_edu5,]
    cols <- c("Four Year Graduation Rate", "Total Cohort Count")

    if (nrow(edu5_plot) != 0) {
      edu5_plot <- edu5_plot %>% select(-c(`Measure Type`)) %>% spread(Variable, Value)
    } else {
      dat <- data.frame(matrix(ncol = 2, nrow=0))
      colnames(dat) <- cols
      edu5_plot <- cbind(edu5_plot, dat)
    }
    if (!"Four Year Graduation Rate" %in% colnames(edu5_plot)) {
      edu5_plot["Four Year Graduation Rate"[!("Four Year Graduation Rate" %in% colnames(edu5_plot))]] = NA
    }
    if (!"Total Cohort Count" %in% colnames(edu5_plot)) {
      edu5_plot["Total Cohort Count"[!("Total Cohort Count" %in% colnames(edu5_plot))]] = NA
    }
    
    shiny::validate(
      need(input$select_edu != "", "Please select a District to populate the chart")
    )    
    shiny::validate(
      need(nrow(edu5_plot) != 0, "No data are available for your selection, try selecting another District")
    )
    e_plot5 <- ggplot(edu5_plot, aes(x=`Race/Ethnicity`, y=`Four Year Graduation Rate`, fill=District,
                                          text=sprintf("%s<br>%s<br>%s", 
                                                       District, 
                                                       paste0(`Race/Ethnicity`, " Cohort Count: ", `Total Cohort Count`),
                                                       paste0("4-Year Grad Rate: ", `Four Year Graduation Rate`, "%")))) +
      geom_bar(stat="identity", position = "dodge") + ylab("Percent") +
      theme_bw() + 
      scale_x_discrete(labels = function(x) str_wrap(x, width=10)) +
      scale_fill_manual(values = dd.col.random)+ 
      theme(axis.text = element_text(size=8),
			      plot.title = element_text(size=10, face="bold"), 
            axis.title=element_text(size=10), 
 			      plot.margin = unit(c(0.1,0.1,1,0.1), "cm")) 
      e_plot5 <- ggplotly(e_plot5, tooltip="text")
      e_plot5 <- e_plot5 %>% layout(
        margin=list(l=50, b=110), 
        annotations = list(x = 1.2, y = -0.3, text = HTML("Source: Connecticut State Department of Education, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"), 
        showarrow = F, xref='paper', yref='paper', 
        xanchor='right', yanchor='auto', xshift=0, yshift=0,
        font=list(size=15, color="grey")),
        barmode = 'group',
        xaxis = list(tickfont = list(size = 10)),
        bargap = 0.3,
        legend = list(x = 100, y = 0.9, font = list(size=10)))
      e_plot5        
    })     
  ########################### 
  eplot6_reactive <- reactive ({
    selected <- input$select_edu
    edu6_plot <- edu6[edu6$District %in% selected & edu6$Value != -6666 & edu6$Value != -9999 & 
                        edu6$Year == max_year_edu6,]
  })
  output$EPlot6 <- renderPlotly({

    shiny::validate(
      need(input$select_edu != "", "Please select a District to populate the chart")
    )    
    shiny::validate(
      need(nrow(eplot6_reactive()) != 0, "No data are available for your selection, try selecting another District")
    )
    e_plot6 <- ggplot(eplot6_reactive(), aes(x=`Race/Ethnicity`, y=`Value`, fill=District,
                                          text=sprintf("%s<br>%s<br>%s", 
                                                       District, `Race/Ethnicity`, paste0(`Value`, "%")))) +
      geom_bar(stat="identity", position = "dodge") + ylab("Percent") +
      theme_bw() + 
      scale_x_discrete(labels = function(x) str_wrap(x, width=10)) +
      scale_fill_manual(values = dd.col.random)+ 
      theme(axis.text = element_text(size=8),
			      plot.title = element_text(size=10, face="bold"), 
            axis.title=element_text(size=10), 
 			      plot.margin = unit(c(0.1,0.1,1,0.1), "cm")) 
      e_plot6 <- ggplotly(e_plot6, tooltip="text")
      e_plot6 <- e_plot6 %>% layout(
        margin=list(l=50, b=140), 
        annotations = list(x = 1, y = -0.49, text = HTML("Source: Connecticut State Department of Education, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"), 
        showarrow = F, xref='paper', yref='paper', 
        xanchor='right', yanchor='auto', xshift=0, yshift=0,
        font=list(size=15, color="grey")),
        barmode = 'group',
        xaxis = list(tickfont = list(size = 10)),
        bargap = 0.3,
        legend = list(x = 100, y = 0.9, font = list(size=10)))
      e_plot6        
    })    
  ########################### 
  output$EPlot7 <- renderPlotly({
    selected <- input$select_edu
    edu7_plot <- edu7[edu7$District %in% selected & edu7$Value != -6666 & edu7$Value != -9999 & 
                        edu7$Year == max_year_edu7,]
    edu7_plot$Valuec <- format(edu7_plot$Value, big.mark = ",", scientific = FALSE)
    
    shiny::validate(
      need(input$select_edu != "", "Please select a District to populate the chart")
    )    
    shiny::validate(
      need(nrow(edu7_plot) != 0, "No data are available for your selection, try selecting another District")
    )
    
    e_plot7 <- ggplot(edu7_plot, aes(x=`Race/Ethnicity`, y=`Value`, fill=District,
                                          text=sprintf("%s<br>%s<br>%s", 
                                                       District, `Race/Ethnicity`, `Valuec`))) +
      geom_bar(stat="identity", position = "dodge") + ylab("Number") +
      theme_bw() + 
      scale_x_discrete(labels = function(x) str_wrap(x, width=10)) +
      scale_fill_manual(values = dd.col.random)+ 
      theme(axis.text = element_text(size=8),
			      plot.title = element_text(size=10, face="bold"), 
            axis.title=element_text(size=10), 
 			      plot.margin = unit(c(0.1,0.1,1,0.1), "cm")) 
      e_plot7 <- ggplotly(e_plot7, tooltip="text")
      e_plot7 <- e_plot7 %>% layout(
        margin=list(l=50, b=140), 
        annotations = list(x = 1, y = -0.49, text = HTML("Source: Connecticut State Department of Education, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"), 
        showarrow = F, xref='paper', yref='paper', 
        xanchor='right', yanchor='auto', xshift=0, yshift=0,
        font=list(size=15, color="grey")),
        barmode = 'group',
        xaxis = list(tickfont = list(size = 10)),
        bargap = 0.3,
        legend = list(x = 100, y = 0.9, font = list(size=10)))
      e_plot7        
    })     
  ###########################
  output$kei <- renderPlotly({
   selected = input$select_edu
   kei_plot <- kei[kei$FixedDistrict %in% selected,]

    shiny::validate(
      need(input$select_edu != "", "Please select a District to populate the chart")
    )    
    shiny::validate(
      need(nrow(kei_plot) != 0, "No data are available for your selection, try selecting another District")
    )
   kei_plot <- gather(kei_plot, Level, Percent, 4:6, factor_key = FALSE)
   kei_plot$Level <- gsub("Level ", "", kei_plot$Level)   
   kei_total_plot <- ggplot(kei_plot, aes(x=Level, y=Percent, fill = FixedDistrict, 
                                          text=sprintf("%s<br>%s<br>%s", FixedDistrict, paste0(Domain, ": ", paste0("Level ", Level)), paste0(Percent, "%")))) +
      geom_bar(stat="identity", position = "dodge") + labs(x= "Level", y="Percent") +
           scale_fill_manual(values = dd.col.random) + 
      theme_bw() + theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)), legend.position = "none", 
                         panel.spacing.x=unit(0.0, "lines")) +
      facet_grid(FixedDistrict~Domain, scales = "free", labeller = label_wrap_gen(width = 10, multi_line = TRUE))
    kei_total_plot <- ggplotly(kei_total_plot, tooltip = "text")
    kei_total_plot <- kei_total_plot %>% 
      layout(margin=list(l=70, b=90, r=150),
             annotations = list(x = 1, y = -0.24,
                            text = HTML("Source: Connecticut State Department of Education, accessed via <a href='http://ctdata.org/' target='_blank'>ctdata.org</a>"),
                            showarrow = F, xref='paper', yref='paper',
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15, color="grey")),
             showlegend = FALSE)
    kei_total_plot
  })
})