server <- function(input, output, session) {
  #make data frame for just selected disease
  disease <- reactive({
    select_disease_crosstabs[as.numeric(input$disease)] %>%
      as.data.frame()
  })
  
  #Get column numbers of first and last years of data
  year1 <- reactive({
    1
  })
  
  year2 <- reactive({
    ncol(disease())
  })
  
  #Selected social data
  selected_social <- reactive({
    social_data[,as.numeric(input$social)]
  })


  #Calculate average incidence per 100,000 population
  disease_av_per_thousand <- reactive({
    req(year2())
    req(year1())

    if(year2()-year1() > 0){
      disease_sum = rowSums(disease()[,year1():year2()])
      disease_av = disease_sum/(year2()-year1()+1)
    }
    else{
      disease_av = disease()[,year1()]
    }
    disease_av_per_thousand = (disease_av/town_size_district$Population)*100000
    return(disease_av_per_thousand)
  })
  
  #Selected Town for label (if any)
  town_index <- reactive({
    as.numeric(input$town)
  })
  
  town_name <- reactive({
    names(CC_towns_vector)[town_index()+1]
  })


  #Disease and social index and year names
  disease_name <- reactive({
    disease_list[as.numeric(input$disease)]
  })
  
  social_name <- reactive({
    social_list[as.numeric(input$social)]
  })
  
  year1_name <- reactive({
    gsub("^.*\\.", "",colnames(disease())[year1()])
  })
  
  year2_name <- reactive({
    gsub("^.*\\.", "",colnames(disease())[year2()])
  })

  district_colors = reactive({c("#b55681","#95c47b","#29a7c6", "#f7986f")})

############################# Scatter plot of disease by social index ########################################## 
  output$ScatterPlot <- renderPlotly({
    #If a town is selected, define annotation
    if(town_index() > 0)
    {
      a <- list(
        x = selected_social()[town_index()],
        y = disease_av_per_thousand()[town_index()],
        text = town_name(),
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 4,
        ax = 20,
        ay = -40
      )
    }
    
    else{a <- NULL} #If not, set annotation to null
    
    #suppress warnings  
    options(warn = -1)
      
    #Produce scatter plot
    plot_ly(x=selected_social(), y=disease_av_per_thousand(),
              type = "scatter", mode = "markers", text = rownames(disease()),
              color = town_size_district$District, size = town_size_district$Population,
              colors = district_colors(),
              sizes = c(5,150), hoverinfo = "text") %>%
        layout(
          yaxis = list(title=paste("Mean Annual ", disease_name(), " Incidence per 100,000 (",
                                   year1_name(), "-", year2_name(), ")", sep = "")),
          xaxis = list(title=social_name()),
          title = paste(disease_name(),"Incidence and\n", social_name(), "\nby Suburban Cook County Municipality"),
          margin = list(b = 60, l = 50, r = 50, t = 120),
          annotations = a
        ) 

    
  })
  
############################# Boxplot of disease incidence by district ###############################################
  output$BoxPlotDisease = renderPlotly({
    
    #If a town is selected, define annotation
    if(town_index() > 0)
    {
      a <- list(
        x = town_size_district$District[town_index()],
        y = disease_av_per_thousand()[town_index()],
        text = town_name(),
        showarrow = TRUE,
        arrowhead = 4,
        ax = 20,
        ay = -40
      )
    }
    else{a <- NULL} #else set annotation to NULL
      
      #Plot boxplot
      plot_ly(y = disease_av_per_thousand(), type = "box", boxpoints = "all", 
              jitter = 0.3, pointpos = 0, color = town_size_district$District,
              colors = district_colors(),
              text = paste(names(CC_towns_vector[2:length(CC_towns_vector)]),"<br>", disease_name(), " Incidence: ",
                           round(disease_av_per_thousand()), " per 100,000", sep = ""), hoverinfo = "text")%>%
        layout(
          yaxis = list(title=paste("Mean Annual ", disease_name(), " Incidence per 100,000 (",
                                   year1_name(), "-", year2_name(), ")", sep = "")),
          xaxis = list(title="Suburban Cook County District"),
          title = paste(disease_name(),"Incidence by District"),
          showlegend = FALSE,
          annotations = a
        )

  })
  
  
############################# Boxplot of social indiacator by district ############################################### 
  output$BoxPlotSocial = renderPlotly({
    
    #If a town is selected, define annotation
    if(town_index() > 0)
    {
      a <- list(
        x = town_size_district$District[town_index()],
        y = selected_social()[town_index()],
        text = town_name(),
        showarrow = TRUE,
        arrowhead = 4,
        ax = 20,
        ay = -40
      )
    }
    else{a = NULL} #else set annotation to NULL
      
      #Plot boxplot
      plot_ly(y = selected_social(), type = "box", boxpoints = "all", 
              jitter = 0.3, pointpos = 0, color = town_size_district$District,
              colors = district_colors(),
              text = paste(names(CC_towns_vector[2:length(CC_towns_vector)]),"<br>", social_name(), ": ",
                           selected_social(), sep = ""), 
              hoverinfo = "text")%>%
        layout(
          yaxis = list(title=social_name()),
          xaxis = list(title="Suburban Cook County District"),
          title = paste(social_name(),"by District"),
          showlegend = FALSE,
          annotations = a
        )

    
  })

  
############################# Map showing the SCC districts ########################################## 
  output$districtMap <- renderLeaflet({
    
    colors = district_colors()
    
    m <- leaflet(cc, options = leafletOptions(minZoom = 9.4)) %>% addProviderTiles(providers$CartoDB.Positron) %>%  setView(lng = -87.86, lat = 41.81, zoom = 10) %>%
      addPolygons(
        fillColor = as.character(colors[cc$DIST]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 4, color = "white", dashArray = "1", fillOpacity = 0.7, bringToFront = TRUE),
        label = cc$CITY,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto")
      )%>%
      addLegend("topright", colors = colors, labels = c("North", "West", "Southwest", "South"),
                title = "Suburban Cook County District",
                opacity = 1)
    
    if(town_index() > 0){
      m <- m %>% 
        addPolylines(stroke=TRUE, weight = 4,color="black", opacity = 1, data=cc[town_index(),],group="highlighted_polygon")
    }
    m
    
      
  })
  
########### Map showing the selected disease incidence rates in all municipalities ################################## 
  output$DiseaseMap = renderLeaflet({
    pal <- colorNumeric("inferno", 0:max(disease_av_per_thousand()+1), reverse = T)

    m<- leaflet(cc, options = leafletOptions(minZoom = 9.4)) %>% addProviderTiles(providers$CartoDB.Positron) %>%  setView(lng = -87.86, lat = 41.81, zoom = 10) %>%
      addPolygons(
        fillColor = pal(disease_av_per_thousand()),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 4, color = "white", dashArray = "1", fillOpacity = 0.7, bringToFront = TRUE),
        #label = paste(cc$CITY, "<br/> Chlamydia Cases:", cl_2017),
        label = sprintf("<strong>%s</strong><br/>%.1f", cc$CITY, disease_av_per_thousand()) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto")
      ) %>%
      addLegend("topright", pal = pal, values = disease_av_per_thousand(),
                title = paste("Mean Annual ", disease_name(), "<br>Incidence per 100,000<br>(",
                              year1_name(), "-", year2_name(), ")", sep = ""),
                #labFormat = labelFormat(prefix = "$"),
                opacity = 1)
    
    if(town_index() > 0){
      m <- m %>% 
        addPolylines(stroke=TRUE, weight = 4,color="black", opacity = 1, data=cc[town_index(),],group="highlighted_polygon")
    }
    m
  })
  
############### Map showing the selected social indicator in all municipalities ################################## 
  output$SocialMap <- renderLeaflet({
    pal <- colorNumeric("inferno", 0:max(selected_social()+1), reverse = T)
    
    m <- leaflet(cc, options = leafletOptions(minZoom = 9.4)) %>% addProviderTiles(providers$CartoDB.Positron) %>%  setView(lng = -87.86, lat = 41.81, zoom = 10) %>%
      addPolygons(
        fillColor = pal(selected_social()),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 4, color = "white", dashArray = "1", fillOpacity = 0.7, bringToFront = TRUE),
        label = sprintf("<strong>%s</strong><br/>%g", cc$CITY, selected_social()) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto")
      ) %>%
      addLegend("topright", pal = pal, values = selected_social(),
                title = social_name(),
                opacity = 1)
    
    if(town_index() > 0){
      m <- m %>% 
        addPolylines(stroke=TRUE, weight = 4,color="black", opacity = 1, data=cc[town_index(),],group="highlighted_polygon")
    }
    m
    
  })
  
################################ Racial makeup pie chart ############################################# 
  output$townRacePie <- renderPlotly({
    #get percent for each race for selected town
    perc_white = social_data$White_perc[town_index()]
    perc_black = social_data$Black_perc[town_index()]
    perc_asian = social_data$Asian_perc[town_index()]
    perc_multi = social_data$multirace_perc[town_index()]
    perc_native = social_data$Native_perc[town_index()]
    
    #calculate percent that did not have one of races listed above
    perc_other = 100 - perc_white - perc_black - perc_asian - perc_multi - perc_native
    
    #Format for plot_ly
    perc = c(perc_white, perc_black, perc_asian, perc_native, perc_multi,  perc_other)
    perc = round(perc, 1)
    names = c("White", "Black", "Asian", "Native American", "2 or more", "Other")
    race = cbind.data.frame(names, perc)  %>% as.data.frame() %>%
      set_colnames(c("Race", "Perc"))  %>% 
      filter(Perc > 0) #filter out races with no representation so that they don't show up outside pie chart
    
    #set colors
    colors = c("White" = "#525174", "Asian" = "#5dd39e", "Black" = "#348aa7","2 or more" = "#bce784",
               "Native" = "#9ba0bc","Other" = "#939196")
    colors = (colors[as.character(race$Race)]) #needed because not every race is in every pie chart
    
    #Plot pie chart
    plot_ly(race, labels = ~Race, values = ~Perc, type = "pie", textinfo = 'label',
            insidetextfont = list(color = '#FFFFFF'), text = ~paste(Perc, "% ", Race, sep = ""), 
            hoverinfo = "text", 
            marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1)),
            showlegend = F, hole = 0.6, colors = colors)%>%
      layout(margin = list(  l = 50,
                             r = 50,
                             b = 100,
                             t = 150,
                             pad = 4),
             title = "Racial Makeup")
  })
  
  
################################ Ethnic makeup pie chart ############################################# 
  output$townEthnicityPie <- renderPlotly({
    #Format data
    perc_hisp = social_data$Hispanic_perc[town_index()]
    perc_no_hisp = 100-perc_hisp
    perc = c(perc_hisp, perc_no_hisp)
    perc = round(perc, 1)
    names = c("Hispanic or Latino", "Not Hispanic or Latino")
    eth = cbind.data.frame(names, perc)  %>% as.data.frame() %>%
      set_colnames(c("Eth", "Perc"))  %>% filter(Perc > 0) 
    
    colors = c("Hispanic or Latino" = "#bce784", "Not Hispanic or Latino" = "#9ba0bc")
    colors = (colors[as.character(eth$Eth)])
    
    #make plot
    plot_ly(eth, labels = ~Eth, values = ~Perc, type = "pie", textinfo = 'label',
            text = ~paste(Perc, "% ", Eth, sep = ""), 
            hoverinfo = "text", 
            marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1)),
            showlegend = F, hole = 0.6, colors = colors)%>%
      layout(margin = list(  l = 50,
                             r = 50,
                             b = 100,
                             t = 150,
                             pad = 4),
             title = "Ethnic Makeup")
    
  })
  
################################ Municipality text info ################################################ 
  #population in Cook
  output$townPop <- renderText({
    HTML(paste0("Population: \n", round(town_size_district$Population[town_index()]/1000,1), "k", sep = ""))
  })
  
  #median household income
  output$townIncome <- renderText({
    paste("Median Household Income:\n $", round(social_data$HouseholdIncome_Median[town_index()]/1000), "k", sep = "")
  })
  
  #SCC district
  output$townDistrict <- renderText({
    paste(town_size_district$District[town_index()], "District")
  })
  
  #town name
  output$town <- renderText({
    town_name()
  })
  

################################ Town disease percentiles graph ############################################# 
  output$townDiseasePercentile <- renderPlotly({
    #calculate percentile for each disease
    percs = sapply(select_disease_crosstabs, function(x){
      disease_sum = rowSums(x) #total number of cases
      disease_av = disease_sum/ncol(x) #average number of cases
      disease_av_per_thousand = (disease_av/town_size_district$Population)*100000 #average number of cases per 100,000
      percentile = ecdf(disease_av_per_thousand[disease_av_per_thousand >0]) #calculate percentile function
      return(percentile(disease_av_per_thousand[town_index()] - 0.0001)) #get percentile for selected town
            #-0.0001 so that towns with zero cases will be in the 0 percentile,
            #otherwise for diseases with very low incidence, 0 cases may be confusingly high percentile
    })
    
    percs = round(percs*100) %>% sort(decreasing = T) #round and sort largest to lowest
    
    #format data table for plot_ly
    table <- data.frame(x = names(percs),
                        y = as.numeric(percs))
    table$x <- factor(table$x, levels = c(as.character(table$x))) #so diseases aren't plotted alphabetically
    table$z = rep(1, nrow(table)) #>=75th percentile
    table$z[table$y < 75] <- 2 #26th-74th percentile
    table$z[table$y < 25] <- 3 #<=25th percentile
    colors = c("#bc4040", "#f9b754","#5fccc4")
    
    #make plot
    plot_ly(data = table, x = ~x, y = ~y, type = "bar",marker = list(color = colors[table$z]),
            text = ~paste(y, "% of SCC municipalities have\na lower ", x, 
                          "\nincidence rate than\nthat of ", town_name(), sep = ""), 
            hoverinfo = "text")%>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(range = c(0,100),title = "Percentile"),
             margin = list(b = 100, l = 200, r = 100, t = 100),
             title = paste(town_name(),"Communicable Disease Incidence Percentiles\nCompared to Other Suburban Cook County Municipalities"),
             showlegend = F)
  })
  
  #Make legend for percentiles graph since plotly won't cooperate
  output$legend <- renderPlot({
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend("center", legend =c('Top Quartile', 'Intermediate', 'Bottom Quartile', 'No Reported Cases'),
           col = c("#bc4040", "#f9b754","#5fccc4", "grey50"), pch = c(15,15,15,0), bty = "n", cex = 1.2,
           pt.cex = 2, horiz = T, border = "grey")
  })
  
################################ Town disease rates graph #############################################
  output$townDiseaseRates <- renderPlotly({
    
    #calculate rates for each disease in selected town
    rates = sapply(select_disease_crosstabs, function(x){
      disease_sum = rowSums(x)
      disease_av = disease_sum/ncol(x)
      disease_av_per_thousand = (disease_av/town_size_district$Population)*100000
      return(disease_av_per_thousand[town_index()])
    })
    
    rates = round(rates,1) %>% sort(decreasing = T) #round and sort largest to smallest
    
    #Format disease names (town name is appended, need to take out)
    names(rates) = gsub("E\\. coli", "E coli", names(rates))
    names(rates) = gsub("\\..*$", "", names(rates))
    names(rates) = gsub("E coli", "E\\. coli", names(rates))
    
    #Format data table for plot_ly
    table <- data.frame(x = names(rates),
                        y = as.numeric(rates))
    table$x <- factor(table$x, levels = c(as.character(table$x))) #so diseases aren't plotted alphabetically
    
    #Make plot
    plot_ly(data = table, x = ~x, y = ~y, type = "bar",marker = list(color = "#6E9DC9"),
            text = ~paste(x, "Incidence Rate:", y, "per 100,000"), hoverinfo = "text") %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Mean Annual Incidence Rate per 100,000"),
             margin = list(b = 100, l = 200, r = 100, t = 100),
             title = paste(town_name(),"Communicable Disease Rates"),
             showlegend = F)
             
  })
  
  output$socialSource <- renderText({
    paste("Source:", social_sources[as.numeric(input$social)])
  })
  
  output$socialIndicatorMapText <- renderText({
    paste("Hover over a municipality to see its name and its ", tolower(social_name()) , " value. If you are interested in 
                 finding out more about that municipality, select it in the 'Find Your Municipality' dropdown
               menu on the sidebar, and navigate to the 'municipality profile' tab.", sep = "")
  })
  
  

}



