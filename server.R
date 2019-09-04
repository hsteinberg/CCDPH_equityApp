server <- function(input, output, session) {
  #make data frame for just selected disease
  disease_index <- reactive({
    as.numeric(input$disease)
  })
  
  disease <- reactive({
    select_disease_crosstabs[disease_index()] %>%
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

  #Selected social data formatted
  selected_social_print <- reactive({
    if(as.numeric(input$social) == 1){
      paste0("$",selected_social() %>% round(0) %>% prettyNum(big.mark = ","))
    }
    else{
      paste0(selected_social() %>% round(1), "%")
    }
    
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

  district_colors = reactive({c("#b55681", "#f7986f","#95c47b","#29a7c6")})
  #district_colors = reactive({randomColor(4)})
  #district_colors = reactive({c("#7FDBD3","#529ABF","#32217A", "#321535")[1:4]})
  #district_colors = reactive({c("black", "grey50", "grey70", "grey90")[4:1]})

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
    p<- plot_ly(x=selected_social(), y=disease_av_per_thousand(),
              type = "scatter", mode = "markers", 
              #text = rownames(disease()),
              text = paste0("<b>",rownames(disease()),"</b>","<br>2010 Population: ",prettyNum(town_size_district$Population, big.mark = ","),  "<br>", social_name(), ": ", selected_social_print(),
                            "<br>", disease_name(), " Incidence: ", round(disease_av_per_thousand(),1), " per 100,000"),
              color = town_size_district$District, size = town_size_district$Population,
              colors = district_colors(), 
              sizes = c(5,150), hoverinfo = "text") %>%
        layout(
          yaxis = list(title=paste("Mean Annual ", disease_name(), "\nIncidence per 100,000 (",
                                   year1_name(), "-", year2_name(), ")",
                                   rep("&nbsp;", 20), #adding space under axis title
                                   rep("\n&nbsp;", 3),
                                   sep = ""),
                       titlefont = list(size = 16),
                       tickfont = list(size = 16)),
          xaxis = list(title=social_name(),
                       titlefont = list(size = 16),
                       tickfont = list(size = 16)),
          title = paste(disease_name(),"Incidence and\n", social_name(), "\nby Suburban Cook County Municipality"),
          margin = list(b = 120, l = 90, r = 0, t = 100),
          annotations = a, cex = 2
        ) 
    # p$elementId <- NULL
    # # Example usage of plotly image
    # plotly_IMAGE(p, 
    #              width = 1200, 
    #              height = 1200, 
    #              format = "png", 
    #              scale = 1, 
    #              out_file = "output.png")
    p
    
    
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
              text = paste("<b>",names(CC_towns_vector[2:length(CC_towns_vector)]),"</b><br>", disease_name(), " Incidence: ",
                           round(disease_av_per_thousand(),1), " per 100,000", sep = ""), hoverinfo = "text")%>%
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
              text = paste("<b>",names(CC_towns_vector[2:length(CC_towns_vector)]),"</b><br>", social_name(), ": ",
                           selected_social_print(), sep = ""), 
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
    
    colors = c(district_colors(),  "#4B5F96", "#CECECE", "#999999")
    
    m <- leaflet(options = leafletOptions(minZoom = 8)) %>% addProviderTiles(providers$CartoDB.Positron) %>%  setView(lng = -87.86, lat = 41.81, zoom = 10) %>%
      addPolygons(data = muni,
                  fillColor = colors[6],
                  stroke = F,
                  dashArray = "3",
                  fillOpacity = 0.5,
                  label = "Unincorporated Area",
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px", direction = "auto")
      ) %>%
      addPolygons(data = chicago,
                  fillColor = colors[7],
                  stroke = F,
                  dashArray = "3",
                  fillOpacity = 0.5,
                  label = "Chicago",
                  #label = sprintf("<strong>%s</strong><br/>%s", "Chicago", "Out of Jurisdiction") %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px", direction = "auto")
      ) %>%
      addPolygons(data = cc2,
        fillColor = as.character(colors[cc2$DIST]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 4, color = "white", dashArray = "1", fillOpacity = 0.7, bringToFront = FALSE),
        label = sprintf("<strong>%s</strong><br/>%s", cc2$CITY, c("North District", "West District", "Southwest District", "South District", "Out of Jurisdiction")[as.numeric(cc2$DIST)]) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto")
      )%>%
      addLegend("topright", colors = c(colors), 
                labels = c("North District","West District", "Southwest District", "South District", "Out of Jurisdiction", "Unincorporated", "Chicago"),
                title = "Suburban Cook County Districts",
                opacity = 0.7)%>%
      addScaleBar(position = "bottomleft")
    
    # if(town_index() > 0){
    #   m <- m %>%
    #     addPolylines(stroke=TRUE, weight = 4,color="black", opacity = 1, data=cc[town_index(),],group="highlighted_polygon")
    # }
    m
    
      
  })
  
  #Add black outline around selected town when selected for all 3 maps
  observeEvent({
    input$disease
    input$social
    input$town
    input$tabs == "Maps"
    input$tabs == "District Map"},{
    if(town_index() > 0){
      sapply(c("districtMap", "DiseaseMap", "SocialMap"), function(map){
        leafletProxy(map) %>%
          clearGroup("highlighted_polygon") %>%
          addPolylines(stroke=TRUE, weight = 4,color="black", opacity = 1, data=cc[town_index(),],group="highlighted_polygon")

      })
    }
  })
  observeEvent({
    input$town},{
    if(town_index() == 0){
      sapply(c("districtMap", "DiseaseMap", "SocialMap"), function(map){
        leafletProxy(map) %>%
          clearGroup("highlighted_polygon")
      })
    }
  })
  
  #trying to include download capability
  # output$downloadDistricts <- downloadHandler(
  #   filename = function() { 
  #     "SCCdistricts.png"
  #   },
  #   content = function(file){
  #     m <- leafletProxy("districtMap")
  #     mapshot(m, file = file, remove_controls = "zoomControl")
  #   }
  #   
  # )
  # 
########### Map showing the selected disease incidence rates in all municipalities ################################## 
  output$DiseaseMap = renderLeaflet({
    pal <- colorNumeric("inferno", 0:max(disease_av_per_thousand()+1), reverse = T)

    m<- leaflet(options = leafletOptions(minZoom = 8)) %>% addProviderTiles(providers$CartoDB.Positron) %>%  setView(lng = -87.86, lat = 41.81, zoom = 10) %>%
      addPolygons(data = muni,
                  fillColor = "#CECECE",
                  stroke = F,
                  dashArray = "3",
                  fillOpacity = 0.5
      ) %>%
      addPolygons(data = cc,
        fillColor = pal(disease_av_per_thousand()),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 4, color = "white", dashArray = "1", fillOpacity = 0.7, bringToFront = FALSE),
        #label = paste(cc$CITY, "<br/> Chlamydia Cases:", cl_2017),
        label = sprintf("<strong>%s</strong><br/>%.1f", cc$CITY, disease_av_per_thousand()) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto")
      ) %>%
      addLegend("topright", pal = pal, values = disease_av_per_thousand(),
                title = paste0(strwrap(paste0("Mean Annual ", disease_name(), " Incidence per 100,000 (",
                              year1_name(), "-", year2_name(), ")"), 20),collapse = "<br>"),
                #labFormat = labelFormat(prefix = "$"),
                opacity = 1)%>%
      addScaleBar(position = "bottomleft")

    # if(town_index() > 0){
    #   m <- m %>%
    #     addPolylines(stroke=TRUE, weight = 4,color="black", opacity = 1, data=cc[town_index(),],group="highlighted_polygon")
    # }
    m
  })
  
############### Map showing the selected social indicator in all municipalities ################################## 
  output$SocialMap <- renderLeaflet({
    pal <- colorNumeric("inferno", 0:max(selected_social()+1), reverse = T)
    
    m <- leaflet(options = leafletOptions(minZoom = 8)) %>% addProviderTiles(providers$CartoDB.Positron) %>%  setView(lng = -87.86, lat = 41.81, zoom = 10) %>%
      addPolygons(data = muni,
                  fillColor = "#CECECE",
                  stroke = F,
                  dashArray = "3",
                  fillOpacity = 0.5
      ) %>%
      addPolygons(data = cc,
        fillColor = pal(selected_social()),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 4, color = "white", dashArray = "1", fillOpacity = 0.7, bringToFront = FALSE),
        #label = sprintf("<strong>%s</strong><br/>%g", cc$CITY, selected_social()) %>% lapply(htmltools::HTML),
        label = paste("<strong>", cc$CITY, "</strong><br/>", selected_social_print())%>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto")
      ) %>%
      addLegend("topright", pal = pal, values = selected_social(),
                title = paste0(strwrap(social_name(),20),collapse = "<br>"),
                opacity = 1)%>%
      addScaleBar(position = "bottomleft")
    
    # if(town_index() > 0){
    #   m <- m %>%
    #     addPolylines(stroke=TRUE, weight = 4,color="black", opacity = 1, data=cc[town_index(),],group="highlighted_polygon")
    # }
    m
    
  })
  
  output$socialSource <- renderText({
    paste("Source:", social_sources[as.numeric(input$social)])
  })
  
  output$social <- renderText({
    paste(social_name(), "in Suburban Cook County")
  })
  
  # output$socialIndicatorMapText <- renderText({
  #   paste("Hover over a municipality to see its name and its ", tolower(social_name()) , " value. If you are interested in 
  #                finding out more about that municipality, select it in the 'Find Your Municipality' dropdown
  #              menu on the sidebar, and navigate to the 'municipality profile' tab.", sep = "")
  # })
  
################################ Racial makeup pie chart ############################################# 
  output$townRacePie <- renderPlotly({
    #get percent for each race for selected town
    perc_white = social_data$White_perc[town_index()]
    perc_black = social_data$Black_perc[town_index()]
    perc_asian = social_data$Asian_perc[town_index()]
    perc_multi = social_data$multirace_perc[town_index()]
    perc_native = social_data$Native_perc[town_index()]
    perc_hispanic = social_data$Hispanic_perc[town_index()]
    
    #calculate percent that did not have one of races listed above
    perc_other = 100 - perc_white - perc_black - perc_asian - perc_multi - perc_native - perc_hispanic
    
    #Format for plot_ly
    perc = c(perc_white, perc_black, perc_asian, perc_native, perc_multi, perc_hispanic, perc_other)
    perc = round(perc, 1)
    names = c("White", "Black", "Asian", "Native", "Two or More Races", "Hispanic/Latinx", "Other")
    race = cbind.data.frame(names, perc)  %>% as.data.frame() %>%
      set_colnames(c("Race", "Perc"))  %>% 
      filter(Perc > 0) #filter out races with no representation so that they don't show up outside pie chart
    
    #set colors
    colors = c("White" = "#525174", "Asian" = "#5dd39e", "Black" = "#348aa7","Two or More Races" = "#bce784",
               "Native" = "#035451","Hispanic/Latinx" = "#9ba0bc",  "Other" = "#939196")
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
             title = "Racial/Ethnic Makeup")
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
    paste0(town_name(), ", IL")
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
             title = paste("How Communicable Disease Rates in", town_name(),"\nCompare to Other Suburban Cook County Municipalities"),
             showlegend = F)
  })
  
  #Make legend for percentiles graph since plotly won't cooperate
  output$legend <- renderPlot({
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend("center", legend =c('Top 25%', 'Intermediate', 'Bottom 25%', 'No Reported Cases'),
           col = c("#bc4040", "#f9b754","#5fccc4", "grey50"), pch = c(15,15,15,0), bty = "n", cex = 1.2,
           pt.cex = 3, horiz = T, border = "grey")
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
  

  

  
################################  Links to CDC page for each disease #############################################
# Reactivity hangs when you use the same uiOutput twice in server, so had to duplicate
  
  output$diseaseLink <- renderUI({
    h5(a(href = as.character(disease_link[disease_name()]), paste("About", disease_name()), target = "_blank")) 
  })
  
  output$diseaseLink2 <- renderUI({
    h3(a(href = as.character(disease_link[disease_name()]), disease_name(), target = "_blank"),
       "in Suburban Cook County") 
  })

}




