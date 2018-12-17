ui <- fluidPage(
  #theme = shinytheme("cerulean"),
  #titlePanel("",windowTitle = "Communicable Disease Health Equity in Cook County"),
  
  #Bring in extra CSS to style application
  includeCSS("app.css"),
  
  #Add Google analytics global tracking code
  # tags$head(HTML('<script async src="https://www.googletagmanager.com/gtag/js?id=UA-107917571-1"></script>')),
  # tags$head(tags$script(HTML(" window.dataLayer = window.dataLayer || [];
  #                             function gtag(){dataLayer.push(arguments);}
  #                             gtag('js', new Date());
  #                             gtag('config', 'UA-107917571-1')"))),
  
  #Building the header 
  fluidRow(class = "header",
           column(class = "headimg", 2, align = "center", img(class = "imggen", src="http://cookcountypublichealth.org/files/images/CCDPH_logo-full.jpg", alt="CCDPH Logo")), 
           column(class = "headtitle", 10, HTML('
                                                <h1 style="font-weight: 700; font-size: 35px">Communicable Disease & Health Equity Data Visualization <span id="beta">Beta</span></h1>
                                                '))
  ),
  
  
  br(),
  #side bar that holds all the user input widgets
  sidebarPanel(width = 3,
                  #select dropdown for disease
                  selectInput(inputId = "disease", "Disease", 
                              choices = disease_choices,
                              selected = 1),
               
               #about disease
               #h4(a(href = textOutput("diseaseLink"), textOutput("diseaseLinkText"), target="_blank")),
               uiOutput("diseaseLink"),
               #print(textOutput("diseaseLink")),
               br(),
           
                  #select dropdown for social
                  selectInput(inputId = "social", "Social Indicator", 
                              choices = social_choices,
                              selected = 1),
           
           
           #drop down menu for town to select
           selectInput(inputId = "town", "Find Your Municipality*",
                                         choices = CC_towns_vector, selected = 0),
           h6("*Municipalities with (pt.) after their names are only partially contained within Cook County.
              Their population sizes represent the portion of the municipality in Cook County. 
              Disease rates for these municipalities may be over or under estimated due to reporting discrepencies
              and small population sizes.")
           ), #end side panel
           

  
  mainPanel(tabsetPanel(type = "tabs",
        tabPanel("About", fluidRow(column(width = 10,
                                 br(),
                                 #h4(strong("Cook County Department of Public Health Communicable Disease & Health Equity"), style = "padding-bottom: 10px; padding-top: 5px"),
                                 strong("What is health equity?"),
                                 p('"Health equity means that everyone has a fair and just 
                                    opportunity to be healthier. This requires removing obstacles 
                                    to health such as poverty, discrimination, and their consequences, 
                                    including powerlessness and lack of access to good jobs with fair pay, 
                                    quality education and housing, safe environments, and health care."',
                                    em("\t-The Robert Wood Johnson Foundation")),
                                 br(),
                                 strong("Health equity in Suburban Cook County"),
                                 p('Suburban Cook County has some of the most affluent, as well as
                                 some of the most disadvantaged, municipalities in the country. We
                                 see these disparities manifested in various health outcomes in
                                 our cities, villages, and towns. Cook County Department of Public Health has
                                 made addressing health equity one of its top priorities in its
                                 Community Health Assessment and Improvement Plan,',
                                 a(href = "http://www.cookcountypublichealth.org/about/weplan", "WePlan2020.", target="_blank"),
                                 'In this app, we allow for the visualization of selected infectious
                                 disease rates in Suburban Cook County and their
                                 correlations with various social indicators related to income,
                                 education, insurance status, place of birth, race, and ethnicity.'),
                                 br(),
                                 strong("How to use this app"),
                                 p("Select a disease and a social indicator of interest from the
                                 sidebar control panel. If you are interested in highlighting
                                 results from a specific municipality, select one from the 'Find
                                 Your Municipality' dropdown menu. Navigate to the",
                                 em('Scatter Plot'),
                                 "and",
                                 em('Box Plot'),
                                 "tabs to visualize the correlation between your
                                 disease and social indicator of interest. Much of the data is
                                 organized by Suburban Cook County Districts, which can be seen
                                 mapped out in the",
                                 em('District Map'),
                                 "tab. To map disease
                                 incidence rates and social indicators, navigate to the",
                                 em('Disease Map'),
                                 "and",
                                 em('Social Indicator Map'), 
                                 "tabs, respectively. To learn more
                                 about the demographics and disease burdens of a specific municipality,
                                 select it in the sidebar control panel, and click on the",
                                 em('Municipality Profile'),
                                 "tab. All graphs and maps in this app are
                                 interactive. Hover over a data point to see more information, and
                                 zoom in and out to areas of particular interest."),
                                 br(),
                                 p("This application is currently in beta testing. Please click ", 
                                 a(href = "mailto:hannah.steinberg@cookcountyhhs.org?Subject=Shiny%20Equity%20App", "here"), 
                                 " to send comments, feedback, or technical questions. Source code for this application can be found ",
                                 a(href = "https://github.com/hsteinberg/CCDPH_equityApp", "here", target="_blank"), 
                                 align = "justify", style = "padding-bottom: 10px;padding-top: 10px;text-align: center; background:  #f3f1f3;border: 3px solid #dbd6db;font-size: 12px; padding-left: 10px; padding-right: 10px;"),
                                 br()
                                 )
                 #add an image to the about panel
                 #,
                 #column(width = 2, br(), br(),
                 #                img(src = "jonah-pettrich-440528-unsplash.jpg", width = "100%"))
                 )),
    tabPanel("Scatter Plot", 
             plotlyOutput("ScatterPlot", height = "720px", width = "100%"),
             br(),
             br(),
             fluidRow(column(width = 11, 
                             strong("Figure Notes:"),
               p("The size of points in the scatter plot are proportional to the municipality population size 
                (as reported in the 2010 United States Census),
                and the color of the points correspond to the Sububan Cook County District of the municipality
                (pink is North, green is West, blue is Southwest, and orange is South)."),
               p("Hover over a point to see which municipality it represents. If you are interested in 
                 finding out more about that municipality, select it in the 'Find Your Municipality' dropdown
                 menu on the sidebar, and navigate to the",
                 em('Municipality Profile'), "tab."),
               p("To see trends in only select district(s), deselect the others in the figure legend."),
               p("To zoom into a particular part of the plot, hover over a corner of the desired area and hold
                  and drag your cursor to highlight the desired area. To return to see the whole plot, double-click
                  anywhere within the plot."),
               p("To see the correlations of different diseases and social indicators, change the selections
                in the dropdown menus on the sidebar."),
               em("It is important to note that only correlations are shown here and plots do not necessarily 
                represent causal relationships."),
               br(),
               br(),
               strong("How to Interpret Scatter Plots:"),
               br(),
               p("These scatter plots help show if there is a correlation between the selected social indicator 
                 and disease rate in Suburban Cook County. If it looks like the points form an uphill line from 
                 left to right, then there is a positive correlation between the two variables (for example: as 
                 unemployment rate increases, so do chlamydia rates). If the points form more of a downhill slope 
                 from left to right, you can say the variables have are negatively correlated (for example: as median 
                 household income increases, chlamydia rates decrease). Alternatively, if it's hard to infer any 
                 pattern from the points, there is likely no correlation between the two variables (for example: 
                 Legionnaires Disease does not appear to be correlated to unemployment rate in Suburban Cook County). 
                 It is also possible that some correlations are only present in certain geographic districts (for 
                 example: pertussis is positively correlated to median household income in the North District, but 
                 this pattern is not clear in the other districts). Please note that only correlations are 
                 shown here and they do not necessarily represent causal relationships 
                 (for example: we cannot say from this data alone that high unemployment rates cause increased chlamydia rates)."),
             br(),
             br()
             ))),

    tabPanel("Box Plots",
             br(),
             plotlyOutput("BoxPlotDisease", height = "600px", width = "100%"),
             br(),
             br(),
             br(),
             br(),
             plotlyOutput("BoxPlotSocial", height = "600px", width = "100%"),
             br(),
             br(),
             fluidRow(column(width = 11, 
                             strong("Figure Notes:"),
                             p("These box plots show the distribution  of the selected disease rate and social indicator within and between Suburban Cook County districts.
                               Each point represents a municipality, and the color of the points and boxes correspond to the municipality's District 
                               (pink is North, green is West, blue is Southwest, and orange is South)."),
                             p("Hover over a point to see which municipality it represents. If you are interested in 
                               finding out more about that municipality, select it in the 'Find Your Municipality' dropdown
                               menu on the sidebar, and navigate to the",
                               em('Municipality Profile'), "tab."),
                             p("To zoom into a particular part of the plot, hover over a corner of the desired area and hold
                               and drag your cursor to highlight the desired area. To return to see the whole plot, double-click
                               anywhere within the plot."),
                             p("To see plots of different diseases and social indicators, change the selections
                               in the dropdown menus on the sidebar."),
                             em("It is important to note that only correlations are shown here and plots do not necessarily 
                                represent causal relationships."))),
                             br(),
                             br(),
                             strong("How to Interpret Box Plots:"),
                             br(),
                             p("These boxplots show the distribution of disease rates and social indicators within and between 
                               Suburban Cook County districts. Each dot represents a municipality, and its vertical position 
                               corresponds to the variable being plotted. The municipalities are grouped horizontally by district. 
                               For each district, there is a box that represents the middle half of values for that district. 
                               The line inside the box represents the median value for that district. If the four district median 
                               lines generally line up, it can be interpreted that there is not a significant difference between 
                               districts for that particular variable (for example:", em("Haemophilus influenza"), "Invasive Disease Incidence does 
                               not differ much by district). However, if the median lines and boxes are at very different positions 
                               vertically, it can be inferred that there is a difference between districts (for example: there 
                               is a higher percentage of Asian individuals in the North District than in the other three districts). 
                               Municipalities that are above or below the lines (aka 'whiskers') projecting from the boxes are 
                               considered outliers; that is, they have a higher or lower value than expected for the district in 
                               which they reside (for example: Western Springs has a relatively high median household income for 
                               the West District). You can hover over any point to see which municipality it represents, and 
                               its value for the factor being plotted."),
                             br()),

    tabPanel("District Map",
             h3("Suburban Cook County Districts"),
             leafletOutput("districtMap", height = "700px", width = "100%"),
             br(),
             p("Suburban Cook County can be divided into four geographic districts.
                The municiaplities within each district have some similarities in terms of
               socioeconomic characteristics and disease incidence rates, but there is also
               a great deal of variation within districts for some indicators. 
               Select a disease or social indicator in the dropdown menus on the sidebar and
               navigate to the 'disease map' or 'social indicator map' to explore these variables."),
             p("Hover over a municipality to see its name. If you are interested in 
                finding out more about that municipality, select it in the 'Find Your Municipality' dropdown
                menu on the sidebar, and navigate to the",
               em('Municipality Profile'), "tab."),
             br(),
             br()),

    tabPanel("Disease Map", 
             uiOutput("diseaseLink2"),
             leafletOutput("DiseaseMap", height = "700px", width = "100%"),
             fluidRow(column(width=12, align = "right", tags$figcaption("Source: Cook County Department of Public Health"))),
             br(),
             p("Hover over a municipality to see its name and disease incidence rate. If you are interested in 
                 finding out more about that municipality, select it in the 'Find Your Municipality' dropdown
               menu on the sidebar, and navigate to the",
               em('Municipality Profile'), "tab."),
             br()),

    tabPanel("Social Indicator Map", 
             h3(textOutput("social")),
             leafletOutput("SocialMap", height = "700px", width = "100%"),
             fluidRow(column(width=12, align = "right",textOutput("socialSource"))),
             br(),
             p("Hover over a municipality to see its name and social indicator value. If you are interested in 
                 finding out more about that municipality, select it in the 'Find Your Municipality' dropdown
               menu on the sidebar, and navigate to the",
               em('Municipality Profile'), "tab."),
             br()),
    
    tabPanel("Municipality Profile", conditionalPanel("input.town != 0",
               h3(textOutput("town")),
               fluidRow(
                 column(width = 4, align = "center", wellPanel(h4(textOutput("townPop")))),
                 column(width = 4, align = "center", wellPanel(h4(textOutput("townIncome")))),
                 column(width = 4, align = "center", wellPanel(h4(textOutput("townDistrict"))))),
               fluidRow(
                column(width = 6, wellPanel(plotlyOutput("townRacePie"))),
                column(width = 6, wellPanel(plotlyOutput("townEthnicityPie")))),
               wellPanel(fluidRow(
                 column(width = 12, plotlyOutput("townDiseasePercentile", height = "600px"))
               ),
               fluidRow(
                 column(width = 12, plotOutput("legend", height = "200px"))
               )),
               fluidRow(
                 column(width = 12, wellPanel(plotlyOutput("townDiseaseRates", height = "700px")))),
               br(),
               strong("Sources:"),
               p("Population data are estimates from the 2010 United States Census. Populations for municipalities
                 only partially within Cook County represent only individuals who live in the 
                 Cook County section of that municipality."),
               p("Median household income, and racial and ethnic makup data are estimates from the 2012-2016
                 American Community Survey 5-Year data."),
               p("Disease incidence data is calculated from cases reported to the Cook County Department of Public Health.")
             ),
             conditionalPanel("input.town == 0",
                              br(),
                              fluidRow(
                                column(width=12, align = "center", 
                                       h4("Select municipality from dropdown menu on sidebar to see its profile"),
                                       em("When a municipality is selected, it will also be highlighted in each plot")))
             
             ))
    



),#end tabset panel
titlePanel("",windowTitle = "Cook County CD Equity Data")

)#end main panel
  
)#end fluid page
