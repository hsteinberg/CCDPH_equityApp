ui <- fluidPage(
  #theme = shinytheme("cerulean"),
  #titlePanel("Communicable Disease Health Equity in Cook County"),
  
  #Bring in extra CSS to style application
  includeCSS("fluapp.css"),
  
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
                                                <h1 style="font-weight: 700; font-size: 40px">Communicable Diseases Health Equity in Cook County, IL
                                                '))
  ),
  
  
  br(),
  #side bar that holds all the user input widgets
  sidebarPanel(width = 3,
                  #select dropdown for disease
                  selectInput(inputId = "disease", "Disease", 
                              choices = disease_choices,
                              selected = 1),
           
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
           

  
  mainPanel(tabsetPanel(type = "pills",
        tabPanel("about", fluidRow(column(width = 10,
                                 br(),
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
                                 Community Health Assessment and Improvement Plan (WePlan2020). In
                                 this app, we allow for the visualization of selected infectious
                                 disease rates in Suburban Cook County as well as their
                                 correlations with various social indicators related to income,
                                 education, insurance status, place of birth, race, and ethnicity.'),
                                 br(),
                                 strong("How to use this app"),
                                 p("Select a disease and a social indicator of interest from the
                                 sidebar control panel. If you are interested in highlighting
                                 results from a specific municipality, select one from the 'Find
                                 Your Municipality' dropdown menu. Navigate to the 'scatter plot'
                                 and 'boxplot' tabs to visualize the correlation between your
                                 disease and social indicator of interest. Much of the data is
                                 organized by Suburban Cook County Districts, which can be seen
                                 mapped out in the 'district map' tab. To see maps of disease
                                 incidence rates and social indicators, navigate to the 'disease
                                 map' and 'social indicator map' tabs, respectively. To learn more
                                 about the makeup and disease burdens of a specific municipality,
                                 select it in the sidebar control panel, and click on the
                                 'municipality profile' tab. All graphs and maps in this app are
                                 interactive. Hover over a data point to see more information, and
                                 zoom in and out to areas of particular interest."),
                                 br(),
                                 br()
                                 )
                 #add an image to the about panel
                 #,
                 #column(width = 2, br(), br(),
                 #                img(src = "jonah-pettrich-440528-unsplash.jpg", width = "100%"))
                 )),
    tabPanel("scatter plot", 
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
                 menu on the sidebar, and navigate to the 'municipality profile' tab."),
               p("To see trends in only select district(s), deselect the others in the figure legend."),
               p("To zoom into a particular part of the plot, hover over a corner of the desired area and hold
                  and drag your cursor to highlight the desired area. To return to see the whole plot, double-click
                  anywhere within the plot."),
               p("To see the correlations of different diseases and social indicators, change the selections
                in the dropdown menus on the sidebar."),
               em("It is important to note that only correlations are shown here and do not necessarily 
                represent causal relationships."),
             br(),
             br()
             ))),

    tabPanel("boxplots",
             br(),
             plotlyOutput("BoxPlotDisease", height = "600px", width = "100%"),
             br(),
             br(),
             br(),
             br(),
             plotlyOutput("BoxPlotSocial", height = "600px", width = "100%"),
             br()),

    tabPanel("distirct map",
             br(),
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
                menu on the sidebar, and navigate to the 'municipality profile' tab."),
             br(),
             br()),

    tabPanel("disease map", 
             br(),
             leafletOutput("DiseaseMap", height = "700px", width = "100%"),
             fluidRow(column(width=12, align = "right", tags$figcaption("Source: Cook County Department of Public Health"))),
             br(),
             p("Hover over a municipality to see its name and incidence rate. If you are interested in 
                 finding out more about that municipality, select it in the 'Find Your Municipality' dropdown
               menu on the sidebar, and navigate to the 'municipality profile' tab."),
             br()),

    tabPanel("social indicator map", 
             br(),
             leafletOutput("SocialMap", height = "700px", width = "100%"),
             fluidRow(column(width=12, align = "right",textOutput("socialSource"))),
             br(),
             textOutput("socialIndicatorMapText"),
             br()),
    
    tabPanel("municipality profile", conditionalPanel("input.town != 0",
               br(),
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
               p("Disease incidence data is calculated from reported cases to the Cook County Department of Public Health.")
             ),
             conditionalPanel("input.town == 0",
                              br(),
                              fluidRow(
                                column(width=12, align = "center", 
                                       h4("Select municipality from dropdown menu on sidebar to see its profile"),
                                       em("When a municipality is selected, it will also be highlighted in each plot")))
             
             ))
    



)#end tabset panel
)#end main panel
  
)#end fluid page
