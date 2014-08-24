counties_rows<- read.csv("http://cs1.ucc.ie/~mlol1/CS6500/data/Percentage_completeness_rows.csv")
counties_rows$X<-NULL
counties_rows<-cbind(as.character(counties_rows$County), as.numeric(counties_rows$percent*100), as.character(counties_rows$variable))
counties_rows<-data.frame(counties_rows)
names(counties_rows)<-c("County","percent","variable")

## UI for app
shinyUI(fluidPage(
 titlePanel("Visualisations for identifiable non-residential buildings in OSM V GEODIRECTORY"),
  
  #input
sidebarPanel(
    helpText(br("Choose from the selection of Nace Codes to see the different completeness rates"),
             br("ALL NACE CODES"),   
             br("G: WHOLESALE AND RETAIL TRADE;REPAIR OF MOTOR VEHICLES AND MOTORCYCLES"),
             br("S: OTHER SERVICE ACTIVITIES"),
             br("O: PUBLIC ADMINISTRATION AND DEFENCE;COMPULSORY SOCIAL SECURITY"),
             br("I: ACCOMMODATION AND FOOD SERVICE ACTIVITIES"),
             br("Q: HUMAN HEALTH AND SOCIAL WORK ACTIVITIES"),
             br("P: Education"))

  )	,

  # output				

    ## <embed percentages heat map here 
  mainPanel( 

    tabsetPanel(
      tabPanel("Choropleth",h4("Choropleth Map of Percentage Completeness"),tags$embed(src="http://www.openheatmap.com/view.html?map=UnideadSaradaVestibulum", 
                                                                            tags$style(".alignRight { align: left; }", media = "all", type = "text/css"), 
                                                                            height = 850, width = 1000 ),),
              # plotOutput("spplot"),
              tabPanel(
              helpText("Display line charts, box plot, scatter plot, bubble charts, histogram, density plot for the compared variables "),    
              selectInput("OSM",label = "Choose an OpenStreetMap (OSM) variable to compare",
                          choices= c(
                            "OSM_ALL",                  
                            "OSM_S",
                            "OSM_G",                  
                            "OSM_O",                  
                            "OSM_P",                  
                            "OSM_I",                  
                            "OSM_Q"
                          ),selected = "OSM_ALL"),
              
              selectInput("GEODIR",label = "Choose a Geodirectory (GEODIR) variable to compare",
                          choices= c(
                            "GEODIR_ALL",
                            "GEODIR_S",
                            "GEODIR_G",
                            "GEODIR_O",
                            "GEDOIR_P",                  
                            "GEODIR_I",                  
                            "GEODIR_Q"),selected = "GEODIR_ALL"),
               htmlOutput("linech"),
               htmlOutput("barch"),
               htmlOutput("scatterch"),
               htmlOutput("histch"),
              #plotOutput("density"),
              sliderInput("binsx",
                          "Number of bins for OSM:",
                          min = 0,
                          max = 50,
                          value = 30),
              sliderInput("binsy",
                          "Number of bins for GEODIR:",
                          min = 100,
                          max = 200,
                          value = 100),
               plotOutput("distPlot"), height = "900px", width = "1000px"),
               tabPanel("Markers",h4("Marker representation of Percentage Completeness"),  
                                 selectInput("char1", "",
                                             choices =levels(counties_rows$variable)),
                htmlOutput("googleVismerged"),

                 htmlOutput("bubblech"), width = "1000px", height = "900px"),
      tabPanel("OSM Users",h4("check building locations, names and types"), tags$embed(src="http://www.openheatmap.com/view.html?map=CavitaryPurpurigenousNeuchtel", 
                                                                                       tags$style(".alignRight { align: left; }", media = "all", type = "text/css"), 
                                                                                       height = 750, width = 1000 ),
               helpText("Log on to Openstreetmap to make updates"),
               a(href="http://www.openstreetmap.org/search?query=Republic%20of%20Ireland#map=7/53.138/-7.530", "Click Here!")
               
              
      )
    )

  )))
