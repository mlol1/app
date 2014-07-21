## UI for app
shinyUI(fluidPage(
  titlePanel("Visualisations for identifiable non-residential buildings in OSM V GEODIRECTORY"),
  
  #input
  sidebarPanel
  (
    helpText("Choose from the selection of Nace Codes to see the different completeness rates"),
    selectInput("var", 
                label = "Choose a variable to display",
                choices = c("Percent Complete: ALL NACE CODES", "Percent Complete: G WHOLESALE AND RETAIL TRADE;REPAIR OF MOTOR VEHICLES AND MOTORCYCLES",
                            "Percent Complete: S OTHER SERVICE ACTIVITIES" ,
                            "Percent Complete: O PUBLIC ADMINISTRATION AND DEFENCE;COMPULSORY SOCIAL SECURITY" ,
                            "Percent Complete: I ACCOMMODATION AND FOOD SERVICE ACTIVITIES",
                            "Percent Complete: Q HUMAN HEALTH AND SOCIAL WORK ACTIVITIES" ,
                            "Percent Complete: P Education"
                ),
                selected = "Percent Complete: ALL NACE CODES"),
    
    sliderInput("range", 
                label = "Range of interest:",
                min = 0, max = 100, value = c(0, 100),animate=TRUE),
    fluidRow(
      column(4, 
             selectInput("char1", "",
                         choices =levels(counties_rows$variable)))),
    
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
    sliderInput("binsx",
                "Number of bins:",
                min = 0,
                max = 50,
                value = 30),
    sliderInput("binsy",
                "Number of bins:",
                min = 100,
                max = 200,
                value = 100)
  ),	

  # output				
  mainPanel(
    
    tabsetPanel(
      tabPanel("Choropleth",h4("Choropleth Map of Percentage Completeness"),plotOutput("spplot"),  
               htmlOutput("linech"),
               htmlOutput("barch"),
               htmlOutput("scatterch"),
               htmlOutput("bubblech"),
               htmlOutput("histch"),
               plotOutput("distPlot")),
               tabPanel("Markers",h4("Marker representation of Percentage Completeness"),htmlOutput("googleVismerged"))
      )
    )

  ))
