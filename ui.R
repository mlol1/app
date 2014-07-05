shinyUI(fluidPage(
  titlePanel("Percentage Completeness Visualisations for identifiable non-residential buildings in OSM V GEODIRECTORY"),
  
  sidebarLayout(
    sidebarPanel(
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
                  min = 0, max = 100, value = c(0, 100))
    ),
    

  
    mainPanel(
      plotOutput("spplot")
    )
    
    )
    
  )
)
