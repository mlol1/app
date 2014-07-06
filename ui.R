## UI for app
shinyUI(fluidPage(
  titlePanel("Visualisations for identifiable non-residential buildings in OSM V GEODIRECTORY"),
  
#shinyUI(pageWithSidebar(
  # title
  headerPanel("Select Options"),
  
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
                min = 0, max = 100, value = c(0, 100)),
    
    helpText("Choose between box plot, histogram, density plot or bar plot "),
    selectInput("dataset","Data:", 
                list(Counties_numbers = "Counties_numbers")),
    uiOutput("variable"), 	# depends on dataset ( set by output$variable in  server.R)
    uiOutput("group"),  		# depends on dataset	( set by output$group in  server.R)
    selectInput("plot.type","Plot Type:", 
                list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")
    ),
    checkboxInput("show.points", "show points", TRUE)
  ),	
  
  # output				
  mainPanel(
    plotOutput("spplot"),
    h3(textOutput("caption")),
    #h3(htmlOutput("caption")),
    uiOutput("plot") # depends on input 
  )
))