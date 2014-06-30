
library(sp)
library(RColorBrewer)

counties <- read.csv("http://cs1.ucc.ie/~mlol1/CS6500/data/Percentage_Completeness.csv")
counties<-as.data.frame(counties)

source("helpers.R")
shinyServer(
  function(input, output) {
    output$spplot <- renderPlot({

    args <- switch(input$var, 
 "Percent Complete: ALL NACE CODES" = list((counties$NACECODE_ALL_PERCENT)*100 ,"darkgreen","% Complete: ALL NACE CODES"),
 "Percent Complete: G WHOLESALE AND RETAIL TRADE;REPAIR OF MOTOR VEHICLES AND MOTORCYCLES" = list((counties$NACECODE_G_PERCENT)*100,"black"," % Complete: G"),
 "Percent Complete: S OTHER SERVICE ACTIVITIES" = list((counties$NACECODE_S_PERCENT)*100,"darkorange","Complete: S"),
 "Percent Complete: O PUBLIC ADMINISTRATION AND DEFENCE;COMPULSORY SOCIAL SECURITY" = list((counties$NACECODE_O_PERCENT)*100,"darkblue","% Complete: O"),
 "Percent Complete: I ACCOMMODATION AND FOOD SERVICE ACTIVITIES" = list((counties$NACECODE_I_PERCENT)*100,"red","% Complete: I"),
 "Percent Complete: Q HUMAN HEALTH AND SOCIAL WORK ACTIVITIES" = list((counties$NACECODE_Q_PERCENT)*100,"brown","% Complete: Q"),
 "Percent Complete: P Education" = list((counties$NACECODE_P_PERCENT)*100,"violet","% Complete: P"))
      
args$min <- input$range[1]
args$max <- input$range[2]

do.call(percent_spplot, args)
    })

output$map <- renderPlot({

  map('worldHires','Ireland')

  }
)
  }
)
