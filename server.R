#based on code from https://gist.github.com/dgrapov/5792778 ; shiny tutorials http://shiny.rstudio.com/tutorial/ ;
#http://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.htm and
#http://spark.rstudio.com/heres/EAHU/
library(shiny)
library(sp)
library(RColorBrewer)
library(googleVis)
library(reshape)
library(reldist)


counties <- read.csv2("http://cs1.ucc.ie/~mlol1/CS6500/data/Percentage_Completeness.csv")
counties$X<-NULL
counties<-as.data.frame(counties)
Counties_numbers<-read.csv2("http://cs1.ucc.ie/~mlol1/CS6500/Counties_numbers.csv")
Counties_numbers$X<-NULL 
raw.data <- Counties_numbers
tab <- raw.data
counties_rows<- read.csv2("http://cs1.ucc.ie/~mlol1/CS6500/data/Percentage_completeness_rows.csv")
counties_rows$X<-NULL
counties_rows<-as.data.frame(counties_rows)
counties_rows_numbers<-read.csv2("http://cs1.ucc.ie/~mlol1/CS6500/Counties_numbers.csv")
counties_rows_numbers$X<-NULL
source("helpers.R")
# shiny server side code for each call
shinyServer(function(input, output, session){
  output$spplot <- renderPlot({
    
    args <- switch(input$var, 
                   "Percent Complete: ALL NACE CODES" = list((counties$ALL_PERCENT)*100 ,"darkgreen","% Complete: ALL NACE CODES"),
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
  
  #update variable and group based on dataset
  output$variable <- renderUI({ 
    obj<-switch(input$dataset,
                "Counties_numbers" = Counties_numbers)   
    var.opts<-namel(colnames(obj))
    selectInput("variable","Variable:", var.opts) # uddate UI     		 
  }) 
  
  
  output$county <- renderUI({ 
    obj<-switch(input$counties,
                "county" = county)   
    var.opts<-namel(colnames(obj))
    selectInput("county","county:", var.opts) # uddate UI     		 
  }) 
  
  output$groups <- renderUI({ 
    obj<-switch(input$counties,
                "Percentage_Completeness" = Percentage_Completeness)   
    var.opts<-namel(colnames(obj))
    selectInput("groups","Groups:", var.opts) # uddate UI 				 
  }) 
  
  
  
  output$googleVismerged = renderGvis({
    df=counties_rows[counties_rows$variable==input$char1,]
    bar=gvisBarChart(df[order(df$percent),],
                     "County","percent",
                     options=list(height=800,fontSize=12,legend="none"))
    geochart=gvisGeoChart(counties_rows[counties_rows$variable==input$char1,],
                          locationvar="County", colorvar="percent",
                          options=list(region="IE", dataMode="regions", 
                                       resolution="provinces",
                                       colorAxis="{colors:['#4daf4a','blue']}"
                          ))
    
    gvisMerge(bar,geochart,horizontal=TRUE)
  })
  
  
  
  
  
  output$group <- renderUI({ 
    obj<-switch(input$dataset,
                "Counties_numbers" = Counties_numbers)	 
    var.opts<-namel(colnames(obj))
    selectInput("group","Groups:", var.opts) # uddate UI 				 
  }) 
  shinyServer(function (input, output) {
    
    output$linech <- renderGvis({
      OSM <- switch(input$OSM,    
                    "OSM_ALL"   = tab$OSM_ALL,
                    "OSM_S"      = tab$OSM_S,
                    "OSM_G"      = tab$OSM_G,
                    "OSM_O"      = tab$OSM_O,
                    "OSM_P"     =  tab$OSM_P,
                    "OSM_I"      = tab$OSM_I,
                    "OSM_Q"      = tab$OSM_Q)
      
      GEODIR <- switch(input$GEODIR,    
                       "GEODIR_ALL" = tab$GEODIR_ALL,
                       "GEODIR_S"   = tab$GEODIR_S,
                       "GEODIR_G"   = tab$GEODIR_G,
                       "GEODIR_O"   = tab$GEODIR_O,
                       "GEDOIR_P"   = tab$GEDOIR_P,
                       "GEODIR_I"   = tab$GEODIR_I,
                       "GEODIR_Q" =  tab$GEODIR_Q)  
      
      
      
      
      #### Plots ###
      
      df=data.frame(County=tab$County, 
                    OSM=OSM, 
                    GEODIR=GEODIR)
      
      
      Line <- gvisLineChart(df,"County", c("OSM","GEODIR"),
                            options=list(
                              title="OSM V GEODIR Comparison by County (Numbers)",
                              titlePosition='out',
                              hAxis="{slantedText:'true',slantedTextAngle:90}",
                              titleTextStyle="{color:'black',fontName:'Courier'}",
                              legend="{color:'black',fontName:'Courier'}",
                              fontSize="10"
                            ))
      
      
      ed.output <- (Line)
      
      return(ed.output)
      
    })
    output$barch <- renderGvis({
      OSM <- switch(input$OSM,    
                    "OSM_ALL"   = tab$OSM_ALL,
                    "OSM_S"      = tab$OSM_S,
                    "OSM_G"      = tab$OSM_G,
                    "OSM_O"      = tab$OSM_O,
                    "OSM_P"     =  tab$OSM_P,
                    "OSM_I"      = tab$OSM_I,
                    "OSM_Q"      = tab$OSM_Q)
      
      GEODIR <- switch(input$GEODIR,    
                       "GEODIR_ALL" = tab$GEODIR_ALL,
                       "GEODIR_S"   = tab$GEODIR_S,
                       "GEODIR_G"   = tab$GEODIR_G,
                       "GEODIR_O"   = tab$GEODIR_O,
                       "GEDOIR_P"   = tab$GEDOIR_P,
                       "GEODIR_I"   = tab$GEODIR_I,
                       "GEODIR_Q" =  tab$GEODIR_Q)  
      
      
      
      
      #### Plots ###
      
      df=data.frame(County=tab$County, 
                    OSM=OSM, 
                    GEODIR=GEODIR)
      
      
      
      Column <- gvisColumnChart(df,"County", c("OSM","GEODIR"))
      
      ed1.output <- (Column)
      
      
      return(ed1.output)
      
    })
    
    output$scatterch <- renderGvis({
      OSM <- switch(input$OSM,    
                    "OSM_ALL"   = tab$OSM_ALL,
                    "OSM_S"      = tab$OSM_S,
                    "OSM_G"      = tab$OSM_G,
                    "OSM_O"      = tab$OSM_O,
                    "OSM_P"     =  tab$OSM_P,
                    "OSM_I"      = tab$OSM_I,
                    "OSM_Q"      = tab$OSM_Q)
      
      GEODIR <- switch(input$GEODIR,    
                       "GEODIR_ALL" = tab$GEODIR_ALL,
                       "GEODIR_S"   = tab$GEODIR_S,
                       "GEODIR_G"   = tab$GEODIR_G,
                       "GEODIR_O"   = tab$GEODIR_O,
                       "GEDOIR_P"   = tab$GEDOIR_P,
                       "GEODIR_I"   = tab$GEODIR_I,
                       "GEODIR_Q" =  tab$GEODIR_Q)  
      
      
      
      
      #### Plots ###
      
      
      
      dat <- data.frame(OSM,GEODIR)
      SC <- gvisScatterChart(dat, 
                             options=list(
                               title="GEODIR~OSM Totals Scatter plot comparison",
                               legend="none",
                               pointSize=10,
                               series="{
                               0: { pointShape: 'circle', color: 'black' }
    }"))
  
      ed2.output <- (SC)
      
      
      return(ed2.output)
      
  })
  
  
  output$bubblech <- renderGvis({
    OSM <- switch(input$OSM,    
                  "OSM_ALL"   = tab$OSM_ALL,
                  "OSM_S"      = tab$OSM_S,
                  "OSM_G"      = tab$OSM_G,
                  "OSM_O"      = tab$OSM_O,
                  "OSM_P"     =  tab$OSM_P,
                  "OSM_I"      = tab$OSM_I,
                  "OSM_Q"      = tab$OSM_Q)
    
    GEODIR <- switch(input$GEODIR,    
                     "GEODIR_ALL" = tab$GEODIR_ALL,
                     "GEODIR_S"   = tab$GEODIR_S,
                     "GEODIR_G"   = tab$GEODIR_G,
                     "GEODIR_O"   = tab$GEODIR_O,
                     "GEDOIR_P"   = tab$GEDOIR_P,
                     "GEODIR_I"   = tab$GEODIR_I,
                     "GEODIR_Q" =  tab$GEODIR_Q)  
    
    
    
    
    #### Plots ###
    
    df=data.frame(County=tab$County, 
                  OSM=OSM, 
                  GEODIR=GEODIR,
                  Percentage_Completeness=(round((OSM/GEODIR)*100)))
    
    Bubble <- gvisBubbleChart(df, idvar="County", 
                              xvar="OSM", yvar="GEODIR",
                              colorvar="County", sizevar="Percentage_Completeness",
                              options=list(
                                pointSize=5,
                                hAxis='{minValue:75, maxValue:125}',
                                chartArea= "{width: '125%', height: '125%'}",
                                width=900, height=500,                           
                                bubble="{textStyle:{color: 'none'}}"))
    
    ed3.output <- (Bubble)
    
    
    return(ed3.output)
    
  })
  
  output$histch <- renderGvis({
    OSM <- switch(input$OSM,    
                  "OSM_ALL"   = tab$OSM_ALL,
                  "OSM_S"      = tab$OSM_S,
                  "OSM_G"      = tab$OSM_G,
                  "OSM_O"      = tab$OSM_O,
                  "OSM_P"     =  tab$OSM_P,
                  "OSM_I"      = tab$OSM_I,
                  "OSM_Q"      = tab$OSM_Q)
    
    GEODIR <- switch(input$GEODIR,    
                     "GEODIR_ALL" = tab$GEODIR_ALL,
                     "GEODIR_S"   = tab$GEODIR_S,
                     "GEODIR_G"   = tab$GEODIR_G,
                     "GEODIR_O"   = tab$GEODIR_O,
                     "GEDOIR_P"   = tab$GEDOIR_P,
                     "GEODIR_I"   = tab$GEODIR_I,
                     "GEODIR_Q" =  tab$GEODIR_Q)  
    #### Plots ###
    
    df=data.frame(County=tab$County, 
                  OSM=OSM, 
                  GEODIR=GEODIR,
                  Percentage_Completeness=(round((OSM/GEODIR)*100)))
    data <- df[as.numeric(df$County),c("OSM", "GEODIR")]
    
    Histogram <- gvisHistogram(data, option=list(title="County Totals",
                                                 legend="{ position: 'none' }",
                                                 colors="['#5C3292', '#1A8763', '#871B47']"))
    ed4.output <- (Histogram)  
    return(ed4.output)
    
  })
  
  output$distPlot <- renderPlot({
    
    OSM <- switch(input$OSM,    
                  "OSM_ALL"   = tab$OSM_ALL,
                  "OSM_S"      = tab$OSM_S,
                  "OSM_G"      = tab$OSM_G,
                  "OSM_O"      = tab$OSM_O,
                  "OSM_P"     =  tab$OSM_P,
                  "OSM_I"      = tab$OSM_I,
                  "OSM_Q"      = tab$OSM_Q)
    
    GEODIR <- switch(input$GEODIR,    
                     "GEODIR_ALL" = tab$GEODIR_ALL,
                     "GEODIR_S"   = tab$GEODIR_S,
                     "GEODIR_G"   = tab$GEODIR_G,
                     "GEODIR_O"   = tab$GEODIR_O,
                     "GEDOIR_P"   = tab$GEDOIR_P,
                     "GEODIR_I"   = tab$GEODIR_I,
                     "GEODIR_Q" =  tab$GEODIR_Q) 
    
    x <- OSM 
    y <- GEODIR
    
    binsx <- seq(min(x), max(x), length.out = input$bins + 1)
    binsy <- seq(min(y), max(y), length.out = input$bins + 1)
    hist(x, breaks = binsx, col = 'darkgray', border = 'white')
    hist(y, breaks = binsy, col = 'blue', border = 'white',add=TRUE)
    box()
  })
  
})
