#based on code from https://gist.github.com/dgrapov/5792778 and shiny tutorials http://shiny.rstudio.com/tutorial/
library(sp)
library(RColorBrewer)

counties <- read.csv("http://cs1.ucc.ie/~mlol1/CS6500/data/Percentage_Completeness.csv")
counties<-as.data.frame(counties)
Counties_numbers<-read.csv("http://cs1.ucc.ie/~mlol1/CS6500/Counties_numbers.csv")
Counties_numbers$x<-NULL
source("helpers.R")
# shiny server side code for each call
shinyServer(function(input, output, session){
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
  
  #update variable and group based on dataset
  output$variable <- renderUI({ 
    obj<-switch(input$dataset,
                "Counties_numbers" = Counties_numbers)   
    var.opts<-namel(colnames(obj))
    selectInput("variable","Variable:", var.opts) # uddate UI 				 
  }) 
  
  output$group <- renderUI({ 
    obj<-switch(input$dataset,
                "Counties_numbers" = Counties_numbers)	 
    var.opts<-namel(colnames(obj))
    selectInput("group","Groups:", var.opts) # uddate UI 				 
  }) 
  
  output$caption<-renderText({
    switch(input$plot.type,
           "plot" 	= 	"plot",
           "histogram" =	"Histogram",
           "density" 	=	"Density plot",
           "bar" 		=	"Bar graph")
  })
  
  
  output$plot <- renderUI({
    plotOutput("p")
  })
  
  #plotting function using ggplot2
  output$p <- renderPlot({
    
    plot.obj<<-list() # not sure why input$X can not be used directly?
    plot.obj$data<<-get(input$dataset) 
    plot.obj$variable<<-with(plot.obj$data,get(input$variable)) 
    plot.obj$group<<-with(plot.obj$data,get(input$group)) 
    
    #dynamic plotting options
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= 	geom_boxplot(),
                      "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                      "density" 	=	geom_density(alpha=.75),
                      "bar" 		=	geom_bar(position="dodge")
    )
    
    require(ggplot2)
    #plotting theme
    .theme<- theme(
      axis.line = element_line(colour = 'darkgray', size = .75), 
      panel.background = element_blank(),  
      plot.background = element_blank()
    )	 
    if(input$plot.type=="boxplot")	{		#control for 1D or 2D graphs 
      p<-ggplot(plot.obj$data, 
                aes(
                  x 		= plot.obj$group, 
                  y 		= plot.obj$variable,
                  fill 	= as.factor(plot.obj$group)
                )
      ) + plot.type
      
      if(input$show.points==TRUE)
      { 
        p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
      }
      
    } else {
      
      p<-ggplot(plot.obj$data, 
                aes(
                  x 		= plot.obj$variable,
                  fill 	= as.factor(plot.obj$group),
                  group 	= as.factor(plot.obj$group)
                 # color 	= as.factor(plot.obj$group)
                )
      ) + plot.type
    }
    
    p<-p+labs(
      fill 	= input$group,
      x 		= "",
      y 		= input$variable
    )  +
      .theme
    print(p)
  })	
})
