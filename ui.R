h4("Exponentiated coefficients"),
verbatimTextOutput("exp_S"),
h4("Probability"),
verbatimTextOutput("probabilities_S")),
tabPanel( "Nacecode I" ,  
          h4("summary of generalised linear model Nacecode I"),
          verbatimTextOutput("summary_I"),
          h4("95% Confidence Intervals for the coefficients"),
          verbatimTextOutput("confint_I"),
          h4("exponentiated coefficients"),
          verbatimTextOutput("exp_I"),
          h4("Probability"),
          verbatimTextOutput("probabilities_I")
          ),tabPanel( "Nacecode P" ,  
                      h4("summary of generalised linear model Nacecode P"),
                      verbatimTextOutput("summary_P"),
                      h4("95% Confidence Intervals for the coefficients"),
                      verbatimTextOutput("confint_P"),
                      h4("exponentiated coefficients"),
                      verbatimTextOutput("exp_P"),
                      h4("Probability"),
                      verbatimTextOutput("probabilities_P")
          ),
tabPanel( "Nacecode O" ,  
          h4("summary of generalised linear model Nacecode O"),
          verbatimTextOutput("summary_O"),
          h4("95% Confidence Intervals for the coefficients"),
          verbatimTextOutput("confint_O"),
          h4("exponentiated coefficients"),
          verbatimTextOutput("exp_O"),
          h4("Probability"),
          verbatimTextOutput("probabilities_O")
)
))),
navbarMenu("Tables",
  id = 'dataset',
  tabPanel('OSM and GEDOIR counties totals', dataTableOutput('mytable1')),
  tabPanel('OSM percentage completeness by County', dataTableOutput('mytable2')),
  tabPanel('Percentage Completeness by County and Nacecode', dataTableOutput('mytable3'))
),
height = "900px", width = "1500px"
      
  
  ), class = "span120"))
))
