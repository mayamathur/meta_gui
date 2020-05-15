
source("startup.R")


library(shiny)


navbarPage( "",
            tabPanel( "Instructions",
                      mainPanel(
                          wellPanel( "To estimate the proportion of scientifically meaningful effect sizes
                                     with different amount of confounding bias, use the tab 'Range of sensitivity parameters'. 
                                     Conduct a random-effects meta-analysis on the relative risk (RR) scale and use the 
                                     first four input fields to enter the pooled effect size, the heterogeneity estimate, and their
                                     variances (squared standard errors). Then enter the standard deviation of the bias factor
                                     across studies (or set it to zero to assess a fixed amount of bias across all studies). 
                                     Enter your minimum threshold of scientific importance on the log-RR scale for 'Minimum effect
                                     size of interest (q).'" ),
                          wellPanel( "To choose a fixed set of sensitivity parameters and estimate (1) the proportion of scientifically meaningful effect sizes
                                     with different amounts of confounding bias, (2) the minimum bias factor to 'explain away' the effect; 
and (3) the minimum confounding strength to 'explain away' the effect, use the tab 'Fixed sensitivity parameters'. 
                                     The inputs are as described above. This tab also plots the estimated observed (confounded)
                                     and true (unconfounded) effect distributions under the specified bias parameters." ),
                          wellPanel( "For help interpreting the results and choosing sensitivity parameters, see
                                     especially the 'Practical use and interpretation' section of:
                                     Mathur MB & VanderWeele TJ (2017). Sensitivity analysis for unmeasured confounding
                                     in meta-analyses. Preprint available at https://osf.io/jkhfg." )
                      )
            ),
           tabPanel( "Range of sensitivity parameters",
                    sidebarLayout(
                        
                        sidebarPanel(
                            numericInput('yr', 'Pooled effect size (log relative risk)', log(1.2), min = 1, max = 9),
                            numericInput('vyr', 'Estimated variance of pooled effect (optional)', 0.01, min = 1, max = 9),
                            numericInput('t2', "Estimated heterogeneity (tau^2)", 0.10, min = 1, max = 9),
                            numericInput('vt2', "Estimated variance of tau^2 (optional)", 0.002, min = 1, max = 9),
                            numericInput('sigB', 'Standard deviation of bias factor on log scale', 0, min = 1, max = 9),
                            numericInput('q', 'Minimum effect size of interest (q)', log(1.1), min = 1, max = 9),
                            numericInput('Bmin', 'X-axis lower limit (log scale)', log(1), min = 1, max = 9),
                            numericInput('Bmax', 'X-axis upper limit (log scale)', log(3), min = 1, max = 9)
             
                        ),
                        
                        mainPanel(
                            plotOutput('plot1')
                
                        )
                    
                    )
           )
            ,
           tabPanel("Fixed sensitivity parameters",
#                     sidebarLayout(
                        
                        sidebarPanel(
                            numericInput('yr.2', 'Pooled effect size (log relative risk)', log(1.2), min = 1, max = 9),
                            numericInput('vyr.2', 'Estimated variance of pooled effect (optional)', 0.01, min = 1, max = 9),
                            numericInput('t2.2', "Estimated heterogeneity (tau^2)", 0.10, min = 1, max = 9),
                            numericInput('vt2.2', "Estimated variance of tau^2 (optional)", 0.002, min = 1, max = 9),
                            numericInput('muB.2', 'Mean of bias factor on log scale', log(1.5), min = 1, max = 9),
                            numericInput('sigB.2', 'Standard deviation of bias factor on log scale', 0, min = 1, max = 9),
                            numericInput('r.2', 'Proportion below which large effects are to be reduced (r)', 0.20, min = 1, max = 9),
                            numericInput('q.2', 'Minimum effect size of interest (q)', log(1.1), min = 1, max = 9)
                            #actionButton("make.plot", "Create distribution plot")
                        ),
    
                    
                        wellPanel( "Proportion of studies with true effects more extreme than q: ", span( textOutput("text1") ) ),
                        wellPanel( "Minimum bias factor (RR scale) to reduce to less than r the proportion of studies with true effects more extreme than q: ", span( textOutput("text2") ) ),
                        wellPanel( "Minimum confounding strength (RR scale) to reduce to less than r the proportion of studies with true effects more extreme than q: ",
                                   span( textOutput("text3") ) ),
                       # wellPanel( "[Scroll down for distribution plot]" ),
                        
                        mainPanel(
                            plotOutput('plot2')
                            
                        )
                    
#                     ) 
           )

)








