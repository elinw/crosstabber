
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Analyze some census data"),

  sidebarLayout(
           sidebarPanel(
                   tags$style(type="text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"
                   ),
                   
                helpText("Choose your data set and variables."),
                uiOutput("datafiles"),

                uiOutput("dependent"),
                uiOutput("independent"),
                uiOutput("control"),
                uiOutput("subsetValue")
          ),
          
          mainPanel(
                  htmlOutput("title"),
                  tabsetPanel(     

                          tabPanel("Table",
                                  
                                htmlOutput("table"),
                
                                htmlOutput("summary")
                          ),
                          tabPanel("Plot",
                                plotOutput("plot")
                          ),
                          tabPanel("About",
                                includeMarkdown("./docs/about.Rmd")         
                          )
                          
                )
                  
          ))
)
)