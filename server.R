
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

library(ggplot2)
library(Hmisc)
library(xtable)
library(hwriter)
library(MASS)
## get a list of datasets
cd<-getwd();
source("./scripts/crosstabs.R")
setwd(cd)
if (file.exists("./datasetlist.csv") )
{
        ## datasetname, title, description
        datasetlist<-read.csv("./datasetlist.csv", stringsAsFactors = FALSE)
} else {
        "Dataset file is missing."
        return()
}

datafiles<-datasetlist$datasetname
datafiles<-setNames(datafiles, datasetlist$title)

emptymatch<-"."

shinyServer(function(input, output) {
        
        output$datafiles <-renderUI({   
                 selectInput("dataset", "Select your data", choices = c(None=".", datafiles ), selected=".")
                }) 

        

        
          getDataPath<- reactive({
                
                        fullpath<-paste0('./data/', input$dataset)
                        })
                
        getDatsetInfo<-function(fullpath)
        {
                datasetname<<-NULL
                datasetname<<-readRDS(fullpath)
                
                varnames<-colnames(datasetname)

                varnames<<-NULL
                varnames<<-varnames[varnames != "Freq"]
                
        }

        
        
        
        output$dependent<-renderUI({

                if (!is.null(input$dataset) & !identical(input$dataset, emptymatch ))
                {

                        fullpath<-getDataPath()
                        getDatsetInfo(fullpath)                        

                        selectInput("dependent", 
                                   label = "Choose a dependent variable ",                   
                                   choices<- c(None='.', varnames),
                                   selected = '.'
                        )
                } else {
                        selectInput("dependent", 
                                    label = "Choose an dependent variable ",
                                    choices<- c(None='.'),
                                    selected = '.'
                                    )

                }
                
        })

        output$independent<-renderUI({ 
                
                if (!is.null(input$dataset) & !identical(input$dataset, emptymatch ))
                {
                        fullpath<-getDataPath()
                        getDatsetInfo(fullpath)                        

                        selectInput("independent", 
                                    label = "Choose an independent variable ",
                                    choices<- c(None='.', varnames),
                                    selected = '.'
                        )
                } else {
                        
                        selectInput("independent", 
                                    label = "Choose an independent variable ",
                                    choices= c(None='.'),
                                    selected = '.'
                                    )
                                  
                }
        })

        output$control<-renderUI({ 

                if (!is.null(input$dataset) & !identical(input$dataset, emptymatch )
                    
                    )
                {
                        fullpath<-getDataPath()
                        getDatsetInfo(fullpath)

                        selectInput("control", 
                                    label = "Choose a control variable ",
                                    choices<- c(None='.', varnames),
                                    selected = '.'
                        )
                } else {
                        selectInput("control", 
                                    label = "Choose a control variable ",
                                    choices<- c(None='.'),
                                    selected = '.'
                                    )

                }
        })

        output$subsetValue<-
                renderUI({
                        if ((!is.null(input$dataset) & !identical(input$dataset, emptymatch ))
                            & (!is.null(input$control) & !identical(input$control, emptymatch ))
                            
                            )
                        {
       
                           
                                subset<-datasetname[[input$control]]
                                subsetValues<-names(table(subset[1]))
       
                                selectInput("subsetValue", 
                                            label = "Choose a subset within the control",                   
                                            choices<- c(None='.', subsetValues),
                                             selected = '.'
                                )

                        } else {
                                selectInput("subsetValue", 
                                            label = "Choose a subset within the control",
                                            choices<- c(None='.'),
                                            selected = '.'
                                )

                        }
                        
                })
        ## Add something here to check for invalid combinations and return a message?)
                
        output$table <- renderTable({
                if (( !is.null(input$dataset) &  !identical(input$dataset, emptymatch) )
                        & ( !identical(input$dependent, emptymatch) & !is.null(input$dependent) )

                    )
                {       
                        fullpath<-getDataPath()
                        getDatsetInfo(fullpath)                        

                        # For now if there is a control variable, subset based on the value selected
                        # This is ugly but it works with xtable which shiny depends on.
                        if (!is.null(input$subsetValue) & !identical(input$subsetValue, emptymatch) 
                                & !is.null(input$control) & !identical(input$control, emptymatch)
                            )
                        {
                                datasetnamec<-datasetname[datasetname[[input$control]] == input$subsetValue,]
                                
                        }
                        
                        # Must be in the correct order.
                        factorsToUse<-c(input$dependent, input$independent)

                        factorsToUse<-factorsToUse[factorsToUse != "."]
                        ctab<<-NULL
                        
                        if (!exists("datasetnamec"))
                        {
                            
                                ctab<<-creatextab( factorsToUse, datasetname)
                        } else {
                                ctab<-creatextab( factorsToUse, datasetnamec)
                        }
                        dimctab<-dim(ctab)

                        ndims<<-NULL
                        ndims<<-length(dimctab)
  
                        if (ndims == 2)
                        {
                                cnames<-colnames(ctab)
                                ncols<-dimctab[2]
                               
                                ctab<-prop.table(ctab,2)*100
                                # Because of datasetc
                                ctab<<-ctab
                                
                                
                        } else if (ndims == 1)
                        {
                        
                                ctab<<-prop.table(ctab)*100
                        }                 
        

                        return(ctab)

                } 

        })
        
        output$plot<-renderPlot({
 
                if (!is.null(input$dataset) & !identical(input$dataset, emptymatch ) & !is.null(ndims))
                {
                        if (!identical(input$dependent, emptymatch ) & identical(input$independent, emptymatch ) 
                            & identical(input$subsetValue, emptymatch )
                            & identical(input$control, emptymatch )
                             
                             & ndims == 1)
                        {
                                plot<-barplot(ctab, beside = TRUE)
                        } else if ((ndims == 2 &  (!is.null(input$dependent) & !identical(input$dependent,emptymatch) )
                                        | (!is.null(input$independent) & !identical(input$independent,emptymatch) ))
                                        & (identical(input$subValue,emptymatch) | is.null(input$subValue))
                                        
                                 ) 
                               
                        {

                                plot<-barplot(ctab, beside = FALSE)
                                ndims<<-NULL
                                ctab<<-NULL
                        } else if (
                                        ((!is.null(input$dependent) & !identical(input$dependent,emptymatch) )
                                         & (!is.null(input$independent) & identical(input$independant,emptymatch))
                                            & (!is.null(input$control) & !identical(input$control,emptymatch))
                                         & !identical(input$subvalue,emptymatch) & ndims == 2)
                                )
                        {


                                ## Change this to show grouped plot?
                                plot<-barplot(ctab, beside = FALSE)
                                 ndims<<-NULL
                                 ctab<<-NULL
                       }
        
                plot
                }                
                
        })

   output$summary<-renderText({

                if (!is.null(input$dataset) & !identical(input$dataset, emptymatch))
                {      
                        selectedData<-datasetlist[datasetlist$datasetname == input$dataset,]
                        #To start off these are set to null.
                        ndims<<-NULL
                        ctab<<-NULL
                
                        text1<-paste("<h3>Information about this data set</h3>")
                        ## Include an HTML description in the data file.
                        text4<-paste0("<h4>",as.character(selectedData["title"]), "</h4>")
                        text2<-as.character(selectedData["dataDescription"])
                        text3<-paste(text1, text4, text2, sep="<br><br> ")

                        return(text3)
                  } else {
                           "<p>Please choose a data set</p>"
                           
                   }
   
          }) 
                output$title<-renderText({
                        title<-""
                        if (!identical(input$dependent, emptymatch) & !is.null(input$dependent) )
                        {
                                title<-paste0("<h4>",input$dependent)
 
                        }
                        
                        if (!is.null(input$independent) & !identical(input$independent, emptymatch))
                        {
                                title<-paste(title,"by", input$independent, "<br/>", sep=" ")
                                
                        }
                        if (!is.null(input$control) & !identical(input$control, emptymatch))
                        {
                                title<-paste0(title, "Controlling for ", input$control, " = ", input$subsetValue, "<br/>")      
                        }
                        if (title != "")
                        {
                                title<-paste0(title, " in Percents</h4>")
                                title<-gsub("_", " ", title)
                        }

                })
   
      })   

