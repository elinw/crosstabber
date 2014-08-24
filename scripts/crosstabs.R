## Some code

#varsToUse is the array of variablesselectd in the UI
# data is the dataset name
creatextab<-function(factorsToUse, data)
{
 
        newform<-as.formula(paste("Freq ~", paste(factorsToUse, collapse="+"), sep=""))
       xtabs(formula= newform, drop.unused.levels = TRUE, data=data)
        
        
        
}

doLoglinear<-function(factorsToUse, data)
{

        formula<-as.formula(paste("Freq ~", paste(factorsToUse, collapse="+"), sep=""))
        xtabdata<-xtabs(formula=formula, drop.unused.levels = TRUE, data=data)
        print(class(formula))

        loglm(formula = formula, data=xtabdata, na.action=TRUE)
        
}

doLoglinearSaturated<-function(factorsToUse, data)
{
        formula<-as.formula(paste("Freq ~", paste(factorsToUse, collapse="+"), sep=""))
        xtabdata<-xtabs(formula=formula, drop.unused.levels = TRUE, data=data)
        formulasat<-as.formula(paste("Freq ~", paste(factorsToUse, collapse="*"), sep=""))

        loglm(formula = formulasat, data=xtabdata)
}