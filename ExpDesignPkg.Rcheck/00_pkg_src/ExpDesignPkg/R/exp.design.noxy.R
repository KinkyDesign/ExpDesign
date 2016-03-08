exp.design.noxy <-
function(dataset,predictionFeature,parameters){
    #dataset:= list of 2 objects - HERE EMPTY 
    #datasetURI:= character sring, code name of dataset
    #dataEntry:= data.frame with 2 columns, 
    #1st:name of compound,2nd:data.frame with values (colnames are feature names)
    #predictionFeature:= character string specifying which is the prediction feature in dataEntry - HERE EMPTY
    #parameters:= list with parameter values-- here awaits a list with seven elemnts: 
    #levels:= A vector of levels for the variables. May be an integer if nVars is specified
    #nVars:= The number of variables.
    #factors:= 'null', 'all' or a number indicating the factor column in the data set.
    #varNames:= The names of the variables, e.g. c('A','B','C').
    #nTrials (a numeric value indicating number of trials suggested, if 0 then an estimated number is suggested),
    #criterion (a character value to indicate which optimal deisgn to apply. Possible values are  'D', 'A', 'I'. Default is 'D'),
    #form (a string indicating the formula of the deisgn- 'linear','quad','cubic','cubicS')),
    #if a character vector is included the form of the design should be linear
    #Outputs can vary, because options could have identical value based on Federov's method. 
    
    dat<- parameters
    if(dat$factors=='null'){fact.data<- gen.factorial(levels=dat$levels,nVars=dat$nVars,varNames=dat$varNames)
    }else{
        fact.data<- gen.factorial(levels=dat$levels,nVars=dat$nVars,factors=dat$factors,varNames=dat$varNames)}
    
    form.int<- if(dat$form=='linear'){as.formula('~.')}else{as.formula(paste('~',dat$form,'(.)',sep=''))}
    desD.int<- 	optFederov(form.int,fact.data,dat$nTrials,evaluateI=TRUE,DFrac=1,nRepeats=100)#
    desD.eval<- eval.design(form.int,fact.data,dat$nTrials)
    
    suggested.trials<- cbind(as.matrix(fact.data),rep(0,nrow(fact.data)))
    suggested.trials[desD.int$rows,(dat$nVars+1)]<- 1

    
    # return:
    
    verbal.in<- c(paste('Ge value is:',desD.int$Ge,'. Ge for optimal design is 1.', sep=''),
                  paste('Diagonality value is:',desD.eval$diagonality,'. Diagonality for minimal confounding is 1.', sep=''))
    
    suggested.trials.ser<- serialize(list(Trials=suggested.trials),connection=NULL)
    #pred.name<- 'suggestedTrials'
    pred.name<- c(paste('suggestedTrials_',dat$varNames,sep=''),'suggestedTrials')
    
    desD.res<- list(design=desD.int$design,selected.rows=desD.int$rows,
                    norm.var=desD.int$Ge,confounding.effect=desD.eval$diagonality,
                    verbal.notes=verbal.in,predictedFeatures=pred.name)#or 'NA'?
    
    m1.ser.list<- list(rawModel=suggested.trials.ser,pmmlModel=NULL,independentFeatures=dat$varNames,
                       predictedFeatures=pred.name,additionalInfo=desD.res)
    
    
    return(m1.ser.list)
    
}
