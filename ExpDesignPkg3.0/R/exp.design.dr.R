exp.design.dr <-
function(dataset,predictionFeature,parameters){
    #dataset:= list of 8 objects - 
    #"meta","visible","dataEntry","features","totalRows","totalColumns","descriptors","_id" 
    #predictionFeature:= character string specifying which is the prediction feature in dataEntry- HERE NULL, 
    #parameters:= list with parameter values-- here awaits a list with three elemnts: 
    #p.vec:= vector of three values corresponding to the expected percentages of the maximum effect 
    #achieved at the first,second, and last doses#, for the remaining doses assumes equal space between percentages of the supplied percentage range
    #dMax:= Dose at which maximum effect occurs, only needed for the beta model, in 0-1 scale  
    #choice:= a character vector awaiting one of the following- 'weight' for weights on doses, 
    #'sampleSize' for sample size per dose, 'tsampleSize' for tailored sample size given the weights
    #designCrit:= one of c("Dopt", "TD", "Dopt&TD", "userCrit") to determine which type of design to calculate
    #optimizer:= one of c("solnp", "Nelder-Mead", "nlminb", "exact"). Algorithm used for calculating the optimal design.
    #Delta:= Target effect needed for calculating "TD" and "TD&Dopt" type designs. 
    #n:= sample size of next cohort -only needed if $choice is 'tSampleSize' for specific sample sie calculations,
    # or 'SampleSize' to specify the upper bound for target sample size.
    #dat<- dataset$dataEntry[,2]# data table
    dat<- dataset
    dat.d<- sort(as.numeric(unlist(dat$features[,3])))
    
    
    if(length(dat.d)>=3){
        
        p.vec<- parameters$p.vec
        #p.vecN<- numeric(length(dat.d))
        #p.vecN[c(1,length(p.vecN))]<- p.vec
        #p.sep<- (p.vec[2]-p.vec[1]-0.05)/(length(x)-2)
        #for(i in 2:(length(p.vecN)-1)){p.vecN[i]<- p.vec[1]+((i-1)*p.sep)}
        
        models.options<- c("emax", "exponential", "logistic", "quadratic","betaMod", "sigEmax")
        models.params <- sapply(models.options,function(x) NULL)
        models.params[[1]]<- guesst(d=dat.d[length(dat.d)],p=p.vec[length(p.vec)],model='emax')
        models.params[[2]]<- guesst(d=dat.d[length(dat.d)],p=p.vec[length(p.vec)],model='exponential',Maxd=dat.d[length(dat.d)])
        models.params[[3]]<- guesst(d=dat.d[c(1,2,length(dat.d))],p=p.vec,model='logistic')
        models.params[[4]]<- guesst(d=dat.d[length(dat.d)],p=p.vec[length(p.vec)],model='quadratic')
        if(dat.d[length(dat.d)]>1){
            dat.dB<- dat.d/dat.d[length(dat.d)]
            models.params[[5]]<- guesst(d=dat.dB[2],p=p.vec[2],model='betaMod',scal=1.2,dMax=parameters$dMax,Maxd=dat.dB[length(dat.d)])
        }else{
            models.params[[5]]<- guesst(d=dat.d[2],p=p.vec[2],model='betaMod',scal=1.2,dMax=parameters$dMax,Maxd=dat.d[length(dat.d)])
        }
        models.params[[6]]<- guesst(d=dat.d[c(2,length(dat.d))],p=p.vec[2:3],model='sigEmax')
        
        models <- Mods(linear = NULL, linlog = NULL, emax = c(dat.d[2], models.params[[1]][1]),
                       exponential = models.params[[2]][1], quadratic = models.params[[4]][1],
                       logistic = models.params[[3]],
                       betaMod = models.params[[5]], sigEmax = models.params[[6]],
                       doses = dat.d)
        #linInt = rbind(c(0.5, 0.75, 1, 1), c(0.5, 1, 0.7, 0.5)),
        #addArgs = list(scal=1.2, off=0.1))
        
        ser.models<- serialize(models,connection=NULL)
        
        p.choice<- parameters$choice
        if(p.choice=='weight'){pred.name<- c('Doses','SuggestedWeights')}
        if(p.choice=='tsampleSize'){pred.name<- c('Doses','SuggestedWeights','SuggestedTailoredSS')}
        if(p.choice=='sampleSize'){pred.name<- c('Doses','SuggestedWeights','SuggestedSampleSize')}
        
        desD.res<- list(predictedFeatures=pred.name,parameters=parameters)#or 'NA'?
        
        m1.ser.list<- list(rawModel=ser.models,pmmlModel=NULL,independentFeatures=NULL,
                           predictedFeatures=pred.name,#NULL,
                           additionalInfo=desD.res)
        
        return(m1.ser.list)
        
        
    }else{
        stop('Need at least three different dose levels to calculate Doptim design.')
        
    }
}
