suggest.trials.dr <-
function(dataset,rawModel,additionalInfo){
    #dataset:= list of 2 objects - 
    #datasetURI:= character sring, code name of dataset
    #dataEntry:= data.frame with 2 columns, 
    #1st:name of compound,2nd:data.frame with values (colnames are feature names)
    #rawModel:= numeric vector showing experimental design results   
    #additionalInfo:= list with summary statistics, returns design matrix with 
    #suggested trials for y (whether or not y was originally supplied)
    
    dat1.m<- rawModel
    dat1.m<- base64Decode(dat1.m,'raw')
    sug.models<- unserialize(dat1.m)
    sug.name<- additionalInfo$predictedFeatures
    sug.params<- additionalInfo$parameters
    
    
    if(sug.params$choice!='sampleSize'){
        optD<- optDesign(sug.models,probs=rep(1/9,9),designCrit = sug.params$designCrit,Delta=sug.params$Delta, optimizer=sug.params$optimizer)
        if(sug.params$choice=='tSampleSize'){
            sug.trials<- cbind(attr(sug.models,'doses'),optD$design,rndDesign(optD$design,sug.params$n))
        }else{sug.trials<- cbind(attr(sug.models,'doses'),optD$design)}
    }else{
        contMat <- optContr(sug.models, w=1)
        tFunc <- function(n){
            powVals <- powMCT(contMat,altModels=sug.models,n=n,sigma = 1,alpha=0.05)
            mean(powVals)}
        
        sSize <- sampSize(upperN = sug.params$n, targFunc = tFunc, target=0.8, alRatio = rep(1,9), verbose = FALSE)
        sug.trials<- cbind(attr(sug.models,'doses'),optD$design,sSize$samp.size)
    }
    
    
    for(i in 1:dim(sug.trials)[1]){
        w1<- as.data.frame(t(sug.trials[i,]))
        colnames(w1)<- sug.name
        if(i==1){p7.1<- list(unbox(w1))
        }else{
            p7.1[[i]]<- unbox(w1)
        }
    }
    p7.2<- list(predictions=p7.1)
    
    return(p7.2)#clust.classes)#as.data.frame(as.array())
}
