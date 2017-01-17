suggest.trials.xy <-
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
    sug.trials<- unserialize(dat1.m)
    sug.trials<- sug.trials$Trials
    sug.name<- additionalInfo$predictedFeatures
    
    
    for(i in 1:length(sug.trials)){
        if(length(sug.name)>1){w1<- data.frame(sug.trials[i],NA)}else{w1<- data.frame(sug.trials[i])}
        colnames(w1)<- sug.name
        if(i==1){p7.1<- list(unbox(w1))
        }else{
            p7.1[[i]]<- unbox(w1)
        }
    }
    p7.2<- list(predictions=p7.1)
    
    
    return(p7.2)#clust.classes)#as.data.frame(as.array())
}
