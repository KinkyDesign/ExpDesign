exp.design.xy<- function(dataset,predictionFeature,parameters){
	#dataset:= list of 2 objects - 
	#datasetURI:= character sring, code name of dataset
	#dataEntry:= data.frame with 2 columns, 
	#1st:name of compound,2nd:data.frame with values (colnames are feature names)
	#predictionFeature:= character string specifying which is the prediction feature in dataEntry, 
	#parameters:= list with parameter values-- here awaits a list with four elemnts: 
	#nTrials (a numeric value indicating number of trials suggested, if 0 then an estimated number is suggested),
	#criterion (a character value to indicate which optimal deisgn to apply. Possible values are  'D', 'A', 'I'. Default is 'D'),
	#form (a string indicating the formula of the deisgn- 'linear','quad','cubic','cubicS')),
	#r2.threshold (numeric value indicating the r2 threshold value. If the data supplied provides r2 value greater 
	#than the threshold value, a stop message is returned.).
	 
	dat<- dataset$dataEntry[,2]# data table

	dat1.yind<- predictionFeature#dat1$predictionFeature #string to indicate dependent variable
	depend.variable<- which(colnames(dat) %in% dat1.yind)

	ind.dat<- colnames(dat)

	dat<- t(dat)

	if(length(depend.variable)!=0){

		y.name<- rownames(dat)[depend.variable[1]]
		y<- dat[depend.variable[1],]
		dat<- dat[-depend.variable[1],]

		ind.y.c<- names(which(y!='NA'))
		ind.y.n<- names(y[-which(y!='NA')])#which((1:length(y)) %in% which(y!='NA') ==FALSE)

		current.dat<- dat[,which(y!='NA')]
		colnames(current.dat)<- names(which(y!='NA'))
		new.dat<- dat[,-which(y!='NA')]
		colnames(new.dat)<- names(y)[-which(y!='NA')]
		y<- y[which(y!=0)]
		
		set.seed(2)
		pls.fit<- plsr(as.numeric(y)~.,data=as.data.frame(t(current.dat)),validation='LOO')#CV

		pred<- pls.fit$validation$pred[,,pls.fit$validation$ncomp]
		dep.var.selected<- pls.fit$coefficients[,,pls.fit$ncomp]
		dep.var.selected<- dep.var.selected[which(dep.var.selected!=0)] 
		r2<- r2.funct(as.numeric(y),pred); adj.r2<- r2.adj.funct(as.numeric(y),pred,length(dep.var.selected))

		if(r2 >= parameters$r2.threshold){warning(paste('No need for more experiments; R2>=',parameters$r2.threshold,sep=''))}
		#stop(paste('No need for more experiments; R2>=',r2.threshold,sep=''))
		#}else{
		#to do for stand-alone: add menu() instead of warning() 
			new.scores<- predict(pls.fit,type='scores',newdata=t(new.dat))
			form.int<- if(parameters$form=='linear'){as.formula('~.')}else{as.formula(paste('~',form,'(.)',sep=''))} 

			desD.int<- optFederov(form.int,new.scores[,1:2],parameters$nTrials,criterion=parameters$criterion,evaluateI=TRUE,DFrac=1,nRepeats=100)
			desD.eval<- eval.design(form.int,desD.int$design,confounding=TRUE)

			named.suggested.trials<- rownames(new.scores)[desD.int$rows]
			rownames(desD.int$design)<- named.suggested.trials
			names(desD.int$rows)<- named.suggested.trials

			suggested.trials<- rep(0,dim(dat)[2])
			suggested.trials[which(colnames(dat) %in% names(desD.int$rows))]<- 1

			pred.name<- c('suggestedTrials')
		#}
	
	}else{

		protc.pca<- prcomp(t(dat),center = TRUE)
		form.int<- if(parameters$form=='linear'){as.formula('~.')}else{as.formula(paste('~',form,'(.)',sep=''))} 
	
		desD.int<- optFederov(form.int,protc.pca$x[,1:4],parameters$nTrials,evaluateI=TRUE,DFrac=1,nRepeats=100)#
		desD.eval<- eval.design(form.int,desD.int$design,confounding=TRUE)

		named.suggested.trials<- rownames(protc.pca$x)[desD.int$rows]
		rownames(desD.int$design)<- named.suggested.trials
		#colnames(desD.int$design)<- c('Comp1','Comp2')
		names(desD.int$rows)<- named.suggested.trials
		
		suggested.trials<- rep(0,dim(protc.pca$x)[1])
		suggested.trials[which(colnames(dat) %in% names(desD.int$rows))]<- 1

		pred.name<- c('suggestedTrials',parameters$newY)

	}

	verbal.in<- c(paste('Ge value is:',desD.int$Ge,'. Ge for optimal design is 1.', sep=''),
	paste('Diagonality value is:',desD.eval$diagonality,'. Diagonality for minimal confounding is 1.', sep=''))

	# return:
	suggested.trials.ser<- serialize(list(Trials=suggested.trials),connection=NULL)


	desD.res<- list(design=desD.int$design,selected.rows=desD.int$rows,
	norm.var=desD.int$Ge,confounding.effect=desD.eval$diagonality,
	r.squared=ifelse(length(depend.variable)!=0,r2,NA),
	adj.r.squared=ifelse(length(depend.variable)!=0,adj.r2,NA),
	verbal.notes=verbal.in,predictedFeatures=pred.name)#or 'NA'?

	m1.ser.list<- list(rawModel=suggested.trials.ser,pmmlModel=NULL,independentFeatures=ind.dat,
	predictedFeatures=pred.name,#NULL,
	additionalInfo=desD.res)


    return(m1.ser.list)

}
