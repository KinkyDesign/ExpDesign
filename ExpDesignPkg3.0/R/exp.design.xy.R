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
	#newY
	 
	dat<- dataset$dataEntry[,2]# data table

	dat1.yind<- predictionFeature#dat1$predictionFeature #string to indicate dependent variable
	depend.variable<- which(colnames(dat) %in% dat1.yind)

	ind.dat<- colnames(dat)


	dat<- t(dat)
	flag1<- 0
	
	
	if(length(depend.variable)!=0){

		y.name<- rownames(dat)[depend.variable[1]]
		y<- dat[depend.variable[1],]
		dat<- dat[-depend.variable[1],]

		#if(length(names(y))==0){names(y)<- as.character(1:length(y))}
		ind.y.c<- names(which(y!='NA'))
		ind.y.n<- names(y[-which(y!='NA')])#which((1:length(y)) %in% which(y!='NA') ==FALSE)

		current.dat<- dat[,which(y!='NA')]
		colnames(current.dat)<- names(which(y!='NA'))
		new.dat<- dat[,-which(y!='NA')]
		colnames(new.dat)<- names(y)[-which(y!='NA')]
		y<- y[which(y!='NA')]#y<- y[which(y!=0)]
		

		#find which one is the categorical independent variable
		cat1<- which(is.na(suppressWarnings(as.numeric(current.dat[,1]))))
		row1<- colnames(new.dat)
		if(length(cat1)!=0){
			cat2<- unique(dat[cat1,])#find levels of categorical variable
			#replace with numeric ids # cat3<- 1:length(cat2)
			for(i in 1:length(cat2)){
				current.dat[cat1,]<- gsub(cat2[i],i,current.dat[cat1,])
				new.dat[cat1,]<- gsub(cat2[i],i,new.dat[cat1,])
			}
			current.dat<- matrix(as.numeric(current.dat),dim(current.dat)[1],dim(current.dat)[2],byrow=T)
			new.dat<- matrix(as.numeric(new.dat),dim(new.dat)[1],dim(new.dat)[2],byrow=T)
		}
		set.seed(2)
		pls.fit<- plsr(as.numeric(y)~.,data=as.data.frame(t(current.dat)),validation='LOO')#CV
		

		pred<- pls.fit$validation$pred[,,pls.fit$validation$ncomp]
		dep.var.selected<- pls.fit$coefficients[,,pls.fit$ncomp]
		dep.var.selected<- dep.var.selected[which(dep.var.selected!=0)] 
		r2<- r2.funct(as.numeric(y),pred); adj.r2<- r2.adj.funct(as.numeric(y),pred,length(dep.var.selected))

		suggested.trials<- rep(0,dim(dat)[2])
		pred.name<- c('suggestedTrials')
		
		if(length(ind.y.n) <= parameters$nTrials){
			#warning(paste('No need for more experiments; R2>=',parameters$r2.threshold,sep=''))}
			#stop(paste('No need for more experiments; R2>=',r2.threshold,sep=''))

			suggested.trials[as.numeric(ind.y.n)]<- 1


		}else{
		##to do for stand-alone: add menu() instead of warning() 

			flag1<- 1

			new.scores<- predict(pls.fit,type='scores',newdata=t(new.dat))
			form.int<- if(parameters$form=='linear'){as.formula('~.')}else{as.formula(paste('~',form,'(.)',sep=''))} 

			desD.int<- optFederov(form.int,new.scores[,1:2],parameters$nTrials,criterion=parameters$criterion,evaluateI=TRUE,DFrac=1,nRepeats=100)
			desD.eval<- eval.design(form.int,desD.int$design,confounding=TRUE)

			named.suggested.trials<- row1[desD.int$rows]#rownames(new.scores)[desD.int$rows]
			rownames(desD.int$design)<- named.suggested.trials
			names(desD.int$rows)<- named.suggested.trials

			suggested.trials[which(colnames(dat) %in% names(desD.int$rows))]<- 1

		}
	
	}else{
		
		flag1<- 1

		dat.in<- na.omit(t(dat))
		dat.in<- data.matrix(as.data.frame(dat.in))
		
		#ammend for suggestedTrials column (fd)
		test.in<- apply(dat.in,2,function(x){length(levels(factor(x)))})
		dat.in<- dat.in[,which(test.in!=1)]


		tmp <- cor(dat.in)
		tmp[upper.tri(tmp)] <- 0
		diag(tmp) <- 0

		dat.new<- dat.in[,which(apply(tmp,2,function(x) all(abs(x) <= 0.9))==TRUE)]

		dat<- t(dat.new)

		protc.pca<- prcomp(na.omit(t(dat)),center = TRUE)#CHANGE!
		form.int<- if(parameters$form=='linear'){as.formula('~.')}else{as.formula(paste('~',form,'(.)',sep=''))} 

		desD.int<- optFederov(form.int,protc.pca$x[,1:4],parameters$nTrials,evaluateI=TRUE,DFrac=1,nRepeats=100)#protc.pca$x[,1:4]
		desD.eval<- eval.design(form.int,desD.int$design,confounding=TRUE)

		named.suggested.trials<- rownames(protc.pca$x)[desD.int$rows]
		rownames(desD.int$design)<- named.suggested.trials
		#colnames(desD.int$design)<- c('Comp1','Comp2')
		names(desD.int$rows)<- named.suggested.trials
		
		suggested.trials<- rep(0,dim(protc.pca$x)[1])
		suggested.trials[which(colnames(dat) %in% names(desD.int$rows))]<- 1

		pred.name<- c('suggestedTrials',parameters$newY)


	}

	if(flag1==1){
		verbal.in<- c(paste('Ge value is:',desD.int$Ge,'. Ge for optimal design is 1.', sep=''),
		paste('Diagonality value is: ',desD.eval$diagonality,'. Diagonality for minimal confounding is 1.', sep=''))
	
		desD.res<- list(design=desD.int$design,selected.rows=desD.int$rows,
		norm.var=desD.int$Ge,confounding.effect=desD.eval$diagonality,
		r.squared=ifelse(length(depend.variable)!=0,r2,NA),
		adj.r.squared=ifelse(length(depend.variable)!=0,adj.r2,NA),
		verbal.notes=verbal.in,
		predictedFeatures=pred.name)#or 'NA'?

	}else{
		verbal.in<- c('All remaining experimented are suggested since the number of trials requested is higher than the empty cells in the data.')

			desD.res<- list(design=NA,selected.rows=as.numeric(ind.y.n),
			norm.var=NA,confounding.effect=NA,
			r.squared=r2,adj.r.squared=adj.r2,verbal.notes=verbal.in,
			predictedFeatures=pred.name)#or 'NA'?

	}
	
	
	# return:
	suggested.trials.ser<- serialize(list(Trials=suggested.trials),connection=NULL)



	m1.ser.list<- list(rawModel=suggested.trials.ser,pmmlModel=NULL,independentFeatures=ind.dat,
	predictedFeatures=pred.name,#NULL,
	additionalInfo=desD.res)


    return(m1.ser.list)

}
