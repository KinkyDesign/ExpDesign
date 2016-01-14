pkgname <- "ExpDesignPkg"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ExpDesignPkg')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("ExpDesignPkg-package")
### * ExpDesignPkg-package

flush(stderr()); flush(stdout())

### Name: ExpDesignPkg-package
### Title: Experimental design
### Aliases: ExpDesignPkg-package ExpDesignPkg
### Keywords: package

### ** Examples


data("dat1")

predF<- "https://apps.ideaconsult.net/enmtest/property/TOX/UNKNOWN_TOXICITY_SECTION/Net+cell+association/8058CA554E48268ECBA8C98A55356854F413673B/3ed642f9-1b42-387a-9966-dea5b91e5f8a"

required.param<- list(nTrials=c(11),criterion='D',form='linear',r2.threshold=0.9)

exp.example<- exp.design.xy(dat1,predF,required.param) 




cleanEx()
nameEx("dat1")
### * dat1

flush(stderr()); flush(stdout())

### Name: dat1
### Title: A sample data object
### Aliases: dat1
### Keywords: datasets

### ** Examples

data(dat1)
## maybe str(dat1) ; plot(dat1) ...



cleanEx()
nameEx("dat1i")
### * dat1i

flush(stderr()); flush(stdout())

### Name: dat1i
### Title: Information for experimental design function suggest.trials.xy
### Aliases: dat1i
### Keywords: datasetsi

### ** Examples

data(dat1i)
## maybe str(dat1i) ; plot(dat1i) ...



cleanEx()
nameEx("dat1m")
### * dat1m

flush(stderr()); flush(stdout())

### Name: dat1m
### Title: Serialized experimental design model file
### Aliases: dat1m
### Keywords: datasetsm

### ** Examples

data(dat1m)
## maybe str(dat1m) ; plot(dat1m) ...



cleanEx()
nameEx("dat1p")
### * dat1p

flush(stderr()); flush(stdout())

### Name: dat1p
### Title: A sample data object
### Aliases: dat1p
### Keywords: datasetsp

### ** Examples

data(dat1p)
## maybe str(dat1p) ; plot(dat1p) ...



cleanEx()
nameEx("dat2i")
### * dat2i

flush(stderr()); flush(stdout())

### Name: dat2i
### Title: Information for experimental design function suggest.trials.noxy
### Aliases: dat2i
### Keywords: datasets2i

### ** Examples

data(dat2i)
## maybe str(dat2i) ; plot(dat2i) ...



cleanEx()
nameEx("dat2m")
### * dat2m

flush(stderr()); flush(stdout())

### Name: dat2m
### Title: Serialized factorial experimental design model file
### Aliases: dat2m
### Keywords: datasets2m

### ** Examples

data(dat2m)
## maybe str(dat2m) ; plot(dat2m) ...



cleanEx()
nameEx("exp.design.noxy")
### * exp.design.noxy

flush(stderr()); flush(stdout())

### Name: exp.design.noxy
### Title: Experimental design function for (full) factorial designs
### Aliases: exp.design.noxy
### Keywords: expDesignNoXY

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


required.param<-  list(levels=3, nVars=3, factors='null', varNames=c('a','b','c'),nTrials=10,criterion='D',form='linear')


exp.example<- exp.design.noxy(null,null,required.param) 



cleanEx()
nameEx("exp.design.xy")
### * exp.design.xy

flush(stderr()); flush(stdout())

### Name: exp.design.xy
### Title: Experimental design function with X and/or y values
### Aliases: exp.design.xy
### Keywords: expDesignXY

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--  or do  help(data=index)  for the standard data sets.

data("dat1")

predF<- "https://apps.ideaconsult.net/enmtest/property/TOX/UNKNOWN_TOXICITY_SECTION/Net+cell+association/8058CA554E48268ECBA8C98A55356854F413673B/3ed642f9-1b42-387a-9966-dea5b91e5f8a"

required.param<- list(nTrials=c(11),criterion='D',form='linear',r2.threshold=0.9)

exp.example<- exp.design.xy(dat1,predF,required.param) 




cleanEx()
nameEx("r2.adj.funct")
### * r2.adj.funct

flush(stderr()); flush(stdout())

### Name: r2.adj.funct
### Title: Adjusted R2 function
### Aliases: r2.adj.funct
### Keywords: r2adj

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

r2.adj.funct(1:10,1:10,2)




cleanEx()
nameEx("r2.funct")
### * r2.funct

flush(stderr()); flush(stdout())

### Name: r2.funct
### Title: R2 function
### Aliases: r2.funct
### Keywords: r2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

r2.funct(1:10,1:10)



cleanEx()
nameEx("suggest.trials.noxy")
### * suggest.trials.noxy

flush(stderr()); flush(stdout())

### Name: suggest.trials.noxy
### Title: Returns suggested trials for a factorial design
### Aliases: suggest.trials.noxy
### Keywords: suggestTFF

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


data("dat2m")
data("dat2i")

pred.res<- suggest.trials.noxy(null, dat2m, dat2i) 




cleanEx()
nameEx("suggest.trials.xy")
### * suggest.trials.xy

flush(stderr()); flush(stdout())

### Name: suggest.trials.xy
### Title: Returns suggested trials when data are available
### Aliases: suggest.trials.xy
### Keywords: suggestT

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--  or do  help(data=index)  for the standard data sets.

data("dat1p")
data("dat1m")
data("dat1i")

pred.res<- suggest.trials.xy(dat1p, dat1m, dat1i) 




### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
