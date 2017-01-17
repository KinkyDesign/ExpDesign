r2.adj.funct <-
function(y,y.new,num.pred){#y==y, y.new=predicted, num.pred=number of idependent variables (predictors)
    y.mean<- mean(y)
    x.in<- sum((y-y.new)^2)/sum((y-y.mean)^2)
    x.in<- 1-x.in #r squared
    
    x.in<- (1-x.in)*((length(y)-1)/(length(y)-num.pred-1))
    x.in<- 1 - x.in 
    return(x.in)
}
