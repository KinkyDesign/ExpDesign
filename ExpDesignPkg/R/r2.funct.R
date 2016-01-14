r2.funct <-
function(y,y.new){#y==y, y.new=predicted
    y.mean<- mean(y)
    x.in<- sum((y-y.new)^2)/sum((y-y.mean)^2)
    x.in<- 1-x.in
    return(x.in)
}
