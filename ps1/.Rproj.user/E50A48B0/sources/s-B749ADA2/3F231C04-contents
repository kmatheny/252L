out<-list() #lists are really useful!! this just initializes this one so that we can use it later. read more @ http://www.r-tutor.com/r-introduction/list

##CTT item analysis
##this function will compute CTT item statistics for a generic item response matrix
item_analysis<-function(resp) { #'resp' is just a generic item response matrix, rows are people columns are items
    pv<-colMeans(resp,na.rm=TRUE) #simple "p-values", which in psychometrics tends to just mean the mean number of points for an item
    r.xt<-numeric() #initializing a vector
    rowSums(resp,na.rm=TRUE)->ss #these are the sum scores/observed scores
    for (i in 1:ncol(resp)) {
        cor(ss,resp[,i],use='p')->r.xt[i]
    }
    cbind(pv,r.xt) #i'll return a matrix consisting of the p-values and the item/total correlations
}

resp1 <-read.table("emp-rasch.txt",header=FALSE)
out[[1]]<-item_analysis(resp1) 

resp2 <-read.table("rasch.txt",header=FALSE)
out[[2]]<-item_analysis(resp2)

par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(3,3,1,1))

plot(out[[1]])
plot(out[[2]])

par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1))
pf<-function(x) {
    plot(density(x[,1]),xlim=c(0,1),xlab="density, p-values")
    plot(density(x[,2]),xlim=c(0,1),xlab="density, item-total correlations")
}
lapply(out,pf)

