library(mirt)
resp<-read.table("nde_math_white_space.txt",header=FALSE)

#CTT Model
get.coors<-function(resp) {
  r.xt<-numeric() #initializing a numeric vector
  ss<-rowSums(resp) #these are the sum scores/observed scores of CTT
  for (i in 1:ncol(resp)) {
    r.xt[i]<-cor(ss,resp[,i]) #for any i, what is this?
  }
  r.xt
}

r <- get.coors(resp) #item total correlations
p <- colMeans(resp) #p values

plot.fun<-function(resp) { #don't worry too much about the details here in the first pass.
  pv<-colMeans(resp,na.rm=TRUE)
  r.xt<-get.coors(resp)
  plot(pv,r.xt,pch=19)
  ##everything below here just goes into the red line
  loess(r.xt~pv)->m
  cbind(pv,fitted(m))->tmp
  lines(tmp[order(tmp[,1]),],col="red")
  NULL
}

plot.fun(resp) #plots item total correlations vs p-values

#Rasch Model
mod1<-mirt(resp,1,itemtype="Rasch")
theta1<-fscores(mod1,method="ML",full.scores=TRUE)

rasch_fit <- itemfit(mod1,fit_stats='infit')

plot(rasch_fit[,4], main='Infit')
plot(rasch_fit[,2], main='Outfit')

plot(extract.mirt(mod1,'parvec')) #plots estimated item easiness

#3PL
mod2<-mirt(resp,1,itemtype="3PL")
theta2<-fscores(mod2,method="ML",full.scores=TRUE)

dtheta <- theta2-theta1
hist(dtheta)
plot(theta2)

matrix(extract.mirt(mod2,'parvec'),ncol=3,byrow=TRUE) -> pars
plot(pars[,1], main = 'Estimated Item Discrimination')  #this might be easiness
plot(pars[,2], main = 'Estimated Item Easiness')        #this might be discrimination
plot(pars[,3], main = 'Estimated Guessing Parameters')  #i don't know why these aren't all between 0 and 1

