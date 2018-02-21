library(mirt)
resp<-read.table("emp-rasch.txt",header=FALSE)


th<-seq(-3,3,length.out=1000)
i1<-seq(0,0,length.out=1000)
i2<-seq(0,0,length.out=1000)
i3<-seq(0,0,length.out=1000)

#probabilities
p1<-function(b) 1/(1+exp(-(th+b)))
p2<-function(a,b) 1/(1+exp(-(a*th+b)))
p3<-function(a,b,g) g + (1-g)/(1+exp(-(a*th+b)))

p1_prime <- function(b) exp(-b-th)/(exp(-b-th)+1)^2
p2_prime <- function(a,b) a*exp(-a*th-b)/(exp(-a*th-b)+1)^2
p3_prime <- function(a,b,g) a*(1-g)*exp(-a*th-b)/(exp(-a*th-b)+1)^2


#models
mod1<-mirt(resp,1,itemtype="Rasch")
mod2<-mirt(resp,1,itemtype="2PL")
mod3<-mirt(resp,1,itemtype="3PL")

#parameter extraction
pars1 <- matrix(extract.mirt(mod1,'parvec'),ncol=1,byrow=TRUE)
pars2 <- matrix(extract.mirt(mod2,'parvec'),ncol=2,byrow=TRUE)
pars3 <- matrix(extract.mirt(mod3,'parvec'),ncol=3,byrow=TRUE)



for(n in 1:54){
  i1 <- i1 + (p1_prime(pars1[n,]))^2/(p1(pars1[n,])*(1-p1(pars1[n,])))
  i2 <- i2 + (p2_prime(pars2[n,1],pars2[n,2]))^2/(p2(pars2[n,1],pars2[n,2])*(1-p2(pars2[n,1],pars2[n,2])))
  i3 <- i3 + (p3_prime(pars3[n,1],pars3[n,2],pars3[n,3]))^2 / (p3(pars3[n,1],pars3[n,2],pars3[n,3])*(1-p3(pars3[n,1],pars3[n,2],pars3[n,3])))
}

se1 = 1/sqrt(i1)
se2 = 1/sqrt(i2)
se3 = 1/sqrt(i3)

plot(th,se1,main='Rasch SE vs. Theta')
plot(th,se2,main='2PL SE vs. Theta')
plot(th,se3,main='3PL SE vs. Theta')
