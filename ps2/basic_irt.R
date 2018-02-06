################################################################
##So now we see the basic structure of item response theory models.
##they are models of human behavior: higher ability translates to a higher probability of correct response in a systematic fashion.

##number of items and people. we'll start small just so that you can see everything, but you'll want to make this bigger downstream.
ni<-50
np<-10000
##now we're going to simulate data according to this model and examine some key properties
set.seed(12311)
##first let's describe the individual level part of the model
th<-rnorm(np)
th.mat<-matrix(th,np,ni,byrow=FALSE) #these are the true abilities. we don't observe them, which will be a real problem for us downstream. but we'll not worry about that today. 
##now the item level part. this is going to look like logistic regression, meaning we will have a slope and an intercept
a<-1 ##for reasons we'll get into later, let's start with a slope equal to 1
b<-rnorm(ni)
b.mat<-matrix(b,np,ni,byrow=TRUE) #these are the item difficulties

################################################################
##now we have to put this stuff together. what we want is a probability of a correct response for a person to an item
##we're goign to use what you may know from logistic regression
inv_logit<-function(x) exp(x)/(1+exp(x))
##now the probability of a correct response is:
pr<-.25+.75*inv_logit(a*th.mat+b.mat)
##we can simulate data using that probability
##here is the kind of sneaky way i like to do it.
test<-matrix(runif(ni*np),np,ni)
resp<-ifelse(pr>test,1,0)

################################################################
##we're going to now consider a prototype of an item response model
##bump up the sample size to a reasonable level before you do this.
coefs<-list()
for (i in 1:ncol(resp)) {
    glm(resp[,i]~th,family="binomial")->mod
    coef(mod)->coefs[[i]]
}
do.call("rbind",coefs)->coefs

##let's consider them separately.
plot(density(coefs[,2])) #what do you make of this?

plot(b,coefs[,1]); abline(0,1) #how about this?

##ALWAYS REMEMBER: in practice you don't ever see th, a, or b. we are cheating here.
##This is what I meant when I said this model was "infeasible"

##in the problem set, you will be asked to recycle this code. 

