#################################################
##Exploration of collections of bernoulli variables
#################################################

set.seed(12311)
x1<-matrix(rbinom(1000,1,.5),100,10)
##Q. Compute all the correlations of the columns of this matrix (x1). What do you notice?
##A. The matrix has 1's along the diagonal (variables correlate perfectly with themselves) and the matrix is symmetric about the diagonal [cor(x,y) = cor(y,x)]
cor(x1)
##Q. Compute the row sums. What is the variation in row sums?
##A. 2.706667 - Given the context of the following question, this represents variance in total test scores.

var(rowSums(x1))
##Q. If you considered the 1s/0s correct and incorrect responses to test items (where the rows are people and the columns are items), does this seem like it could have come from a realistic scenario?
##A. Probably not, but it depends on what the test looked like (is it super random general knowledge type questions?).  One might expect a test to have "easy" and "hard" questions, which this randomly constructed matrix does not indicate.

#################################################
##Feel free to ignore this bit. I'm going to generate a new set of data. 
set.seed(12311)
th<-matrix(rnorm(100),100,10,byrow=FALSE)
diff<-matrix<-matrix(rnorm(10),100,10,byrow=TRUE)
kern<- exp(th - diff)
pr<-kern/(1+kern)
test<-matrix(runif(1000),100,10)
x2<-ifelse(pr>test,1,0)

##Q. Now go back through the above questions and see what you make of this new matrix x2. Specifically, how does it compare to the first matrix x1 in terms of whether it seems like a realistic set of item responses? What characteristics (feel free to explore other features of the data) influence your opinion on this point?
##A. Looking at histograms of row sums in x1 and x2, x2 is less clearly normally distributed - meaning that there are respondants at lower ability levels, but a sharper drop off after 7 (and 8) correct.  More than anything, the difference in the variance between the column sums (61.7 in x and 242.7 in x2) implies that all the items in x1 would have been of similar difficulty, whereas the items in x2 are of varying difficulty.  In practice, it's hard to write items that are of similar difficulty and have respondants of similar scores have wildly different response profiles. 
##Q. Compute all the correlations of the columns of this matrix (x1). What do you notice?
##A. Same as above.
cor(x2)
##Q. Compute the row sums. What is the variation in row sums?
##A. 5.111111 - The variation in row sums here is higher.  The data is also somewhat more uniformly distributed than in x1, so it makes sense that the variance would be higher in this situation.
rowSums(x2)
var(rowSums(x2))
##Q. If you considered the 1s/0s correct and incorrect responses to test items (where the rows are people and the columns are items), does this seem like it could have come from a realistic scenario?
##A.  Considering the column sums, it's clear that there are items that are more difficult (20 correct responses) and less difficult (69 or 73 correct responses), so in this sense, it is somewhat more realistic.