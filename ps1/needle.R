n <- 1000 #number of needles
d <- 1 #line spacing
l <- 1 #needle length

orient <- matrix(NA,n,4)  #matrix to hold all of the information about the needles.

orient[,1] <- runif(n,0,d)  #generates the position of the lefthand side of the needle as a function of distance from the line to its left
orient[,2] <- runif(n,-pi/2, pi/2) #generates the angular orientation of the needle
orient[,3] <- orient[,1]+l*cos(orient[,2]) #determines the horizontal span of the needle
orient[,4] <- orient[,3] >= d #checks of the righthand side of the needle will have crossed the next line
p <- sum(orient[,4])/n  
p
