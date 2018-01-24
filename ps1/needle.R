n <- 1000 #number of needles
d <- 1 #line spacing
l <- 1 #needle length


orient <- matrix(NA,n,4)

orient[,1] <- runif(n,0,d)
orient[,2] <- runif(n,-pi/2, pi/2)
orient[,3] <- orient[,1]+l*cos(orient[,2])
orient[,4] <- orient[,3] >= d

p <- sum(orient[,4])/n

p
