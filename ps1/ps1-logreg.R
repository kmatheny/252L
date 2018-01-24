load('ps1-logreg.Rdata')
glm(y1~x,df,family="binomial")->m1
glm(y2~x,df,family="binomial")->m2

#QA: How would you compare the association between y1/y2 & x? 
#AA: 
#QB: How would you interpret the regression coefficients from (say) m1? 
#AB: 
#QC: Do m1 and m2 show equivalent model fit? Can you notice anything peculiar about either y1 or y2 (in terms of their association with x)?
#AC: 

plot(df$x,df$y1)
curve(predict(m1,data.frame(x=x),type="resp"),add=TRUE)
plot(df$x,df$y2)
curve(predict(m2,data.frame(x=x),type="resp"),add=TRUE)
