for (returnName in names(returns[1:16])) {
  x <- unlist(returns[returnName])
  # png for automatically saving the plot to a file
  png(filename=paste("Plots question 5/", returnName, ".png", sep=""))
  
  n=length(x)
  q.grid = (1:n)/(n+1)
  df=c(1,2,4,7,11,20)
  par(mfrow=c(3,2))
  for(j in 1:6) {
    qqplot(x, qt(q.grid,df=df[j]),
           main=paste(returnName,"df=", df[j]) )
    abline(lm(qt(c(.25,.75),df=df[j])~quantile(x,c(.25,.75))))
  }
  
  dev.off()
}