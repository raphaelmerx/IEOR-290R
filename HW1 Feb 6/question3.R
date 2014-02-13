# this for loop for the empirical density function
for (returnName in names(returns[1:16])) {
  x <- unlist(returns[returnName])
  # png for automatically saving the plot to a file
  png(filename=paste("Plots question 3/", returnName, ".png", sep=""))
  
  # prob = TRUE for prababilities, not counts
  h <- hist(x, breaks=15, prob=TRUE, 
            main = paste("Density function for" , returnName))
  
  xfit <- seq(min(x),max(x), length=40)
  yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
  lines(xfit,yfit)
  
  # dev.off() saves the file and closes it
  dev.off()
}

# this for loop for the empirical CDF
for (returnName in names(returns[1:16])) {
  x <- unlist(returns[returnName])
  # png for automatically saving the plot to a file
  png(filename=paste("Plots question 3/ECDF", returnName, ".png", sep=""))
  
  # this function creates an ECDF and overlays it with a normal distr with same mean and variance
  chart.ECDF(x, main = paste("CDF for" , returnName))
  
  # dev.off() saves the file and closes it
  dev.off()
}