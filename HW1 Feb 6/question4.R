for (returnName in names(returns[1:16])) {
  vector <- unlist(returns[returnName])
  # the shapiro test needs less than 5000 values to work
  if (length(vector)>5000) {
    # take a representative sample of the vector
    vector <- sample(vector,5000)
  }
  
  W = shapiro.test(vector)$statistic
  pValue = shapiro.test(vector)$p.value
  
  print(paste(returnName,'W=',W,
              'p-value = ',pValue ))
}

# result: the ones that have a p-value higher than 0.01 are
# yearlySP500, logYearlySP500, yearlyRussell, logYearlyRussell
# for those returns, the null hypothises is accepted. 
