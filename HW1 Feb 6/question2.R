# import moments library to calculate mean, sample stdev, skewness and kurtosis
library("moments", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

for (returnName in names(returns)) {
  vector <- unlist(returns[returnName])
  print(returnName)
  print(paste("mean is" , mean(vector)))
  print(paste("sd is" , sd(vector)))
  print(paste("skewness is" , skewness(vector)))
  print(paste("kurtosis is" , kurtosis(vector)))
}