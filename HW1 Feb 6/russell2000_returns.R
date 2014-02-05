russell2000 = read.csv("russell2000_daily.csv")

calculateReturns <- function(price,period){
  Returns <- numeric()
  i <- 1
  while(! is.na(price[i+period])) {
    thisReturn = (price[i]-price[i+period]) / price[i+period]
    Returns <- c(Returns, thisReturn)
    i <- i+period
  }
  Returns
}

# approx 21 = 252/12 trading days in a month

dailyReturns <- calculateReturns(russell2000$Close,1)
weeklyReturns <- calculateReturns(russell2000$Close,5)
monthlyReturns <- calculateReturns(russell2000$Close,21)
yearlyReturns <- calculateReturns(russell2000$Close,252)

hist(yearlyReturns, xlim = range(-0.5,0.7), breaks=10)
