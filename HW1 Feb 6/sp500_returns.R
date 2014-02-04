sp500 = read.csv("sp500_daily.csv")

calculateDailyReturns <- function(Close){
  Return <- vector()
  for (i in 1:(length(Close)-1)) {
    Return[i] <- (Close[i] - Close[i+1]) / Close[i+1]
  }
  Return
}

calculateWeeklyReturns <- function(Close){
  weeklyReturns <- numeric()
  i <- 1
  while(! is.na(Close[i+5])) {
    thisReturn = (Close[i]-Close[i+5]) / Close[i+5]
    weeklyReturns <- c(weeklyReturns, thisReturn)
    i <- i+5
  }
  weeklyReturns
}

# approx 21 = 252/12 trading days in a month
calculateMonthlyReturns <- function(Close){
  monthlyReturns <- numeric()
  i <- 1
  while(! is.na(Close[i+21])) {
    thisReturn = (Close[i]-Close[i+21]) / Close[i+21]
    monthlyReturns <- c(monthlyReturns, thisReturn)
    i <- i+21
  }
  monthlyReturns
}

calculateYearlyReturns <- function(Close){
  yearlyReturns <- numeric()
  i <- 1
  while(! is.na(Close[i+252])) {
    thisReturn = (Close[i]-Close[i+252]) / Close[i+252]
    yearlyReturns <- c(yearlyReturns, thisReturn)
    i <- i+252
  }
  yearlyReturns
}


dailyReturns <- calculateDailyReturns(sp500$Close)
weeklyReturns <- calculateWeeklyReturns(sp500$Close)
monthlyReturns <- calculateMonthlyReturns(sp500$Close)
yearlyReturns <- calculateYearlyReturns(sp500$Close)

hist(yearlyReturns, xlim = range(-0.5,0.7), breaks=10)
