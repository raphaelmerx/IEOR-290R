# data downloaded from Yahoo finance as CSV
sp500 = read.csv("sp500_daily.csv")
russell2000 = read.csv("russell2000_daily.csv")

# price is a vector made of closing prices
# period is changed to calculate daily, weekly, monthly or yearly prices
# returnType for gross or net returns
calculateReturns = function(price,period=1,returnType='net'){
  Returns = numeric()
  i = 1
  # while we can calculate a return
  while(! is.na(price[i+period])) {
    if (returnType=='net') {
      # the price vector is sorted by most recent -> oldest
      thisReturn = (price[i]- price[i+period]) / price[i+period]
    } else if (returnType=='gross') {
      thisReturn = (price[i]) / price[i+period]
    }
    Returns = c(Returns, thisReturn)
    i = i+period
  }
  Returns
}

# put returns in a list to iterate over them more easily
returns = list(
  #rev() to have the time serie in order
  dailySP500 = rev(calculateReturns(sp500$Close,1)),
  weeklySP500 = rev(calculateReturns(sp500$Close,5)),
  # approx 21 = 252/12 trading days in a month
  monthlySP500 = rev(calculateReturns(sp500$Close,21)),
  yearlySP500 = rev(calculateReturns(sp500$Close,252)),
  
  logDailySP500 = rev(log(calculateReturns(sp500$Close,1, 'gross'))),
  logWeeklySP500 = rev(log(calculateReturns(sp500$Close,5, 'gross'))),
  logMonthlySP500 = rev(log(calculateReturns(sp500$Close,21, 'gross'))),
  logYearlySP500 = rev(log(calculateReturns(sp500$Close,252, 'gross'))),
  
  dailyRussell = rev(calculateReturns(russell2000$Close,1)),
  weeklyRussell = rev(calculateReturns(russell2000$Close,5)),
  monthlyRussell = rev(calculateReturns(russell2000$Close,21)),
  yearlyRussell = rev(calculateReturns(russell2000$Close,252)),
  
  logDailyRussell = rev(log(calculateReturns(russell2000$Close,1, 'gross'))),
  logWeeklyRussell = rev(log(calculateReturns(russell2000$Close,5, 'gross'))),
  logMonthlyRussell = rev(log(calculateReturns(russell2000$Close,21, 'gross'))),
  logYearlyRussell = rev(log(calculateReturns(russell2000$Close,252, 'gross')))
  )

for (returnName in names(returns[1:16])) {
  x <- unlist(returns[returnName])
  # png for automatically saving the plot to a file
  png(filename=paste("Plots question1/", returnName, ".png", sep=""))

  plot.ts(x, main = returnName)
  
  # dev.off() saves the file and closes it
  dev.off()
}