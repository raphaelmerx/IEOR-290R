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
  dailySP500 = calculateReturns(sp500$Close,1),
  weeklySP500 = calculateReturns(sp500$Close,5),
  # approx 21 = 252/12 trading days in a month
  monthlySP500 = calculateReturns(sp500$Close,21),
  yearlySP500 = calculateReturns(sp500$Close,252),
  
  logDailySP500 = log(calculateReturns(sp500$Close,1, 'gross')),
  logWeeklySP500 = log(calculateReturns(sp500$Close,5, 'gross')),
  logMonthlySP500 = log(calculateReturns(sp500$Close,21, 'gross')),
  logYearlySP500 = log(calculateReturns(sp500$Close,252, 'gross')),
  
  dailyRussell = calculateReturns(russell2000$Close,1),
  weeklyRussell = calculateReturns(russell2000$Close,5),
  monthlyRussell = calculateReturns(russell2000$Close,21),
  yearlyRussell = calculateReturns(russell2000$Close,252),
  
  logDailyRussell = log(calculateReturns(russell2000$Close,1, 'gross')),
  logWeeklyRussell = log(calculateReturns(russell2000$Close,5, 'gross')),
  logMonthlyRussell = log(calculateReturns(russell2000$Close,21, 'gross')),
  logYearlyRussell = log(calculateReturns(russell2000$Close,252, 'gross'))
  )

hist(returns$logYearlyRussell)

