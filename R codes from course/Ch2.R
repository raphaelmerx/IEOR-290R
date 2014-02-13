dat = read.csv("Stock_FX_bond.csv",header=TRUE)
names(dat)
attach(dat)
par(mfrow=c(1,2))
plot(GM_AC, type="l")
plot(F_AC)

n = dim(dat)[1]
GMReturn = GM_AC[2:n]/GM_AC[1:(n-1)] - 1
FReturn = F_AC[2:n]/F_AC[1:(n-1)] - 1
par(mfrow=c(1,1))
plot(GMReturn,FReturn)

GMReturnL = log(GM_AC[2:n]/GM_AC[1:(n-1)])
FReturnL = log(F_AC[2:n]/F_AC[1:(n-1)])
par(mfrow=c(1,3))
plot(GMReturn,FReturn)
plot(GMReturnL,FReturnL)
plot(seq(1,10),by=1)
cor1=cor(GMReturn,FReturn)
cor2=cor(GMReturnL,FReturnL)

niter = 1e5 # number of iterations
below = rep(0,niter) # set up storage
set.seed(6796)
for (i in 1:niter)
{
  r = rnorm(45,mean=.05/253,
            sd=.23/sqrt(253)) # generate random numbers
  logPrice = log(1e6) + cumsum(r)
  minlogP = min(logPrice) # minimum price over next 45 days
  below[i] = as.numeric(minlogP < log(950000))
}
mean(below)

niter = 1e5 # number of iterations
returnrate = rep(0,niter) # set up storage
set.seed(9852)
for (i in 1:niter)
{
  r = rnorm(100,mean=.05/253,
            sd=.23/sqrt(253)) # generate random numbers
  logPrice = log(1e6) + cumsum(r)
  firsthitbelow=min(c(101,which(logPrice<log(95000))))
  firsthitup=min(c(101,which(logPrice>log(110000))))
  finalprice=exp(logPrice[min(c(firsthitbelow, firsthitup, 100))])
  returnrate[i]=(finalprice-1000000)/50000/min(c(firsthitbelow, firsthitup, 100))
}
mean(returnrate)

