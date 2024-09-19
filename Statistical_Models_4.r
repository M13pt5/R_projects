setwd("/Users/tymen/Downloads")


#4a
arima.sim()

arma=arima.sim(100,model=list(c(3,0,2)),sd=sqrt(2))
plot(arma,ylab="",main="ARMA(3,2), sd = sqrt(2.25), n = 100")

arma.mle = arima(arma, order = c(3,0,2),method="ML",include.mean=F)
acf(resid(arma.mle), main="Sample ACF of the residuals")
Box.test(resid(arma.mle),type="Box-Pierce")$p.value # the Portmanteau test


#4b
arma.mle = arima(arma, order = c(3,0,0),method="",include.mean=F)
acf(resid(arma.mle), main="Sample ACF of the residuals")
Box.test(resid(arma.mle),type="Box-Pierce")$p.value # the Portmanteau test







#5a

male = mdeaths
female = fdeaths
plot(male)
plot(female)

#5b

decomp=decompose(male)
plot(decomp); decomp$figure
# for example detrended and deseasonalized component is
plot((decomp$x-decomp$trend-decomp$seasonal))
plot(decomp$x-decomp$trend) # detrended component
remained = na.omit(decomp$x-decomp$trend-decomp$seasonal)

for (p in 1:5){
  for(q in 1:5){
    print(AIC(arima(remained, order=c(p,0,q))))
  }
}

#final model is ARMA(1,4)

#5c

male



#6a
library('TTR')
library('quantmod')
getSymbols("APLE",from='2021-11-19',to='2022-11-21')
stock=APLE

#6b
macd<-MACD(Cl(stock),nFast=12,nSlow=26,nSig=9,percent=FALSE); macd

n.na=sum(is.na(macd$signal)) # 33=26-1+9-1, the number of NA's in macd$signal
# the first time moment when both macd and signal are well defined is
t1=time(macd[n.na+1,]); t1 #=time(macd[nSlow+nSig-1,]) # row 34=26+9-1 
macd2=macd[time(macd)>=t1] # macd2 starting from the date t1
plot(Cl(stock))  # plot of the closing prices of the stock
plot(macd2$macd) # plot of MACD line
lines(macd2$signal,col="blue") # plot of Signal line
barChart(stock,theme=chartTheme('white'),TA=NULL) # bar chart
addMACD(fast=12,slow=26,signal=9) #type="EMA") # add MACD to it

#6c
buy<-as.numeric(ifelse(macd2$macd < macd2$signal,1,-1)); buy

price<-as.numeric(Cl(stock[time(stock)>=t1])); n=length(price)
gain1=price[n]-price[1]; gain1

 

