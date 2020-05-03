library(Quandl)
oil.ts <- Quandl("FRED/DCOILBRENTEU", trim_start="1987-11-10", trim_end="2019-04-01", type="zoo")
oil.tsw <-Quandl("FRED/DCOILBRENTEU", trim_start="1987-11-10", trim_end="2019-04-01", type="zoo", collapse="weekly")
oil.tsm <-Quandl("FRED/DCOILBRENTEU", trim_start="1987-11-10", trim_end="2019-04-01", type="ts", collapse="monthly")
matrix(oil.tsm)
plot(oil.tsm, xlab="Year", ylab="Price, $", type="l")
lines(lowess(oil.tsm), col="red", lty="dashed")
plot(decompose(oil.tsm, type="multiplicative"))

dloiltsm=diff(log(oil.tsm)) 
plot(diff(oil.tsm))
lines(lowess(diff(oil.tsm)), col="red", lty="dashed")
shapiro.test(dloiltsm)  

#histogram and a Q-Q plot
par(mfrow=c(2,1))        # set up the graphics
hist(dloiltsm, prob=T, 12)   # histogram
lines(density(dloiltsm))     # add this to the histogram
qqnorm(dloiltsm)             # normal Q-Q plot
qqline(dloiltsm) 
# difference the logged data


#correlation structure of dljj using various techniques:
lag.plot(dloiltsm,9, do.lines=F)  

#ACF and PACF
acf(dloiltsm,20)           # note the difference
acf(ts(dloiltsm),20)       # in these two lines
dloiltsm                   # print these 
ts(dloiltsm)               # to see the difference
acf(dloiltsm,20,type="partial") # partial correlation
pacf(dloiltsm,20) 

#structural decomposition of log(jj) = trend + season + error using lowess:
plot(stl(log(oil.tsm), "per"))   

oil.tsm[1]

oil.tsm1<-oil.tsm[1:372]
#Q = factor(rep(1:12,31))  
trend=time(oil.tsm1)
reg = lm(log(oil.tsm1)~0+trend, na.action=NULL)
summary(reg)
reg$coef

model.matrix(reg)
plot.ts(log(oil.tsm),type="o")
lines(fitted(reg), col=2) 

par(mfrow=c(2,1))
plot.ts(resid(reg))   # residuals 
acf(resid(reg),20) 


#2
oil.tsm2<-oil.tsm[1:240]
Q = factor(rep(1:12,20))  
trend=time(oil.tsm2)
reg1 = lm(log(oil.tsm2)~0+trend+Q, na.action=NULL)
summary(reg)

reg$coef


##TREND
trend=time(oil.tsm1)
reg2 = lm(log(oil.tsm1)~0+trend, na.action=NULL)
summary(reg)
sr<-summary(reg)
sr$coefficients

