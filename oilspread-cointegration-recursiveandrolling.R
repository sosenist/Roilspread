install.packages('urca')
install.packages('tseries')
install.packages('dynlm') #for function `dynlm()`
install.packages('vars') # for function `VAR()`
install.packages('nlWaldTest') # for the `nlWaldtest()` function
install.packages('lmtest') #for `coeftest()` and `bptest()`.
install.packages('broom') #for `glance(`) and `tidy()`
install.packages('PoEdata') #for PoE4 datasets
install.packages('car')#for `hccm()` robust standard errors
install.packages('carData')
install.packages('sandwich')
install.packages('knitr') #for `kable()`
install.packages('forecast') 
install.packages('Quandl')


library(tseries) # for `adf.test()`
#install.packages("dynlm")

library(dynlm) #for function `dynlm()`
library(vars) # for function `VAR()`
library(nlWaldTest) # for the `nlWaldtest()` function
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(PoEdata) #for PoE4 datasets
library(car) #for `hccm()` robust standard errors
library(carData)
library(sandwich)
library(knitr) #for `kable()`
library(forecast) 
#install.packages("fredr")

library(urca)
library(vars)

quandl.api(sxyDyaiVG6xRnny82Sq4, http = c("GET", "PUT", "POST", "DELETE"),
           postdata = NULL)
library(Quandl)
api_key=sxyDyaiVG6xRnny82Sq4
Quandl.auth(sxyDyaiVG6xRnny82Sq4)
oil.ts <- Quandl("FRED/DCOILBRENTEU", trim_start="1987-11-10", trim_end="2019-05-01", type="zoo")
oil.tsw <-Quandl("FRED/DCOILBRENTEU", trim_start="1987-11-10", trim_end="2019-05-01", type="zoo", collapse="weekly")
oil.tsm <-Quandl("FRED/DCOILBRENTEU", trim_start="1987-11-10", trim_end="2019-05-01", type="ts", collapse="monthly")
oil2.tsm <-Quandl("FRED/DCOILWTICO", trim_start="1987-11-10", trim_end="2019-05-01", type="ts", collapse="monthly")
oil3.tsm <-Quandl("OPEC/ORB", trim_start="2004-11-10", trim_end="2019-05-01", type="zoo", collapse="daily")
dubai<-fred$series.observations(series_id = 'POILDUBUSDM')
oil4.tsm<-Quandl("ODA/POILDUB_USD", api_key="sxyDyaiVG6xRnny82Sq4", trim_start="2004-11-10", trim_end="2019-05-01", type="zoo")
plot(oil4.tsm)
lines(oil3.tsm)
#wykres 1
plot(oil.tsm, main="Historyczne ceny ropy BRENT i WTI 1987-2019", xlab="Rok", ylab="Cena, $", type="l")
lines(oil2.tsm, col="blue", type="l")
#lines(lowess(oil.tsm), col="red", lty="dashed")
legend("topleft",inset=.04, legend=c("WTI", "Brent"),
       col=c("blue", "black"), lty=1:1, cex=1.0)

#spread wykres
oil.tsms <- Quandl("FRED/DCOILBRENTEU", trim_start="2000-01-01", trim_end="2019-05-01", type="ts", collapse="monthly")

x<-oil.tsms
head(x)
str(x)
as.Date(x)
oil2.tsms <-Quandl("FRED/DCOILWTICO", trim_start="2000-01-01", trim_end="2019-05-01", type="ts", collapse="monthly")
y<-oil2.tsms
oil.spread<-(x-y)
plot(oil.spread,main="Spread cen ropy Brent-WTI", ylim=c(-15,30))
abline(h=0, col="red",lty="dashed")
#?plot
# Test ADF
########################################
summary(ur.df(oil.tsm, type="trend",lags = 1))
summary(ur.df(oil.tsm, type="drift",lags = 1))
summary(ur.df(diff(oil.tsm), type="drift", lags=1))
#nie moge odrzucic hipotezy jako, ze nie ma stacjonarnosci
#has unitroot or nonstationarity

# Test PP
#######################################
summary(ur.pp(oil.tsm, type = "Z-tau", model="constant"))
summary(ur.pp(oil.tsm, type = "Z-tau", model = "constant"))

# Test KPSS
########################################
summary(ur.kpss(oil.tsm, type = "mu"))
summary(ur.kpss(diff(oil.tsm), type = "mu"))


##SENSYTYWNOSC
oil.tsw <-Quandl("FRED/DCOILBRENTEU", trim_start="1987-11-10", trim_end="2019-05-01", type="zoo", collapse="weekly")
oil2.tsw <-Quandl("FRED/DCOILWTICO", trim_start="1987-11-10", trim_end="2019-05-01", type="zoo", collapse="weekly")
data_model<-data.frame(oil.tsw,oil2.tsw)


############# Expanding window ################
## run the loop over the total length of the series

# find the length of the series
T <- dim(data_model)[1]
start_point <- 100
interval_length <- 50
num_intervals <- round( (T-start_point) / interval_length)

test_results <- matrix(NA, num_intervals- 2, 2)
time_points <- rep(NA, num_intervals - 2)

class(time_points) <- "Date"

for (t in 1 : (num_intervals - 2) ){
  
  
  expanding_window_data <- data_model[1: (start_point + (t * interval_length)),]
  
  expanding_window_data_ts <- as.xts(expanding_window_data)
  date_frame <- index(expanding_window_data_ts)
  time_points[t] <- date_frame[length(date_frame)]
  
  print(dim(expanding_window_data)[1])
  
  K1<-if ((VARselect(expanding_window_data, lag.max = 10, type="trend")$selection[1])>=2){
    print(VARselect(expanding_window_data, lag.max = 10, type="trend")$selection[1])
  } else print("2")
  
  
  oil.jo<-ca.jo(expanding_window_data,K=K1, type="trace",ecdet = "trend", spec = "longrun")
  S = summary(oil.jo)
  test_results[t, ] <- S@teststat
}

# plot the test statistic
results_xts<- xts(test_results[,2], order.by = time_points)
plot(results_xts)

#plot with critical value
library(zoo)
library(ggplot2)

critical.value1<-25.32

z<-zoo(critical.value1,as.Date(time_points))

test_data <-
  data.frame(
    statystyka_testu = test_results[,2],
    wartosc_krytyczna = z,
    data = time_points)
    
ggplot(test_data, aes(data)) + 
  geom_line(aes(y = statystyka_testu, colour = "red")) + 
  geom_line(aes(y = wartosc_krytyczna, colour = "blue")) +
  scale_color_discrete(name="Legenda", labels=c("wartość krytyczna","statystyka testu")) +
  theme(
    legend.position = c(1,1),
    legend.justification = c("right", "top")
  )


#--------------------------------


############# Rolling window ################
## run the loop over the rolling interval over the window

# find the length of the series
T <- dim(data_model)[1]
# lenght of the window for rolling computations
rolling_window <- 520
# step for rolling 
step_rolling <- 13

start_point <- 1

num_rolling_windows <- round((T - rolling_window - 2 + step_rolling) / step_rolling)

test_results <- matrix(NA, num_rolling_windows - 1 , 2)
time_points <- rep(NA, num_rolling_windows - 1)

class(time_points) <- "Date"


for (t in 1:(num_rolling_windows-1)){
  
  data_window <- (start_point + (step_rolling * (t - 1))) : (start_point + (step_rolling * (t - 1)) + rolling_window) 
  
  
  
  expanding_window_data <- data_model[data_window,]
  
  expanding_window_data_ts <- as.xts(expanding_window_data)
  date_frame <- index(expanding_window_data_ts)
  time_points[t] <- date_frame[length(date_frame)]
  
  print(dim(expanding_window_data)[1])
  
  K1<-if ((VARselect(expanding_window_data, lag.max = 10, type="trend")$selection[1])>=2){
    print(VARselect(expanding_window_data, lag.max = 10, type="trend")$selection[1])
  } else print("2")
  
  
  oil.jo<-ca.jo(expanding_window_data,K=K1, type="trace",ecdet = "trend", spec = "longrun")
  S = summary(oil.jo)
  test_results[t, ] <- S@teststat
}

# plot the test statistic
results_xts <- xts(test_results[,2], order.by = time_points)
plot(results_xts)

z<-zoo(critical.value1,as.Date(time_points))
test_data <-
  data.frame(
    statystyka_testu = test_results[,2],
    wartosc_krytyczna = z,
    data = time_points)
length(test_results[,2])

ggplot(test_data, aes(data)) + 
  geom_line(aes(y = statystyka_testu, colour = "red")) + 
  geom_line(aes(y = wartosc_krytyczna, colour = "blue")) +
  scale_color_discrete(name="Legenda", labels=c("wartość krytyczna","statystyka testu"))+
  theme(
    legend.position = c(1,1),
    legend.justification = c("right", "top")
  )

#--------------------------------

results_xts <- xts(test_results[,2], order.by = time_points)
plot(results_xts)

z<-zoo(critical.value1,as.Date(time_points))
test_data <-
  data.frame(
    statystyka_testu = test_results[,2],
    wartosc_krytyczna = z,
    data = time_points)
length(test_results[,2])

library(ggplot2)
ggplot(test_data, aes(data)) + 
  geom_line(aes(y = statystyka_testu, colour = "red")) + 
  geom_line(aes(y = wartosc_krytyczna, colour = "blue")) +
  scale_color_discrete(name="Legenda", labels=c("wartość krytyczna","statystyka testu"))+
  theme(
    legend.position = c(1,1),
    legend.justification = c("right", "top")
  )

