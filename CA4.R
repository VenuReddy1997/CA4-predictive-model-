#Load R Packages 
library("ggplot2")
library('forecast')
library('tseries')
#importing csv file
df <- read.csv("diabetic.csv", header=TRUE)
#Examine the data set 
ggplot(df, aes(age,risk)) + geom_line() + xlab('age in years')  + ylab("Risk factor after eye laser treatment") 
count_ts = ts(df[, c('risk')])
#now we apply the tsclean() so that it will cleane the data set by finding the missing values and plot again
df$clean_cnt = tsclean(count_ts)
ggplot() +
  geom_line(data = df, aes(x = age, y = clean_cnt)) + ylab('risk factor after cleaning')
#Even after removing outliers, the daily data is still pretty volatile. Visually, 
#we could a draw a line through the series tracing its bigger troughs and peaks while smoothing out noisy fluctuations.
#The wider the window of the moving average, the smoother original series becomes.
df$cnt_ma = ma(df$clean_cnt, order=7) # using the clean count with no outliers
df$cnt_ma30 = ma(df$clean_cnt, order=30)


ggplot() +
  geom_line(data = df, aes(x= age, y = clean_cnt, colour = "Counts")) +
  geom_line(data = df, aes(x = age, y = cnt_ma,   colour = "average of 7 risk factors"))  +
  geom_line(data = df, aes(x = age, y = cnt_ma30, colour = "average of 30 risk factors"))  +
  ylab('risk factor of 3 types')
#Decompose Your Data 
count_ma = ts(na.omit(df$cnt_ma), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
#Stationarity check for our data set
adf.test(count_ma, alternative = "stationary")#Augmented Dickey-Fuller Test
#Autocorrelations and Choosing Model Order
Acf(count_ma, main='')#auto corelation function

Pacf(count_ma, main='')#partial acf 
count_d1 = diff(deseasonal_cnt, differences = 4) #differincing withe 4 because in the result it shows 4 accoring to ACF we choose 4.
plot(count_d1)
adf.test(count_d1, alternative = "stationary")
Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')
auto.arima(deseasonal_cnt, seasonal=FALSE)
# Fitting an ARIMA model
fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(4,0,4) Model Residuals')
fit2 = arima(deseasonal_cnt, order=c(4,0,6))
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')
arima(x = deseasonal_cnt, order = c(4, 0, 6))
fcast <- forecast(fit2, h=30)
plot(fcast)
hold <- window(ts(deseasonal_cnt), start=50)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(50:100)]), order=c(4,0,6))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))
# in the graph the blue line goes linerly so now we take the sesonal as true and again fit in the ARIMA model 
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality
seas_fcast <- forecast(fit_w_seasonality, h=30)

plot(seas_fcast)
# haer we get the ARIMA model for our data set. 
