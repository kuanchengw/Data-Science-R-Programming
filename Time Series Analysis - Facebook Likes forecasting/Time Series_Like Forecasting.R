setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(xts)
library(dplyr)
library(ggplot2)
library(forecast)
library(gridExtra)
library(tseries)
library(astsa)
library(TSstudio)

data = read.csv("Facebook Insights Data Export - Total Likes and Follows_201804-202004.csv")

#Date Formating
data$Date = data$Date %>% as.character.Date()%>%as.Date(format = "%m/%d/%y")
data$Date
data$Index <- seq.int(nrow(data))
data$Year = lubridate::year(data$Date)
data$Month = lubridate::month(data$Date)
data$Day = lubridate::day(data$Date)

#Transform data into time series data
like  = data$Total_Likes%>%
  ts(start=c(2018,04,27) ,frequency=365)

ts_plot(like, slider=T)

#Create weekly and monthly data
#Weekly data
like_weekly  = xts(data[,colnames(data) %in% c("Total_Likes")], order.by = data$Date)%>%
  to.period(period="weeks")
like_weekly = like_weekly[,"..Close"]%>%ts(start = c(2018,4,27),frequency = 52)
#Monthly data
like_monthly  = xts(data[,colnames(data) %in% c("Total_Likes")], order.by = data$Date)%>%
  to.period(period="month")
like_monthly = like_monthly[,"..Close"]%>%ts(start = c(2018,4),end =c(2020,4),frequency = 12)


#Exploratory Data Analysis#
#Seasonal plots
#Daily
Daily_seasonal = ggseasonplot(diff(like), year.labels=T) +
  ylab("Likes") +
  xlab("Day")+
  ggtitle("Seasonal plot: Daily Likes")
#Weekly
Weekly_seasonal = ggseasonplot(diff(like_weekly), year.labels=T) +
  ylab("Likes") +
  ggtitle("Seasonal plot: Weekly Likes")
#Monthly
Monthly_seasonal  = ggseasonplot(diff(like_monthly), year.labels=T) +
  ylab("Likes") +
  ggtitle("Seasonal plot: Monthly Likes")

grid.arrange(Daily_seasonal,Weekly_seasonal,Monthly_seasonal, nrow = 3)

#Polar seasonal plot (Weekly)
ggseasonplot(diff(like_weekly), polar=TRUE) +
  ylab("Likes") +
  ggtitle("Seasonal plot: Weekly Likes")

#Autocorrelation
ggAcf(diff(like),lag =90)
#The autocorrelations for small lags tend to be large and positive
#Values slowly decrease as the lags increase
#Result: There is a trend in this data
#Seasonal lags are not obvious

#Time Series Decomposition
decomposedlike <- decompose(like) 
plot(decomposedlike)
#Clear increasing trend
#No obvious seasonality

#Create seasonal-adjusted data
seasonal_adjusted = decomposedlike$x-decomposedlike$seasonal

#Plot 
autoplot(cbind("Original Data" = like,"Seasonal-Adjusted Data"=seasonal_adjusted))+
  ylab("Likes")+
  ggtitle("Original data & Seasonal adjusted data")


#Stationarity and differencing

#Use Dickey-Fuller Test to test stationarity
#1.Use original data
adf.test(like, alternative = "stationary")
#Result: High p-value-> fail to reject the null. data is non-stationary

#2.Use ndiffs function to see times of differencing
ndiffs(like)

#3.Difference the data, helping stabilise the mean of a time series by removing changes in the level of a time series
adf.test(diff(like), alternative = "stationary")
#Result: p-value is little bit higher than 0.05-> fail to reject the null. data is non-stationary

#4.Take a second-order differencing 
adf.test(diff(diff(like)), alternative = "stationary")
#Result: low p-value-> reject the null. data is stationary

#Visualize the differenced data
cbind("Original" = like,
      "Single\n differenced" = diff(like),
      "Doubly\n differenced" = diff(diff(like))) %>%
  autoplot(facets=TRUE) +
  xlab("Year") +
  ggtitle("Total Likes Data Differencing")


#Visualization
cbind("Original" = like_weekly,
      "Single\n differenced" = diff(like_weekly),
      "Doubly\n differenced" = diff(diff(like_weekly))) %>%
  autoplot(facets=TRUE) +
  xlab("Year") +
  ggtitle("Total Likes Data Differencing")
#As both the Dickey-Fuller Test and the chart showed:
#Doubly differenced data is more stable(stationary) data

#Modeling
#Transform the data to xts data
like_xts =xts(data$Total_Likes, order.by = data$Date)
colnames(like_xts)="Total_Likes"
periodicity(like_xts)
#weekly data
like_week_xts =xts(data$Total_Likes, order.by = data$Date)%>%to.period(period="weeks")
like_week_xts = like_week_xts[,"..Close"]
colnames(like_week_xts)="Total_Likes"
periodicity(like_week_xts)
#monthly data
like_month_xts=xts(data$Total_Likes, order.by = data$Date)%>%to.period(period="month")
like_month_xts = like_month_xts[,"..Close"]
like_month_xts = like_month_xts[-c(nrow(like_month_xts)),]
colnames(like_month_xts)="Total_Likes"
periodicity(like_month_xts)

#fit the data with ARIMA
fit = auto.arima(like_xts)
#Test the model
test  = sarima(like, 0,2,3)
test$ttable
#Auto ARIMA also shows Doubly differenced data has better model(lowest AICc)

#Do the same to weekly data
fit_weekly = auto.arima(like_week_xts)
test_w = sarima(like_week_xts,0,2,1)

#Do the same to monthly data
fit_monthly = auto.arima(like_month_xts)
test_m = sarima(like_month_xts,0,2,0)

#Do the monthly data to ets model
fit_ets =ets(like_month_xts)

#check residuals
#check the mean of residuals
fit %>% residuals()%>% mean()
fit_weekly %>% residuals()%>% mean()
fit_monthly %>% residuals()%>% mean()
fit_ets %>% residuals()%>% mean()
#The mean is close to 0

### Ensure residuals are white noise series
checkresiduals(fit)
checkresiduals(fit_weekly)
checkresiduals(fit_monthly)
checkresiduals(fit_ets)
#they look reasonalbly align with normal distribution shape

#Ljung Box test 
Box.test(residuals(fit),lag=10,type="Ljung")
Box.test(residuals(fit_weekly),lag=10,type="Ljung")
Box.test(residuals(fit_monthly),lag=10,type="Ljung")
Box.test(residuals(fit_ets),lag=10,type="Ljung")
#Given the high p-value, the residuals are not distinguishable from a white noise series

# Let's see if the residuals look like a normal distribution with a qq plot
qqnorm(residuals(fit)); qqline(residuals(fit))
qqnorm(residuals(fit_weekly)); qqline(residuals(fit_weekly))
qqnorm(residuals(fit_monthly)); qqline(residuals(fit_monthly))
qqnorm(residuals(fit_ets)); qqline(residuals(fit_ets))
#The Q-Q plot shows that normality is probably a reasonably good approximation

#forecast half-year 
fc_daily = forecast(fit,h= 180) 
fc_weekly = forecast(fit_weekly,h= 26) 
fc_monthly = forecast(fit_monthly,h= 6) 
fc_ets = fit_ets%>%forecast(h=6)

plot(fc_daily)+
 abline(h = 100000,col = 'red')
plot(fc_weekly,main = "Weekly ARIMA")+
  abline(h = 100000,col = 'red')
plot(fc_monthly,main = "Monthly ARIMA")+
  abline(h = 100000,col = 'red')
plot(fc_ets,main = "Monthly ETS")+
  abline(h = 100000,col = 'red')

#Model Evaluation: Training and Testing
#Evaluate daily ARIMA model
train1 <- like_xts[1:648,]
test1 <- like_xts[-c(1:648),]
fit_train1 = auto.arima(train1) 
fc1 = forecast(fit_train1, h = 90)
accuracy(fc1,test1)

#plot the forecast vs test data
fcmean = fc1%>%as.data.frame()%>%select(c('Point Forecast'))
fcmean$Date = seq(as.Date('2020-02-04'), as.Date('2020-05-03'), by = "day")
fcmean=xts(fcmean$'Point Forecast', order.by = fcmean$Date)
colnames(fcmean)="Total_Likes"
#plot
plot(fcmean,col = "blue")
lines(test1,col = 'red')

#Evaluate weekly ARIMA model
train2 <- like_week_xts[1:94,]
test2 <- like_week_xts[-c(1:94),]
fit_train2 = auto.arima(train2) 
fc2 = forecast(fit_train2, h = 12)
accuracy(fc2,test2)

#Evaluate monthly ARIMA model
train3 <- like_month_xts[1:22,]
test3 <- like_month_xts[-c(1:22),]
fit_train3 = auto.arima(train3) 
fc3 = forecast(fit_train3, h = 3)
accuracy(fc3,test3)

#Evaluate monthly ETS model
fit_train4 = ets(train3) 
fc4 = forecast(fit_train4, h = 3)
accuracy(fc4,test3)

#Cross-validation
fc_function <- function(x, h){forecast(Arima(x, order=c(0,2,3)), h=h)}
e <- tsCV(like_xts, forecastfunction=fc_function, h=180)
mse <- colMeans(e^2, na.rm = T)
data.frame(h = 1:180, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()+ggtitle("Cross-Validation")

#Prepare data for the gganimation plot
fcdata = fc_daily%>%as.data.frame()
fcdata = fcdata$'Point Forecast'%>%round()%>%as.data.frame()
fcdata$Date = seq(as.Date("2020/5/4"), as.Date("2020/10/30"), "days")
colnames(fcdata) = c("Total_Likes","Date")
past = data%>%select(c("Total_Likes","Date"))
past_future=full_join(past,fcdata)
past_future$Index <- seq.int(nrow(past_future))

library(gganimate)
#Animation Plot - Past data
ggplot(data, aes(Index, Total_Likes, label = Total_Likes)) + 
  geom_line() + 
  geom_segment(aes(xend = 738,yend = Total_Likes), linetype = 2, colour = 'blue') + 
  geom_point(size = 2) + 
  geom_text(aes(label = Date), hjust = 1.5) + 
  transition_reveal(Index)+ 
  coord_cartesian(clip = 'off') +
  labs(title = 'Total Likes on Twohappyshibas Fan Page', y = 'Total Likes', x = 'Days') 


#Animation Plot - Past and Future data
ggplot(past_future, aes(Index, Total_Likes, label = Total_Likes)) + 
  geom_hline(yintercept=100000, col = "coral1")+
  geom_line() + 
  geom_segment(aes(xend = 938,yend = Total_Likes), linetype = 2, col = 'blue') + 
  geom_point(size = 2, col = 'blue') + 
  geom_text(aes(label = Date), vjust = -0.75, hjust = .95, col = 'blue') + 
  transition_reveal(Index)+ 
  coord_cartesian(clip = 'off') +
  labs(title = 'Total Likes on Twohappyshibas Facebook Fan Page', y = 'Total Likes', x = 'Days') 

plot(fc_daily)
abline(h=100000, col = 'red')
