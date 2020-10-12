setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(xts)
library(dplyr)
library(ggplot2)
library(forecast)
library(gridExtra)
library(tseries)
library(astsa)
library(TSstudio)

data = read.csv("Only Prediction.csv")
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
par(mfrow=c(1,2))
#Animation Plot - Past and Future data
ggplot(data, aes(Index, Total_Likes, label = Total_Likes)) + 
  geom_hline(yintercept=100000, col = "chartreuse3")+
  geom_line() + 
  geom_line(data, mapping = aes(x= Index,y = Prediction), color = 'chocolate2')+
  geom_segment(aes(xend = 170,yend = Total_Likes), linetype = 2, col = 'blue') + 
  geom_point(size = 2, col = 'blue') + 
  geom_text(aes(label = Date), vjust = -0.75, hjust = 1.65, col = 'blue') + 
  geom_text(aes(label = Total_Likes), vjust = -0.75, hjust = .95, col = 'blue') +
  transition_reveal(Index)+ 
  coord_cartesian(clip = 'off') +
  labs(title = 'Total Likes vs Prediction on Twohappyshibas Facebook Fan Page', y = 'Total Likes', x = 'Days') 

#Animation Plot - Past and Future data
ggplot(data, aes(Index, Total_Likes-Prediction, label = Total_Likes-Prediction)) + 
  geom_line() + 
  geom_hline(yintercept=0, col = "coral1")+
  labs(title = 'Difference between Prediction and Actual Numbers', y = 'Total Likes Difference', x = 'Days') 
  
  # geom_segment(aes(xend = 170,yend = Total_Likes-Prediction), linetype = 2, col = 'blue') + 
  # geom_point(size = 2, col = 'blue') + 
  # geom_text(aes(label = Date), vjust = -0.75, hjust = 1.65, col = 'blue') + 
  # geom_text(aes(label = Total_Likes-Prediction), vjust = -0.75, hjust = 1.3, col = 'blue') +
  # transition_reveal(Index)+ 
  # coord_cartesian(clip = 'off') +


