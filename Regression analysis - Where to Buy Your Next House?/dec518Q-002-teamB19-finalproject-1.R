setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

house = read.csv('realestate.csv')

# Get to Know the data
names(house)
summary(house)

# Load necessary tools and packages
library(stats)
library(plyr)
library(gapminder)
library(dplyr)
library(tidyverse)
library(zipcode)
library(maps)
library(viridis)
library(ggthemes)
library(stringi)

# Create list assigning State abbreviations to Region
# NorthEast Region
NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont","New Jersey","New York","Pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)
# MidWest Region
MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin","Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD")
MW.ref <- c(MW.name,MW.abrv)
# South Region
S.name <- c("Delaware","District of Columbia","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","West Virginia","Alabama","Kentucky","Mississippi","Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL","KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)
# West Region
W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana","Utah","Nevada","Wyoming","Alaska","California","Hawaii","Oregon","Washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA","HI","OR","WA")
W.ref <- c(W.name,W.abrv)
# Create list assigning all region lists together
region.list <- list(Northeast=NE.ref,Midwest=MW.ref,South=S.ref,West=W.ref)


# 1.1 Data Preparation - Eliminating Outliers
house2 <- 
  mutate(house,state = stri_sub(ZipName,-2,-1)) %>%
  filter(Days.on.Market<300, Total.Listing.Count<1500, Median.Listing.Price<1500000)

# 1.2 Data Preparation - Assign geographical regions based on zip names
house2$regions <- sapply(house2$state,function(x) names(region.list)[grep(x,region.list)])
head(house2)

# 1.3 Data Preparation - Load USA map according to zipcodes
data(zipcode)
house2$ZipCode <- clean.zipcodes(house2$ZipCode)
head(house2)
#merge zipcodes and create lat and lon
house2 = merge(house2, zipcode, by.x="ZipCode", by.y="zip")
#load map
us<-map_data('state')


summary(house2)

# 2.1 Exploratory Data Analysis - Heat Maps
#map per region (days on market)
ggplot(house2,aes(longitude,latitude)) +
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = Days.on.Market),size=1,alpha=1) +
  theme_classic()+
  xlim(-125,-65)+ylim(20,50) + scale_fill_viridis(option = "B",direction=-1) + 
  scale_color_viridis(option = "B",direction=-1)

#total listings
ggplot(house2,aes(longitude,latitude)) +
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = Total.Listing.Count),size=1,alpha=1) +
  theme_classic()+
  xlim(-125,-65)+ylim(20,50) + scale_fill_viridis(option = "B",direction=-1) + 
  scale_color_viridis(option = "B",direction=-1)

#median listing price
ggplot(house2,aes(longitude,latitude)) +
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = Median.Listing.Price),size=1,alpha=1) +
  theme_classic()+
  xlim(-125,-65)+ylim(20,50) + scale_fill_viridis(option = "B",direction=-1) + 
  scale_color_viridis(option = "B",direction=-1)

#new listings
ggplot(house2,aes(longitude,latitude)) +
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = New.Listing.Count),size=1,alpha=1) +
  theme_classic()+
  xlim(-125,-65)+ylim(20,50) + scale_fill_viridis(option = "B",direction=-1) + 
  scale_color_viridis(option = "B",direction=-1)


# 2.2 Exploratory Data Analysis - Target variable Analysis
ggplot(house2, aes(x = Days.on.Market, fill = regions)) + geom_histogram() + facet_grid(regions ~ .) + theme(legend.position="none")

ggplot(house2,aes(longitude,latitude)) +
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = Days.on.Market),size=1,alpha=1) +
  theme_classic()+
  xlim(-125,-65)+ylim(20,50) + scale_fill_viridis(option = "B",direction=-1) + 
  scale_color_viridis(option = "B",direction=-1)


# 2.3 Exploratory Data Analysis - Variable Distribution 
# Before Log Transformation
ggplot(data=house2, aes(x=Days.on.Market))+
  geom_histogram(binwidth=2,fill= 'indianred2')

ggplot(data=house2, aes(x=Median.Listing.Price))+
  geom_histogram(binwidth=10000,fill= 'indianred2')

ggplot(data=house2, aes(x=Active.Listing.Count))+
  geom_histogram(binwidth=2,fill= 'indianred2')

# After Log Transformation
ggplot(data=house2, aes(x=Days.on.Market))+
  scale_x_log10()+
  geom_histogram(binwidth=0.05,fill= 'indianred2')

ggplot(data=house2, aes(x=Median.Listing.Price))+
  scale_x_log10()+
  geom_histogram(binwidth=0.05,fill= 'indianred2')

ggplot(data=house2, aes(x=Active.Listing.Count))+
  scale_x_log10()+
  geom_histogram(binwidth=0.1,fill= 'indianred2')


# 2.5 Exploratory Data Analysis - Correlation between Variables
#Days on Market by Active Listing Count - Linear Regression
ggplot(house2,aes(x= Active.Listing.Count  , y=Days.on.Market) ) +
  geom_point(size=0.05, col = 'indianred2')+
  ggtitle('Days on Market by Active Listing Count')+
  geom_smooth(method="lm")

#Days on Market by Active Listing Count - Loess Regression
ggplot(house2,aes(x= Active.Listing.Count  , y=Days.on.Market)) +
  geom_point(size=0.05, col = 'darkorange2')+
  ggtitle('Days on Market by Active Listing Count')+
  geom_smooth(method="loess")

#Days on Market by Average Listing Price - Linear Regression
ggplot(house2,aes(x= Avg.Listing.Price   , y=Days.on.Market)) +
  geom_point(size=0.05, col = 'indianred2')+
  ggtitle('Days on Market by Average Listing Price')+
  geom_smooth(method="lm")

#Days on Market by Average Listing Price - Loess Regression
ggplot(house2,aes(x= Avg.Listing.Price   , y=Days.on.Market)) +
  geom_point(size=0.05, col = 'darkorange2')+
  ggtitle('Days on Market by Average Listing Price')+
  geom_smooth(method="loess")

# 2.6 Exploratory Data Analysis - Boxplots
#boxplot new listings
ggplot(house2, aes(x=regions, y = New.Listing.Count, fill=regions)) +geom_boxplot()

#boxplot days per region
ggplot(house2, aes(x=regions, y = Days.on.Market, fill=regions)) +geom_boxplot()

#boxplot price per region
ggplot(house2, aes(x=regions, y=Median.Listing.Price, fill=regions)) +geom_boxplot(outlier.size = .1)+theme(axis.text.x = element_text(angle=90,hjust=1))

#boxplot price increase
ggplot(house2, aes(x=regions, y = Price.Increase.Count.M.M, fill=regions)) +geom_boxplot()

#boxplot price decrease april to march
ggplot(house2, aes(x=regions, y = Price.Decrease.Count.M.M, fill=regions)) +geom_boxplot()

#boxplot price decrease 2018 to 2019
ggplot(house2, aes(x=regions, y = Price.Decrease.Count.Y.Y, fill=regions)) +geom_boxplot()


# 2.7 Exploratory Data Analysis - Interaction Analysis
# Median Listing Price Vs. Region
ggplot(house2,aes(x=Median.Listing.Price  , y=Days.on.Market,col= regions)) +
  geom_point(size=0.05, col= 'black')+
  ggtitle('Days on Market vs Median Listing Price')+
  geom_smooth(method="lm")

# Active Listing Count Vs. Region
ggplot(house2,aes(x=Active.Listing.Count  , y=Days.on.Market)) +
  geom_point(size=0.05, col= 'black')+
  ggtitle('Days on Market vs Active Listing Count')+
  geom_smooth(method="lm")

# New Listing Count Vs. Region
ggplot(house2,aes(x= New.Listing.Count  , y=Days.on.Market)) +
  geom_point(size=0.05, col = 'black')+
  ggtitle('Days on Market by New Listing Count')+
  geom_smooth(method="lm")

# Active Listing Count Vs. New Listing Count
ggplot(house2,aes(x=Active.Listing.Count, y=New.Listing.Count)) +
  geom_point(size=0.05, col= 'black')+
  ggtitle('New Listing Count vs Active Listing Count')+
  geom_smooth(method="lm")

# 3.1 Modeling - Basic Intuitive Model
lm1 = lm(Days.on.Market~as.factor(regions) + Median.Listing.Price + Active.Listing.Count + New.Listing.Count, data = house2)
summary(lm1)
plot(lm1)


# 3.2 Modeling - Intuitive Log Transformed Model
lm2 = lm(log(Days.on.Market)~as.factor(regions)+log(Median.Listing.Price)+as.factor(regions)*log(Median.Listing.Price)+log(Active.Listing.Count)+New.Listing.Count+log(Active.Listing.Count)*New.Listing.Count, data = house2)
summary(lm2)
plot(lm2)
step(lm2,direction = 'backward')

# 3.3 Automatic Backwards Selection
FitAll = lm(log(Days.on.Market)~as.factor(regions)+log(Median.Listing.Price)+ Active.Listing.Count + New.Listing.Count+ Active.Listing.Count*New.Listing.Count +
              Price.Decrease.Count+Price.Increase.Count+Pending.Listing.Count+Avg.Listing.Price+
              Total.Listing.Count+Pending.Ratio, data = house2)
summary(FitAll)
#Total.Listing.Count came up with singularities and Price increase count is insignificant
FitAll.minus.Total.Listing.Count.Price.Increase.Count = lm(log(Days.on.Market)~as.factor(regions)+log(Median.Listing.Price)+ Active.Listing.Count + New.Listing.Count +
                                                             Price.Decrease.Count+Pending.Listing.Count+Avg.Listing.Price+
                                                             Pending.Ratio, data = house2)
summary(FitAll.minus.Total.Listing.Count.Price.Increase.Count)
step(FitAll.minus.Total.Listing.Count.Price.Increase.Count,direction = 'backward')


