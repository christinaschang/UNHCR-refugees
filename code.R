rm(list = ls())

# load and read data
library(foreign)
library(readr)

# clean & manipulate data
library(tidyverse)
library(dplyr)
library(reshape2)
library(plyr)

# plots & tables
library(ggplot2)

# time series
library(tseries)
library(dynlm)
library(urca)

#forecast
library(forecast)
library(scales)

#interactive
library(plotly)

# Load data, clean, merge

setwd("/Users/Berlin/Desktop/HertieDataScience/final project")

d1 <- read_csv("unhcr_popstats_people of concern.csv", skip = 2, na = c("","*"))
d2 <- read_csv("unhcr_popstats_refugee status.csv", skip = 2, na = c("", "*"))

View(d1)
View(d2)

# by visual inspection of the file, we don't want R to read the first two rows. 
# na argument to specify that any blanks ("") or asterisks (“*”) would be considered missing data, NA. 
# The asterisks are specified to be redacted information, based on the UNHCR website.

str(d1)
# Year correctly structured as integer
# Country destination and origin correctly structured as character
# All other variables should be numeric 

d1[4:11] <- lapply(d1[4:11], as.numeric)

summary(d1) # years from 1951 to 2016

str(d2)
# Year correctly structured as integer
# Country destination and origin correctly structured as character
# Do not need RSD procedure type information
# All other variables should be numeric 

d2[4] <- NULL

d2[4:13] <- lapply(d2[4:13], as.numeric)

summary(d2) # years from 2000 to 2016

df <- merge(d1, d2, by = c("Year", "Country / territory of asylum/residence", "Origin"))

View(df)
str(df)
summary(df) # after merge, data before 2000 drops out.

# identify missing data
apply(df,2, function(x) sum(is.na(x)))

# Exploratory Data Analysis

POC_df <- ddply(df, .(Year), summarize, year_sum_Refugees = sum(`Total Population`, na.rm = TRUE))

mean(POC_df$year_sum_Refugees)


PoC_count <- df[c(1,4:10)]

PoC_count <- melt(PoC_count, id=c("Year"))

valueBox(POC, icon = "fa-user-o"
sum(POC)

POC <- round(mean(POC_df$year_mean_Refugees), digits = 0)

POC <- format(round(mean(POC_df$year_sum_Refugees), digits = 0), 1e6, big.mark=",", scientific=FALSE)

View(POC)

Refugees <- ddply(df, .(Year), summarize, year_mean_Refugees = (sum(`Refugees (incl. refugee-like situations)`, na.rm = TRUE))
View(Refugees)


IDPs <-  ddply(df, .(Year), summarize, year_mean_IDPs = mean(`Internally displaced persons (IDPs)`, na.rm = TRUE))

Refugees_df <- ddply(df, .(Year), summarize, year_mean_Refugees = mean(`Refugees (incl. refugee-like situations)`, na.rm = TRUE))
View(Refugees_df)

mean(Refugees_df$year_mean_Refugees)

Returned_Refugees_df <- ddply(df, .(Year), summarize, year_mean_Returned.Refugees = mean(`Returned refugees`, na.rm = TRUE))

mean(Returned_Refugees_df$year_mean_Returned.Refugees)

View(Returned_Refugees_df)

View(IDPs)
sum(IDPs)

# Historical Trends

# (1) By People of Concern

# subset for only PoC category counts by year

PoC_count <- df[c(1,4:10)]

PoC_count <- melt(PoC_count, id=c("Year"))

str(PoC_count)

plot1 <- ggplot(PoC_count,aes(Year,value, na.rm = TRUE)) +
  geom_bar(aes(fill=variable),stat="identity") +
  labs(title="UNHCR Population Statistics Database",
       subtitle="Populations of Concern (2000 - 2016)",
       x="Year", 
       y="Number of People (Millions)")

ggplotly(plot1)

# Note: Starting from 2013, the number of refugees and pending cases for aslyum seekers
# increased dramatically. 

# (2) Percent Change in Total Population

Year_Pop <- aggregate(df$`Total Population`, by=list(Year = df$Year), FUN=sum, na.rm = TRUE)

Year_Pop$rate <- NA

Year_Pop$rate[which(Year_Pop$Year>2000)] = 100*(diff(Year_Pop$x)/Year_Pop[-nrow(Year_Pop),]$x)

View(Year_Pop)

plot2 <- ggplot(Year_Pop, aes(x= Year, y= rate)) + geom_line() + 
  labs(title="Percent Change in People of Concern",
       subtitle="(2000 - 2016)",
       x="Year", 
       y="Percent Change")

ggplotly(plot2)

# Note: Supports the observations made in the previous graph

# Top Destination and Origin Graphs

destination_country_total <- df %>%
  group_by(`Country / territory of asylum/residence`, Year) %>%
  summarise(Total = sum(`Total Population`))

View(destination_country_total)

top_destcountries <- destination_country_total %>%
  group_by(`Country / territory of asylum/residence`) %>%
  summarise(Total = sum(Total, na.rm = TRUE)) %>%
  top_n(20)

View(top_destcountries)

top_destcountries2 <- as.character(top_destcountries$`Country / territory of asylum/residence`)

plot3 <- destination_country_total %>%
  filter(`Country / territory of asylum/residence` %in% top_destcountries2) %>%
  ggplot(mapping = aes(x = Year, y = Total)) +
  geom_line() + coord_cartesian(ylim = c(0, 3e6)) +
  facet_wrap(~`Country / territory of asylum/residence`, ncol=4)

ggplotly(plot3)

origin_country_total <- df %>%
  group_by(Origin, Year) %>%
  summarise(Total = sum(`Total Population`))

top_origcountries <- origin_country_total %>%
  group_by(Origin) %>%
  summarise(Total = sum(Total, na.rm = TRUE)) %>%
  top_n(20)

top_origcountries2 <- as.character(top_origcountries$Origin)

plot4 <- origin_country_total %>%
  filter(Origin %in% top_origcountries2) %>%
  ggplot(mapping = aes(x = Year, y = Total)) +
  geom_line() + coord_cartesian(ylim = c(0, 1e7)) +
  facet_wrap( ~ Origin, ncol=4)

ggplotly(plot4)

# Time Series Analysis
# When events are events influenced by past events, we have a time-series
# Time series allows for delayed effects or effects that persist over time

# Y = Total PoC in Germany 
# X = Total PoC in all countries
# t = Years (2000 = 2016)

# What is the causal effect on Y of a change in X over time? 
# What is the causal effect on the total POCs in Germany of change in the total POC in the world over time? 

# Create new data frame 

Germany_Poc <- df %>% group_by(`Country / territory of asylum/residence`, Year) %>% 
  filter('Germany'  %in% `Country / territory of asylum/residence`) %>% 
  summarise(German_Total = sum(`Total Population`, na.rm = TRUE))

View(Germany_Poc)

df_ts <- merge(Germany_Poc, Year_Pop, by = "Year")
  
View(df_ts)

# declare variable to be time series using ts()
df_ts$Year <- ts(df_ts$Year)
df_ts$German_Total<- ts(df_ts$German_Total)
df_ts$x <- ts(df_ts$x)

summary(m1 <- dynlm(German_Total ~ x, data = df_ts))
# positive and substantially small coefficient, but not statistically significant

# However, can only use OLS regression in Time Series data if meets following: 

# (1) Test for Weak Dependence / Persistence
summary(dynlm(German_Total ~ L(German_Total, 1), data = df_ts))
# The rho is less than 1, so stability condition is met

plot5 <- acf(df_ts$German_Total, na.action = na.pass, lag.max = 5)

plot(plot5, main = "Autocorrelation of Total POCs in Germany")

# Correlation coefficient is statistically insignificant after 1 lag, so it is not persistent

# Conclusion: dynamically complete model

# (2) Test for Stationary 

# Unit Root - Dickey Fuller Test 
adf.test(df_ts$German_Total)
# p-value is less than .05, so it has no unit root

# Trends
par(mfrow = c(1, 2))
plot(df_ts$German_Total)
plot(df_ts$x)

# German_Total looks like a stochastic (inconsistent) trend
# x looks like a deterministic trend

# so it is non-stationary

# Need to account for trend: 

# method 1: first differencing  before regression
df_ts$d.German_Total <- c(NA, diff(df_ts$German_Total))
df_ts$d.x <- c(NA, diff(df_ts$x))

par(mfrow = c(1, 2))
plot(df_ts$d.German_Total)
plot(df_ts$d.x)

summary(m2 <- dynlm(d.German_Total ~ d.x, data = df_ts))
# positive and substantially small coefficient, but not statistically significant

# problem: we lose statistical power as we lose observations

# method 2: detrend before regression
fit1 <- lm(German_Total ~ Year, df_ts)
df_ts$resid.German_Total <- residuals(fit1)

fit2 <- lm(x ~ Year, df_ts)
df_ts$resid.x <- residuals(fit2)

summary(m3 <- dynlm(resid.German_Total ~ resid.x, data = df_ts))
# positive and substantially small coefficient, but not statistically significant

# So even after detrending, there is no statistically significant coefficient
# to show a causal effect on the total POCs in Germany 
# of change in the total POC in the world over time. 

# As we have monthly data on asylum-seekers, perhaps we can use it to predict future
# numbers of asylum-seekers. 

df3 <- read_csv("unhcr_popstats_export_asylum_seekers_monthly_2017_12_04_203715.csv", skip = 2)

View(df3)
str(df3)
summary(df3)

df3[5] <- lapply(df3[5], as.numeric)

apply(df3,2, function(x) sum(is.na(x)))

df3$Value[is.na(df3$Value)] <- 0

Germany_Total.Monthly <- df3 %>%
  group_by(`Country / territory of asylum/residence`, Year, Month) %>%
  summarise(Total = sum(Value))

Germany_monthly <- ts(Germany_Total.Monthly$Total, 
                      start = c(1999, 1), frequency = 12)

# (1) weak persistence
# (a) stability condition
summary(dynlm(Germany_monthly ~ L(Germany_monthly, 1)))
# rho is less than 1, so it meets the stability condition for weak dependency
# (b) persistence

acf(Germany_monthly, na.action = na.pass, lag.max = 40)

# (2) stationarity 
# (a) unit root
adf.test(Germany_monthly)
# (b) trends
autoplot(as.zoo(Germany_monthly), geom = "line")

plot(decompose(Germany_monthly))



stl(Germany_monthly, s.window="periodic")

plot(forecast(auto.arima(Germany_monthly), 30), 
     main = "ARIMA Forecast: Germany Asylum Seeker Arrivals", 
     ylab = "Number of Asylum Seekers", 
     xlab = "Year", ylim=c(0, 90000))

forecast(auto.arima(Germany_monthly), 24)

plot(forecast(tbats(Germany_monthly), 30), 
     main = "TBATS Forecast: Germany Asylum Seeker Arrivals", 
     ylab = "Number of Asylum Seekers", 
     xlab = "Year", ylim=c(0, 90000))

forecast(tbats(Germany_monthly), 24)
# What about if we use monthly data available in Germany? 