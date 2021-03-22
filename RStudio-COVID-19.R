# Importing the data
data <- read.csv("D:/Covid19.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

# Descriptive statistics
library(plyr)
Des <- ddply(data, c("Gender"), 
             summarise, N = length(Age),
             min = min(Age), max = max(Age),
             mean = round(mean(Age),2),
             sd = round(sd(Age),2),
             median = median(Age))

# Plotting example
ggplot(data, aes(weeks, tests))+
  geom_boxplot()+ 
  geom_smooth(method = "lm",
              se=FALSE, aes(group=1))+ 
  xlab("Weeks")+
  ylab("Number of tests")+
  theme(axis.text.x = element_text
        (angle = -90, vjust = 0.5))+
  theme(axis.text=element_text(size=12))
# Pearson's correlation
cor.test(data$tests, 
         data$cases, method="pearson")
library("ggpubr")
ggscatter(data, x = "cases", 
          y = "tests", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, 
          cor.method = "pearson",
          xlab = "Daily confirmed cases", 
          ylab = "Daily test")

#Installing forecast package
install.packages('forecast', dependencies = TRUE)
library(forecast)

# Time series formatting
data$Date <- as.Date(data$Date, format = "%d-%m-%Y")
daily_cases <- ts(data[,2], start = c(2020,55), frequency = 366)
daily_recovered <- ts(data[,5], start = c(2020,55), frequency = 366)
daily_deaths <- ts(data[,4], start = c(2020,55), frequency = 366)

# Fitting a neural network
set.seed(1985)
covid.nnetar <- nnetar(daily_cases, repeats = 20, p=10, P = 1, size=6, lambda="auto", scale.inputs=TRUE)

# Print model summary
summary(covid.nnetar$model[[1]])

# Forecasting upcoming 60 days
covid.fcast <- forecast(covid.nnetar, h=60, level = c(80, 95))

# Print forecasts summary
print(summary(covid.fcast))

# Plotting of figure 9-A
plot(covid.fcast, col="black",
     ylab="COVID-19 daily cases in IRAQ",
     xlab="Period", lwd=2, lty=1)+
  lines(covid.fcast$fitted, 
        lwd=2, col="red")
abline(v=2020.2, col="blue", lty=2)
abline(v=2020.67, col="blue", lty=2)
abline(v=2020.82, col="blue", lty=2)
legend(x= "topleft", inset=.02, 
       legend=c("Daily cases", 
                "Fitted model", "Forecasted"), 
       col=c("black", "red", "blue"), 
       lty = c(1, 1, 1), lwd = 2, cex=0.8)

# Measure performance
checkresiduals(covid.fcast)
plot(covid.fcast$residuals, ylab="Residuals", xlab="Period")
abline(h=30, col="blue", lty=2)
abline(h=-30, col="blue", lty=2)

# Fitting ARIMA model
fit_arima <- auto.arima(daily_cases, 
                        ic="aic", trace = TRUE)
fcast_arima <- forecast(fit_arima, h=60, 
                        level = c(80, 95))
autoplot(fcast_arima)
print(summary(fcast_arima))

# Fitting ETS model
fit_ets <- ets(daily_cases)
fcast_ets <- forecast(fit_ets, 
                      h=60, level = c(80, 95))
autoplot(fcast_ets)
print(summary(fcast_ets))