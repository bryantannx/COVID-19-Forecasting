library(forecast)
library(ggplot2)

#####Preprocessing data#####
xy = read.csv('covid_case_numbers.csv')

x = c(1:446) #no. of days since 1/21/20
y = as.vector(xy[275,])
y = y[5:length(y)]
y = ts(y)
plot(x, y, type="p", col="blue", main="Case numbers over time", ylab="Case numbers", xlab="Days since 1/21/20")

trainY = y[1:436] #training and test data
trainY = diff(trainY)
testY = y[437:length(y)]
testY = diff(testY)

#####Training model and finding optimal alpha#####
alpha = seq(0.01, 0.99, 0.01)
RMSE = rep(0, 99)
j = 1
for (i in alpha){
  fit = ses(trainY, alpha=i, h=10)
  RMSE[j] = accuracy(fit, testY)[2,2]
  j = j + 1
}

bestalpha = alpha[which(RMSE==min(RMSE))]
plot(alpha, RMSE)

#####Output forecast#####
fcast = ses(trainY, alpha=bestalpha, h=10)
summary(fcast)
accuracy(fcast, testY)
plotTestData = append(trainY, testY)
p1 <- autoplot(fcast) + autolayer(ts(plotTestData)) + autolayer(ts(trainY)) + ggtitle("Predicted vs. actuals differences for test set") + ylab("Differences")
p1

A = as.numeric(xy[275,]) #compare predicted vs actual case numbers
A = A[5:length(A)]
pred = rep(0, 446)
pred[1] = A[1]
for (i in seq(2,446,1)){
  pred[i] = bestalpha * A[i-1] + (1 - bestalpha) * pred[i-1]
}
p2 <- autoplot(ts(pred)) + autolayer(ts(A)) + ggtitle("Predicted vs. actual COVID-19 case numbers") + ylab("ts(P)")
p2