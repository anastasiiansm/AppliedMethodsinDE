#Lab3
library(ggplot2)
library(stargazer)
library(data.table)
library(Hmisc)
setwd("D:/Nova Sbe/PHD course/Applied Methods/Week 1")
list.files()
sales <- read.csv("sales-data.csv")
dt.sales <- data.table(sales)
rm(sales)

ncol(dt.sales)
nrow(dt.sales)
colnames(dt.sales)
head(dt.sales)
stargazer(dt.sales, type = "text")

summary(dt.sales)
qplot( data = dt.sales
       , x = advertising
       , y = sales
       , geom = "point") +
  theme_bw()

dt.sales[, cor(sales, advertising)]
#the correlation coefficient is statistically significant
dt.sales[, rcorr(sales, advertising)]


#Simple regression analysis
lm.sales <- lm(sales ~ advertising, data=dt.sales)
summary(lm.sales)
stargazer(lm.sales, type = "text")
#estimated regression equation
coeffs = coefficients(lm.sales)
coeffs

#Interpretation
#β0 = 147.6 gives us the average sales level when the advertising investment is zero.
#β1 = 13.4 gives us the increase in sales that results from a 1 unit (dollar) increase in advertising investment.
#R2 gives us the percentage of the variation in sales that is explained by the variation in the advertising investment.

#Plot the relationship between advertising and sales now also plotting the regression line.
qplot( data = dt.sales
       , x = advertising
       , y = sales
       , geom = c("point", "smooth")
       , method = lm) +
  theme_bw() +
  labs( x = "advertising dollars", y = "sales dollars")

#Predicted values 
#obtain the predicted sales for an advertising investment of 100
advertising = 100
sales = coeffs[1] + coeffs[2]*advertising
sales

#Alternatively, you can use the “predict” function to do this automatically. We first wrap the parameters
#inside a new data table variable called newdata.
my.budget = data.table(advertising=100)
predict(lm.sales, my.budget)
predict(lm.sales, my.budget, interval="predict")
