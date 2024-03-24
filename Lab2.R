#Lab2
library(ggplot2)
library(stargazer)
library(data.table)

setwd("D:/Nova Sbe/PHD course/Applied Methods/Week 1")
load("ceosal2.RData")
dt.ceo.salaries <- data.table(data)
rm(data)

nrow(dt.ceo.salaries)
dt.ceo.salaries[, sum(grad)] #How many CEOS have a graduate degree? or
nrow(dt.ceo.salaries[grad==1,]) 
dt.ceo.salaries[, sum(grad)]/nrow(dt.ceo.salaries) #What is the percentage of CEOs with graduate degrees? or 
dt.ceo.salaries[, mean(grad)]
dt.ceo.salaries[, mean(salary)] #What is the average CEO salary? or 
mean(dt.ceo.salaries[, salary])
#What is the mean CEO salary for those with a graduate degree?
dt.ceo.salaries[grad==1, mean(salary)]
#What is the mean CEO salary for those without a graduate degree?
dt.ceo.salaries[grad==0, mean(salary)]
#How many CEOs are have/don’t have a college degree?
dt.ceo.salaries[ , list(n_ceo=.N), by = college]
#Can we say that the mean salary is statistically different from 800?
t.test(dt.ceo.salaries[, salary], mu = 800)
#Is the average salary different for CEOs with a graduate degree and those without?
t.test(dt.ceo.salaries[, salary] ~ dt.ceo.salaries[, grad])
#alternatively
dt.ceo.salaries[ , t.test (salary ~ grad)]
#alternatively
t.test(dt.ceo.salaries[grad==0, salary] , dt.ceo.salaries[grad==1, salary])

#Creating a table with descriptive statistics
dt.ceo.salaries[, list( mean_salary = mean(salary)
                        , sd_salary = sd(salary)
                        , min_salary = min(salary)
                        , max_salary = max(salary)
                        , median_salary = median(salary))]

#compute the summary statistics for different groups
dt.ceo.salaries[, list( mean_salary = mean(salary)
                        , sd_salary = sd(salary)
                        , min_salary = min(salary)
                        , max_salary = max(salary)), by = list(grad, college)]

#stargazer function to get summary statistics for all variables in your data set
stargazer(dt.ceo.salaries, type = "text")

#get summary statistics for a subset of your observations and for a specific list of variables
stargazer(dt.ceo.salaries[grad==1, list(age, salary)], type = "text")

###Plots

#salary
qplot( data = dt.ceo.salaries
       , x = salary
       , geom = "histogram")
#age
qplot( data = dt.ceo.salaries
       , x = age
       , geom = "histogram")

#scatterplot
qplot( data = dt.ceo.salaries
       , x = sales
       , y = profits
       , geom = "point")

#barplot
qplot( data = dt.ceo.salaries
       , x = factor(grad)
       , geom = "bar")

#line
qplot( data = dt.ceo.salaries
       , x = sales
       , y = profits
       , geom = "line")

#facet wrap
qplot( data = dt.ceo.salaries,
        x = salary,
        geom = "histogram") + facet_wrap(~ grad)

#customizing plots 
#salary
qplot( data = dt.ceo.salaries
       , x = salary
       , geom = "histogram"
       , fill = factor(grad, levels = c(0,1), labels = c("Yes", "No"))) +
  theme_bw() +
  ylim(0,50) +
  xlim(0, 4000) +
  labs( title = "MY PLOT", x = "CEO Salary", y = "Number of CEOs", fill = "Grad. Degree")
