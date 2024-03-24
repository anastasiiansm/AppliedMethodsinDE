#lab 2b
library(ggplot2)
library(stargazer)
library(data.table)

setwd("D:/Nova Sbe/PHD course/Applied Methods/Week 2")
load("ceosal2.RData")

dt.ceo.salaries <- data.table(data)
rm(data)

nrow(dt.ceo.salaries)
dt.ceo.salaries[, sum(grad)]
nrow(dt.ceo.salaries[grad==1,])

#What is the percentage of CEOs with graduate degrees?
dt.ceo.salaries[, sum(grad)]/nrow(dt.ceo.salaries)
#or
dt.ceo.salaries[, mean(grad)]
dt.ceo.salaries[, mean(salary)]
mean(dt.ceo.salaries[, salary])
dt.ceo.salaries[grad==1, mean(salary)]
dt.ceo.salaries[grad==0, mean(salary)]
dt.ceo.salaries[ , list(n_ceo=.N), by = college]
t.test(dt.ceo.salaries[, salary], mu = 800)

#Is the average salary different for CEOs with a graduate degree and those without?
t.test(dt.ceo.salaries[, salary] ~ dt.ceo.salaries[, grad])
dt.ceo.salaries[ , t.test (salary ~ grad)]
t.test(dt.ceo.salaries[grad==0, salary] , dt.ceo.salaries[grad==1, salary])

#Creating a table with descriptive statistics
dt.ceo.salaries[, list( mean_salary = mean(salary)
                        , sd_salary = sd(salary)
                        , min_salary = min(salary)
                        , max_salary = max(salary)
                        , median_salary = median(salary))]
dt.ceo.salaries[, list( mean_salary = mean(salary)
                        , sd_salary = sd(salary)
                        , min_salary = min(salary)
                        , max_salary = max(salary)), by = list(grad, college)]
stargazer(dt.ceo.salaries, type = "text")
stargazer(dt.ceo.salaries[grad==1, list(age, salary)], type = "text")

#Histogram
qplot( data = dt.ceo.salaries
       , x = salary
       , geom = "histogram")
qplot( data = dt.ceo.salaries
       , x = age
       , geom = "histogram")
qplot( data = dt.ceo.salaries
       , x = sales
       , y = profits
       , geom = "point")
qplot( data = dt.ceo.salaries
       , x = factor(grad)
       , geom = "bar")
qplot( data = dt.ceo.salaries
       , x = sales
       , y = profits
       , geom = "line")
qplot( data = dt.ceo.salaries
       , x = salary
       , geom = "histogram") + facet_wrap(~ grad)

#Customized plots
qplot( data = dt.ceo.salaries
       , x = salary
       , geom = "histogram"
       , fill = factor(grad, levels = c(0,1), labels = c("Yes", "No"))) +
  theme_bw() +
  ylim(0,50) +
  xlim(0, 4000) +
  labs( title = "MY PLOT", x = "CEO Salary", y = "Number of CEOs", fill = "Grad. Degree")

