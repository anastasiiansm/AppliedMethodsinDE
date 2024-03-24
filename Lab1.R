#Lab 1 
setwd("D:/Nova Sbe/PHD course/Applied Methods")
#some math to try
1+1
3*4
24/6
(2*10) - (3*4)
2^3
8^(1/3)
sqrt(25) # square root
log(1) # natural log
my.sum <- 10 + 10 # save result
my.sum # display result
my.sum/10

load("D:/Nova Sbe/PHD course/Applied Methods/Week 1/ceosal2.RData")
library(data.table)
dt.ceo.salaries <- data.table(data)
rm(data)
names(dt.ceo.salaries)
colnames(dt.ceo.salaries)
ncol(dt.ceo.salaries)
nrow(dt.ceo.salaries)
head(dt.ceo.salaries)
tail(dt.ceo.salaries)
View(dt.ceo.salaries)
dt.ceo.salaries[1, ] # shows first row and all columns
dt.ceo.salaries[ , salary] # shows all rows of variable "salary"
dt.ceo.salaries[1, salary] # shows first row of variable "salary"
dt.ceo.salaries[1:10, list(salary, age)] # shows first ten rows of the variables "salary" and "age"
dt.ceo.salaries[order(age)] # order ascending (default)
dt.ceo.salaries[order(-age)] # order descending
dt.ceo.salaries[age<=45,] # select only CEOs with less than 45 years
dt.young.ceo.salaries <- dt.ceo.salaries[age<=45,] # creates a new data table
dt.ceo.salaries[age<=45 & grad==1,]
dt.ceo.salaries[, log_salary:=log(salary)]
dt.ceo.salaries[, age_squared:=age^2]
dt.ceo.salaries[, log_salary:=NULL] #Deleting a variable from the data table
