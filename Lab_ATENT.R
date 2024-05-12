rm(list = ls())

library(data.table)

setwd("D:/Nova Sbe/PHD course/Applied Methods")


#Statistics

#number of observations
n_observations <- nrow(dt.wages)
cat("Number of observations:", n_observations, "\n")

#number of variables
n_variables <- ncol(dt.wages)
cat("Number of variables:", n_variables, "\n")

#from slides

#structure of the data
str(dt.wages)

#peek at the data 
head(dt.wages, 3)
tail(dt.wages, 3)

#descriptive statistics
summary(dt.wages)

library(stargazer)
stargazer(dt.wages, type ='text')
stargazer(dt.wages, type ='text', iq = TRUE)

#difference-in-means estimator when treatment is “south,” and the outcome is wage
south_wages <- dt.wages[south == TRUE]
other_wages <- dt.wages[south == FALSE]


treated_mean_wage <- mean(south_wages$wage)
control_mean_wage <- mean(other_wages$wage)

difference_in_means <- treated_mean_wage - control_mean_wage
cat("Difference-in-means estimator (ATE):", difference_in_means)

#race and gender as control variables (in “x”)
formula <- wage ~ south + nonwhite + female
regression_model <- lm(formula, data = dt.wages)
summary(regression_model)

# estimate the regression and account for potentially heterogeneous treatment effects
formula <- wage ~ south + nonwhite + south * nonwhite
regression_model <- lm(formula, data = dt.wages)
summary(regression_model)

#2-step fitted regression

 # Function to fit regression for a specific treatment group
fit_regression <- function(data, treatment_condition) {
# Subset data for the treatment condition
subset_data <- data[south == treatment_condition]

formula <- wage ~ nonwhite + female  

# Fit regression model
 model <- lm(formula, data = subset_data)

 # Extract predicted wage values for all individuals
predicted_values <- predict(model, newdata = dt.wages)

return(predicted_values)
}

# Treatment group indicator
treatment <- dt.wages[, south]

# Fit regression for treated and non-treated groups
treated_predicted <- fit_regression(dt.wages, TRUE)
control_predicted <- fit_regression(dt.wages, FALSE)

# Calculate predicted outcomes for treated and non-treated for all individuals
predicted_treated <- treated_predicted
predicted_control <- control_predicted

# Calculate ATE and ATET
ATE <- mean(predicted_treated - predicted_control)
treated_indicators <- dt.wages[, south]  # Access treatment indicator directly

# Ensure treatment indicators are 0/1 (logical) for ATET calculation
treated_indicators <- as.logical(treated_indicators)  # Convert to logical (TRUE/FALSE)

ATET <- mean(treated_indicators * (predicted_treated - predicted_control)) / mean(treated_indicators)

# Print results
cat("ATE:", ATE, "\n")
cat("ATET:", ATET, "\n")






#Explore heterogeneity (couldn't do that)

#3
#Reasons why "South" as RV might be fladed
#1. Selection bias, people who move to south might be different from whom was born
#2. Factors which might influence the decision to move and the outcome
#3. Data actualization. Do people who moved to the south still have the preliminary reason to stay. 

#Why "South" as RV might be justified
#1. Cost of living
#2. Generazility in a case of other population to relocate
#3. The main reason why people moved

#Covariative statistics
south_group <- dt.wages[south == TRUE,]  # Filter south group
not_south_group <- dt.wages[south == FALSE,]  # Filter non-south group

# Get summary statistics for both groups
summary(south_group[, nonwhite:exper])  
summary(not_south_group[, nonwhite:exper])

#The proportion of non-white individuals appears slightly higher in the south group (mean: 0.1444) compared to the non-south group (mean: 0.07965). While the difference is not substantial
#Both groups have similar medians (2 years) for tenure, suggesting a balanced distribution.
#However, the south group has a slightly higher mean (4.86 years) and a larger range (0-31 years) compared to the non-south group (mean: 5.24 years, range: 0-44 years). This could indicate a slightly higher concentration of individuals with longer tenure in the south
#Both groups show similar medians (15 years and 12 years) for experience. However, the south group has a higher mean (17.94 years) and a wider range (1-51 years) compared to the non-south group (mean: 16.51 years, range: 1-49 years). This suggests a potential imbalance, with the south group potentially having more individuals with higher experience levels.

library(MatchIt)
library(rlang)

# Estimate propensity scores
propensity_model <- glm(formula = south ~ nonwhite + female + exper, data = dt.wages, family = binomial)
dt.wages$propensity_score <- predict(propensity_model, type = "response")

# Perform matching
matched_data <- matchit(south ~ nonwhite + female + exper, data = dt.wages, method = "nearest")

matched_data

# Difference in means
treated <- subset(dt.wages, south == 1)
control <- subset(dt.wages, south == 0)

mean_treated <- mean(treated$wage)
mean_control <- mean(control$wage)
mean_difference <- mean_treated - mean_control

# Standard error
se <- sqrt(var(treated$wage)/length(treated) + var(control$wage)/length(control))

# Confidence interval
lower_bound <- mean_difference - 1.96 * se
upper_bound <- mean_difference + 1.96 * se

# Print results
cat("Estimated Treatment Effect (Difference in Means):", mean_difference, "\n")
cat("95% Confidence Interval:", lower_bound, "-", upper_bound, "\n")

#on average, individuals in the South have a lower wage compared to those not in the South
#the confidence interval ranges from approximately -2.76 to 1.18. Since this interval includes negative and positive values, it suggests some uncertainty about the true direction and magnitude of the treatment effect

column_names <- names(dt.wages)
print(column_names)

adjusted_model <- lm(wage ~ south + nonwhite + female + exper + educ + tenure, data = dt.wages)
summary(adjusted_model)

#longer job tenure is associated with higher wages