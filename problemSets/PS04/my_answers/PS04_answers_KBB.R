
# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}




# here is where you load any necessary packages
lapply(c("car", "stargazer"),  pkgTest)


setwd("~/Trintiy College/Applied Statistical Analysis 1/Problem Sheets/my_answers_PS04")




data(Prestige)
help(Prestige)




# [1](a) Create a new variable professional by recoding the variable type so 
# that professionals are coded as 1, and blue and white collar workers are 
# coded as 0 

Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
head(Prestige)


# [1](b) Run a linear model with prestige as an outcome and income, 
# professional, and the interaction of the two as predictors (Note: this is a 
# continuous dummy interaction.)

reg1 <- lm(prestige ~ income * professional, data = Prestige)
summary(reg1)

stargazer(reg1)




# [1](c) Write the prediction equation based on the result.

#prestige_hat = beta_0 + beta_1*income + beta_2*professional + beta_3*(income*professional)


#prestige_hat = reg1$coefficients[1] + 
              # reg1$coefficients[2]*Prestige$income + 
              # reg1$coefficients[3]*Prestige$professional + 
              # reg1$coefficients[4]*Prestige$income*Prestige$professional

# prestige_hat =  21.14226 + 
#                 0.003170909 * income + 
#                 37.781279955 * professional + 
#                 -0.002325709 * professional * income + 
#                 error




# [1](f) What is the effect of a $1,000 increase in income on prestige score for 
# professional occupations? In other words, we are interested in the marginal 
# effect of income when the variable professional takes the value of 1.
# Calculate the change in y associated with a $1,000 increase in income based 
# on your answer for (c)

reg1 # view the coefficients again


# for professionals we have professional = 1 which gives the following equation:

# prestige_hat = 21.142259 + 0.003171 × income + 37.781280 − 0.002326 × income
pres_hat <- reg1$coefficients[1] + reg1$coefficients[2]*Prestige$income + reg1$coefficients[3]+reg1$coefficients[4]*Prestige$income



# prestige_hat = 58.92354 + (0.003171 − 0.002326) × income
income_prof_coeff <- reg1$coefficients[2] + reg1$coefficients[4]


# $1000 change in income is associated with a
income_prof_effect <- 1000 * income_prof_coeff
income_prof_effect


#[1](g) What is the effect of changing ones occupations from non-professional to 
# professional when her income is $6,000? We are interested in the marginal 
# effect of professional jobs when the variable income takes the value of 6000. 
# Calculate the change in y based on your answer for (c)


# for non professionals we have professional = 0 which give the following equation:


# pres_hat <- reg1$coefficients[1] + reg1$coefficients[2]*Prestige$income  
# pres_hat <- 21.14226 + 0.003170909 * income
income_non_prof_coeff <- reg1$coefficients[2]


# $6000 change in income is associated with a
income_non_prof_effect <- 6000 * income_non_prof_coeff

income_non_prof_effect




# ==============================================================================


# QUESTION 2
# ----------


# [2](a) Use the results from a linear regression to determine whether having 
# these yard signs in a precinct affects vote share (e.g., conduct a hypothesis 
# test with $\alpha = .05$).


# calculate the test statistic
# beta_0 / SE
t_stat <- 0.042/0.016
# = 2.625


# calculate the degrees of freedom
df <- 131 - 2 -1
# = 128


# Compute the two-tailed p-value (2x for tails)
p_val <- pt(t_stat, df,  lower.tail = FALSE,) * 2
p_val


# [2](b)  Use the results to determine whether being next to precincts with 
# these yard signs affects vote share (e.g., conduct a hypothesis test with 
# $\alpha = .05$).


# calculate the test statistic
# beta_0 / SE
t_stat_2 <- 0.042/0.013
# = 3.230769


# Compute the two-tailed p-value (2x for tails)
p_val_2 <- pt(t_stat_2, df,  lower.tail = FALSE,) * 2
p_val_2


