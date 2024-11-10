#####################
# load libraries
# set wd
# clear global .envir
#####################

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
# ex: stringr
lapply(c("stargazer"),  pkgTest)



# set wd for current folder
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("C:/Users/15kof/OneDrive/Documents/Trintiy College/Applied Statistical Analysis 1/Problem Sheets/my_answers_PS03")

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")





# QUESTION 1
#============

# (1.1) Run a regression where the outcome variable is voteshare 
# and the explanatory variable is difflog.

reg1 <- lm(voteshare ~ difflog, data = inc.sub) 

# display the summary of the regression results
summary(reg1)

# get the latex code 
stargazer(reg1)



# (1.2) Make a scatter plot of the two variables and add the regression line.

# save file
png("plots-and-tables/scatterplot_voteshare_difflog.png")


# scatter plot
plot(inc.sub$difflog, 
     inc.sub$voteshare,
     
     xlab = "Difference in Log Campaign Spending (difflog)",
     ylab = "Incumbent's Vote Share",
     main = "Scatterplot of Vote Share vs. Difference in Campaign Spending",
     pch = 21,  # circles for points
     col = "blue")  # point color

# add red regression line
abline(reg1, col = "red", lwd = 2) 


# close the PNG to save
dev.off()



# (1.3) Save the residuals of the model in a separate object 
res1 <- reg1$residuals
head(res1,12)
stargazer(head(res1,12))


# (1.4) Write the prediction equation.

# y_hat = B0 + B1 * X
y_hat1 <- reg1$coefficients[1] + reg1$coefficients[2] * inc.sub$difflog
head(y_hat1,12)

# get the latex code
stargazer(head(y_hat1,12))






# QUESTION 2
#============


# (2.1) Run a regression where the outcome variable is presvote 
# and the explanatory variable is difflog.

reg2 <- lm(presvote ~ difflog, data = inc.sub) 

# display the summary of the regression results
summary(reg2)

# get the latex code
stargazer(reg2)



# (2.2) Make a scatter plot of the two variables and add the regression line.

# save file
png("plots-and-tables/scatterplot_presvote_difflog.png")


# scatter plot
plot(inc.sub$difflog, 
     inc.sub$presvote,
     
     xlab = "Difference in Log Campaign Spending (difflog)",
     ylab = "Presidential Vote Share of Incumbent’s Party",
     main = "Scatterplot of Vote Share vs. Difference in Campaign Spending",
     pch = 21,  # circles for points
     col = "blue")  # point color

# add red regression line
abline(reg2, col = "red", lwd = 2) 


# close the PNG to save
dev.off()



# (2.3) Save the residuals of the model in a separate object 
res2 <- reg2$residuals
head(res2,12)
stargazer(head(res2,12))



# (2.4) Write the prediction equation.
# y_hat = B0 + B1 * X
y_hat2 <- reg2$coefficients[1] + reg2$coefficients[2] * inc.sub$difflog
head(y_hat2,12)
stargazer(head(y_hat2,12))





# QUESTION 3
#============

# (3.1) Run a regression where the outcome variable is voteshare 
# and the explanatory variable is presvote

reg3 <- lm(voteshare ~ presvote, data = inc.sub) 


# display the summary of the regression results
summary(reg3)

# get the latex code
stargazer(reg3)



# (3.2) Make a scatter plot of the two variables and add the regression line.

# save file
png("plots-and-tables/scatterplot_voteshare_presvote.png")


# scatter plot
plot(inc.sub$presvote, 
     inc.sub$voteshare,
     
     xlab = "Presidential Vote Share of 
     Incumbent’s Party",
     
     ylab = "Incumbent's Vote Share",
     
     main = "Scatterplot of Vote Share vs. 
     Difference in Campaign 
     Spending",
     
     pch = 21,  # circles for points
     col = "blue")  # point color

# add red regression line
abline(reg3, col = "red", lwd = 2) 


# close the PNG to save
dev.off()




# (3.3) Write the prediction equation.
# y_hat = B0 + B1 * X
y_hat3 <- reg3$coefficients[1] + reg3$coefficients[2] * inc.sub$presvote
head(y_hat3,12)
stargazer(head(y_hat3,12))





# QUESTION 4
#============

# (4.1) Run a regression where the outcome variable is 
# the residuals from Question 1 and the explanatory variable is 
# the residuals from Question 2.

reg4 <- lm(res1 ~ res2) 

# display the summary of the regression results
summary(reg4)

# get the latex code
stargazer(reg4)



# (4.2) Make a scatter plot of the two variables and add the regression line.

# save file
png("plots-and-tables/scatterplot_res1_res2.png")

# scatter plot
plot(res2, 
     res1,
     
     xlab = "res2",
     ylab = "res1",
     main = "Scatterplot of Vote Q2 Residuals vs. 
      Q1 Residuals",
     pch = 21,  # circles for points
     col = "blue")  # point color

# add red regression line
abline(reg4, col = "red", lwd = 2) 


# close the PNG to save
dev.off()



#------------

# (4.3) Write the prediction equation.
# y_hat = B0 + B1 * X
y_hat4 <- reg4$coefficients[1] + reg4$coefficients[2] * res2
head(y_hat4,12)
stargazer(head(y_hat4,12))



# QUESTION 5
#============



# (5.1)  Run a regression where the outcome variable is the incumbents 
# voteshare and the explanatory variables are difflog and presvote

reg5 <- lm(voteshare ~ difflog + presvote, data = inc.sub) 

# display the summary of the regression results
summary(reg5)

# get the latex code
stargazer(reg5)


#------------

# (5.2) Write the prediction equation.
# y_hat = B0 + B1*X + B2*X
y_hat5 <- reg5$coefficients[1] + 
          reg5$coefficients[2] * inc.sub$difflog + 
          reg5$coefficients[3] * inc.sub$presvote
head(y_hat5,12)
stargazer(head(y_hat5,12))



# (5.3)
# What is it in this output that is identical to the output in Question 4? 
#-------------------------------------------------------------------------

reg5$coefficients # Coefficient for presvote in reg5

# get the latex code
stargazer(reg5$coefficients)

# Note: This coefficient represents the effect of the variable 'presvote' 
# on the outcome variable 'voteshare', while controlling for 'difflog'.




reg4$coefficients  # Coefficient for res2 in reg4

# get the latex code
stargazer(reg4$coefficients)

# Note: This coefficient represents the effect of the residuals from the
# regression of 'presvote' on 'difflog' (denoted as 'res2') on the residuals 
# of 'voteshare' on 'difflog' (denoted as 'res1').



# Both reg5$coefficients[3] and reg4$coefficients[2] are coefficients that 
# describe the relationship between the variable presvote and the outcome variable 
# voteshare, but they do so in different contexts. Specifically, they both 
# quantify how variations in presidential vote share (whether direct or through 
# residuals) affect the vote share of incumbents:



# Why do you think this is the case?
#-----------------------------------
# The similarity between reg5$coefficients[3] and reg4$coefficients[2] arises from 
# the interconnected nature of the relationships they represent. Both coefficients 
# reflect how the presidential vote share is associated with the vote share of 
# incumbents: in reg5, this relationship is direct and considers all variance, 
# while in reg4, it focuses on the variance not explained by "difflog." The 
# coefficient from reg4 captures the relationship of the residuals—parts of the 
# votes not explained by "difflog"—while reg5 isolates the influence of "presvote" 
# on "voteshare." Additionally, if "presvote" and "difflog" are correlated, the 
# unexplained variations in "presvote" (as captured in "res2") are likely to 
# reflect similar unexplained patterns in "voteshare" (as captured in "res1"). 
# Consequently, both coefficients capture the same underlying relationship from 
# different perspectives, illustrating how different statistical models can reveal 
# similar insights about the same data relationships.
