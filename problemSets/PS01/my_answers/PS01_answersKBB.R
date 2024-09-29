#####################
# load libraries
setwd("C:/Users/15kof/OneDrive/Documents/Trintiy College/Applied Statistical Analysis 1/Problem Sheets/my_answers/ ")

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
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)



#####################
# Problem 1
#####################

# assign the data
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98,
       80, 97, 95, 111, 114, 89, 95, 126, 98)



### 90% Confidence level ###

# first define the point estimate (mean)
y_mean <- mean(y) # Point estimate

# next define the standard error
y_err <- sd(y)/sqrt(length((y)))

# calculate the degrees of freedom
deg_free <- length(y) - 1

# as n<30 the clt cannot be used
t_stat_1 <- qt(1 - 0.05, deg_free)

# upper bound
upper_90 <- y_mean + t_stat_1*y_err
# lower bound
lower_90 <- y_mean - t_stat_1*y_err

# put them together and output
confint90 <- c(round(lower_90, 3), "," , round(upper_90,3) ) 

cat("The 90% confidence interval for the average student IQ in the school is:\n",
    "[", confint90, "]" )


#------------------------------------------
# after completing step 1 and 2 of the hypothesis test on the .tex file
# STEP 3
# calculate the t-statistic, mu0 = 100
t_stat_2 <- (y_mean - 100) / y_err
# t = -0.5957439 


#STEP 4
# calculate the p-value
p_val <- pt(t_stat_2, deg_free, lower.tail = FALSE)
# p = 0.7215383
p_val


# compute the hypothesis test to verify
hyp_test <- t.test(y,
                   mu=100,
                   conf.level = 0.95,
                   alternative =  "greater")

# show the t statistic
hyp_test$statistic
# t = -0.5957439 

# show the p value
hyp_test$p.value
# p = 0.7215383


# we fail to reject the null as the p-value is greater than 0.05

#####################
# Problem 2
#####################

# read in the data
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

# assign the variable names to investigate
var <- c("Y", "X1", "X2", "X3")

# save the plot
png('images/pairs_plot.png')

# pairs plot
pairs(expenditure[var],
      lower.panel = NULL,  # remove lower triangle
      panel = panel.smooth,  # smooth line of best fit to help visualise
)
# finish saving the graph
dev.off()


#------------------------------------


# define the names of the regions
regions <- c("Northeast", "North Central", "South", "West")

# save the plot
png('images/Y_against_Region.png')

# box plot
boxplot(Y ~ Region, 
        data = expenditure,
        main = "Per Capita Expenditure on Housing Assistance by Region",
        xlab = "Region", 
        ylab = "Per Capita Expenditure (Y)",
        col = "lightblue",
        xaxt = "n", # remove the axis labels
        outpch=21, # make the outliers solid red circles
        outbg="red")

# add the axis labels
axis(1, at=1:4, labels=regions)

# add legend
legend("bottomleft",
       pch = 21,          
       pt.bg = "red",     
       legend = "Outliers")

# finish saving the graph
dev.off()

#------------------------------------
# save the plot
png('images/Y_against_X1.png')

# plot of X1 against Y
plot(Y ~ X1, data = expenditure,
     pch = 19,
     cex = 1.5, # increase the size of the points
     xlab = "per capita personal income in state (X1)", 
     ylab = "expenditure on housing assistance(Y)", 
     main = "The relationship between Y and X1")

# finish saving the graph
dev.off()


# save the plot
png('images/Y_against_X1_with_legend.png')

# plot of X1 against Y coloured by region
plot(Y ~ X1, data = expenditure,
        col = Region, # colour is region repentant
        pch = Region, # shape is region dependent 
        cex = 1.5, # increase the size of the points
        xlab = "per capita personal income in state (X1)", 
        ylab = "expenditure on housing assistance(Y)", 
        main = "The relationship between Y and X1\n grouped by region"
     )

# add legend
legend("topleft",
       legend = regions,
       pch = unique(expenditure$Region),
       col = unique(expenditure$Region)
       )

# finish saving the graph
dev.off()





