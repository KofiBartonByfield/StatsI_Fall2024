rm(list=ls())
getwd()
setwd("C:/Users/15kof/OneDrive/Documents/Trintiy College/Applied Statistical Analysis 1/Problem Sheets/my_answers_PS02")

# import the relevant library
library("stargazer")


# construct the data as a table
# create matrix with 3 columns and 2 rows
bribe_data <- matrix(c(14,6,7,7,7,1), ncol=3, byrow=TRUE)

# specify the column names and row names of matrix
colnames(bribe_data) <- c('Not Stopped', 
                          'Bribe requested',  
                          'Stopped/given warning')

rownames(bribe_data) <- c('Upper class','Lower class ')


# display
bribe_data


# Calculating the χ^2 test statistic
#-----------------------------------

# save f_o
fo <- bribe_data

#calculate f_e
grand_total <-  sum(bribe_data)

# pre-allocate empty vector
fe <- c()

# using f_e = row total / grand total * column total
for (i in 1:nrow(bribe_data)){
  
  for (j in 1:ncol(bribe_data)){
    
    fe_i <-   sum(bribe_data[,j]) / grand_total * sum(bribe_data[i,])
    
    fe <- append(fe, fe_i)
  }
}

# transform fe into the same dimensions as fo
fe <- matrix(fe, ncol=3, byrow=TRUE)

# specify the column names and row names of matrix
colnames(fe) <- c('Not Stopped', 
                  'Bribe requested',  
                  'Stopped/given warning')

rownames(fe) <- c('Upper class','Lower class ')


# display
fe

# obtain latex code for the tables
stargazer(fe)
stargazer(fo)


# to calculate the test statistic
test_stat <- sum( (fe - fo)^2 / fe)
# 3.791168
test_stat


# run R's built in Chi squared test
chi_test <- chisq.test(bribe_data)
chi_test$statistic
#  3.791168 


# compare the results to verify our method
chi_test$statistic
test_stat
#  3.791168


# calculate the p-value from the test statistic 
#-----------------------------------------------

# degrees of freedom
degf <- (nrow(bribe_data) -1) * (ncol(bribe_data)-1)
# 2

# run R's built in Chi p-value squared test 
pchisq(test_stat , 
        df = degf,
        lower.tail=FALSE)
# 0.1502306


# Calculate the standardized residuals for each cell
#----------------------------------------------------

# pre-allocate empty vector
z <- c()

# calculate each element of the residuals   
for (i in 1:nrow(bribe_data)){
  
  for (j in 1:ncol(bribe_data)){
    
    se <- sqrt(fe[i,j] *
               (1 - sum(bribe_data[,j]) / grand_total) *
               (1 - sum(bribe_data[i,]) / grand_total) 
               )
    
    z_i <- (fo[i,j]-fe[i,j]) / se 
    
    z <- append(z, z_i)

    }
}

# transform the residuals into the same dimensions as bribe data
stand_res <- matrix(z, ncol=3, byrow=TRUE)

# specify the column names and row names of matrix
colnames(stand_res) <- c('Not Stopped', 
                         'Bribe requested',  
                         'Stopped/given warning')

rownames(stand_res) <- c('Upper class', 'Lower class ')

# display
stand_res

# obtain latex code for table
stargazer(stand_res)


# QUESTION 2
#============

# read in the data
econ <- read.csv(url("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"))

# have a look at the data
str(econ)

# save the structure for the latex file
sink("images-tables/structure.txt")
str(econ)
sink() 


# Run the bivariate regression
biv_reg <- lm(water ~ reserved, data = econ) 


# display the summary of the regression results
summary(biv_reg)

# display the coefficients
biv_reg$coefficients
#  (Intercept)    reserved 
#      14.738318    9.252423 


# save the lm for the latex file
sink("images-tables/lm.txt")
summary(biv_reg)
sink() 


# save the lm coefficients for the latex file
sink("images-tables/lm_coeff.txt")
biv_reg$coefficients
sink()  


# save the plot for latex
png('images-tables/regression_plot.png')

    # plot the regression
    plot(econ$reserved, econ$water,
         xlab = "Reservation Policy",
         ylab = "New or Repaired Water Facilities",
         main = paste("Bivariate Regression of Drinking Water",
                      "Facilities on Reservation Policy Measures", 
                      sep = "\n"),
         xaxt = "n"    # suppress the x-axis labels
    )
    
    # regression line
    abline(biv_reg, col = "blue")
    
    # custom x-axis labels
    axis(1, at = c(0, 1), labels = c("Not Reserved", "Reserved"))
    
    # plot points for intercepts with red and green filled circles
    points(0, biv_reg$coefficients[1], 
           pch = 16, 
           col = "red", 
           cex = 1.5)  # Not Reserved
    
    points(1, biv_reg$coefficients[2] + biv_reg$coefficients[1], 
           pch = 16, 
           col = "green", 
           cex = 1.5)  # Reserved
    
    # legend
    legend("topleft",
           legend = c("Regression Line", 
                      round(biv_reg$coefficients[1],3), 
                      round(biv_reg$coefficients[2] + biv_reg$coefficients[1],3)
                      ),
           
           col = c("blue", "red", "green"),
           pch = c(NA, 16, 16), 
           lty = c(1, NA, NA),  
           bty = "n"  # removes legend box
    )
    


dev.off()





















