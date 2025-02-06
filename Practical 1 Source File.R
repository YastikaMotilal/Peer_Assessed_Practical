# Opening code to initialize Quarto and see what the data looks like 
library(quarto)
airquality

# Finding all the rows that have missing values in airquality dataset
rows_with_na <- airquality[rowSums(is.na(airquality)) > 0, ]
print(rows_with_na) # printing all rows that have missing values

# Cleaning the data to exclude the rows with missing values
airquality_cleaned <- airquality[complete.cases(airquality), ]

# Getting the mean, standard deviation, minimum and maximum for temperature 
meanTemp <- mean(airquality_cleaned$Temp) 
sdTemp <- sd(airquality_cleaned$Temp)
minTemp <- min(airquality_cleaned$Temp)
maxTemp <- max(airquality_cleaned$Temp)
# Now printing out the output
cat("The mean temperature is", meanTemp, "and the standard deviation is", sdTemp)
cat("The minimum temperature is", minTemp, "and the maximum temperature is", maxTemp)

# Geting the mean, standard deviation, minimum and maximum for ozone level 
meanOzone <- mean(airquality_cleaned$Ozone) 
sdOzone <- sd(airquality_cleaned$Ozone)
minOzone <- min(airquality_cleaned$Ozone)
maxOzone <- max(airquality_cleaned$Ozone)
# Now printing the output
cat("The mean ozone level is", meanOzone, "and the standard deviation is", sdOzone)
cat("The minimum ozone level is", minOzone, "and the maximum ozone level is", maxOzone)
# First creating a design matrix 
X <- model.matrix(dist ~ speed, data = cars)
Y <- cars$dist

# Now finding the beta coefficients using the solve() function
beta <- solve(t(X) %*% X) %*% t(X) %*% Y
print(beta)

fit <- lm(dist ~ speed, data = cars)
summary(fit)

usethis::use_git()

