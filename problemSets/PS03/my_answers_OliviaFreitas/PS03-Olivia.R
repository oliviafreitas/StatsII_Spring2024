# Set the working directory to the location of your CSV files
setwd("C:/Users/User/Documents/GitHub/StatsII_Spring2024/problemSets/PS03")

# Question 1

#1

# Load necessary libraries
library(nnet)
library(dplyr)

# Let's load the dataset
data <- read.csv("gdpChange.csv")

## We should first check the few rows of the dataset
head(data)

# We can now create a new factor column for GDPWdiff categories
data$GDPWdiff_category <- cut(data$GDPWdiff, breaks = c(-Inf, 0, Inf), labels = c("negative", "positive"))

# Convert the GDPWdiff_category variable to a factor
data$GDPWdiff_category <- factor(data$GDPWdiff_category, levels = c("negative", "positive"))

# Add "no change" category for observations where GDPWdiff is zero
data$GDPWdiff_category <- factor(data$GDPWdiff_category, levels = c("no change", "positive", "negative"))

data$GDPWdiff_category[data$GDPWdiff == 0] <- "no change"

# Set "no change" as the reference category
data$GDPWdiff_category <- relevel(data$GDPWdiff_category, ref = "no change")

# Fit the unordered multinomial logit model
model <- multinom(GDPWdiff_category ~ REG + OIL, data = data)

# View the model summary
summary(model)


#2


# Load necessary library
library(MASS)

# Fit the ordered multinomial logit model
ordered_model <- polr(GDPWdiff_category ~ REG + OIL, data = data, Hess = TRUE)

# View the model summary
summary(ordered_model)


#Question 2

#a

# Load the dataset
data <- read.csv("MexicoMuniData.csv")

# Check the structure of the data
str(data)

# Run Poisson regression
poisson_model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
                     data = data,
                     family = poisson)

# Summarize the model
summary(poisson_model)



#c 

# Hypothetical values
competitive_district <- 1
marginality_06 <- 0
PAN_governor_06 <- 1

# Create a data frame with the hypothetical values
hypothetical_data <- data.frame(
  competitive.district = competitive_district,
  marginality.06 = marginality_06,
  PAN.governor.06 = PAN_governor_06
)

# Predict the mean number of visits
mean_visits <- predict(poisson_model, newdata = hypothetical_data, type = "response")

# Print the estimated mean number of visits
print(mean_visits)