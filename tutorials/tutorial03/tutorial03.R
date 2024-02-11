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
# lapply(c("stringr"),  pkgTest)

lapply(c("tidyverse"),  pkgTest)


# set wd for current folder

setwd(".")
setwd("C:\\Users\\User\\Documents\\GitHub")
current_directory <- getwd()
print(current_directory)


## Binary logits:

# Employing a sample of 1643 men between the ages of 20 and 24 from the U.S. National Longitudinal Survey of Youth.
# Powers and Xie (2000) investigate the relationship between high-school graduation and parents' education, race, family income, 
# number of siblings, family structure, and a test of academic ability. 

#The dataset contains the following variables:
# hsgrad Whether: the respondent was graduated from high school by 1985 (Yes or No)
# nonwhite: Whether the respondent is black or Hispanic (Yes or No)
# mhs: Whether the respondent’s mother is a high-school graduate (Yes or No)
# fhs: Whether the respondent’s father is a high-school graduate (Yes or No)
# income: Family income in 1979 (in $1000s) adjusted for family size
# asvab: Standardized score on the Armed Services Vocational Aptitude Battery test 
# nsibs: Number of siblings
# intact: Whether the respondent lived with both biological parents at age 14 (Yes or No)

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt")

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt",
                         stringsAsFactors = TRUE)

summary(graduation)

# Drop problematic cases
graduation <- graduation[-which(graduation$nsibs < 0),]

#  Option 3: 
#  Coerce from a character vector to a logical vector
graduation$hsgrad <- as.logical(as.numeric(as.factor(graduation$hsgrad))-1) 


## a) Run the logit regression
mod <- glm(hsgrad ~ ., # period functions as omnibus selector (kitchen sink additive model)
           data = graduation, 
           family = "binomial")

mod <- glm(hsgrad ~ ., 
           data = graduation, 
           family = binomial(link = "logit")) # same as above (logit is default arg)

summary(mod)

## Likelihood ratio test
#  Create a null model
nullMod <- glm(hsgrad ~ 1, # 1 = fit an intercept only (i.e. sort of a "mean") 
               data = graduation, 
               family = "binomial")

#  Run an anova test on the model compared to the null model 
anova(nullMod, mod, test = "Chisq")
anova(nullMod, mod, test = "LRT") # LRT is equivalent

##  Extracting confidence intervals (of the coefficients)
?confint
exp(confint(mod)) # Remember: transform to odds ratio using exp()

# An option for making a data.frame of confidence intervals and coefficients
confMod <- data.frame(cbind(lower = exp(confint(mod)[,1]), 
                            coefs = exp(coef(mod)), 
                            upper = exp(confint(mod)[,2])))

# Then use this to make a plot
ggplot(data = confMod, mapping = aes(x = row.names(confMod), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") + 
  coord_flip() +
  labs(x = "Terms", y = "Coefficients")

model.matrix( ~ unique(nsibs), data = graduation) 

# A better function to help with this is expand.grid()
with(graduation, expand.grid(nonwhite = unique(nonwhite),
                             mhs = unique(mhs),
                             fhs = unique(fhs)))

# Consider for instance if we had a model just consisting of factors:
mod2 <- glm(hsgrad ~ nonwhite + mhs + fhs, 
            data = graduation, 
            family = "binomial")

predicted_data <- with(graduation, expand.grid(nonwhite = unique(nonwhite),
                                               mhs = unique(mhs),
                                               fhs = unique(fhs)))

predicted_data <- cbind(predicted_data, predict(mod2, 
                                                newdata = predicted_data,
                                                type = "response",
                                                se = TRUE))
?cut
graduation$nsibs_cut <- cut(graduation$nsibs, 
                            breaks = c(0, 0.9, 1, 3, Inf), 
                            include.lowest = TRUE,
                            labels = c("None", "One", "Two_Three", "FourPlus"))

mod3 <- glm(hsgrad ~., 
            data = graduation[,!names(graduation) %in% c("nsibs", "nsibs_f")], 
            family = "binomial")
summary(mod3)
summary(mod)

# Extract confidence intervals around the estimates
confMod3 <- data.frame(cbind(lower = exp(confint(mod3)[,1]), 
                             coefs = exp(coef(mod3)), 
                             upper = exp(confint(mod3)[,2])))

# Plot the estimates and confidence intervals
ggplot(data = confMod3, mapping = aes(x = row.names(confMod3), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,8,1)) +
  labs(x = "Terms", y = "Coefficients")

# Now we can use the code in Jeff's lecture to fill out the confidence intervals 
# and predicted probability (see lecture)
predicted_data <- within(predicted_data,
                         {PredictedProb <- plogis(fit)
                         LL <- plogis(fit - (1.96 * se.fit))
                         UL <- plogis(fit + (1.96 * se.fit))
                         })


# (a) Perform a logistic regression of hsgrad on the other variables in the data set.
# Compute a likelihood-ratio test of the omnibus null hypothesis that none of the explanatory variables influences high-school graduation. 
# Then construct 95-percent confidence intervals for the coefficients of the seven explanatory variables. 
# What conclusions can you draw from these results? Finally, offer two brief, but concrete, interpretations of each of the estimated coefficients of income and intact.

# (b) The logistic regression in the previous problem assumes that the partial relationship between the log-odds of high-school graduation and number of siblings is linear. 
# Test for nonlinearity by fitting a model that treats nsibs as a factor, performing an appropriate likelihood-ratio test. 
# In the course of working this problem, you should discover an issue in the data. 
# Deal with the issue in a reasonable manner. 
# Does the result of the test change?


