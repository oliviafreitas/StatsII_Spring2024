# Install and load the eha library
install.packages("eha")
library(eha)

# Load the child dataset
data(child)

# Fit the Cox PH model
fit <- coxreg(Surv(enter, exit, event) ~ sex + m.age, data = child, coxph = TRUE)

# Display the summary of the model
summary(fit)

