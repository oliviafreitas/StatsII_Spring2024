# Question 1

# We will star by defining the Kolmogorov-Smirnov test function
ks_test <- function(data) {
  n <- length(data)
  empirical_cdf <- ecdf(data)
  D <- max((1:n)/n - empirical_cdf(data), empirical_cdf(data) - (0:(n-1))/n)
  
# Now we will calculate p-value using approximation method
  d <- D * sqrt(n)
  p_value <- sqrt(2 * pi) / d * sum(exp(-((2 * (1:100) - 1)^2 * pi^2) / (8 * d^2)))
  
  return(list(statistic = D, p_value = p_value))
}

# Set seed for reproducibility
set.seed(123)

# We will then generate 1,000 Cauchy random variables
cauchy_data <- rcauchy(1000, location = 0, scale = 1)

# We can now perform the Kolmogorov-Smirnov test
result <- ks_test(cauchy_data)

# Print the test statistic and p-value
print(result)



# Question 2

# We starting by setting the seed for reproducibility
set.seed(123)

# We can now generate the data
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75 * data$x + rnorm(200, 0, 1.5)

# Fit OLS regression using the optim function with BFGS method
ols_bfgs <- optim(par = c(0, 0), fn = function(beta) {
  sum((data$y - beta[1] - beta[2] * data$x)^2)
}, method = "BFGS")

# Extract coefficients
coefficients_bfgs <- ols_bfgs$par

# Fit OLS regression using lm function
ols_lm <- lm(y ~ x, data = data)

# Extract coefficients
coefficients_lm <- coef(ols_lm)

# We can now check equivalence
equivalence <- all.equal(coefficients_bfgs, coefficients_lm)
if (is.null(equivalence)) {
  cat("Coefficients are equivalent.\n")
} else {
  cat("Coefficients are not equivalent:\n", equivalence, "\n")
}
