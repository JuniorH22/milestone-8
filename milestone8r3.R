library(readr)
fars <- read_csv("fars_clean.csv", show_col_types = FALSE)
# numeric columns  and using the cleaned complete cases only.
fars$persons <- as.numeric(fars$persons)
fars$fatals  <- as.numeric(fars$fatals)
fars$hour    <- as.numeric(fars$hour)

keep <- complete.cases(fars[, c("fatals", "persons", "hour")])
fars2 <- fars[keep, c("fatals", "persons", "hour")]

# Fitting the linear model 
model <- lm(fatals ~ persons + hour, data = fars2)
sm <- summary(model)

# Printing coefficients and simple fit metrics to the console to check my work.
coef_table <- data.frame(term = rownames(sm$coefficients), sm$coefficients, row.names = NULL)
print(coef_table)
cat("R-squared:", round(sm$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(sm$adj.r.squared, 4), "\n")
cat("Residual SE:", round(sm$sigma, 4), "\n")

# scatterand  regression line - i will explain why its looks like this in the docx
plot(fars2$persons, fars2$fatals,
  main = "Persons vs Fatalities",
  xlab = "Persons",
  ylab = "Fatalities"
)
abline(lm(fatals ~ persons, data = fars2))

# Residual hist
hist(resid(model),
  main = "Residuals",
  xlab = "Residual"
)

plot(fitted(model), resid(model),
  main = "Fitted vs Residual",
  xlab = "Fitted fatals",
  ylab = "Residual"
)
abline(h = 0)
