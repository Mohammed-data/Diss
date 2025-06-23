# ============================
# Dissertation Analysis Script (Final Clean Version)
# Author: Mohammed Omar
# Date: 2025-05-27
# ============================

# Load Required Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)
library(urca)
library(lmtest)
library(vars)
library(forecast)
library(broom)
library(corrplot)
library(car)

# Step 1: Import UK Data
uk <- read_excel("~/Downloads/RStudio Disso Data.xlsx", sheet = "UK", 
                 col_types = c("date", rep("numeric", 9)))
colnames(uk) <- c("Date", "HPI", "Unemp", "GDP", "Income", "Bank_Rate", 
                  "Debt", "Permits", "Tender", "Build_Costs")

# Step 2: First Differences
uk_diff <- data.frame(
  HPI = diff(uk$HPI),
  Unemp = diff(uk$Unemp),
  GDP = diff(uk$GDP),
  Income = diff(uk$Income),
  Bank_Rate = diff(uk$Bank_Rate),
  Debt = diff(uk$Debt),
  Permits = diff(uk$Permits),
  Tender = diff(uk$Tender),
  Build_Costs = diff(uk$Build_Costs)
) %>% na.omit()

# Step 3: ADF Test Output
cat("ADF Test Results:\n")
for (var in colnames(uk_diff)) {
  test <- adf.test(uk_diff[[var]])
  cat(var, "- p-value:", round(test$p.value, 4), "\n")
}

# Step 4: KPSS Test Output
cat("\nKPSS Test Results:\n")
for (var in colnames(uk_diff)) {
  test <- ur.kpss(uk_diff[[var]], type = "mu")
  stat <- test@teststat
  crit <- test@cval["5pct"]
  decision <- ifelse(stat < crit, "Yes", "No")
  cat(var, "- KPSS stat:", round(stat, 4), ", 5% critical:", crit, ", Stationary:", decision, "\n")
}

# Step 5: Second Differences
uk_diff$Income_2diff <- c(NA, diff(uk_diff$Income))
uk_diff$Debt_2diff <- c(NA, diff(uk_diff$Debt))
uk_diff <- na.omit(uk_diff)

# Step 6: Correlation Heatmap
cor_matrix <- cor(uk_diff[, sapply(uk_diff, is.numeric)])
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         mar = c(1,1,2,1), tl.cex = 0.9)

# Step 7: Quarterly Regression
regression_data_q <- data.frame(
  HPI = uk_diff$HPI,
  Income = uk_diff$Income_2diff,
  Unemp = uk_diff$Unemp,
  GDP = uk_diff$GDP,
  Debt = uk_diff$Debt_2diff,
  Rate = uk_diff$Bank_Rate,
  Permits = uk_diff$Permits,
  Tender = uk_diff$Tender,
  Costs = uk_diff$Build_Costs
)
model_q <- lm(HPI ~ ., data = regression_data_q)
print(summary(model_q))
print(AIC(model_q))
print(BIC(model_q))
print(durbinWatsonTest(model_q))
print(bptest(model_q))
print(shapiro.test(residuals(model_q)))

# Step 8: Annual Regression
uk$Year <- format(uk$Date, "%Y")
uk_annual <- uk %>%
  group_by(Year) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

model_a <- lm(HPI ~ Income + Unemp + GDP + Debt + Bank_Rate + Permits + Tender + Build_Costs, data = uk_annual)
print(summary(model_a))
print(AIC(model_a))
print(BIC(model_a))
print(durbinWatsonTest(model_a))
print(bptest(model_a))
print(shapiro.test(residuals(model_a)))

# Step 9: Granger Causality
cat("\nGranger Causality Results:\n")
quarterly_vars <- list(
  Income_2diff = "Income",
  Unemp = "Unemployment",
  GDP = "GDP",
  Debt_2diff = "Debt",
  Bank_Rate = "Bank Rate",
  Permits = "Planning Approvals",
  Tender = "Tender Price Index",
  Build_Costs = "Building Costs"
)
for (var in names(quarterly_vars)) {
  formula <- as.formula(paste("HPI ~", var))
  test <- grangertest(formula, order = 1, data = uk_diff)
  cat(quarterly_vars[[var]], "- F:", round(test$F[2], 3), ", p-value:", round(test$`Pr(>F)`[2], 5), "\n")
}

# Step 10: VAR Model
var_data <- data.frame(
  HPI = uk_diff$HPI,
  Debt = uk_diff$Debt_2diff,
  Permits = uk_diff$Permits,
  Unemp = uk_diff$Unemp,
  Income = uk_diff$Income_2diff,
  Bank_Rate = uk_diff$Bank_Rate
) %>% na.omit()

var_model <- VAR(var_data, p = 2, type = "const")

# Step 11: IRFs
plot(irf(var_model, impulse = "Debt", response = "HPI", n.ahead = 10, boot = TRUE), main = "IRF: Debt → HPI")
plot(irf(var_model, impulse = "Permits", response = "HPI", n.ahead = 10, boot = TRUE), main = "IRF: Permits → HPI")

# Step 12: FEVD
fevd_result <- fevd(var_model, n.ahead = 10)
fevd_HPI <- fevd_result$HPI
fevd_df <- as.data.frame(fevd_HPI)
fevd_df$Step <- 1:nrow(fevd_df)
fevd_long <- pivot_longer(fevd_df, -Step, names_to = "Variable", values_to = "Share")

ggplot(fevd_long, aes(x = Step, y = Share, color = Variable)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(title = "FEVD – Contribution to HPI Forecast Error Over Time",
       x = "Forecast Step (Quarter)",
       y = "Variance Explained",
       color = "Variable") +
  theme_minimal()

# Step 13: Elasticity (Log-Log Annual)
uk_annual_log <- uk_annual %>%
  mutate(
    log_HPI = log(HPI),
    log_Income = log(Income),
    log_GDP = log(GDP),
    log_Unemp = log(Unemp),
    log_Debt = log(Debt),
    log_Bank_Rate = log(Bank_Rate),
    log_Permits = log(Permits),
    log_Tender = log(Tender),
    log_Costs = log(Build_Costs)
  )

elasticity_model <- lm(log_HPI ~ log_Income + log_Unemp + log_GDP + 
                         log_Debt + log_Bank_Rate + log_Permits +
                         log_Tender + log_Costs, 
                       data = uk_annual_log)
print(summary(elasticity_model))

# Step 14: ARIMA Forecasting
hpi_ts <- ts(uk$HPI, start = c(2000, 1), frequency = 4)
model_arima <- auto.arima(hpi_ts)
forecast_hpi <- forecast(model_arima, h = 8)
plot(forecast_hpi, main = "ARIMA Forecast of UK House Prices (Next 2 Years)", ylab = "HPI", xlab = "Year")
