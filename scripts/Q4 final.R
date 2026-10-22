# ============================================================
# Q4 - Data Preparation (completed so far)
# ============================================================

library(tidyverse)

# 1. Load data
listings <- read_csv("listings_airbnb.csv", guess_max = 30000)

# 2. Confirm no duplicate IDs
sum(duplicated(listings$id))

# 3. Check missing data for key variables
listings %>%
  select(price, bedrooms, bathrooms, review_scores_rating) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100))

# 4. Clean and log-transform price
listings <- listings %>%
  mutate(
    price_num = parse_number(price),
    log_price = log(price_num)
  )

# 5. Median imputation grouped by property_type
# (more accurate than a single overall median, since bedroom/bathroom
# counts and review scores can differ a lot by property type)
listings <- listings %>%
  group_by(property_type) %>%
  mutate(
    bedrooms = ifelse(is.na(bedrooms), median(bedrooms, na.rm = TRUE), bedrooms),
    bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm = TRUE), bathrooms),
    review_scores_rating = ifelse(is.na(review_scores_rating),
                                  median(review_scores_rating, na.rm = TRUE),
                                  review_scores_rating)
  ) %>%
  ungroup()


# ============================================================
# Q4 - Nested Regression Analysis (continues from Q4_data_preparation.R)
# ============================================================

library(tidyverse)
library(lmtest)
library(car)

# Cap extreme price outliers at 99th percentile, then re-log
cap <- quantile(listings$price_num, 0.99, na.rm = TRUE)
listings <- listings %>%
  mutate(price_capped = pmin(price_num, cap),
         log_price = log(price_capped))

model_df <- listings %>%
  select(log_price, room_type, accommodates, bedrooms, bathrooms,
         neighbourhood_cleansed, review_scores_rating, host_is_superhost) %>%
  drop_na()

# Nested (blocked) models
m1 <- lm(log_price ~ room_type + accommodates + bedrooms + bathrooms,
         data = model_df)
m2 <- lm(log_price ~ room_type + accommodates + bedrooms + bathrooms +
           neighbourhood_cleansed, data = model_df)
m3 <- lm(log_price ~ room_type + accommodates + bedrooms + bathrooms +
           neighbourhood_cleansed + review_scores_rating, data = model_df)
m4 <- lm(log_price ~ room_type + accommodates + bedrooms + bathrooms +
           neighbourhood_cleansed + review_scores_rating + host_is_superhost,
         data = model_df)

# R² and incremental R² per block
sapply(list(m1, m2, m3, m4), function(m) summary(m)$r.squared)
anova(m1, m2, m3, m4)

# Full model coefficients and significance
summary(m4)
# plot
library(ggplot2)

r2_blocks <- data.frame(
  block = factor(c("Capacity", "+ Location", "+ Reviews", "+ Superhost"),
                 levels = c("Capacity", "+ Location", "+ Reviews", "+ Superhost")),
  incremental_r2 = c(54.6, 3.4, 0.2, 0.1)
)

ggplot(r2_blocks, aes(x = block, y = incremental_r2)) +
  geom_col(fill = "#2E75B6") +
  geom_text(aes(label = incremental_r2), vjust = -0.5) +
  labs(title = "Incremental R\u00b2 by variable block (%)", x = NULL, y = NULL) +
  theme_minimal()

# Diagnostics
bptest(m4)                 # heteroskedasticity
shapiro.test(sample(residuals(m4), 5000))  # normality

# ---- Held-out evaluation (RMSE, MAE, % within ±15%) ----
set.seed(42)
train_idx <- sample(seq_len(nrow(model_df)), size = 0.8 * nrow(model_df))
train_df <- model_df[train_idx, ]
test_df  <- model_df[-train_idx, ]

m4_train <- lm(log_price ~ room_type + accommodates + bedrooms + bathrooms +
                 neighbourhood_cleansed + review_scores_rating + host_is_superhost,
               data = train_df)

pred_price   <- exp(predict(m4_train, newdata = test_df))
actual_price <- exp(test_df$log_price)

rmse <- sqrt(mean((pred_price - actual_price)^2))
mae  <- mean(abs(pred_price - actual_price))
within_15pct <- mean(abs(pred_price - actual_price) / actual_price <= 0.15)

cat("RMSE: $", round(rmse, 2), "\n")
cat("MAE: $", round(mae, 2), "\n")
cat("Within 15%:", round(within_15pct * 100, 1), "%\n")
