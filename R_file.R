# load necessary libraries for data analysis
library(tidyverse)
library(car) # needed for vif


# the data is stored in a personal github repository and loaded from there
url1 <- "https://raw.githubusercontent.com/Russell-97/NBA-Teams-Win-Determinants/refs/heads/main/Data/Team%20Stats%20Per%20Game.csv"
url2 <- "https://raw.githubusercontent.com/Russell-97/NBA-Teams-Win-Determinants/refs/heads/main/Data/Team%20Summaries.csv"
per_game_data <- read_csv(url1)
summary_data <- read_csv(url2)

# Drop the League Average rows from both datasets
per_game_data  <- per_game_data  |> filter(team != "League Average")
summary_data <- summary_data |> filter(team != "League Average")

# Join wins (w) from summary dataset into per_game dataset
merged_data <- per_game_data |>
  left_join(
    summary_data |> select(season, abbreviation, w),
    by = c("season", "abbreviation")
  )

head(merged_data)

data <- merged_data |>
  select(season, team, abbreviation, w, pts_per_game, ast_per_game, fg_percent, 
         stl_per_game, blk_per_game, trb_per_game) |>
  drop_na()   # remove rows with any missing value in these columns

head(data)

summary_stats <- data |>
  select(-c(season, team, abbreviation)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  summarise(
    n      = n(),
    mean   = round(mean(value,   na.rm = TRUE), 3),
    sd     = round(sd(value,     na.rm = TRUE), 3),
    min    = round(min(value,    na.rm = TRUE), 3),
    median = round(median(value, na.rm = TRUE), 3),
    max    = round(max(value,    na.rm = TRUE), 3),
    .groups = "drop"
  )

summary_stats

scatter_df <- data %>%
  pivot_longer(
    cols = c(pts_per_game, ast_per_game, fg_percent,
             stl_per_game, blk_per_game, trb_per_game),
    names_to = "predictor",
    values_to = "value"
  )

ggplot(scatter_df, aes(x = value, y = w)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ predictor, scales = "free_x", ncol = 3) +
  labs(
    title = "Season Wins versus Candidate Predictors",
    x = "Predictor value",
    y = "Season wins"
  ) +
  theme_minimal()

# Fitting the separate models for comparison
reg_offensive  <- lm(w ~ pts_per_game + ast_per_game + fg_percent, data = data)
reg_defensive  <- lm(w ~ stl_per_game + blk_per_game + trb_per_game, data = data)

# Fitting the full model to hold all metrics constant
reg_full <- lm(w ~ pts_per_game + ast_per_game + fg_percent + stl_per_game + blk_per_game + trb_per_game, data = data)

print("Offensive model")
print(summary(reg_offensive))

print("Defensive model")
print(summary(reg_defensive))

print("Full model")
print(summary(reg_full))

vif(reg_full)

residuals_full <- resid(reg_full)
fitted_vals <- fitted(reg_full)

# 1. Plot: Residuals against Fitted Values
plot(fitted_vals, residuals_full, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", 
     ylab = "Residuals",
     pch = 20)
abline(h = 0, col = "red", lty = 2)

# 2. Plot: Residuals against a Covariate (using fg_percent as an example)
plot(data$fg_percent, residuals_full, 
     main = "Residuals vs Covariate (fg_percent)",
     xlab = "Field Goal Percentage", 
     ylab = "Residuals",
     pch = 20)
abline(h = 0, col = "red", lty = 2)

# 3. Plot: Normal Quantile Plot
qqnorm(residuals_full)
qqline(residuals_full, col = "red", lty = 2)

# Standardizing the Covariates so that they all are on the same scale for comparison purposes
data_std <- data |>
  mutate(across(c(pts_per_game, ast_per_game, fg_percent, 
                  stl_per_game, blk_per_game, trb_per_game), scale))

# Fit the FULL model on the standardized data to hold all metrics constant
reg_full_std <- lm(w ~ pts_per_game + ast_per_game + fg_percent + 
                     stl_per_game + blk_per_game + trb_per_game, data = data_std)

# Print the standardized full model
summary(reg_full_std)

# Manual t-Test for Offensive and Defensive Metrics: fg_percent and trb_per_game as Proxies
beta_fg <- coef(reg_full_std)["fg_percent"]
beta_trb <- coef(reg_full_std)["trb_per_game"]

# Extracting variances and covariance from the variance-covariance matrix
vcov_mat <- vcov(reg_full_std)
var_fg <- vcov_mat["fg_percent", "fg_percent"]
var_trb <- vcov_mat["trb_per_game", "trb_per_game"]
cov_fg_trb <- vcov_mat["fg_percent", "trb_per_game"]

# Calculating Standard Error of the difference
se_diff <- sqrt(var_fg + var_trb - 2 * cov_fg_trb)

# Calculate the t-statistic
t_stat <- (beta_fg - beta_trb) / se_diff

df <- df.residual(reg_full_std)
p_value <- 2 * pt(-abs(t_stat), df)

cat(sprintf("The p-value is %.4f\n", p_value))
