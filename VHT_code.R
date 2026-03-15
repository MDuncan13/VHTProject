library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(nullabor)
library(tidyverse)
library(binom)

# Figure 2.1
# Weak relationship example
x <- seq(0, 10, length.out = 75)
y <- 0.2 * x + rnorm(75, sd = 2)
df_eda <- data.frame(x, y)

# EDA example
p_EDA <- ggplot(df_eda, aes(x, y)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "EDA Example",
       x = "X",
       y = "Y")

set.seed(2)

# Simulated nonlinear data
x2 <- seq(0, 6, length.out = 75)
y2 <- 0.5*x2 + 0.3*(x2^2) + rnorm(75, sd = 2)
fit <- lm(y2 ~ x2)   # intentionally *incorrect* linear model
residuals <- resid(fit)
df_md <- data.frame(x2, residuals)

# MD example
p_MD <- ggplot(df_md, aes(x2, residuals)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Residual VS Fitted - MD Example",
       x = "Fitted",
       y = "Residuals")

grid.arrange(p_EDA, p_MD, nrow = 1)



# Figure 2.2: Add confidence bands
p_EDA <- ggplot(df_eda, aes(x, y)) +
  geom_point(size = 2, alpha = 0.7) +
  # put a line and confidence bands
  geom_smooth(
    method = "lm",
    se = TRUE,
    level = 0.95,
    linewidth = 1
  ) +
  theme_minimal() +
  labs(title = "EDA Example",
       x = "X",
       y = "Y")

p_MD <- ggplot(df_md, aes(x2, residuals)) +
  geom_point(size = 2, alpha = 0.7) +
  # put a line and confidence bands
  geom_smooth(
    method = "loess",
    se = TRUE,
    level = 0.95,
    linewidth = 1
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Residual VS Fitted - MD Example",
       x = "Fitted",
       y = "Residuals")

grid.arrange(p_EDA, p_MD, nrow = 1)


# F-test for Table 2.1
# randomise the true data panel
set.seed(NULL)
df <- data.frame(
  group = factor(rep(c("A", "B"), each = 100)),
  value = c(rnorm(100, mean = 0, sd = 1),
            rnorm(100, mean = 0, sd = 2)))
# create the lineup data
lineup_data <- lineup(null_permute("group"), df)

true_pos <- attr(lineup_data, "pos")

# extract only the true data
true_data <- subset(lineup_data, .sample == true_pos)

# plot the lineup
ggplot(lineup_data, aes(x = value, colour = group)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ .sample)+
  theme_minimal()

# plot only the true data
ggplot(true_data, aes(x = value, colour = group)) +
  geom_density(alpha = 0.5, linewidth = 1)+
  theme_minimal()


# Figure 2.3
# simulate the data
sim_data <- data.frame(x = rnorm(n = 100, mean = 0, sd = 1))

sim_data$y <- rnorm(100, mean = sim_data$x, sd = 1)

lineup_data <- lineup(null_permute('y'), sim_data)

set.seed(NULL)
# Use lineup protocol
ggplot(lineup_data, aes(x, y)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ .sample) +
  theme_minimal()

set.seed(42)

# Figure 2.4: Box plot 
box_data <- data.frame(value = rnorm(200, mean = 0, sd = 1))

ggplot(box_data, aes(x = "", y = value)) +
  geom_boxplot(alpha = 0.7, fill = "#619CFF") +
  theme_minimal() +
  labs(x = "", y = "Value")

# Figure 2.5: Density plot
density_data <- data.frame(value = rnorm(500, mean = 0, sd = 1))

ggplot(density_data, aes(x = value)) +
  geom_density(linewidth = 0.8, fill = "#619CFF", alpha = 0.4) +
  theme_minimal() +
  labs(x = "Value", y = "Density")

# Figure 2.6: Violin plot
violin_data <- data.frame(value = rnorm(200, mean = 0, sd = 1))

ggplot(violin_data, aes(x = "", y = value)) +
  geom_violin(alpha = 0.7, fill = "#619CFF") +
  theme_minimal() +
  labs(x = "", y = "Value")

# Figure 2.7: Absolute density plot
abs_data <- data.frame(value = rnorm(500, mean = 0, sd = 1))

ggplot(abs_data, aes(x = abs(value))) +
  geom_density(linewidth = 0.8, fill = "#619CFF", alpha = 0.4) +
  theme_minimal() +
  labs(x = "|Value|", y = "Density")

# Figure 3.1
# method for lineup
normrand <- function(data) {
  tibble(
    x = rnorm(n),
    y = rnorm(n)
  )
}


rho <- 0.7
sigma <- matrix(c(1, rho, rho, 1), ncol = 2)
truth <- as.data.frame(MASS::mvrnorm(n, mu = c(0, 0), Sigma = sigma))
names(truth) <- c("x", "y")
lu <- lineup(normrand, true = truth, n = 2)
p<- ggplot(lu,aes(x, y)) +
  geom_point( color = "aquamarine4") +
  facet_wrap(~ .sample)
p


# Plots used for the Survey
# F-test
# set.seed as Null so that location is randomized
set.seed(NULL)

# data frame for sd = 1.2
df <- data.frame(
  group = factor(rep(c("A", "B"), each = 100)),
  value = c(rnorm(100, mean = 0, sd = 1),
            rnorm(100, mean = 0, sd = 1.2)))

# create the lineup data
lineup_data <- lineup(null_permute("group"), df)

# density plots
ggplot(lineup_data, aes(x = abs(value), colour = group)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ .sample)

ggplot(lineup_data, aes(x = value, colour = group)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ .sample)

# violin plot
ggplot(lineup_data, aes(x = group, y = value, fill = group)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~ .sample)

# boxplot
ggplot(lineup_data, aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  facet_wrap(~ .sample)

attr(lineup_data, "pos")

# data frame for sd = 1.7
df <- data.frame(
  group = factor(rep(c("A", "B"), each = 100)),
  value = c(rnorm(100, mean = 0, sd = 1),
            rnorm(100, mean = 0, sd = 1.7)))

# create the lineup data
lineup_data <- lineup(null_permute("group"), df)

# density plots
ggplot(lineup_data, aes(x = abs(value), colour = group)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ .sample)

ggplot(lineup_data, aes(x = value, colour = group)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ .sample)

# violin plot
ggplot(lineup_data, aes(x = group, y = value, fill = group)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~ .sample)
# boxplot
ggplot(lineup_data, aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  facet_wrap(~ .sample)

attr(lineup_data, "pos")


# upload survey data
# change to your url
data <- ggplot2data <- read_excel(
  "C:/Users/Loizos/OneDrive/Desktop/dissertation/Survey/Visual Hypothesis Testing Survey.xlsx"
)

# true data panels
trues <- c(2, 19, 5, 5, 10, 2, 6, 1, 7)
# data preparation so it is easier to use
data$participant <- 1:nrow(data)
names(data) <- make.names(names(data))
choice_cols <- grep("Which.image.is.least.like", names(data), value = TRUE)
conf_cols   <- grep("How.confident.are.you", names(data), value = TRUE)

choices_long <- data %>%
  select(participant, all_of(choice_cols)) %>%
  pivot_longer(
    -participant,
    names_to = "lineup",
    values_to = "choice",
    values_transform = list(choice = as.numeric)
  )

conf_long <- data %>%
  select(participant, all_of(conf_cols)) %>%
  pivot_longer(
    -participant,
    names_to = "lineup",
    values_to = "confidence",
    values_transform = list(confidence = as.numeric)
  )

long_data <- choices_long %>%
  mutate(confidence = conf_long$confidence)

long_data$lineup <- as.numeric(gsub("[^0-9]", "", long_data$lineup))
long_data$lineup[is.na(long_data$lineup)] <- 1

long_data <- long_data %>%
  filter(!is.na(choice))

info <- data %>%
  select(participant,
         "What.is.the.most.advanced.statistics.you.have.done.",
         "What.year.are.you.currently.studying.in.")

long_data <- left_join(long_data, info, by = "participant")

names(long_data)[names(long_data) == 'What.is.the.most.advanced.statistics.you.have.done.'] <- 'stats_level'
names(long_data)[names(long_data) == 'What.year.are.you.currently.studying.in.'] <- 'year'

long_data <- long_data %>%
  mutate(
    type = if_else(lineup %in% c(1, 6), "violin",
                   if_else(lineup %in% c(2, 7), "density",
                           if_else(lineup %in% c(3, 8), "box",
                                   if_else(lineup %in% c(4, 5, 9), "abs", NA_character_)))),
    
    sd = if_else(lineup %in% c(1, 2, 3, 4, 5), 1.7,
                 if_else(lineup %in% c(6, 7, 8, 9), 1.2, NA_real_))
  )
long_data$correct <- as.numeric(long_data$choice ==
                                  trues[long_data$lineup])



m <- 20
chance <- 1/m

prop.test(sum(long_data$correct),
          nrow(long_data),
          p = chance,
          alternative = "greater")


aggregate(correct ~ type, long_data, mean)
aggregate(correct ~ sd, long_data, mean)

# Figure 4.1 (a) and (b)
info <- info %>%
 mutate(
    stats_level = recode(What.is.the.most.advanced.statistics.you.have.done.,
                         "Pre-Honours (e.g. Stats Y2, Intro to Data Science)" = "Pre-Honours",
                         "Msc In stats related field" = "MSc",
                         "Honours (e.g. Statistical Methodology)" = "Honours",
                         "PhD in stats related field" = "PhD"
    )
  )

data <- data %>%
  mutate(What.year.are.you.currently.studying.in. = recode(What.year.are.you.currently.studying.in.,
                                                           "Member of Faculty" = "Faculty"))

ggplot(data, aes(x = What.year.are.you.currently.studying.in.)) +
  geom_bar(fill = "steelblue", color = "black", width = 0.7) +
  labs(
    title = "Distribution of Participants by Year of Study",
    x = "Year of Study",
    y = "Number of Participants"
  ) +
  theme_minimal(base_size = 14)
ggplot(info, aes(x = stats_level)) +
  geom_bar(fill = "steelblue", color = "black", width = 0.7) +
  labs(
    title = "Distribution of Statistics Experience by Year of Study",
    x = "Year of Study",
    y = "Number of Participants"
  ) +
  theme_minimal(base_size = 14)

# Figure 4.2: Accuracy by lineup type 
ggplot(long_data, aes(x = type, y = correct)) +
  geom_jitter(height = 0.05, width = 0.15, alpha = 0.4) +
  stat_summary(fun = mean, geom = "point", size = 5, colour = "red") +
  labs(
    x     = "Lineup type",
    y     = "Probability of correct identification",
    title = "Correct Identification by Lineup Type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Section 4.2: Fisher test for null plot in survey
lineup4 <- long_data %>% filter(lineup == 4)
lineup5 <- long_data %>% filter(lineup == 5)

nullq_table <- matrix(
  c(sum(lineup4$correct),          
    nrow(lineup4) - sum(lineup4$correct),   
    sum(lineup5$correct),          
    nrow(lineup5) - sum(lineup5$correct)),
  nrow = 2,
  dimnames = list(
    c("Correct", "Incorrect"),
    c("Lineup 4 (Original)", "Lineup 5 (Altered)")
  )
)

fisher.test(nullq_table)

# Figure 4.3: Accuracy by confidence level
ggplot(long_data, aes(x = confidence, y = correct)) +
  geom_jitter(height = 0.05, width = 0.15, alpha = 0.4) +
  stat_summary(fun = mean, geom = "point", size = 5, colour = "blue") +
  labs(
    x     = "Confidence Level",
    y     = "Probability of correct identification",
    title = "Correct Identification by Confidence Level"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Figure 4.4: Accuracy by year of study 
long_data$year <- factor(
  long_data$year,
  levels = c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5",
             "Msc", "PhD", "Member of Faculty")
)

ggplot(long_data, aes(x = year, y = correct)) +
  geom_jitter(height = 0.05, width = 0.15, alpha = 0.4) +
  stat_summary(fun = mean, geom = "point", size = 5, colour = "purple") +
  labs(
    x     = "Year of Study",
    y     = "Probability of correct identification",
    title = "Correct Identification by Year of Study"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Figure 4.5
#  Wilson 95% confidence interval 
wilson_ci <- function(k, n, z = 1.96) {
  p      <- k / n
  denom  <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half   <- (z / denom) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  c(lo = centre - half, hi = centre + half)
}

#  Accuracy by (type × sd) with Wilson CIs
plot_df <- long_data %>%
  mutate(
    sd_f = factor(sd, levels = c(1.2, 1.7)),
    type = factor(type, levels = c("abs", "box", "density", "violin"))
  ) %>%
  group_by(type, sd_f) %>%
  summarise(n = n(), k = sum(correct), p = k / n, .groups = "drop") %>%
  rowwise() %>%
  mutate(lo = wilson_ci(k, n)[1], hi = wilson_ci(k, n)[2]) %>%
  ungroup()

# Detection accuracy by sd, with different plot type – with Wilson CIs (Figure 4.5)
ggplot(plot_df, aes(x = sd_f, y = p, group = 1)) +
  geom_hline(yintercept = chance, linetype = "dashed", linewidth = 0.7) +
  geom_line(linewidth = 1.0, colour = "orange") +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                width = 0.12, linewidth = 1.0, colour = "orange") +
  geom_point(size = 4.2, colour = "orange") +
  geom_text(aes(label = sprintf("%.0f%%", 100 * p)), vjust = -0.9, size = 3.6) +
  facet_wrap(~ type, nrow = 1) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Detection accuracy by null-generating variability",
    x     = "Standard deviation of null plots (sd)",
    y     = "Accuracy (mean \u00b1 95% CI)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text       = element_text(face = "bold"),
    axis.title       = element_text(face = "bold")
  )

# Figure 5.7: power plot
m <- 20
chance <- 1/m
alpha <- 0.05

# make a function to find x_alpha
find_x_alpha <- function(K, chance, alpha = 0.05) {
  for (x in 0:K) {
    if (1 - pbinom(x - 1, size = K, prob = chance) <= alpha) return(x)
  }
  return(K + 1)
}

power_plot_df <- long_data %>%
  group_by(type, sd) %>%
  summarise(
    K = n(),
    # number of the correct
    X = sum(correct),
    # observed accuracy
    p_hat = X / K,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    x_alpha = find_x_alpha(K, chance, alpha),
    # compute Wilson confidence intervals
    p_low  = binom.confint(X, K, methods = "wilson")$lower,
    p_high = binom.confint(X, K, methods = "wilson")$upper,
    
    # estimate the power interval
    power_hat  = 1 - pbinom(x_alpha - 1, size = K, prob = p_hat),
    power_low  = 1 - pbinom(x_alpha - 1, size = K, prob = p_low),
    power_high = 1 - pbinom(x_alpha - 1, size = K, prob = p_high),
    power_null = 1 - pbinom(x_alpha - 1, size = K, prob = chance)
  ) %>%
  ungroup()
# Plot the results
ggplot(power_plot_df,
       aes(x = sd, y = power_hat, color = type, fill = type, group = type)) +
  geom_ribbon(aes(ymin = power_low, ymax = power_high),
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(data = power_plot_df, aes(yintercept = power_null),
             linetype = "dashed", color = "black") +
  facet_wrap(~ type) +
  theme_bw() +
  labs(x = "Effect size (sd)",
       y = "Estimated lineup-test power (P(reject H0))")


# logistic regression model with type as explanatory variable
model <- glm(correct ~ as.factor(type), data = long_data, 
             family = 'binomial')

summary(model)

# logistic regression model with type and year as explanatory variables
model_level <- glm(correct ~ as.factor(type)+ as.factor(year),
                   long_data, family = 'binomial')
summary(model_level)
# multicollinearity test
library(car)

vif(model_level)

# logistic regression model with type and sd as explanatory variables
model_type_sd <- glm(correct ~ as.factor(type) + as.factor(sd),
                     data = long_data,
                     family = 'binomial')

summary(model_type_sd)
# multicollinearity test

vif(model_type_sd)

# interaction model
interaction_model <- glm(correct ~ as.factor(type) * as.factor(sd), 
                         long_data, family = 'binomial')

anova(interaction_model)

# Section 5.1 table and median accuracy
participant_accuracy <- long_data %>%
  group_by(participant) %>%
  summarise(
    accuracy = mean(correct, na.rm = TRUE)
  )

median_acc <- median(participant_accuracy$accuracy)
median_acc


participant_accuracy <- participant_accuracy %>%
  mutate(
    accuracy_group = ifelse(accuracy > median_acc,
                            "Higher accuracy",
                            "Lower accuracy")
  )

participant_accuracy

participant_accuracy$accuracy_group <- factor(
  participant_accuracy$accuracy_group,
  levels = c("Higher accuracy", "Lower accuracy")
)
