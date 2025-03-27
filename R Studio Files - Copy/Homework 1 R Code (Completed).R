setwd("C:/Users/zwehl/OneDrive/Documents/R Studio Files")
setwd("C:/Users/zwehl/OneDrive - Kennesaw State University/R Studio Files")
library(readxl)
library(dplyr)
library(ggplot2)
Womens_Basketballs_Phase1 <- read_excel("Womens Basketballs.xlsx", sheet = 1)
Womens_Basketballs_Phase2 <- read_excel("Womens Basketballs.xlsx", sheet = 2)
data_phase1 <- Womens_Basketballs_Phase1 %>% select(-`Sample Number`)
samples_phase1 <- data_phase1 %>%
  rowwise() %>%
  mutate(
    x_bar = mean(c_across(everything())),
    R = max(c_across(everything())) - min(c_across(everything()))
  ) %>%
  ungroup()
x_bar_bar_phase1 <- mean(samples_phase1$x_bar)
R_bar_phase1 <- mean(samples_phase1$R)
A2 <- 0.577
D3 <- 0
D4 <- 2.115
UCL_x_bar_phase1 <- x_bar_bar_phase1 + A2 * R_bar_phase1
LCL_x_bar_phase1 <- x_bar_bar_phase1 - A2 * R_bar_phase1
UCL_R_phase1 <- D4 * R_bar_phase1
LCL_R_phase1 <- D3 * R_bar_phase1
out_of_control_xbar_phase1 <- which(samples_phase1$x_bar > UCL_x_bar_phase1 | samples_phase1$x_bar < LCL_x_bar_phase1)
out_of_control_R_phase1 <- which(samples_phase1$R > UCL_R_phase1 | samples_phase1$R < LCL_R_phase1)
offending_points <- unique(c(out_of_control_xbar_phase1, out_of_control_R_phase1))
data_phase1_revised <- data_phase1[-offending_points, ]
samples_phase1_revised <- data_phase1_revised %>%
  rowwise() %>%
  mutate(
    x_bar = mean(c_across(everything())),
    R = max(c_across(everything())) - min(c_across(everything()))
  ) %>%
  ungroup()
x_bar_bar_phase1_revised <- mean(samples_phase1_revised$x_bar)
R_bar_phase1_revised <- mean(samples_phase1_revised$R)

UCL_x_bar_phase1_revised <- x_bar_bar_phase1_revised + A2 * R_bar_phase1_revised
LCL_x_bar_phase1_revised <- x_bar_bar_phase1_revised - A2 * R_bar_phase1_revised
UCL_R_phase1_revised <- D4 * R_bar_phase1_revised
LCL_R_phase1_revised <- D3 * R_bar_phase1_revised
#Phase 1 X Chart
ggplot(samples_phase1_revised, aes(x = 1:nrow(samples_phase1_revised))) +
  geom_line(aes(y = x_bar), color = "blue") +
  geom_point(aes(y = x_bar)) +
  geom_hline(yintercept = UCL_x_bar_phase1_revised, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_x_bar_phase1_revised, color = "red", linetype = "dashed") +
  geom_hline(yintercept = x_bar_bar_phase1_revised, color = "green") +
  labs(title = "Revised Phase I X-bar Chart", x = "Sample", y = "X-bar") +
  theme_minimal()
#Phase 1 R Chart
ggplot(samples_phase1_revised, aes(x = 1:nrow(samples_phase1_revised))) +
  geom_line(aes(y = R), color = "blue") +
  geom_point(aes(y = R)) +
  geom_hline(yintercept = UCL_R_phase1_revised, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_R_phase1_revised, color = "red", linetype = "dashed") +
  geom_hline(yintercept = R_bar_phase1_revised, color = "green") +
  labs(title = "Revised Phase I R Chart", x = "Sample", y = "R") +
  theme_minimal()
#Phase 2
samples_phase2 <- Womens_Basketballs_Phase2 %>%
  select(-`Sample Number`) %>%
  rowwise() %>%
  mutate(
    x_bar = mean(c_across(everything())),
    R = max(c_across(everything())) - min(c_across(everything()))
  ) %>%
  ungroup()
#Phase 2 XBar and R Chart Revised Limit
ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2))) +
  geom_line(aes(y = x_bar), color = "blue") +
  geom_point(aes(y = x_bar)) +
  geom_hline(yintercept = UCL_x_bar_phase1_revised, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_x_bar_phase1_revised, color = "red", linetype = "dashed") +
  geom_hline(yintercept = x_bar_bar_phase1_revised, color = "green") +
  labs(title = "Phase II X-bar Chart", x = "Sample", y = "X-bar") +
  theme_minimal()
ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2))) +
  geom_line(aes(y = R), color = "blue") +
  geom_point(aes(y = R)) +
  geom_hline(yintercept = UCL_R_phase1_revised, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_R_phase1_revised, color = "red", linetype = "dashed") +
  geom_hline(yintercept = R_bar_phase1_revised, color = "green") +
  labs(title = "Phase II R Chart", x = "Sample", y = "R") +
  theme_minimal()
#Question 5 , estimate the fraction non-conforming, the Cp ratio, the Cpk ratio, and the Cpm ratio
USL <- 28.5625
LSL <- 28.4375
target <- 28.5
x_bar <- 28.5021
s <- 0.1493
Z_USL <- (USL - x_bar) / s
Z_LSL <- (x_bar - LSL) / s
fraction_non_conforming <- pnorm(Z_LSL) + (1 - pnorm(Z_USL))
Cp <- (USL - LSL) / (6 * s)
Cpk <- min((USL - x_bar) / (3 * s), (x_bar - LSL) / (3 * s))
Cpm <- (USL - LSL) / (6 * sqrt(s^2 + (x_bar - target)^2))
list(
  fraction_non_conforming = fraction_non_conforming,
  Cp = Cp,
  Cpk = Cpk,
  Cpm = Cpm
)