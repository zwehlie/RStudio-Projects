setwd("C:/Users/zwehl/OneDrive/Documents/R Studio Files")
setwd("C:/Users/zwehl/OneDrive - Kennesaw State University/R Studio Files")
library(readxl)
library(dplyr)
library(ggplot2)
Womens_Basketballs_Phase1 <- read_excel("Womens Basketballs.xlsx", sheet = 1)
Womens_Basketballs_Phase2 <- read_excel("Womens Basketballs.xlsx", sheet = 2)
#control charts
A2 <- 0.577  
D3 <- 0  
D4 <- 2.115  
B3 <- 0 
B4 <- 2.089  
samples_phase1 <- Womens_Basketballs_Phase1 %>%
  select(-`Sample Number`) %>%
  rowwise() %>%
  mutate(
    x_bar = mean(c_across(everything())),  
    R = max(c_across(everything())) - min(c_across(everything())),  
    s = sd(c_across(everything()))  
  ) %>%
  ungroup()
x_bar_bar_phase1 <- mean(samples_phase1$x_bar)
R_bar_phase1 <- mean(samples_phase1$R)
s_bar_phase1 <- mean(samples_phase1$s)
UCL_x_bar_phase1 <- x_bar_bar_phase1 + A2 * R_bar_phase1
LCL_x_bar_phase1 <- x_bar_bar_phase1 - A2 * R_bar_phase1
UCL_R_phase1 <- D4 * R_bar_phase1
LCL_R_phase1 <- D3 * R_bar_phase1
UCL_s_phase1 <- B4 * s_bar_phase1
LCL_s_phase1 <- B3 * s_bar_phase1  
ggplot(samples_phase1, aes(x = 1:nrow(samples_phase1))) +
  geom_line(aes(y = x_bar), color = "blue") +
  geom_point(aes(y = x_bar)) +
  geom_hline(yintercept = UCL_x_bar_phase1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_x_bar_phase1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = x_bar_bar_phase1, color = "green") +
  labs(title = "Phase I X-bar Chart", x = "Sample", y = "X-bar") +
  theme_minimal()
ggplot(samples_phase1, aes(x = 1:nrow(samples_phase1))) +
  geom_line(aes(y = R), color = "blue") +
  geom_point(aes(y = R)) +
  geom_hline(yintercept = UCL_R_phase1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_R_phase1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = R_bar_phase1, color = "green") +
  labs(title = "Phase I R Chart", x = "Sample", y = "R") +
  theme_minimal()
ggplot(samples_phase1, aes(x = 1:nrow(samples_phase1))) +
  geom_line(aes(y = s), color = "blue") +
  geom_point(aes(y = s)) +
  geom_hline(yintercept = UCL_s_phase1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_s_phase1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = s_bar_phase1, color = "green") +
  labs(title = "Phase I S Chart", x = "Sample", y = "S") +
  theme_minimal()

#Phase II Calculate X-bar, R, and S 
samples_phase2 <- Womens_Basketballs_Phase2 %>%
  select(-`Sample Number`) %>%
  rowwise() %>%
  mutate(
    x_bar = mean(c_across(everything())),  
    R = max(c_across(everything())) - min(c_across(everything())),  
    s = sd(c_across(everything()))  
  ) %>%
  ungroup()
x_bar_bar_phase2 <- mean(samples_phase2$x_bar)
R_bar_phase2 <- mean(samples_phase2$R)
s_bar_phase2 <- mean(samples_phase2$s)
UCL_x_bar_phase2 <- x_bar_bar_phase2 + A2 * R_bar_phase2
LCL_x_bar_phase2 <- x_bar_bar_phase2 - A2 * R_bar_phase2
UCL_R_phase2 <- D4 * R_bar_phase2
LCL_R_phase2 <- D3 * R_bar_phase2
UCL_s_phase2 <- B4 * s_bar_phase2
LCL_s_phase2 <- B3 * s_bar_phase2  
ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2))) +
  geom_line(aes(y = x_bar), color = "blue") +
  geom_point(aes(y = x_bar)) +
  geom_hline(yintercept = UCL_x_bar_phase2, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_x_bar_phase2, color = "red", linetype = "dashed") +
  geom_hline(yintercept = x_bar_bar_phase2, color = "green") +
  labs(title = "Phase II X-bar Chart", x = "Sample", y = "X-bar") +
  theme_minimal()
ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2))) +
  geom_line(aes(y = R), color = "blue") +
  geom_point(aes(y = R)) +
  geom_hline(yintercept = UCL_R_phase2, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_R_phase2, color = "red", linetype = "dashed") +
  geom_hline(yintercept = R_bar_phase2, color = "green") +
  labs(title = "Phase II R Chart", x = "Sample", y = "R") +
  theme_minimal()

ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2))) +
  geom_line(aes(y = s), color = "blue") +
  geom_point(aes(y = s)) +
  geom_hline(yintercept = UCL_s_phase2, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_s_phase2, color = "red", linetype = "dashed") +
  geom_hline(yintercept = s_bar_phase2, color = "green") +
  labs(title = "Phase II S Chart", x = "Sample", y = "S") +
  theme_minimal()
#Cp, Cpk, Cpm Calculations
USL <- 28.5625 
LSL <- 28.4375 
target <- 28.5 
x_bar <- 28.5021 
s <- 0.1493  
# Z-scores 
Z_USL <- (USL - x_bar) / s
Z_LSL <- (x_bar - LSL) / s
# Fraction non-conforming
fraction_non_conforming <- pnorm(Z_LSL) + (1 - pnorm(Z_USL))
# Calculate Cp, Cpk, and Cpm
Cp <- (USL - LSL) / (6 * s)
Cpk <- min((USL - x_bar) / (3 * s), (x_bar - LSL) / (3 * s))
Cpm <- (USL - LSL) / (6 * sqrt(s^2 + (
  
  