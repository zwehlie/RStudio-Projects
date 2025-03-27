library(readxl)
library(dplyr)
library(ggplot2)
Womens_Basketballs_Phase1 <- read_excel("Womens Basketballs.xlsx", sheet = 1)
Womens_Basketballs_Phase2 <- read_excel("Womens Basketballs.xlsx", sheet = 2)

#control limits
A2 <- 0.577  
D3 <- 0  
D4 <- 2.115  
B3 <- 0 
B4 <- 2.089  

#Phase 1
samples_phase1 <- Womens_Basketballs_Phase1 %>%
  select(-`Sample Number`) %>%
  rowwise() %>%
  mutate(
    x_bar = mean(c_across(everything())),  
    R = max(c_across(everything())) - min(c_across(everything()))  
  ) %>%
  ungroup()
x_bar_bar_phase1 <- mean(samples_phase1$x_bar)
R_bar_phase1 <- mean(samples_phase1$R)
UCL_x_bar_phase1 <- x_bar_bar_phase1 + A2 * R_bar_phase1
LCL_x_bar_phase1 <- x_bar_bar_phase1 - A2 * R_bar_phase1
UCL_R_phase1 <- D4 * R_bar_phase1
LCL_R_phase1 <- D3 * R_bar_phase1
#Phase 1 X-bar Chart
ggplot(samples_phase1, aes(x = 1:nrow(samples_phase1), y = x_bar)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_hline(yintercept = UCL_x_bar_phase1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_x_bar_phase1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = x_bar_bar_phase1, color = "green") +
  labs(title = "Phase 1 X-bar Chart", x = "Sample", y = "X-bar") +
  theme_minimal()

#Phase 1 R Chart
ggplot(samples_phase1, aes(x = 1:nrow(samples_phase1), y = R)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_hline(yintercept = UCL_R_phase1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_R_phase1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = R_bar_phase1, color = "green") +
  labs(title = "Phase 1 R Chart", x = "Sample", y = "R") +
  theme_minimal()

#Phase 1 S Chart
samples_phase1 <- Womens_Basketballs_Phase1 %>%
  select(-`Sample Number`) %>%
  rowwise() %>%
  mutate(s = sd(c_across(everything()))) %>%
  ungroup()

#Mean of standard deviations S
s_bar_phase1 <- mean(samples_phase1$s)

#Control limits for Phase 1 S chart
UCL_s_phase1 <- B4 * s_bar_phase1
LCL_s_phase1 <- B3 * s_bar_phase1 

#Phase 1 S chart
ggplot(samples_phase1, aes(x = 1:nrow(samples_phase1), y = s)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_hline(yintercept = UCL_s_phase1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_s_phase1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = s_bar_phase1, color = "green") +
  labs(title = "Phase 1 S Chart", x = "Sample", y = "Standard Deviation (S)") +
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
x_bar_bar_phase2 <- mean(samples_phase2$x_bar)
R_bar_phase2 <- mean(samples_phase2$R)
UCL_x_bar_phase2 <- x_bar_bar_phase2 + A2 * R_bar_phase2
LCL_x_bar_phase2 <- x_bar_bar_phase2 - A2 * R_bar_phase2
UCL_R_phase2 <- D4 * R_bar_phase2
LCL_R_phase2 <- D3 * R_bar_phase2

#Phase 2 X-bar Chart
ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2), y = x_bar)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_hline(yintercept = UCL_x_bar_phase2, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_x_bar_phase2, color = "red", linetype = "dashed") +
  geom_hline(yintercept = x_bar_bar_phase2, color = "green") +
  labs(title = "Phase 2 X-bar Chart", x = "Sample", y = "X-bar") +
  theme_minimal()

#Phase 2 R Chart
ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2), y = R)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_hline(yintercept = UCL_R_phase2, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_R_phase2, color = "red", linetype = "dashed") +
  geom_hline(yintercept = R_bar_phase2, color = "green") +
  labs(title = "Phase 2 R Chart", x = "Sample", y = "R") +
  theme_minimal()

#Phase 2 S Chart

samples_phase2 <- Womens_Basketballs_Phase2 %>%
  select(-`Sample Number`) %>%
  rowwise() %>%
  mutate(s = sd(c_across(everything()))) %>%
  ungroup()

#Mean of standard deviations S
s_bar_phase2 <- mean(samples_phase2$s)

#Control limits for Phase 2 S chart
UCL_s_phase2 <- B4 * s_bar_phase2
LCL_s_phase2 <- B3 * s_bar_phase2 

#Phase 2 S chart
ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2), y = s)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_hline(yintercept = UCL_s_phase2, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_s_phase2, color = "red", linetype = "dashed") +
  geom_hline(yintercept = s_bar_phase2, color = "green") +
  labs(title = "Phase 2 S Chart", x = "Sample", y = "Standard Deviation (S)") +
  theme_minimal()
#Cp, Cpk, and Cpm calcu.
USL <- 28.5625  
LSL <- 28.4375  
target <- 28.5   
x_bar <- 28.5021 
s <- 0.1493      

Cp <- (USL - LSL) / (6 * s)
Cpk <- min((USL - x_bar) / (3 * s), (x_bar - LSL) / (3 * s))
Cpm <- (USL - LSL) / (6 * sqrt(s^2 + (x_bar - target)^2))
Cp
Cpk
Cpm

           
           