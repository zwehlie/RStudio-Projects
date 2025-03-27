setwd("C:/Users/zwehl/OneDrive/Documents/R Studio Files")
> setwd("C:/Users/zwehl/OneDrive - Kennesaw State University/R Studio Files")
> library(readxl)
> Womens_Basketballs <- read_excel("Womens Basketballs.xlsx")
> View(Womens_Basketballs)
> library(readxl)
> library(dplyr)
> library(qcc)
Womens_Basketballs_Phase1 <- read_excel("Womens Basketballs.xlsx", sheet = 1)
> View(Womens_Basketballs_Phase1)
> data_phase1 <- Womens_Basketballs_Phase1 %>% select(-`Sample Number`)
> samples <- data_phase1 %>%
  +     rowwise() %>%
  +     summarise(
    +         x_bar = mean(c_across(everything())),  # Calculate mean for each sample
    +         R = max(c_across(everything())) - min(c_across(everything()))  # Calculate range for each sample
    +     )
> x_bar_bar <- mean(samples$x_bar)
> R_bar <- mean(samples$R)
> A2 <- 0.577
> D3 <- 0
> D4 <- 2.115
> UCL_x_bar <- x_bar_bar + A2 * R_bar
> LCL_x_bar <- x_bar_bar - A2 * R_bar
> UCL_R <- D4 * R_bar
> LCL_R <- D3 * R_bar
> cat("X-bar (Mean of means):", x_bar_bar, "\n")
X-bar (Mean of means): 28.50205 
> cat("R-bar (Mean of ranges):", R_bar, "\n")
R-bar (Mean of ranges): 0.1493333 
> cat("X-bar Chart Control Limits: UCL =", UCL_x_bar, ", LCL =", LCL_x_bar, "\n")
X-bar Chart Control Limits: UCL = 28.58822 , LCL = 28.41589 
> cat("R Chart Control Limits: UCL =", UCL_R, ", LCL =", LCL_R, "\n")
R Chart Control Limits: UCL = 0.31584 , LCL = 0 
> qcc_x_bar <- qcc(samples$x_bar, type = "xbar", sizes = 5, center = x_bar_bar, std.dev = R_bar, limits = c(LCL_x_bar, UCL_x_bar))
Warning message:
  In qcc(samples$x_bar, type = "xbar", sizes = 5, center = x_bar_bar,  :
           'std.dev' is not used when limits is given
         > qcc_R <- qcc(samples$R, type = "R", sizes = 5, center = R_bar, std.dev = R_bar, limits = c(LCL_R, UCL_R))