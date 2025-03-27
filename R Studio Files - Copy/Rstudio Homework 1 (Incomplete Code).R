setwd("C:/Users/zwehl/OneDrive/Documents/R Studio Files")
setwd("C:/Users/zwehl/OneDrive - Kennesaw State University/R Studio Files")
library(readxl)
 library(dplyr)
 Womens_Basketballs_Phase1 <- read_excel("Womens Basketballs.xlsx", sheet = 1)
 data_phase1 <- Womens_Basketballs_Phase1 %% select(-`Sample Number`)
 samples_phase1 <- data_phase1 %%
  +     rowwise() %%
  +     summarise(
    +         x_bar = mean(c_across(everything())),   
    +         R = max(c_across(everything())) - min(c_across(everything())) 
    +     )
 x_bar_bar_phase1 <- mean(samples_phase1$x_bar)
 R_bar_phase1 <- mean(samples_phase1$R)
 A2 <- 0.577
 D3 <- 0
 D4 <- 2.115
 
   UCL_x_bar_phase1 <- x_bar_bar_phase1 + A2 * R_bar_phase1
 LCL_x_bar_phase1 <- x_bar_bar_phase1 - A2 * R_bar_phase1
 UCL_R_phase1 <- D4 * R_bar_phase1
 LCL_R_phase1 <- D3 * R_bar_phase1
 out_of_control_xbar_phase1 <- which(samples_phase1$x_bar  UCL_x_bar_phase1 | samples_phase1$x_bar < LCL_x_bar_phase1)
 out_of_control_R_phase1 <- which(samples_phase1$R  UCL_R_phase1 | samples_phase1$R < LCL_R_phase1)
 # Phase II
   Womens_Basketballs_Phase2 <- read_excel("Womens Basketballs.xlsx", sheet = 2)
 data_phase2 <- Womens_Basketballs_Phase2 %% select(-`Sample Number`)
 samples_phase2 <- data_phase2 %%
  +     rowwise() %%
  +     summarise(
    +         x_bar = mean(c_across(everything())),  
    +         R = max(c_across(everything())) - min(c_across(everything()))  
    +     )
 UCL_x_bar_phase2 <- UCL_x_bar_phase1
 LCL_x_bar_phase2 <- LCL_x_bar_phase1
 UCL_R_phase2 <- UCL_R_phase1
 LCL_R_phase2 <- LCL_R_phase1
 out_of_control_xbar_phase2 <- which(samples_phase2$x_bar  UCL_x_bar_phase2 | samples_phase2$x_bar < LCL_x_bar_phase2)
 out_of_control_R_phase2 <- which(samples_phase2$R  UCL_R_phase2 | samples_phase2$R < LCL_R_phase2)
 if (length(out_of_control_xbar_phase1)  0 || length(out_of_control_R_phase1)  0) {
  +     print
  + library(readxl)
  + Womens_Basketballs <- read_excel("Womens Basketballs.xlsx")
  + View(Womens_Basketballs)
  + phase1_data_revised <- data_phase1[-c(out_of_control_xbar_phase1, out_of_control_R_phase1), ]
  + samples_phase1_revised <- phase1_data_revised %%
    +     rowwise() %%
    +     summarise(
      +         x_bar = mean(c_across(everything())),
      +         R = max(c_across(everything())) - min(c_across(everything()))
      +     )
  + 
    + x_bar_bar_phase1_revised <- mean(samples_phase1_revised$x_bar)
    + R_bar_phase1_revised <- mean(samples_phase1_revised$R)
    + 
      + UCL_x_bar_phase1_revised <- x_bar_bar_phase1_revised + A2 * R_bar_phase1_revised
      + LCL_x_bar_phase1_revised <- x_bar_bar_phase1_revised - A2 * R_bar_phase1_revised
      + UCL_R_phase1_revised <- D4 * R_bar_phase1_revised
      + LCL_R_phase1_revised <- D3 * R_bar_phase1_revised
      + }
 
   UCL_x_bar_phase2 <- UCL_x_bar_phase1
 LCL_x_bar_phase2 <- LCL_x_bar_phase1
 UCL_R_phase2 <- UCL_R_phase1
 LCL_R_phase2 <- LCL_R_phase1
 ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2))) +
  +     geom_line(aes(y = x_bar), color = "blue") +
  +     geom_point(aes(y = x_bar)) +
  +     geom_hline(yintercept = UCL_x_bar_phase2, color = "red", linetype = "dashed") +
  +     geom_hline(yintercept = LCL_x_bar_phase2, color = "red", linetype = "dashed") +
  +     geom_hline(yintercept = x_bar_bar_phase1, color = "green") +  # Grand mean from Phase I
  +     labs(title = "Phase II X-bar Chart", x = "Sample", y = "X-bar") +
  +     theme_minimal()
Error in ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2))) : 
  could not find function "ggplot"
 library(ggplot2)
 ggplot(samples_phase1, aes(x = 1:nrow(samples_phase1))) +
  +     geom_line(aes(y = x_bar), color = "blue") +
  +     geom_point(aes(y = x_bar)) +
  +     geom_hline(yintercept = UCL_x_bar_phase1, color = "red", linetype = "dashed") +
  +     geom_hline(yintercept = LCL_x_bar_phase1, color = "red", linetype = "dashed") +
  +     geom_hline(yintercept = x_bar_bar_phase1, color = "green") +
  +     labs(title = "Phase I X-bar Chart", x = "Sample", y = "X-bar") +
  +     theme_minimal()
 ggplot(samples_phase1, aes(x = 1:nrow(samples_phase1))) +
  +     geom_line(aes(y = R), color = "blue") +
  +     geom_point(aes(y = R)) +
  +     geom_hline(yintercept = UCL_R_phase1, color = "red", linetype = "dashed") +
  +     geom_hline(yintercept = LCL_R_phase1, color = "red", linetype = "dashed") +
  +     geom_hline(yintercept = R_bar_phase1, color = "green") +
  +     labs(title = "Phase I R Chart", x = "Sample", y = "R") +
  +     theme_minimal()
 out_of_control_xbar_phase1 <- which(samples_phase1$x_bar  UCL_x_bar_phase1 | samples_phase1$x_bar < LCL_x_bar_phase1)
 out_of_control_R_phase1 <- which(samples_phase1$R  UCL_R_phase1 | samples_phase1$R < LCL_R_phase1)
 ggplot(samples_phase1, aes(x = 1:nrow(samples_phase1))) +
  +     geom_line(aes(y = R), color = "blue") +
  +     geom_point(aes(y = R)) +
  +     geom_hline(yintercept = UCL_R_phase1, color = "red", linetype = "dashed") +
  +     geom_hline(yintercept = LCL_R_phase1, color = "red", linetype = "dashed") +
  +     geom_hline(yintercept = R_bar_phase1, color = "green") +
  +     labs(title = "Phase I R Chart", x = "Sample", y = "R") +
  +     theme_minimal()
 # Phase II
   Womens_Basketballs_Phase2 <- read_excel("Womens Basketballs.xlsx", sheet = 2)
 data_phase2 <- Womens_Basketballs_Phase2 %% select(-`Sample Number`)
 samples_phase2 <- data_phase2 %%
  +     rowwise() %%
  +     summarise(
    +         x_bar = mean(c_across(everything())),  # Calculate mean for each sample
    +         R = max(c_across(everything())) - min(c_across(everything()))  # Calculate range for each sample
    +     )
 UCL_x_bar_phase2 <- UCL_x_bar_phase1
 LCL_x_bar_phase2 <- LCL_x_bar_phase1
 UCL_R_phase2 <- UCL_R_phase1
 LCL_R_phase2 <- LCL_R_phase1
 ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2))) +
  +     geom_line(aes(y = x_bar), color = "blue") +
  +     geom_point(aes(y = x_bar)) +
  +     geom_hline(yintercept = UCL_x_bar_phase2, color = "red", linetype = "dashed") +
  +     geom_hline(yintercept = LCL_x_bar_phase2, color = "red", linetype = "dashed") +
  +     geom_hline(yintercept = x_bar_bar_phase1, color = "green") +  # Grand mean from Phase I
  +     labs(title = "Phase II X-bar Chart", x = "Sample", y = "X-bar") +
  +     theme_minimal()
 ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2))) +
  +     geom_line(aes(y = R), color = "blue") +
  +     geom_point(aes(y = R)) +
  +     geom_hline(yintercept = UCL_R_phase2, color = "red", linetype = "dashed") +
  +     geom_hline(yintercept = LCL_R_phase2, color = "red", linetype = "dashed") +
  +     geom_hline(yintercept = R_bar_phase1, color = "green") +  # Average range from Phase I
  +     labs(title = "Phase II R Chart", x = "Sample", y = "R") +
  +     theme_minimal()
 #Question 3
   library(readxl)
 library(dplyr)
 library(ggplot2)
 Womens_Basketballs_Phase1 <- read_excel("Womens Basketballs.xlsx", sheet = 1)
 data_phase1 <- Womens_Basketballs_Phase1 %% select(-`Sample Number`)
 samples_phase1 <- data_phase1 %%
  +     rowwise() %%
  +     summarise(
    +         x_bar = mean(c_across(everything())),
    + R = max(c_across(everything())) - min(c_across(everything()))
    + )
 x_bar_bar_phase1 <- mean(samples_phase1$x_bar)
 R_bar_phase1 <- mean(samples_phase1$R)
 A2 <- 0.577
 D3 <- 0
 D4 <- 2.115
 
   UCL_x_bar_phase1 <- x_bar_bar_phase1 + A2 * R_bar_phase1
 LCL_x_bar_phase1 <- x_bar_bar_phase1 - A2 * R_bar_phase1
 UCL_R_phase1 <- D4 * R_bar_phase1
 LCL_R_phase1 <- D3 * R_bar_phase1
 out_of_control_xbar_phase1 <- which(samples_phase1$x_bar  UCL_x_bar_phase1 | samples_phase1$x_bar < LCL_x_bar_phase1)
 out_of_control_R_phase1 <- which(samples_phase1$R  UCL_R_phase1 | samples_phase1$R < LCL_R_phase1)
 if(length(out_of_control_xbar_phase1)  0 || length(out_of_control_R_phase1)  0) {
  + offending_points <- unique(c(out_of_control_xbar_phase1, out_of_control_R_phase1))
  + data_phase1_revised <- data_phase1[-offending_points, ]
  + samples_phase1_revised <- data_phase1_revised %%
    +     rowwise() %%
    +     summarise(
      +         x_bar = mean(c_across(everything())),
      +         R = max(c_across(everything())) - min(c_across(everything()))
      +     )
  + x_bar_bar_phase1_revised <- mean(samples_phase1_revised$x_bar)
  + R_bar_phase1_revised <- mean(samples_phase1_revised$R)
  + 
    + UCL_x_bar_phase1_revised <- x_bar_bar_phase1_revised + A2 * R_bar_phase1_revised
    + LCL_x_bar_phase1_revised <- x_bar_bar_phase1_revised - A2 * R_bar_phase1_revised
    + UCL_R_phase1_revised <- D4 * R_bar_phase1_revised
    + LCL_R_phase1_revised <- D3 * R_bar_phase1_revised
    + } else {
      + x_bar_bar_phase1_revised <- x_bar_bar_phase1
      + R_bar_phase1_revised <- R_bar_phase1
      + UCL_x_bar_phase1_revised <- UCL_x_bar_phase1
      + LCL_x_bar_phase1_revised <- LCL_x_bar_phase1
      + UCL_R_phase1_revised <- UCL_R_phase1
      + LCL_R_phase1_revised <- LCL_R_phase1
      + ggplot(samples_phase1_revised, aes(x = 1:nrow(samples_phase1_revised))) +
        +     geom_line(aes(y = x_bar), color = "blue") +
        +     geom_point(aes(y = x_bar)) +
        +     geom_hline(yintercept = UCL_x_bar_phase1_revised, color = "red", linetype = "dashed") +
        +     geom_hline(yintercept = LCL_x_bar_phase1_revised, color = "red", linetype = "dashed") +
        +     geom_hline(yintercept = x_bar_bar_phase1_revised, color = "green") +
        +     labs(title = "Revised Phase I X-bar Chart", x = "Sample", y = "X-bar") +
        +     theme_minimal()
      + ggplot(samples_phase1_revised, aes(x = 1:nrow(samples_phase1_revised))) +
        +     geom_line(aes(y = R), color = "blue") +
        +     geom_point(aes(y = R)) +
        +     geom_hline(yintercept = UCL_R_phase1_revised, color = "red", linetype = "dashed") +
        +     geom_hline(yintercept = LCL_R_phase1_revised, color = "red", linetype = "dashed") +
        +     geom_hline(yintercept = R_bar_phase1_revised, color = "green") +
        +     labs(title = "Revised Phase I R Chart", x = "Sample", y = "R") +
        +     theme_minimal()
      library(readxl)
       library(dplyr)
       library(ggplot2)
       Womens_Basketballs_Phase1 <- read_excel("Womens Basketballs.xlsx", sheet = 1)
       data_phase1 <- Womens_Basketballs_Phase1 %% select(-`Sample Number`)
       samples_phase1 <- data_phase1 %%
        +     mutate(
          +         x_bar = rowMeans(select(., everything())),
          +         R = apply(select(., everything()), 1, function(x) max(x) - min(x))
          +     )
       x_bar_bar_phase1 <- mean(samples_phase1$x_bar)
       R_bar_phase1 <- mean(samples_phase1$R)
       A2 <- 0.577
       D3 <- 0
       D4 <- 2.115
       
         UCL_x_bar_phase1 <- x_bar_bar_phase1 + A2 * R_bar_phase1
         LCL_x_bar_phase1 <- x_bar_bar_phase1 - A2 * R_bar_phase1
         UCL_R_phase1 <- D4 * R_bar_phase1
         LCL_R_phase1 <- D3 * R_bar_phase1
         out_of_control_xbar_phase1 <- which(samples_phase1$x_bar  UCL_x_bar_phase1 | samples_phase1$x_bar < LCL_x_bar_phase1)
         out_of_control_R_phase1 <- which(samples_phase1$R  UCL_R_phase1 | samples_phase1$R < LCL_R_phase1)
         offending_points <- unique(c(out_of_control_xbar_phase1, out_of_control_R_phase1))
         data_phase1_revised <- data_phase1[-offending_points, ]
         samples_phase1_revised <- data_phase1_revised %%
          +     mutate(
            +         x_bar = rowMeans(select(., everything())),
            +         R = apply(select(., everything()), 1, function(x) max(x) - min(x))
            +     )
         library(readxl)
         library(dplyr)
         library(ggplot2)
         Womens_Basketballs_Phase1 <- read_excel("Womens Basketballs.xlsx", sheet = 1)
         data_phase1 <- Womens_Basketballs_Phase1 %% select(-`Sample Number`)
         samples_phase1 <- data_phase1 %%
          +     rowwise() %%
          +     mutate(
            +         x_bar = mean(c_across(everything())), 
            +         s = sd(c_across(everything())) 
            +     ) %%
          +     ungroup()
         x_bar_bar_phase1 <- mean(samples_phase1$x_bar)
         s_bar_phase1 <- mean(samples_phase1$s)
         B3 <- 0  
         B4 <- 2.089  
         A3 <- 1.427 
         UCL_x_bar_phase1 <- x_bar_bar_phase1 + A3 * s_bar_phase1
         LCL_x_bar_phase1 <- x_bar_bar_phase1 - A3 * s_bar_phase1
         UCL_s_phase1 <- B4 * s_bar_phase1
         LCL_s_phase1 <- B3 * s_bar_phase1
         out_of_control_xbar_phase1 <- which(samples_phase1$x_bar  UCL_x_bar_phase1 | samples_phase1$x_bar < LCL_x_bar_phase1)
         out_of_control_s_phase1 <- which(samples_phase1$s  UCL_s_phase1 | samples_phase1$s < LCL_s_phase1)
         offending_points <- unique(c(out_of_control_xbar_phase1, out_of_control_s_phase1))
         data_phase1_revised <- data_phase1[-offending_points, ]
         samples_phase1_revised <- data_phase1_revised %%
          +     rowwise() %%
          +     mutate(
            +         x_bar = mean(c_across(everything())),
            +         s = sd(c_across(everything()))
            +     ) %%
          +     ungroup()
         x_bar_bar_phase1_revised <- mean(samples_phase1_revised$x_bar)
         s_bar_phase1_revised <- mean(samples_phase1_revised$s)
         
           UCL_x_bar_phase1_revised <- x_bar_bar_phase1_revised + A3 * s_bar_phase1_revised
           LCL_x_bar_phase1_revised <- x_bar_bar_phase1_revised - A3 * s_bar_phase1_revised
           UCL_s_phase1_revised <- B4 * s_bar_phase1_revised
           LCL_s_phase1_revised <- B3 * s_bar_phase1_revised
           ggplot(samples_phase1_revised, aes(x = 1:nrow(samples_phase1_revised))) +
            +     geom_line(aes(y = x_bar), color = "blue") +
            +     geom_point(aes(y = x_bar)) +
            +     geom_hline(yintercept = UCL_x_bar_phase1_revised, color = "red", linetype = "dashed") +
            +     geom_hline(yintercept = LCL_x_bar_phase1_revised, color = "red", linetype = "dashed") +
            +     geom_hline(yintercept = x_bar_bar_phase1_revised, color = "green") +
            +     labs(title = "Revised Phase I X-bar Chart (s-chart)", x = "Sample", y = "X-bar") +
            +     theme_minimal()
           ggplot(samples_phase1_revised, aes(x = 1:nrow(samples_phase1_revised))) +
            +     geom_line(aes(y = s), color = "blue") +
            +     geom_point(aes(y = s)) +
            +     geom_hline(yintercept = UCL_s_phase1_revised, color = "red", linetype = "dashed") +
            +     geom_hline(yintercept = LCL_s_phase1_revised, color = "red", linetype = "dashed") +
            +     geom_hline(yintercept = s_bar_phase1_revised, color = "green") +
            +     labs(title = "Revised Phase I s Chart", x = "Sample", y = "s") +
            +     theme_minimal()
           Womens_Basketballs_Phase2 <- read_excel("Womens Basketballs.xlsx", sheet = 2)
           samples_phase2 <- Womens_Basketballs_Phase2 %%
            +     select(-`Sample Number`) %%
            +     rowwise() %%
            +     mutate(
              +         x_bar = mean(c_across(everything())),
              +         s = sd(c_across(everything()))
              +     ) %%
            +     ungroup()
           ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2))) +
            +     geom_line(aes(y = x_bar), color = "blue") +
            +     geom_point(aes(y = x_bar)) +
            +     geom_hline(yintercept = UCL_x_bar_phase1_revised, color = "red", linetype = "dashed") +
            +     geom_hline(yintercept = LCL_x_bar_phase1_revised, color = "red", linetype = "dashed") +
            +     geom_hline(yintercept = x_bar_bar_phase1_revised, color = "green") +
            +     labs(title = "Phase II X-bar Chart (s-chart)", x = "Sample", y = "X-bar") +
            +     theme_minimal()
           ggplot(samples_phase2, aes(x = 1:nrow(samples_phase2))) +
            +     geom_line(aes(y = s), color = "blue") +
            +     geom_point(aes(y = s)) +
            +     geom_hline(yintercept = UCL_s_phase1_revised, color = "red", linetype = "dashed") +
            +     geom_hline(yintercept = LCL_s_phase1_revised, color = "red", linetype = "dashed") +
            +     geom_hline(yintercept = s_bar_phase1_revised, color = "green") +
            +     labs(title = "Phase II s Chart", x = "Sample", y = "s") +
            +     theme_minimal()
           