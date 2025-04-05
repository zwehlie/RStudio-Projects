#Homework 3
carseats <- read.table("C:/Users/zwehl/OneDrive/Desktop/Stat 7210 Work Files/carseats2.txt", header = TRUE)

#Problem 1: Question 1
carseats <- read.table("carseats2.txt", header = TRUE)
carseats$HighSales <- as.factor(carseats$HighSales)
#logistic regression model (price variable)
model1 <- glm(HighSales ~ Price, data = carseats, family = binomial)
summary(model1)

#Problem 1: Question 3
pred_prob <- predict(model1, newdata = data.frame(Price = 100), type = "response")
pred_prob

# Problem 1: Question 4
model2 <- glm(HighSales ~ Price + US, data = carseats, family = binomial)
summary(model2)

# Problem 1: Question 5
pred_prob_us <- predict(model2, newdata = data.frame(Price = 100, US = "Yes"), type = "response")
pred_prob_us

# Problem 1: Question 6
pred_prob_non_us <- predict(model2, newdata = data.frame(Price = 100, US = "No"), type = "response")
pred_prob_non_us



#Homework 3: Problem 2
shills <- read.table("C:/Users/zwehl/OneDrive/Desktop/Stat 7210 Work Files/SHillsRaces.txt",header = TRUE, sep = ",", fill = TRUE)


# Problem 2: Question 1
extern_resid <- rstudent(modelA)[33]
extern_resid
t_value_U33 <- summary(modelB)$coefficients["U33", "t value"]
t_value_U33



#Problem 2: Question 2
#The F-test for testing Model A versus Model B 
anova_results <- anova(modelA, modelB)
anova_results 

F_stat <- anova_results$"F"[2]
F_stat 
F_stat_comparison <- (extern_resid)^2
F_stat_comparison 


#Problem 2: Question 3
modelA_exclude <- lm(Time ~ Distance + Climb, data = shills, subset = (1:nrow(shills)) != 33)
summary(modelA_exclude)



#Problem 2: Question 4
coef_modelB <- coef(modelB)[c("(Intercept)", "Distance", "Climb")]
coef_modelA_exclude <- coef(modelA_exclude)[c("(Intercept)", "Distance", "Climb")]
coef_modelB
coef_modelA_exclude







