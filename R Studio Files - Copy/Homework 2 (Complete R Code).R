install.packages("readxl")
install.packages("qcc")

library(readxl)
library(readxl)
library(qcc)

file_path <- "C:/Users/zwehl/OneDrive - Kennesaw State University/R Studio Files/Assignments Final Products/Baggage Claim (1).xlsx"
baggage_data <- read_excel(file_path)

# Calculate the mean and standard deviation
luggage_times_matrix <- as.matrix(baggage_data[,2:6]) 
luggage_times <- as.vector(luggage_times_matrix)
mean_luggage_time <- mean(luggage_times)
std_luggage_time <- sd(luggage_times)

mean_luggage_time
std_luggage_time

# Question 2: CUSUM Chart
target_mean <- 10  # This is the target mean
k <- 0.5  
h <- 5

cusum_pos <- numeric(length(luggage_times))
cusum_neg <- numeric(length(luggage_times))

for (i in 2:length(luggage_times)) {
  cusum_pos[i] <- max(0, cusum_pos[i-1] + (luggage_times[i] - (target_mean + k)))
  cusum_neg[i] <- max(0, cusum_neg[i-1] + ((target_mean - k) - luggage_times[i]))
}

# Plot CUSUM Chart 
plot(cusum_pos, type="l", col="blue", ylim=c(min(c(cusum_pos, cusum_neg)), max(c(cusum_pos, cusum_neg))),
     ylab="CUSUM", xlab="Observation", main="CUSUM Chart for Luggage Claim Times")
lines(cusum_neg, col="red")
abline(h=h, col="green", lty=2)  
legend("topleft", legend=c("CUSUM Positive", "CUSUM Negative", "Decision Interval (h=5)"),
       col=c("blue", "red", "green"), lty=c(1,1,2))

#Question 3 EWMA

luggage_times_matrix <- as.matrix(baggage_data[,2:6]) 
luggage_times <- as.vector(luggage_times_matrix)

#Verify that luggage_times
print(luggage_times)

#target mean and parameters for EWMA chart
target_mean <- 10  # company's goal 
lambda <- 0.2      # Smoothing constant
L <- 2.962         # Control limit multiplier 

#Calculate the EWMA values
ewma <- numeric(length(luggage_times))
ewma[1] <- luggage_times[1]  # First EWMA value is the first observation

#Calculate subsequent EWMA values
for (i in 2:length(luggage_times)) {
  ewma[i] <- lambda * luggage_times[i] + (1 - lambda) * ewma[i - 1]
}

# Calculate the standard deviation and control limits
std_luggage_time <- sd(luggage_times)  # Standard deviation of luggage times
sigma_ewma <- std_luggage_time * sqrt(lambda / (2 - lambda))  # Standard deviation 
ucl <- target_mean + L * sigma_ewma  # (UCL)
lcl <- target_mean - L * sigma_ewma  # (LCL)

#Plot EWMA chart
plot(ewma, type = "l", col = "blue", ylim = range(lcl, ewma, ucl),
     ylab = "EWMA", xlab = "Observation", main = "EWMA Chart for Luggage Claim Times")

abline(h = target_mean, col = "green", lty = 2)  # Target mean line
abline(h = ucl, col = "red", lty = 2)  # UCL
abline(h = lcl, col = "red", lty = 2)  # LCL


legend("topleft", legend = c("EWMA", "Target Mean", "Control Limits"),
       col = c("blue", "green", "red"), lty = c(1, 2, 2))

# Verify control limits
print(paste("UCL:", ucl, "LCL:", lcl))

  
  
  