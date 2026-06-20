#Hypothesis Testing 2 Samples
# Given data
mean1 <- 8120.23
mean2 <- 6959.33
n1 <- 13
n2 <- 3
s1 <- 3020.16
s2 <- 1126.86

#calculate t statistics
t_stat = (mean1-mean2-0)/(sqrt((s1^2/n1)+(s2^2/n2))) 

#calculate degree of freedom
v =((s1^2/n1)+(s2^2/n2))^2/((((s1^2/n1)^2)/(n1-1))+(((s2^2/n2)^2)/(n2-1)))

# Calculate p-value
p_value = 2 * pt(-abs(t_stat), floor(v))

#find t-value from the table
alpha = 0.05
t.alpha = qt(alpha/2, floor(v))




#Correlation Test
x <- c(8517, 5550, 4885, 8057, 6788, 5777, 8267, 5779, 5664, 12233, 7248, 6171, 6457, 13325, 8250, 13473)
y <- c(5342, 3765, 3505, 5707, 4678, 4107, 5322, 3903, 3834, 6770, 4796, 3342, 3915, 7823, 4176, 8897)

r = cor(x,y)
r
cat("Correlation coefficient (r):", r, "\n")

plot(x,y, xlim=c(0,15000), ylim=c(0,10000), xlab="Income Mean", ylab="Expenditure Mean")

n <- length(x)
df = n-2
t_value <- r / sqrt((1 - r^2) / df)

# Print the degrees of freedom and the t-value
cat("Degrees of freedom (df):", df, "\n")
cat("t-value:", t_value, "\n")

# Calculate the p-value for the t-test
p_value <- 2 * pt(-abs(t_value), df)

# Print the p-value
cat("p-value:", p_value, "\n")

# Determine if the p-value is less than the significance level (e.g., 0.05)
if (p_value < 0.05) {
  cat("Reject the null hypothesis: There is a significant linear correlation.\n")
} else {
  cat("Fail to reject the null hypothesis: There is no significant linear correlation.\n")
  
}





#Regression test
x <- c(8517, 5550, 4885, 8057, 6788, 5777, 8267, 5779, 5664, 12233, 7248, 6171, 6457, 13325, 8250, 13473)
y <- c(5342, 3765, 3505, 5707, 4678, 4107, 5322, 3903, 3834, 6770, 4796, 3342, 3915, 7823, 4176, 8897)

model <- lm(y~x)
model

summary (model)

plot (x,y)
abline (model)




#ANNOVA Test
# Given data
n <-n1 <- n2 <- n3 <- n4 <- 2
k <- 4
mean1 <- 5331
mean2 <- 6908.5
mean3 <- 12779
mean4 <- 7422.5
sd1 <- 630.74
sd2 <- 1921.21
sd3 <- 772.16
sd4 <- 897.32

# 1. Calculate mean, standard deviation, and variance BETWEEN samples
overall_mean <- mean(c(mean1, mean2, mean3, mean4))

# Sum of squares between (SSB)
sd_between <- sqrt(((mean1-overall_mean)^2 + (mean2-overall_mean)^2 + 
                      (mean3-overall_mean)^2 + (mean4-overall_mean)^2)/(4-1))

variance_between <- n*sd_between^2

# 2. Calculate variance WITHIN samples
# Sum of squares within (SSW)
variance_within <- (sd1^2 + sd2^2 + sd3^2 + sd4^2)/k

F_statistic <- variance_between / variance_within

# 4. Calculate degrees of freedom
numerator <- k - 1
denominator <- k * (n - 1)

# 5. Find critical value with alpha = 0.05 from F-distribution table
alpha <- 0.05
f.alpha <- qf(1 - alpha, numerator, denominator)

# Calculate p-value
p_value <- 1 - pf(F_statistic, numerator, denominator)       
       




  