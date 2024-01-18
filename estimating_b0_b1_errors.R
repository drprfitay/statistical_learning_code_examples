


noise_var <- 6 ** 2
true_slope <- 8
true_intercept <- 2
true_model <- function(x) {true_slope * x + true_intercept}

get_sample_data <- function(n)  { 
    x <- runif(n, -2,2); 
    y <- true_model(x) + rnorm(n, sd=sqrt(noise_var));
    return(data.frame(x=x,y=y))
}


N_iter = 1000
sample_size = 50

correct_interval_b0 <- c()
correct_interval_b1 <- c()

for (i in 1:N_iter) {

  sampled <- get_sample_data(sample_size)
  linear_reg <- lm(y~x, data=sampled)
  
 
  b0 <- linear_reg$coefficients[1]
  b1 <- linear_reg$coefficients[2]
  
  stand_error_slope <- noise_var / sum((sampled$x - mean(sampled$x)) ** 2)
  stand_error_intercept <- noise_var * ((1 / sample_size) +  (mean(sampled$x) ** 2 / sum((sampled$x - mean(sampled$x)) ** 2)))
  
  b1_confidence_interval  <- c(b1 + qnorm(.025) * sqrt(stand_error_slope),
                               b1 + qnorm(1 - .025) * sqrt(stand_error_slope))
  
  print(b1_confidence_interval)
  b0_confidence_interval  <- c(b0 + qnorm(.025) * sqrt(stand_error_intercept),
                               b0 + qnorm(1 - .025) * sqrt(stand_error_intercept))
  
  correct_interval_b1 <- c(correct_interval_b1,
                            as.numeric(true_slope > b1_confidence_interval[1] & 
                                       true_slope < b1_confidence_interval[2]))
  
  correct_interval_b0 <- c(correct_interval_b0,
                           as.numeric(true_intercept > b0_confidence_interval[1] & 
                                      true_intercept < b0_confidence_interval[2]))
                               
}


barplot(table(correct_interval_b1) / N_iter)
abline(h=.05, lty=2)

barplot(table(correct_interval_b0) / N_iter)
abline(h=.05, lty=2)






# Get the distribution of errors for randomly estimated values

error_distribution_b0 <- c()
error_distribution_b1 <- c()

estimation_iterations <- 250
for (i in 1:estimation_iterations) {
  noise_var <- runif(1, 2,50)
  true_slope <- runif(1, -25,25)
  true_intercept <- runif(1,-5,5)
  
  testing_range <- runif(2, -50,50)
  
  true_model <- function(x) {true_slope * x + true_intercept}
  get_sample_data <- function(n)  { 
    x <- runif(n, min(testing_range),max(testing_range)); 
    y <- true_model(x) + rnorm(n, sd=sqrt(noise_var));
    return(data.frame(x=x,y=y))
  }
  
  
  N_iter = 200
  sample_size = floor(runif(1,10, 50))
  
  correct_interval_b0 <- c()
  correct_interval_b1 <- c()
  
  for (j in 1:N_iter) {
    
    sampled <- get_sample_data(sample_size)
    linear_reg <- lm(y~x, data=sampled)
    
    
    b0 <- linear_reg$coefficients[1]
    b1 <- linear_reg$coefficients[2]
    
    stand_error_slope <- noise_var / sum((sampled$x - mean(sampled$x)) ** 2)
    stand_error_intercept <- noise_var * ((1 / sample_size) +  (mean(sampled$x) ** 2 / sum((sampled$x - mean(sampled$x)) ** 2)))
    
    b1_confidence_interval  <- c(b1 + qnorm(.025) * sqrt(stand_error_slope),
                                 b1 + qnorm(1 - .025) * sqrt(stand_error_slope))
    
    
    b0_confidence_interval  <- c(b0 + qnorm(.025) * sqrt(stand_error_intercept),
                                 b0 + qnorm(1 - .025) * sqrt(stand_error_intercept))
    
    correct_interval_b1 <- c(correct_interval_b1,
                             as.numeric(true_slope > b1_confidence_interval[1] & 
                                          true_slope < b1_confidence_interval[2]))
    
    correct_interval_b0 <- c(correct_interval_b0,
                             as.numeric(true_intercept > b0_confidence_interval[1] & 
                                          true_intercept < b0_confidence_interval[2]))
    
  }
  
  
  b1_error_fraction <- (table(correct_interval_b1) / N_iter)[1]
  b0_error_fraction <- (table(correct_interval_b0) / N_iter)[1]
  
  print(sprintf("Estimating model y = %.2f * x + %.2f + noise(0,sigma**2=%.2f)", true_slope, true_intercept, noise_var))
  print(sprintf("Estimating over %d samples range [%.2f,%.2f]", sample_size, min(testing_range), max(testing_range)))
  print(sprintf("Over %d iterations, confidence interval were wrong (%.3f for b0; %.3f for b1)", N_iter, b0_error_fraction, b1_error_fraction))
  
  
  error_distribution_b0 <- c(error_distribution_b0, b0_error_fraction)
  error_distribution_b1 <- c(error_distribution_b1, b1_error_fraction)
}

hist(error_distribution_b0, breaks=15)
abline(v=.05, lty=2, lwd=2, col="red")
abline(v=mean(error_distribution_b0), lty=2, lwd=2, col="black")
legend(x="topright", legend = c("true", "mean"), col=c("red","black"), lty=c(2,2), lwd=c(2,2))  

hist(error_distribution_b1, breaks=15)
abline(v=.05, lty=2, lwd=2, col="red")
abline(v=mean(error_distribution_b1), lty=2, lwd=2, col="black")
legend(x="topright", legend = c("true", "mean"), col=c("red","black"), lty=c(2,2), lwd=c(2,2))  



