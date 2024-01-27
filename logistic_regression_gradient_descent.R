library("pheatmap")
library("reshape2")
library("ggplot2")
library("cowplot")
ph <- function(...) {pheatmap(cluster_rows=F, cluster_cols=F, ...)}

logistic_function <- function(x, b0, b1) {1/(1+exp(-(b0 + b1*x)))}

N_samples = 500
group_A <- rnorm(N_samples, 0, 3)
group_B <- rnorm(N_samples, 11, 4)

learning_rate_b0 <- .001
learning_rate_b1 <- .001

labels <- rep(c(0,1), each=N_samples)

b0_start = 0.5
b1_start = 0.5

test_range <- seq(min(group_A), max(group_B), length.out=1000)
plot(c(group_A, group_B), labels)
lines(x=test_range, logistic_function(test_range,b0_start, b1_start))



likelihood <- function(b0,
                 b1)
{
  return(prod(logistic_function(group_B, b0, b1)) * prod(1-logistic_function(group_A, b0,b1)))
}

likelihood_ln <- function(b0, b1) {
  return(sum(log(logistic_function(group_B, b0,b1))) + sum(log(1 - logistic_function(group_A, b0,b1))))
}

delta_b0 <- function(b0,
                     b1) {
  
  return(sum(logistic_function(c(group_A, group_B), b0, b1) - labels))
}

delta_b1 <- function(b0,
                     b1) {
  
  return(sum((logistic_function(c(group_A, group_B), b0, b1) - labels) * c(group_A, group_B)))
}
  
  
  



N_iter <- 10001
b0 <- b0_start
b1 <- b1_start
grad_mt <- matrix(nrow=3,ncol=(N_iter))
rownames(grad_mt) <- c("b0", "b1", "likelihood")

grad_mt["b0", 1] <- b0
grad_mt["b1", 1] <- b1
grad_mt["likelihood", 1] <- likelihood(b0, b1)

for (iter in 2:N_iter) {
  
  b0 <- b0 - learning_rate_b0 * delta_b0(b0 , b1)
  b1 <- b1 - learning_rate_b1 * delta_b1(b0 , b1)
  
  grad_mt["b0", iter] <- b0
  grad_mt["b1", iter] <- b1
  grad_mt["likelihood", iter] <- likelihood_ln(b0, b1)
  
}

plot(c(group_A, group_B), labels)
lines(x=test_range, logistic_function(test_range,b0_start, b1_start), lwd=2, lty=2)
lines(x=test_range, logistic_function(test_range,b0, b1), col="red", lwd=2, lty=2)







val_mt <- matrix(nrow=200,ncol=200)

b0_vals <-  seq(min(grad_mt["b0",]), max(grad_mt["b0",]), length.out=200)
b1_vals <-  seq(min(grad_mt["b1",]), max(grad_mt["b1",]), length.out=200)
for (b0_idx in 1:200) {
  for (b1_idx in 1:200) {
    val_mt[b0_idx, b1_idx] <- likelihood_ln(b0_vals[b0_idx],
                                            b1_vals[b1_idx])
    
  }
}


colnames(val_mt) <- b1_vals
rownames(val_mt) <- b0_vals

melted <- melt(val_mt)
colnames(melted) <- c("x", "y", "Likelihood")


g_gradient <- 
ggplot(melted, aes(x=x,y=y)) + geom_tile(aes(fill=Likelihood)) + 
  scale_fill_distiller(palette="RdYlBu") +
  geom_line(data=as.data.frame(t(grad_mt)), aes(x=b0, y=b1)) +
  theme_classic() + 
  xlab("b0") + 
  ylab("b1")



data <- data.frame(x=c(group_A, group_B), y=labels)
test_data <- data.frame(x=test_range, y=logistic_function(test_range,b0, b1))

g_test <- 
  ggplot(data=data, aes(x=x, y=y)) +geom_point() + xlab("X") + ylab("Label") +
    geom_line(data=test_data, linetype="dashed", col="red", size=1)


plot_grid(g_test, g_gradient, nrow=1)
