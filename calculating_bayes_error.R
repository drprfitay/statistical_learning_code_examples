library(MASS)
library(pheatmap)


ph <- function(mt, ...) {pheatmap(mt,cluster_rows=F, cluster_cols=F, ...)}
len <- length

set.seed(1000)

N_gaussians_to_combine <- 10
means_class_1 = mvrnorm(N_gaussians_to_combine,c(1,0),diag(rep(1,2)))
means_class_2 = mvrnorm(N_gaussians_to_combine,c(0,1),diag(rep(1,2)))


N_mat <- 150
empty_mat <- matrix(rep(0,N_mat*N_mat),N_mat)

density_class_1 <- empty_mat
density_class_2 <- empty_mat
classification_boundary <- empty_mat

x <- seq(-4,5,length.out=N_mat)
y <- seq(-4,5,length.out=N_mat)

dx=x[2]-x[1]
dy=y[2]-y[1]


for (i in 1:N_mat) {
  for (j in 1:N_mat) {
    for (k in 1:N_gaussians_to_combine)
    {
      density_class_1[i,j] <-
        density_class_1[i,j] +
        (1/N_gaussians_to_combine) * dmvnorm(c(x[i],y[j]),means_class_1[k,],diag(rep(1,2)/5)) *dx*dy
      
      density_class_2[i,j] <- 
        density_class_2[i,j] + 
        (1/N_gaussians_to_combine) * dmvnorm(c(x[i],y[j]),means_class_2[k,],diag(rep(1,2)/5)) * dx * dy
    }
  }
}


for (i in 1:N_mat) {
  for (j in 1:N_mat) {
    classification_boundary[i,j] <- which.max(c(density_class_1[i,j],
                             density_class_2[i,j]))
  }
}


prob_wrong_class_B <- c()
prob_wrong_class_A <- c()

for (i in 1:N_mat) {
  for (j in 1:N_mat) {
    prob_wrong_class_A <- c(prob_wrong_class_A, as.numeric(classification_boundary[i,j] != 1) * density_class_1[i,j])
    prob_wrong_class_B <- c(prob_wrong_class_B, as.numeric(classification_boundary[i,j] != 2) * density_class_2[i,j])
  }
}


bayes_error = mean(c(sum(prob_wrong_class_B), sum(prob_wrong_class_A)))


ph(density_class_1)
ph(density_class_2)
ph(classification_boundary, border_col=NA)




N_testing = 500

gaussian_label_test_class_1 = sample(N_gaussians_to_combine,N_testing,TRUE)
gaussian_label_test_class_2 = sample(N_gaussians_to_combine,N_testing,TRUE)

test_class_1_data = t(sapply(gaussian_label_test_class_1,function(x) mvrnorm(1,means_class_1[x,],diag(rep(1,2)/5))))
test_class_2_data = t(sapply(gaussian_label_test_class_2,function(x) mvrnorm(1,means_class_2[x,],diag(rep(1,2)/5))))

test_class = rep(c(1,2), each=N_testing)


test = list(X = rbind(test_class_1_data,test_class_2_data), 
            Y = test_class)

errors <- 
sapply(1:30, function(k_val) {
  gaussian_label_training_class_1 = sample(N_gaussians_to_combine,N_mat,TRUE)
  gaussian_label_training_class_2 = sample(N_gaussians_to_combine,N_mat,TRUE)
  
  
  training_class_1_data = t(sapply(gaussian_label_training_class_1,function(x) mvrnorm(1,means_class_1[x,],diag(rep(1,2)/5))))
  training_class_2_data = t(sapply(gaussian_label_training_class_2,function(x) mvrnorm(1,means_class_2[x,],diag(rep(1,2)/5))))
  
  training_class = rep(c(1,2), each=N_mat)
  training = list(X = rbind(training_class_1_data,training_class_2_data),
                  Y = training_class)
  
  predicted_knn_class <- knn(training$X, test$X, training$Y, k=k_val)
  
  tabulated <-  table(predicted_knn_class,test$Y)
  error <- 1 - sum(diag(tabulated))/sum(tabulated)
  return(error)
})

