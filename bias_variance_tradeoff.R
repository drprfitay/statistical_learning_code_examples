q <- seq(from=0, to=1, by=0.1)
y <- 500 + 0.4 * (q-10)^3


q <- seq(from=0, to=1, length.out=200)
f <- function(x) {
  return((6 * x - 2)^2 * sin(12 * x - 
                               4))
}
noise <- rnorm(length(q), mean=1, sd=2)
noisy.y <- y(q) + noise



nreps=200
n_df = 20
resmt <- matrix(rep(0, times=nreps * n_df), nrow=nreps)




for (rep_i in 1:nreps) {
  
  all_indices <- 1:len(q)
  
  train_ind <- sample(all_indices, 150)
  test_ind <- all_indices[!all_indices %in% train_ind]

  for (poly_df in 1:n_df)   {
    
    #model <- lm(noisy.y[train_ind] ~ poly(q[train_ind],poly_df, raw=T)); 
    #predicted <- unlist(lapply(q[test_ind], function(x) {ply(x, model$coefficients)}))
    
    
    model <- smooth.spline(q[train_ind], noisy.y[train_ind], df=poly_df)
    predicted <- predict(model, q[test_ind])$y
    
    mse <- mean((predicted - y(q[test_ind])) ** 2)
    
    resmt[rep_i, poly_df] <- mse
  }
}

a <- lm(noisy.y ~ poly(q,5, raw=T)); 
plot(q, unlist(lapply(q, function(x) {ply(x, a$coefficients)})), type="l", col="red", lwd=2); points(q, noisy.y);



f = function(x) {
  x ^ 2
}

gen_sim_data = function(f, sample_size = 100) {
  x = runif(n = sample_size, min = 0, max = 1)
  y = rnorm(n = sample_size, mean = f(x), sd = 0.3)
  tibble(x, y)
}

gen_sim_data = function(f, sample_size = 100) {
  x = runif(n = sample_size, min = 0, max = 1)
  eps = rnorm(n = sample_size, mean = 0, sd = 0.75)
  y = f(x) + eps
  tibble(x, y)
}

set.seed(1)
n_sims = 250
n_models = 4
x = data.frame(x = seq(0, 1, length.out=10)) # fixed point at which we make predictions

pred_list = list()

for (i in 1:n_df) {
  pred_list[[i]] <- matrix(0, nrow = n_sims, ncol = 10)
}



for (sim in 1:n_sims) {
  
  # simulate new, random, training data
  # this is the only random portion of the bias, var, and mse calculations
  # this allows us to calculate the expectation over D
  sim_data = gen_sim_data(f)
  
  # fit models
  #fit_0 = lm(y ~ 1,                   data = sim_data)
  
  for (poly_df in 1:n_df) {
  
    fit = lm(y ~ poly(x, degree = poly_df), data = sim_data)
    pred_list[[poly_df]][sim,]  = predict(fit, x)
  }
  
  
  

}

get_bias = function(estimate, truth) {
  mean(estimate) - truth
}

get_mse = function(truth, estimate) {
  mean((estimate - truth) ^ 2)
}

get_var = function(estimate) {
  mean((estimate - mean(estimate)) ^ 2)
}

bias = apply(predictions, 2, get_bias, truth = f(x = 0.90))
variance = apply(predictions, 2, get_var)
#mse = lapply(pred_list, function(predictions) {mean(unlist(lapply(1:ncol(predictions), function(ci) {mean((predictions[,ci] - f(x)[ci,]) ** 2)})))})
