#Problem Set 8 

#QUESTION 4
set.seed(100)

N <- 100000
K <- 10

X <- matrix(rnorm(N * K), nrow = N, ncol = K)
X[, 1] <- 1  

eps <- rnorm(N, mean = 0, sd = 0.5)

beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

Y <- X %*% beta + eps



#QUESTION 5
beta_ols_closed <- solve(t(X) %*% X) %*% t(X) %*% Y

print(beta_ols_closed)
#           [,1]
#[1,]  1.5005793
#[2,] -0.9956182
#[3,] -0.2486498
#[4,]  0.7471903
#[5,]  3.5017669
#[6,] -1.9994365
#[7,]  0.5011339
#[8,]  0.9987400
#[9,]  1.2528300
#[10,]  1.9993846

print(beta)
#[1]  1.50 -1.00 -0.25  0.75  3.50 -2.00  0.50  1.00  1.25  2.00



#QUESTION 6 
ols_gradient <- function(beta, X, Y, learning_rate) {
  grad <- -2 * t(X) %*% (Y - X %*% beta)
  beta <- beta - learning_rate * grad
  return(beta)
}

learning_rate <- 0.0000003
num_iterations <- 1000

beta_ols_gd <- rep(0, K)

for (i in 1:num_iterations) {
  beta_ols_gd <- ols_gradient(beta_ols_gd, X, Y, learning_rate)
}

print(beta_ols_gd)
#[,1]
#[1,]  1.5005793
#[2,] -0.9956182
#[3,] -0.2486498
#[4,]  0.7471903
#[5,]  3.5017669
#[6,] -1.9994365
#[7,]  0.5011339
#[8,]  0.9987400
#[9,]  1.2528300
#[10,]  1.9993846



#QUESTION 7 
library(nloptr)
ols_objective <- function(beta, X, Y) {
  residuals <- Y - X %*% beta
  return(sum(residuals^2))
}

ols_gradient <- function(beta, X, Y) {
  residuals <- Y - X %*% beta
  return(-2 * t(X) %*% residuals)
}

beta_ols_lbfgs <- nloptr::nloptr(
  x0 = rep(0, K),
  eval_f = function(beta) ols_objective(beta, X, Y),
  eval_grad_f = function(beta) ols_gradient(beta, X, Y),
  opts = list(algorithm = "NLOPT_LD_LBFGS", xtol_rel = 1e-6)
)$solution

print(beta_ols_lbfgs)
#[1]  1.5005793 -0.9956182 -0.2486498  0.7471903  3.5017669 -1.9994365
#[7]  0.5011339  0.9987400  1.2528300  1.9993846


#Nelder-Mead
beta_ols_nm <- nloptr::nloptr(
  x0 = rep(0, K),
  eval_f = function(beta) ols_objective(beta, X, Y),
  opts = list(algorithm = "NLOPT_LN_NELDERMEAD", xtol_rel = 1e-6)
)$solution

print(beta_ols_nm)
#[1]  0.9767063 -0.9680386 -0.1361704  1.1277353  3.2668671 -2.1447235
#[7]  0.5663044  1.0422382  1.4966523  2.3011765



#QUESTION 8 
mle_objective <- function(theta, X, Y) {
  beta <- theta[1:(length(theta) - 1)]
  sig <- theta[length(theta)]
  residuals <- Y - X %*% beta
  return(dim(X)[1] * log(sig) + crossprod(residuals) / (2 * sig^2))
}

gradient <- function(theta, Y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta) - 1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta) - 1)] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
  grad[length(theta)] <- dim(X)[1] / sig - crossprod(Y - X %*% beta) / (sig^3)
  return(grad)
}

beta_init <- rep(0, K)
sigma_init <- 1

theta_init <- c(beta_init, sigma_init)

beta_mle <- nloptr::nloptr(
  x0 = theta_init,
  eval_f = function(theta) mle_objective(theta, X, Y),
  eval_grad_f = function(theta) gradient(theta, Y, X),
  opts = list(algorithm = "NLOPT_LD_LBFGS")
)$solution

print(beta_mle[1:K])
#[1] -16.424178 -13.183912  11.473798 -17.726251  -8.478435  17.824047
#[7]   3.648497  -3.786587  -4.250991 -11.634915



#QUESTION 9 
beta_ols_lm <- lm(Y ~ X - 1)
print(beta_ols_lm)

library(modelsummary)
modelsummary(beta_ols_lm, output = "table.tex")
