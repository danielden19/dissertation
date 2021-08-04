xi = matrix(c(0.9, 0.01, 0.01, 0.08, 0.04, 0.85, 0.01, 0.1, 
              0.01, 0.01, 0.94, 0.04, 0.005, 0.005, 0.05, 0.94),  
            nrow = 4, byrow = TRUE)
xi
mu = c(1,15,80,150)
sigma = c(0.5, 2, 8, 10)
sim = simulate_normal_hmm(xi, mu, sigma, 5, c(0.8, 0.05, 0.07, 0.08))
(sim2 = simulate_normal_hmm(xi, mu, sigma, 20, c(0.8, 0.05, 0.07, 0.08)))
sim2
(sim3 = simulate_normal_hmm(xi, mu, sigma, 20, c(0.8, 0.05, 0.07, 0.08)))

xi2 = matrix(c(50, 1, 1, 1, 1, 50, 1, 1, 1, 1, 50, 1, 1, 1, 1, 50), nrow = 4, ncol = 4, byrow = TRUE)
xi2 = xi2/53
rowSums(xi2)
sim4 = simulate_normal_hmm(xi2, mu, sigma, 1000, c(0.6, 0.3, 0.07, 0.03))
sim4
sim
forward_filtering(sim, 4, c(0.8, 0.05, 0.07, 0.08), xi, mu, sigma)

forward_filtering_LSE(sim, 4, c(0.8, 0.05, 0.07, 0.08), xi, mu, sigma)

ff[[2]]
ff_LSE[[2]]
all.equal(ff[[2]],ff_LSE[[2]])

## All the filtered probabilities are equal

backwards_smoothing(sim, 4, c(0.8, 0.05, 0.07, 0.08), xi, mu, sigma)
backwards_sampling(sim, 4, c(0.8, 0.05, 0.07, 0.08), xi, mu, sigma)

gibbs_test = my_gibbs(10, prior_mu = list(m = 12, v = 5, mu = c(1, 10, 50, 100), sigmasq = c(0.5, 1, 2, 3)), 
                      prior_sigmasq = list(a = 3, b = 3, sigmasq = c(1, 3, 6, 9)), 
                      prior_xi = prior_xi,
                      y = sim2, K = 4, S0_distribution = c(0.5, 0.3, 0.15, 0.05))

(gibbs_test2 = my_gibbs(1000, prior_mu = list(m = 12, v = 5, mu = c(1, 10, 50, 100), sigmasq = c(0.5, 1, 2, 3)), 
                       prior_sigmasq = list(a = 3, b = 3, sigmasq = c(1, 3, 6, 9)), 
                       prior_xi = prior_xi,
                       y = sim4, K = 4, S0_distribution = c(0.5, 0.3, 0.15, 0.05)))

colMeans(gibbs_test[[1]])
colMeans(gibbs_test[[2]])
colMeans(gibbs_test[[3]]) # Would need to write a function to get the mean of xi
colMeans(gibbs_test[[4]])

par(mfrow = c(2,2))
hist(gibbs_test2[[1]][,1], xlab = "Value", main = "mu_1")
hist(gibbs_test2[[1]][,2], xlab = "Value", main = "mu_2")
hist(gibbs_test2[[1]][,3], xlab = "Value", main = "mu_3")
hist(gibbs_test2[[1]][,4], xlab = "Value", main = "mu_4")
par(mfrow = c(1,1))

plot(c(1:length(sim4[[1]])), sim4[[1]], type = "n")
lines(c(1:length(sim4[[1]])), sim4[[1]])

getmode(gibbs_test2[[4]])[[1]] - sim4[[2]]
plot(1:1001, getmode(gibbs_test2[[4]])[[1]], type = "l")
lines(1:1001, sim4[[2]], col = "red")

diagnostics(gibbs_test2[[3]][-(1:500),])

backwards_sampling(sim2, 4, c(0.5, 0.3, 0.15, 0.05), 
                   matrix(c(0.85,0.05, 0.05, 0.05, 
                            0.1, 0.85, 0.04, 0.01, 
                            0.01, 0.04, 0.9, 0.05, 
                            0.01, 0.03, 0.06, 0.9), nrow = 4, ncol = 4, byrow = TRUE),
                   rnorm(1, c(1, 10, 50, 100), c(0.5, 1, 3, 5)),
                   1/rgamma(1, 3, 3))

mu_FCD(0.5,0.25,sim, sigma, 4)
mu_FCD(0.5,0.25,sim2, sigma, 4)
mu_FCD(0.5,0.25,sim3, sigma, 4)

sigmasq_FCD(2,4,sim, mu, 4)
sigmasq_FCD(0.5,0.25,sim2, sigma, 4)
sigmasq_FCD(0.5,0.25,sim3, sigma, 4)
sigmasq_FCD(a = 3, b = 3, y, mu = c(1, 10, 50, 100), 4)

xi_FCD(xi, sim)
xi_FCD(xi, sim2)
xi_FCD(xi, sim3)
xi_FCD(prior_xi, sim3)

## Testing a 3-state chain
mu = c(1, 20, 40)
sigmasq = c(0.5, 2, 1)

xi_k3 = matrix(1, nrow = 3, ncol = 3)
diag(xi_k3) = 33
xi_k3 = xi_k3 / 35
rowSums(xi_k3)
sim_k3 = simulate_normal_hmm(xi_k3, mu, sigmasq, 1000, c(0.2,0.4,0.4))
plot(ts(sim_k3[[1]]))

prior_xi = matrix(1, nrow = 3, ncol = 3)
diag(prior_xi) = 33
gibbs_test_k3 = my_gibbs(10000, prior_mu = list(m = 12, v = 10000), 
                       prior_sigmasq = list(a = 3, b = 3), 
                       prior_xi = prior_xi,
                       y = sim_k3[[1]], K = 3, S0_distribution = c(rep(1/3, 3)))

backwards_sampling(sim_k3[[1]], 3, c(0.2,0.4,0.4), xi_k3, mu, sigmasq)

diagnostics(gibbs_test_k3[[1]][-(1:5000),])
#
diagnostics(gibbs_test_k3[[2]][-(1:5000),])
#
diagnostics(gibbs_test_k3[[3]][-(1:5000),])
#
## All look to be sound with regards to plots

par(mfrow=c(1,1), ask=FALSE)
plot(1:length(sim_k3[[2]]), getmode(gibbs_test_k3[[4]])[[1]], type = "l")
lines(1:length(sim_k3[[2]]), sim_k3[[2]], col = "red")

## Testing a 5-state chain
mu = c(1, 30, 70, 150, 200)
sigmasq = c(0.5, 2, 1, 2, 2)

xi_k5 = matrix(1, nrow = 5, ncol = 5)
diag(xi_k5) = 67
xi_k5 = xi_k5 / 71
rowSums(xi_k5)
sim_k5 = simulate_normal_hmm(xi_k5, mu, sigmasq, 1000, c(0.2,0.1,0.3,0.25,0.15))
plot(ts(sim_k5[[1]]))

prior_xi = matrix(1, nrow = 5, ncol = 5)
diag(prior_xi) = 67
gibbs_test_k5 = my_gibbs(10000, prior_mu = list(m = 12, v = 10000), 
                         prior_sigmasq = list(a = 3, b = 3), 
                         prior_xi = prior_xi,
                         y = sim_k5[[1]], K = 5, S0_distribution = c(rep(1/5, 5)))


diagnostics(gibbs_test_k5[[1]][-(1:5000),])
#
diagnostics(gibbs_test_k5[[2]][-(1:5000),])
#
diagnostics(gibbs_test_k5[[3]][-(1:5000),])


## All look to be sound with regards to plots

par(mfrow=c(1,1), ask=FALSE)
plot(1:length(sim_k5[[2]]), getmode(gibbs_test_k5[[4]])[[1]], type = "l")
lines(1:length(sim_k5[[2]]), sim_k5[[2]], col = "red")

# Testing the beta_tilde_FCD
beta_tilde_FCD(sim4[[1]], list("v" = 10000, "m" = 0), x_tilde, sigmasq, beta = c(1.5, 1.7, 4, 9), states = sim4[[2]], 4)

x_tilde = data.frame(data$Weekend, data$Bank.Holiday.2, data$NE_CWV)
x_tilde$sin_365 = sin(2*pi*c(1:4030)/365.25)
x_tilde$cos_365 = cos(2*pi*c(1:4030)/365.25)
x_tilde = as.matrix(x_tilde)


#####################


## Testing the Gibbs sampler on actual data
prior_xi = matrix(1, 4, 4)
diag(prior_xi) = 60
mu = rep(0, 4)
sigmasq = rep(1000, 4)
m = 0
v = 10000
prior_mu = list(m = m, v = v, mu = mu, sigmasq = sigmasq)
a = 3
b = 3
prior_sigmasq = list(a = a, b = b, sigmasq = sigmasq)
y = data$log_NE_DM
S0_distribution
niters
gibbs_test_data = my_gibbs(niters = niters, prior_mu = prior_mu, prior_sigmasq = prior_sigmasq, 
         prior_xi = prior_xi, y = y, K = K, S0_distribution = S0_distribution)
backwards_sampling(y, K, S0_distribution, prior_xi, mu, sigmasq)

diagnostics(gibbs_test_data[[1]][-(1:5000),])
#
diagnostics(gibbs_test_data[[2]][-(1:5000),])
#
diagnostics(gibbs_test_data[[3]][-(1:5000),])

par(mfrow = c(2,1))
getmode(gibbs_test_data[[4]])
plot(1:4031, getmode(gibbs_test_data[[4]])[[1]], type = "l")
plot(1:4030, data$log_NE_DM, type = "l")

## Next I need to make this work with tilde inclusions

states = getmode(gibbs_test_data[[4]])
x = matrix(0, nrow = length(states), ncol = K)
for (i in 1:length(states)) {
  x[i, states[i]] = 1
}

v = matrix(0, 5, 5)
diag(v) = 1
v = 10000*v 
beta_tilde_prior = list(m = rep(0, 5), v = v)
beta_tilde_FCD(y, beta_tilde_prior, x_tilde, sigmasq, beta, states, K)

beta_tilde_FCD = function(y, beta_tilde_prior, x, sigmasq, beta, states, K){
  N = length(y)
  Z = y - beta[states[-1]]
  #x_tilde = x[, -(1:K)]
  updated_beta_tilde = numeric(5)
  sum1 = matrix(0, 5, 5)
  sum2 = numeric(5)
  for (t in 1:N) {
    sum1 = sum1 + x_tilde[t,]%*%t(x_tilde[t,])/sigmasq[states[t+1]]
    sum2 = sum2 + x_tilde[t,]*Z[t]/sigmasq[states[t+1]]
  }
  beta_tilde_mean = solve(solve(beta_tilde_prior$v) + sum1) %*% (solve(beta_tilde_prior$v)%*%beta_tilde_prior$m + sum2)
  beta_tilde_sd = solve(solve(beta_tilde_prior$v) + sum1)
  updated_beta_tilde = rmvnorm(1, mean = beta_tilde_mean, sigma = beta_tilde_sd)
  # Finish calculation of mean and variance
  return(updated_beta_tilde)
}

gibbs_test_data_tilde = my_gibbs(niters, prior_mu, prior_sigmasq, prior_xi, beta_tilde_prior, y, K, S0_distribution, beta, x_tilde)

