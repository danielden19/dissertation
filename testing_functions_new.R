#xi = matrix(c(0.9, 0.01, 0.01, 0.08, 0.04, 0.85, 0.01, 0.1, 
#              0.01, 0.01, 0.94, 0.04, 0.005, 0.005, 0.05, 0.94),  
#            nrow = 4, byrow = TRUE)
#xi
mu = c(1, 15, 80, 150)
sigmasq = c(0.5, 2, 2, 1.5)
#sim = simulate_normal_hmm(xi, mu, sigma, 5, c(0.8, 0.05, 0.07, 0.08))
#(sim2 = simulate_normal_hmm(xi, mu, sigma, 20, c(0.8, 0.05, 0.07, 0.08)))
#sim2
#(sim3 = simulate_normal_hmm(xi, mu, sigma, 20, c(0.8, 0.05, 0.07, 0.08)))

xi2 = matrix(c(50, 1, 1, 1, 1, 50, 1, 1, 1, 1, 50, 1, 1, 1, 1, 50), nrow = 4, ncol = 4, byrow = TRUE)
xi2 = xi2 / 53
rowSums(xi2)
sim4 = simulate_normal_hmm(xi2, mu, sigmasq, 1000, c(0.6, 0.3, 0.07, 0.03))
plot(ts(sim4[[1]]))
#sim
#forward_filtering(sim, 4, c(0.8, 0.05, 0.07, 0.08), xi, mu, sigma)

#forward_filtering_LSE(sim, 4, c(0.8, 0.05, 0.07, 0.08), xi, mu, sigma)

#ff[[2]]
#ff_LSE[[2]]
#all.equal(ff[[2]],ff_LSE[[2]])
## All the filtered probabilities are equal

#backwards_smoothing(sim, 4, c(0.8, 0.05, 0.07, 0.08), xi, mu, sigma)
#backwards_sampling(sim, 4, c(0.8, 0.05, 0.07, 0.08), xi, mu, sigma)

#gibbs_test = my_gibbs(10, prior_mu = list(m = 12, v = 5, mu = c(1, 10, 50, 100), sigmasq = c(0.5, 1, 2, 3)), 
#                      prior_sigmasq = list(a = 3, b = 3, sigmasq = c(1, 3, 6, 9)), 
#                      prior_xi = prior_xi,
#                      y = sim2, K = 4, S0_distribution = c(0.5, 0.3, 0.15, 0.05))

prior_xi = matrix(c(50, 1, 1, 1, 1, 50, 1, 1, 1, 1, 50, 1, 1, 1, 1, 50), nrow = 4, ncol = 4, byrow = TRUE)
gibbs_test2 = my_gibbs(10000, prior_mu = list(m = 12, v = 10000), 
                       prior_sigmasq = list(a = 3, b = 3), 
                       prior_xi = prior_xi,
                       y = sim4[[1]], K = 4, S0_distribution = c(0.25, 0.25, 0.25, 0.25))

#colMeans(gibbs_test[[1]])
#colMeans(gibbs_test[[2]])
#colMeans(gibbs_test[[3]]) # Would need to write a function to get the mean of xi
#colMeans(gibbs_test[[4]])

#par(mfrow = c(2,2))
#hist(gibbs_test2[[1]][,1], xlab = "Value", main = "mu_1")
#hist(gibbs_test2[[1]][,2], xlab = "Value", main = "mu_2")
#hist(gibbs_test2[[1]][,3], xlab = "Value", main = "mu_3")
#hist(gibbs_test2[[1]][,4], xlab = "Value", main = "mu_4")
#par(mfrow = c(1,1))  

diagnostics(gibbs_test2[[1]][-(1:5000),])
#
diagnostics(gibbs_test2[[2]][-(1:5000),])
#
diagnostics(gibbs_test2[[3]][-(1:5000),])
# 

par(mfrow=c(1,1), ask=FALSE)
plot(1:length(sim4[[2]]), getmode(gibbs_test2[[4]])[[1]], type = "l")
lines(1:length(sim4[[2]]), sim4[[2]], col = "red")

# 
# 
# backwards_sampling(sim2, 4, c(0.5, 0.3, 0.15, 0.05), 
#                    matrix(c(0.85,0.05, 0.05, 0.05, 
#                             0.1, 0.85, 0.04, 0.01, 
#                             0.01, 0.04, 0.9, 0.05, 
#                             0.01, 0.03, 0.06, 0.9), nrow = 4, ncol = 4, byrow = TRUE),
#                    rnorm(1, c(1, 10, 50, 100), c(0.5, 1, 3, 5)),
#                    1/rgamma(1, 3, 3))
# 
# mu_FCD(0.5,0.25,sim, sigma, 4)
# mu_FCD(0.5,0.25,sim2, sigma, 4)
# mu_FCD(0.5,0.25,sim3, sigma, 4)
# 
# sigmasq_FCD(2,4,sim, mu, 4)
# sigmasq_FCD(0.5,0.25,sim2, sigma, 4)
# sigmasq_FCD(0.5,0.25,sim3, sigma, 4)
# sigmasq_FCD(a = 3, b = 3, y, mu = c(1, 10, 50, 100), 4)
# 
# xi_FCD(xi, sim)
# xi_FCD(xi, sim2)
# xi_FCD(xi, sim3)
# xi_FCD(prior_xi, sim3)
# 
# ## Testing a 3-state chain
# 
# (xi_k3 = matrix(c(0.85, 0.11, 0.04, 0.05, 0.85, 0.1, 0.01, 0.12, 
#               0.87), nrow = 3, byrow = TRUE))
# 
# mu_k3 = c(1,40,80)
# sigma_k3 = c(0.5, 2, 8)
# (sim_k3 = simulate_normal_hmm(xi_k3, mu_k3, sigma_k3, 100, c(rep(1/3, 3))))
# 
# plot(c(1:100), sim_k3[[1]], type = "n")
# lines(c(1:100), sim_k3[[1]])
# 
# forward_filtering(sim_k3, 3, c(rep(1/3,3)), xi_k3, mu_k3, sigma_k3)
# forward_filtering_LSE(sim_k3, 3, c(rep(1/3,3)), xi_k3, mu_k3, sigma_k3)
# 
# backwards_smoothing(sim_k3, 3, c(rep(1/3,3)), xi_k3, mu_k3, sigma_k3)
# backwards_sampling(sim_k3, 3, c(rep(1/3,3)), xi_k3, mu_k3, sigma_k3)
# all.equal(sim_k3[[2]], backwards_sampling(sim_k3, 3, c(rep(1/3,3)), xi_k3, mu_k3, sigma_k3))
# ## TRUE - so the backwards sampling is working correctly
# 
# mu_FCD(m = 20, v = 5, y = sim_k3, sigmasq = sigma_k3, K = 3)
# sigmasq_FCD(a = 5, b = 5, y = sim_k3, mu = mu_k3, K = 3)
# xi_FCD(xi_prior = matrix(c(60, 1, 1, 1, 60, 1, 1, 1, 60), nrow = 3), y = sim_k3)
# 
# (gibbs_test_k3 = my_gibbs(niters = 10, prior_mu = list(m = 25, v = 10, mu = c(3, 50, 100), sigmasq = c(0.5, 1, 1.5)), 
#                           prior_sigmasq = list(a = 5, b = 5, sigmasq = c(1, 2, 3)),
#                          prior_xi = matrix(c(60, 1, 1, 1, 60, 1, 1, 1, 60), nrow = 3),
#                          y = sim_k3, K = 3, S0_distribution = c(rep(1/3, 3))))
# 
# par(mfrow = c(3,1))
# hist(gibbs_test_k3[[1]][,1], xlab = "Value", main = "mu_1")
# hist(gibbs_test_k3[[1]][,2], xlab = "Value", main = "mu_2")
# hist(gibbs_test_k3[[1]][,3], xlab = "Value", main = "mu_3")
# par(mfrow = c(1,1))
# 
# getmode(gibbs_test_k3[[4]])
# diagnostics(gibbs_test_k3[[1]])
# diagnostics(gibbs_test_k3[[2]])
# diagnostics(gibbs_test_k3[[3]])
# 
# ## All works without any errors for K = 3
# 
# ## Now try a 5-state chain
# 
# xi_k5 = matrix(0.04, nrow = 5, ncol = 5)
# diag(xi_k5) = 0.84
# 
# mu_k5 = c(1, 40, 80, 150, 300)
# sigma_k5 = c(0.5, 2, 8, 5, 10)
# (sim_k5 = simulate_normal_hmm(xi_k5, mu_k5, sigma_k5, 100, c(rep(1/5, 5))))
# 
# plot(c(1:100), sim_k5[[1]], type = "n")
# lines(c(1:100), sim_k5[[1]])
# 
# forward_filtering(sim_k5, 5, c(rep(1/5,5)), xi_k5, mu_k5, sigma_k5)
# forward_filtering_LSE(sim_k5, 5, c(rep(1/5,5)), xi_k5, mu_k5, sigma_k5)
# 
# backwards_smoothing(sim_k5, 5, c(rep(1/5,5)), xi_k5, mu_k5, sigma_k5)
# backwards_sampling(sim_k5, 5, c(rep(1/5,5)), xi_k5, mu_k5, sigma_k5)
# all.equal(sim_k5[[2]], backwards_sampling(sim_k5, 5, c(rep(1/5,5)), xi_k5, mu_k5, sigma_k5))
# ## TRUE - so the backwards sampling function is working correctly
# 
# mu_FCD(m = 50, v = 10, y = sim_k5, sigmasq = sigma_k5, K = 5)
# sigmasq_FCD(a = 10, b = 7, y = sim_k5, mu = mu_k5, K = 5)
# prior_xi_k5 = matrix(1, nrow = 5, ncol = 5)
# diag(prior_xi_k5) = 60
# xi_FCD(xi_prior = prior_xi_k5, y = sim_k5)
# 
# (gibbs_test_k5 = my_gibbs(niters = 10, prior_mu = list(m = 60, v = 15, mu = c(3, 50, 100, 125, 250, 350), 
#                                                        sigmasq = c(0.5, 1, 1.5, 5, 15)), 
#                           prior_sigmasq = list(a = 15, b = 7, sigmasq = c(1, 2, 3, 4, 5)),
#                           prior_xi = prior_xi_k5, y = sim_k5, K = 5, 
#                           S0_distribution = c(rep(1/5, 5))))
# 
# par(mfrow = c(2,3))
# hist(gibbs_test_k5[[1]][,1], xlab = "Value", main = "mu_1")
# hist(gibbs_test_k5[[1]][,2], xlab = "Value", main = "mu_2")
# hist(gibbs_test_k5[[1]][,3], xlab = "Value", main = "mu_3")
# hist(gibbs_test_k5[[1]][,4], xlab = "Value", main = "mu_4")
# hist(gibbs_test_k5[[1]][,5], xlab = "Value", main = "mu_5")
# par(mfrow = c(1,1))
# 
# getmode(gibbs_test_k5[[4]])
# diagnostics(gibbs_test_k5[[1]])
# diagnostics(gibbs_test_k5[[2]])
# diagnostics(gibbs_test_k5[[3]])

