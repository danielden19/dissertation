## Function
y[i+1]  # Markov chain
xi      # Transition matrix
S0      # Initial distribution

install.packages("drat")
drat::addRepo("jr-packages")
install.packages("jrRstan")
library(jrRstan)

simulate_normal_hmm = function(xi, means, variances, length_seq, S0_distribution) {
  r = length(means) # This is the number of states
  s = numeric(length_seq+1) # The state sequence - note that the first element is S0 not S1
  y = numeric(length_seq) # The observable sequence - note that the first element is Y1
  s[1] = sample(r, 1, prob=S0_distribution)
  if(isTRUE(all(rowSums(xi)==1)&&all(xi>=0))){  # Ensure the transition matrix is stochastic - need to confirm whether to keep this
    if(isTRUE(all(means == sort(means)))){
      for(i in 1:length_seq) {
        # A) Sample s[i+1] - just like s[1] above except the prob argument in the sample function will
        # be the s[i]-th row of xi
        s[i+1] = sample(r, 1, prob = xi[s[i],])
        # B) Sample y[i] - from a normal distribution where the mean and variance will be 
        # the s[i+1]-th element of means and variances
        y[i] = rnorm(n = 1, mean = means[s[i+1]], sd = sqrt(variances[s[i+1]]))
      }
      # Return a list containing both y and s
      return(list(y,s))
    }
    else(return("Values of mu must be increasing from mu_1 up to mu_K"))
  }
  else(return("Transition matrix is not stochastic"))
}


## Forward filter coding - one-step ahead prediction

forward_filtering = function(y, K, S0_distribution, xi, mu, sigmasq){
  N = length(y[[1]])
  samples = y[[1]]
  filtered_probs = matrix(nrow = N+1, ncol = K)
  filtered_probs[1,] = S0_distribution
  one_step_ahead_forecasts = matrix(nrow = N, ncol = K)
  numerator = numeric(K)
  for (t in 1:N) {  # N is the maximum amount of time (=T)
    # One step ahead prediction
    for (l in 1:K) {  # K is the number of states
      one_step_ahead_forecasts[t,l] = xi[1,l]*filtered_probs[t,1]
      for (k in 2:K) {  # Here using k to iterate through the rows of xi
        one_step_ahead_forecasts[t,l] = one_step_ahead_forecasts[t,l] + xi[k,l]*filtered_probs[t,k]
        # k is step-ahead state, l is the current state??
      } 
    }
    # Filtering
    for (k in 1:K) {
      numerator[k] = dnorm(samples[t], mean = mu[k], sd = sqrt(sigmasq[k]))*one_step_ahead_forecasts[t,k]
    }
    denominator = sum(numerator)
    filtered_probs[t+1,] = numerator/denominator
  }
  return(list(one_step_ahead_forecasts, filtered_probs))
}


forward_filtering_LSE = function(y, K, S0_distribution, xi, mu, sigmasq){
  N = length(y[[1]])
  samples = y[[1]]
  filtered_probs = matrix(nrow = N+1, ncol = K)
  filtered_probs[1,] = S0_distribution
  one_step_ahead_forecasts = matrix(nrow = N, ncol = K)
  numerator = numeric(K)
  for (t in 1:N) {  # N is the maximum amount of time (=T)
    # One step ahead prediction
    for (l in 1:K) {  # K is the number of states
      one_step_ahead_forecasts[t,l] = xi[1,l]*filtered_probs[t,1]
      for (k in 2:K) {  # Here using k to iterate through the rows of xi
        one_step_ahead_forecasts[t,l] = one_step_ahead_forecasts[t,l] + xi[k,l]*filtered_probs[t,k]
        # k is step-ahead state, l is the current state??
      } 
    }
    Ltmax = max(dnorm(samples[t], mean = mu[1:K], sd = sqrt(sigmasq[1:K]), log = TRUE))
    # Filtering
    for (k in 1:K) {
      numerator[k] = exp(dnorm(samples[t], mean = mu[k], sd = sqrt(sigmasq[k]), log = TRUE)-Ltmax)*one_step_ahead_forecasts[t,k]
    }
    denominator = sum(numerator)
    filtered_probs[t+1,] = numerator/denominator
  }
  return(list(one_step_ahead_forecasts, filtered_probs))
}


## Backwards smoothing
backwards_smoothing = function(y, K, S0_distribution, xi, mu, sigmasq){
  N = length(y[[1]])
  samples = y[[1]]
  filtering_function = forward_filtering_LSE(y, K, S0_distribution, xi, mu, sigmasq)
  filtered_probs = filtering_function[[2]]
  #smoothed_probs = matrix(nrow = N, ncol = K)
  smoothed_probs = matrix(nrow = N+1, ncol = K)
  numerator = numeric(K)
  denominator = matrix(nrow = K, ncol = K)
  # Line below added
  smoothed_probs[N+1,] = filtered_probs[N+1,]
  for (t in N:1) {
    for (l in 1:K) {
      for (k in 1:K){
        #numerator[k] = xi[l,k]*filtered_probs[t,l]*filtered_probs[t+1,k]
        numerator[k] = exp(log(xi[l,k]) + log(filtered_probs[t,l]) + log(smoothed_probs[t+1,k]))
        for (j in 1:K) {
          #denominator[j,k] = xi[j,k]*filtered_probs[t,j]
          denominator[j,k] = exp(log(xi[j,k]) + log(filtered_probs[t,j]))
        }
      }
      #smoothed_probs[t,] = sum(numerator)/rowSums(denominator)
      smoothed_probs[t, l] = sum(numerator / colSums(denominator))
    }
  }
  return(smoothed_probs)
}


backwards_sampling = function(y, K, S0_distribution, xi, mu, sigmasq){
  N = length(y[[1]])
  samples = y[[1]]
  filtering_function = forward_filtering_LSE(y, K, S0_distribution, xi, mu, sigmasq)
  filtered_probs = filtering_function[[2]]
  ##smoothed_probs = matrix(nrow = N, ncol = K)
  #smoothed_probs = matrix(nrow = N+1, ncol = K)
  smoothed_states = numeric(N+1)
  numerator = numeric(K)
  #denominator = matrix(nrow = K, ncol = K)
  ## Line below added
  #smoothed_probs[N+1,] = filtered_probs[N+1,]
  smoothed_states[N+1] = sample(K, 1, filtered_probs[N+1,], replace=TRUE)
  for (t in N:1) {
    for (l in 1:K) {
      #for (k in 1:K){
      ##numerator[k] = xi[l,k]*filtered_probs[t,l]*filtered_probs[t+1,k]
      #numerator[k] = exp(log(xi[l,k]) + log(filtered_probs[t,l]) + log(smoothed_probs[t+1,k]))
      numerator[l] = exp(log(xi[l,smoothed_states[t+1]]) + log(filtered_probs[t,l]))
      #for (j in 1:K) {
      ##denominator[j,k] = xi[j,k]*filtered_probs[t,j]
      #denominator[j,k] = exp(log(xi[j,k]) + log(filtered_probs[t,j]))
      #}
      #}
      ##smoothed_probs[t,] = sum(numerator)/rowSums(denominator)
      #smoothed_probs[t, l] = sum(numerator / colSums(denominator))
    }
    smoothed_states[t] = sample(K, 1, numerator / sum(numerator), replace=TRUE)
  }
  return(smoothed_states)
}


install.packages("MCMCpack")
library(MCMCpack)
rdirichlet()

install.packages("invgamma")
library(invgamma)

my_gibbs = function(niters, prior_mu, prior_sigmasq, prior_xi, y, K, S0_distribution) {
  ## Setting up storage
  N = length(y[[1]])
  mu_samples = matrix(NA, niters+1, K)
  sigmasq_samples = matrix(NA, niters+1, K)
  xi_samples = matrix(NA, niters+1, K*K)
  ## Initialise from prior
  #mu = rnorm(1, prior_mu$mean, prior_mu$sd)
  #sigmasq = 1 / rgamma(1, prior_sigmasq$a, prior_sigmasq$b)
  # Should this be different? Changed to below
  mu = rnorm(K, prior_mu$mu, sqrt(prior_mu$sigmasq))
  sigmasq = 1/rgamma(K, prior_sigmasq$a, prior_sigmasq$b)
  xi = matrix(0, K, K)
  s_running_tally = matrix(0, nrow = N+1, ncol = K)
  for(i in 1:K) {
    xi[i,] = rdirichlet(1, prior_xi[i,])[1,]
  }
  mu_samples[1,] = mu
  sigmasq_samples[1,] = sigmasq
  xi_samples[1,] = as.numeric(t(xi))
  # Big loop
  for(iter in 1:niters) {
    # Sample from FCD of states given parameters
    s = backwards_sampling(y, K, S0_distribution, xi, mu, sigmasq)
    # Sample from FCDs of parameters given states
    mu = mu_FCD(prior_mu$m, prior_mu$v, y, prior_sigmasq$sigmasq, K)
    sigmasq = sigmasq_FCD(prior_sigmasq$a, prior_sigmasq$b, y, prior_mu$mu, K)
    xi = xi_FCD(prior_xi, y)
    # Store
    mu_samples[1+iter,] = mu
    sigmasq_samples[1+iter,] = sigmasq
    xi_samples[1+iter,] = as.numeric(t(xi))
    # Tally the states, ie keep running total of number of times
    # time t is 1, 2, ..., K for each t
    for (t in 1:(N+1)) {
      s_running_tally[t,s[t]] = s_running_tally[t,s[t]] + 1
    }
  }
  colnames(mu_samples) = paste("mu[",1:K,"]", sep = "")
  colnames(sigmasq_samples) = paste("sigmasq[",1:K,"]", sep = "")
  colnames(xi_samples) = paste("xi[", rep(1:K, each = K),",", rep(1:K, times = K),"]", sep = "")
  s_running_tally = s_running_tally / niters
  return(list(mu_samples, sigmasq_samples, xi_samples, s_running_tally))
}


getmode = function(s_running_tally){
  mode_vector = numeric(nrow(s_running_tally))
  for (i in 1:nrow(s_running_tally)) {
    mode_vector[i] = which.max(s_running_tally[i,])
  }
  unique_values = unique(mode_vector)
  mode = unique_values[which.max(tabulate(match(mode_vector, unique_values)))]
  return(list(mode_vector, mode))
}


install.packages("rstan")
library(rstan)
library(jrRstan)
rstan_options(autowrite = TRUE)
options(mc.cores = parallel::detectCores())


# write functions:
# mu_FCD function
mu_FCD = function(m, v, y, sigmasq, K){
  states = y[[2]]
  values = sort(unique(states[-1]))
  mu = rep(NA, K)
  for (i in values) {
    Nk = length(which(states[-1]==i))
    ybar = 1/Nk*sum(y[[1]][which(states[-1]==i)], na.rm = TRUE)
    mu[i] = rnorm(1, mean = (v*Nk*ybar+sigmasq[i]*m)/(v*Nk+sigmasq[i]), 
                  sd = sqrt((sigmasq[i]*v)/(v*Nk+sigmasq[i])))
  }
  not_values = setdiff(1:K, values)
  if(length(not_values)>0){
    for (i in not_values) {
      mu[i] = rnorm(1, mean = m, sd = sqrt(v))
    }
  }
  return(sort(mu))
}


# sigmasq_FCD function
sigmasq_FCD = function(a, b, y, mu, K){
  states = y[[2]]
  values = sort(unique(states[-1]))
  samples = y[[1]]
  sigmasq = rep(NA, K)
  yt_minus_muk_sq = numeric(length(y[[1]]))
  for (i in values) {
    times = which(states[-1] == i)
    for (j in times) {
      yt_minus_muk_sq[j] = (samples[j] - mu[i])^2
    }
    Nk = length(which(states[-1] == i))
    sigmasq[i] = 1/rgamma(1, shape = a + Nk/2, 
                          scale = b + sum(yt_minus_muk_sq[times]/2)) 
  }
  not_values = setdiff(1:K, values)
  if(length(not_values)>0){
    for (i in not_values) {
      sigmasq[i] = 1/rgamma(1, shape = a, scale = b)
    }
  }
  return(sigmasq)
}


# xi_FCD function
xi_FCD = function(xi_prior, y){
  states = y[[2]]
  state_transitions = matrix(0, nrow = nrow(xi_prior), ncol = ncol(xi_prior))
  updated_xi = matrix(0, nrow = nrow(xi_prior), ncol = ncol(xi_prior))
  for (i in 1:(length(states)-1)) {
    state_transitions[states[i], states[i+1]] = state_transitions[states[i], states[i+1]] + 1
  }
  for (j in 1:nrow(xi_prior)) {
    updated_xi[j,] = rdirichlet(1, xi_prior[j,]+state_transitions[j,])
  }
  return(updated_xi)
}

