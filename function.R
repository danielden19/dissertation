## Function
#y[i+1]  # Markov chain
#xi      # Transition matrix
#S0      # Initial distribution

#install.packages("drat")
#drat::addRepo("jr-packages")
#install.packages("jrRstan")
library(jrRstan)

simulate_normal_hmm = function(xi, means, variances, length_seq, S0_distribution) {
  r = length(means) # This is the number of states
  s = numeric(length_seq+1) # The state sequence - note that the first element is S0 not S1
  y = numeric(length_seq) # The observable sequence - note that the first element is Y1
  s[1] = sample(r, 1, prob=S0_distribution)
  if(isTRUE(all(rowSums(xi)==1)&&all(xi>=0))){  # Ensure the transition matrix is stochastic - need to confirm whether to keep this
    if(isTRUE(all(means == sort(means)))){  # Need to make sure means are increasing
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
  N = length(y) # Find length of the series
  samples = y
  filtered_probs = matrix(nrow = N+1, ncol = K) # Set up matrix for the filtered probabilities
  filtered_probs[1,] = S0_distribution  # Set first row of the filtered probabilities matrix as the S0 distribution
  one_step_ahead_forecasts = matrix(nrow = N, ncol = K) # Set up matrix for the one-step-ahead forecasts
  numerator = numeric(K)  # Set up an empty vector for the numerator
  for (t in 1:N) {  # N is the maximum amount of time (=T)
    # One step ahead prediction
    for (l in 1:K) {  # K is the number of states
      one_step_ahead_forecasts[t,l] = xi[1,l]*filtered_probs[t,1]
      for (k in 2:K) {  # Here using k to iterate through the rows of xi
        one_step_ahead_forecasts[t,l] = one_step_ahead_forecasts[t,l] + xi[k,l]*filtered_probs[t,k]
      } 
    }
    # Filtering
    for (k in 1:K) {
      # Calculate values for the numerator by sampling the normal distribution and multiplying by the one-step-ahead forecasts
      numerator[k] = dnorm(samples[t], mean = mu[k], sd = sqrt(sigmasq[k]))*one_step_ahead_forecasts[t,k]
    }
    denominator = sum(numerator)
    filtered_probs[t+1,] = numerator/denominator  # Calculate the current row of the filtered probabilities
  }
  return(list(one_step_ahead_forecasts, filtered_probs))
}


forward_filtering_LSE = function(y, K, S0_distribution, xi, mu, sigmasq){
  ## Using the log-sum-exp trick
  N = length(y) # Find length of the series
  samples = y
  filtered_probs = matrix(nrow = N+1, ncol = K) # Set up matrix for the filtered probabilities
  filtered_probs[1,] = S0_distribution  # Set first row of the filtered probabilities matrix as the S0 distribution
  one_step_ahead_forecasts = matrix(nrow = N, ncol = K) # Set up matrix for the one-step-ahead forecasts
  numerator = numeric(K)  # Set up an empty vector for the numerator
  for (t in 1:N) {  # N is the maximum amount of time (=T)
    # One step ahead prediction
    for (l in 1:K) {  # K is the number of states
      one_step_ahead_forecasts[t,l] = xi[1,l]*filtered_probs[t,1]
      for (k in 2:K) {  # Here using k to iterate through the rows of xi
        one_step_ahead_forecasts[t,l] = one_step_ahead_forecasts[t,l] + xi[k,l]*filtered_probs[t,k]
        # k is step-ahead state, l is the current state??
      } 
    }
    # Calculate Ltmax by finding the maximum value from a vector of normal samples
    Ltmax = max(dnorm(samples[t], mean = mu[1:K], sd = sqrt(sigmasq[1:K]), log = TRUE))
    # Filtering
    for (m in 1:K) {
      # Calculate values for the numerator by sampling the normal distribution and multiplying by the one-step-ahead forecasts
      numerator[m] = exp(dnorm(samples[t], mean = mu[m], sd = sqrt(sigmasq[m]), log = TRUE)-Ltmax)*one_step_ahead_forecasts[t,m]
    }
    denominator = sum(numerator)
    filtered_probs[t+1,] = numerator/denominator  # Calculate the current row of the filtered probabilities
  }
  return(list(one_step_ahead_forecasts, filtered_probs))
}


## Backwards smoothing
backwards_smoothing = function(y, K, S0_distribution, xi, mu, sigmasq){
  N = length(y)
  samples = y
  # Perform the forward filtering (with log-sum-exp trick) to give the filtered probabilities
  filtering_function = forward_filtering_LSE(y, K, S0_distribution, xi, mu, sigmasq)
  filtered_probs = filtering_function[[2]]
  smoothed_probs = matrix(nrow = N+1, ncol = K)  # Set up a matrix for the smoothed probabilities
  numerator = numeric(K)
  denominator = matrix(nrow = K, ncol = K)  # Set up an empty vector and matrix for numerator and denominator respectively
  # Set the last row of the smoothed probabilities to be the last row of the filtered probabilities
  smoothed_probs[N+1,] = filtered_probs[N+1,]
  for (t in N:1) {  # Loop from N down to 1
    for (l in 1:K) {
      for (k in 1:K){
        # Work  on the logarithmic scale for the numerator and denominator
        numerator[k] = exp(log(xi[l,k]) + log(filtered_probs[t,l]) + log(smoothed_probs[t+1,k]))
        for (j in 1:K) {
          denominator[j,k] = exp(log(xi[j,k]) + log(filtered_probs[t,j]))
        }
      }
      smoothed_probs[t, l] = sum(numerator / colSums(denominator))  # Calculate the current value of the smoothed probabilities
    }
  }
  return(smoothed_probs)
}


backwards_sampling = function(y, K, S0_distribution, xi, mu, sigmasq){
  N = length(y)
  samples = y
  # Perform the forward filtering (with log-sum-exp trick)
  filtering_function = forward_filtering_LSE(y, K, S0_distribution, xi, mu, sigmasq)
  filtered_probs = filtering_function[[2]]
  # Set up empty vectors for the smoothed states and numerator
  smoothed_states = numeric(N+1)
  numerator = numeric(K)
  # Set the last value of the smoothed states by sampling from the number of states (K), using the last row of the
  # filtered probabilities as the vector of probability weights
  smoothed_states[N+1] = sample(K, 1, filtered_probs[N+1,], replace=TRUE)
  for (t in N:1) {  # Loop back from N to 1
    for (l in 1:K) {
      numerator[l] = exp(log(xi[l,smoothed_states[t+1]]) + log(filtered_probs[t,l]))
    }
    # Take a sample from K, using the numerator/sum of numerator as the vector of probability weights
    smoothed_states[t] = sample(K, 1, numerator / sum(numerator), replace=TRUE)
  }
  return(smoothed_states)
}


#install.packages("MCMCpack")
library(MCMCpack)


my_gibbs = function(niters, prior_mu, prior_sigmasq, prior_xi, y, K, S0_distribution) {
  ## Setting up storage
  N = length(y)
  ## Set up empty matrices for mu, sigmasq and xi samples
  mu_samples = matrix(NA, niters+1, K)
  sigmasq_samples = matrix(NA, niters+1, K)
  xi_samples = matrix(NA, niters+1, K*K)
  ## Initialise from prior
  mu = rnorm(K, prior_mu$m, sqrt(prior_mu$v))
  sigmasq = 1/rgamma(K, prior_sigmasq$a, prior_sigmasq$b)
  xi = matrix(0, K, K)
  s_running_tally = matrix(0, nrow = N+1, ncol = K)
  ## Loop through K to initialise xi
  for(i in 1:K) {
    xi[i,] = rdirichlet(1, prior_xi[i,])[1,]
  }
  # Set the first rows of the samples as the initial values for mu, sigmasq and xi (respectively)
  mu_samples[1,] = mu
  sigmasq_samples[1,] = sigmasq
  xi_samples[1,] = as.numeric(t(xi))
  # Loop through the number of iterations
  for(iter in 1:niters) {
    # Sample from FCD of states given parameters
    s = backwards_sampling(y, K, S0_distribution, xi, mu, sigmasq)
    # Sample from FCDs of parameters given states
    mu = mu_FCD(prior_mu$m, prior_mu$v, y, s, sigmasq, K)
    sigmasq = sigmasq_FCD(prior_sigmasq$a, prior_sigmasq$b, y, s, mu, K)
    xi = xi_FCD(prior_xi, s)
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
  # Set the column names of the samples to allow for diagnostics
  colnames(mu_samples) = paste("mu[",1:K,"]", sep = "")
  colnames(sigmasq_samples) = paste("sigmasq[",1:K,"]", sep = "")
  colnames(xi_samples) = paste("xi[", rep(1:K, each = K),",", rep(1:K, times = K),"]", sep = "")
  # Calculate s_running_tally as a proportion rather than tally
  s_running_tally = s_running_tally / niters
  return(list(mu_samples, sigmasq_samples, xi_samples, s_running_tally))
}


getmode = function(s_running_tally){
  # Function to find the mode of each iteration of s_running_tally
  mode_vector = numeric(nrow(s_running_tally))
  for (i in 1:nrow(s_running_tally)) {
    # Find which value of K corresponds to the maximum value of each row
    mode_vector[i] = which.max(s_running_tally[i,])
  }
  # Find which values are included in the mode vector
  unique_values = unique(mode_vector)
  # Calculate the overall mode of the mode vector
  mode = unique_values[which.max(tabulate(match(mode_vector, unique_values)))]
  return(list(mode_vector, mode))
}


#install.packages("rstan")
#library(rstan)
#library(jrRstan)
#rstan_options(autowrite = TRUE)
#options(mc.cores = parallel::detectCores())


# write functions:
# mu_FCD function
mu_FCD = function(m, v, y, states, sigmasq, K){
  # Determine which states have been sampled
  values = sort(unique(states[-1]))
  # Set up vector of NA's for mu
  mu = rep(NA, K)
  # Loop through the vector of values
  for (i in values) {
    # Nk is the number of occurrences of the current state
    Nk = length(which(states[-1]==i))
    # Calculate ybar (i.e., the mean of y for the current state)
    ybar = 1/Nk*sum(y[which(states[-1]==i)], na.rm = TRUE)
    # Calculate the value of mu for the current state using the posterior from the FCDs
    mu[i] = rnorm(1, mean = (v*Nk*ybar+sigmasq[i]*m)/(v*Nk+sigmasq[i]), 
                  sd = sqrt((sigmasq[i]*v)/(v*Nk+sigmasq[i])))
  }
  # In case a state has not been sampled, we will set the corresponding mu values to be based of the prior
  not_values = setdiff(1:K, values)
  if(length(not_values)>0){
    for (i in not_values) {
      mu[i] = rnorm(1, mean = m, sd = sqrt(v))
    }
  }
  # Return the mu's sorted in ascending order
  return(sort(mu))
}


# sigmasq_FCD function
sigmasq_FCD = function(a, b, y, states, mu, K){
  values = sort(unique(states[-1]))
  samples = y
  # Set up necessary vectors
  sigmasq = rep(NA, K)
  yt_minus_muk_sq = numeric(length(y))
  # Loop through vector of values
  for (i in values) {
    # Find the vector locations of each state
    times = which(states[-1] == i)
    for (j in times) {
      yt_minus_muk_sq[j] = (samples[j] - mu[i])^2
    }
    Nk = length(which(states[-1] == i))
    # Calculate values for sigmasq using the posterior from the FCD's
    sigmasq[i] = 1/rgamma(1, shape = a + Nk/2, 
                          rate = b + sum(yt_minus_muk_sq[times]/2)) 
  }
  # Again, in case a state has not been sampled, we will set the corresponding sigmasq values to be based of the prior
  not_values = setdiff(1:K, values)
  if(length(not_values)>0){
    for (i in not_values) {
      sigmasq[i] = 1/rgamma(1, shape = a, rate = b)
    }
  }
  return(sigmasq)
}


# xi_FCD function
xi_FCD = function(xi_prior, states){
  state_transitions = matrix(0, nrow = nrow(xi_prior), ncol = ncol(xi_prior))
  updated_xi = matrix(0, nrow = nrow(xi_prior), ncol = ncol(xi_prior))
  for (i in 1:(length(states)-1)) {
    # Count up the number of transitions from state to state in a matrix
    state_transitions[states[i], states[i+1]] = state_transitions[states[i], states[i+1]] + 1
  }
  for (j in 1:nrow(xi_prior)) {
    # Update xi using the prior plus the state transitions
    updated_xi[j,] = rdirichlet(1, xi_prior[j,]+state_transitions[j,])
  }
  return(updated_xi)
}

beta_tilde_vector = function(){}






Y_FCD = function(x, beta, sigmasq, states, K){
  values = sort(unique(states[-1]))
  # Does S need to be found via backwards sampling?
  # Should x be found via the x_t_vector function instead?
  N = length(x)
  Y = matrix(0, nrow = N, ncol = K)
  x_tilde = x[, -1:-K]
  beta_tilde = beta[, -1:-K]
  Y[1,] = mvrnorm(n = 1, mu = x[1]%*%beta, Sigma = sqrt(sigmasq[states[states[1]]]))
  for (t in 2:N) {
    Y[t,] = mvrnorm(n = 1, mu = beta[states[t]] + x_tilde[t,]%*%beta_tilde, 
                    sigma = sqrt(sigmasq[states[states[t]]]))
  }
}

x_t_vector = function(S, length, w, b, K, data){
  w = data$Weekend
  b = data$Bank.Holiday.2
  cwv = data$CWV
  x_t = numeric(K+5)
  for (t in 1:length) {
    x_t[S[t]] = 1
    x_t[K+1] = w[t]
    x_t[K+2] = b[t]
    x_t[K+3] = cwv[t]
    x_t[K+4] = sin(2*pi*t/365.25)
    x_t[K+5] = cos(2*pi*t/365.25) 
  }
}


