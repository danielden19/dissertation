## Function
y[i+1]  # Markov chain
xi      # Transition matrix
S0      # Initial distribution

xi = matrix(c(0.9, 0.01, 0.01, 0.08, 0.04, 0.85, 0.01, 0.1, 
                0.01, 0.01, 0.94, 0.04, 0.005, 0.005, 0.05, 0.94),  
              nrow = 4, byrow = TRUE)

markov_chain = function(xi, means, variances, length_seq, S0_distribution){
  current_state # = what do I put here?
  y = numeric(length_seq)
  y[1] = # something for S0 distribution
  for (i in 2:length_seq) {
    current_state = sample(ncol(xi), size = 1, prob = xi[current_state,])
    theta = rnorm(n = 1, mean = means[current_state], sd = sqrt(variances[current_state]))
    y[i] = y[i-1] + theta
  }
}

simulate_normal_hmm = function(xi, means, variances, length_seq, S0_distribution) {
  r = length(means) # This is the number of states
  s = numeric(length_seq+1) # The state sequence - note that the first element is S0 not S1
  y = numeric(length_seq) # The observable sequence - note that the first element is Y1
  s[1] = sample(r, 1, prob=S0_distribution)
  if(isTRUE(all(rowSums(xi)==1)&&all(xi>=0))){  # Ensure the transition matrix is stochastic
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
  else(return("Transition matrix is not stochastic"))
}

mu = c(1,10,30,60)
sigma = c(0.5, 5, 8, 10)
sim = simulate_normal_hmm(xi, mu, sigma, 5, c(0.8, 0.05, 0.07, 0.08))
plot(sim[[1]], type = 'n')
lines(sim[[1]])
xi

## Forward filter coding - one-step ahead prediction


forward_filtering = function(y, K, S0_distribution, xi, mu, sigmasq){
  #N = nrow(y)
  N = length(y)
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
      numerator[k] = dnorm(y[t], mean = mu[k], sd = sqrt(sigmasq[k]))*one_step_ahead_forecasts[t,k]
    }
    denominator = sum(numerator)
    filtered_probs[t+1,] = numerator/denominator
  }
  return(list(one_step_ahead_forecasts, filtered_probs))
}

sim
forward_filtering(sim[[1]], 4, c(0.8, 0.05, 0.07, 0.08), xi, mu, sigma)
