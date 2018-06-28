# We offer a framework that can estimate statespace models by Bayesian statistics. 
# The specific case is the Renshaw-Haberman model
# Input: ....
# Output: Estimated parameters of the Renshaw Haberman model




# Clean enviroment and load packages ----
rm(list=ls())
library(dlm)
library(dplyr)
library(plyr)
library(reshape)
library(mvtnorm) # multivariate normal distributions
library(MCMCpack)
library(reshape2)
library(matlib)




# ---- LOAD UK DATA ----




# ---- SPECIFY ITERATIONS AND SET INITIAL VALUES ----

# ITERATIONS
iterations <- 10
burn_in_period <- 15000

# TIME AND AGE DIMENSION
p <- as.numeric(length(unique(uk_data$Age))) # considered ages, here from 65-95 = 31 for UK data
big_t <- as.numeric(length(unique(uk_data$Year))) # considered time periods/years, here 1970-2010 = 41 for UK data

# PSI VECTOR:
# Set up initial values
beta_x_0 <- matrix(1/p, nrow = p, ncol = 1) # skal nok være 31 x 1
beta_x_gamma_0 <- matrix(1/p, nrow = p, ncol = 1) # skal nok være 31 x 1
alpha_x_0 <- uk_data %>% group_by(Age) %>% summarise_at(vars(Male), funs(mean(., na.rm=TRUE))) # bemærk, at der her skal være en værdi for hver aldersgruppe => 31-dim vector. Test: test <- mean(uk_data[which(uk_data$Age == 65) , 3], na.rm = T)
alpha_x_0 <- matrix(alpha_x_0$Male, nrow = p, ncol = 1)
theta_0 <- -0.1
eta_0 <- -0.1
lambda_0 <- 0.5
sigma_sq_epsilon_0 <- 0.01
sigma_sq_kappa_0 <- 0.01
sigma_sq_gamma_0 <- 0.01
# Put the initial values into the "correct elements" (i.e correct names) that are to be chnged throughout the iterations
beta_x_vector <- beta_x_0
beta_x_gamma_vector <- beta_x_gamma_0
alpha_x_vector <- alpha_x_0
theta <- theta_0
eta <- eta_0
lambda <- lambda_0
sigma_sq_epsilon <- sigma_sq_epsilon_0
sigma_sq_kappa <- sigma_sq_kappa_0
sigma_sq_gamma <- sigma_sq_gamma_0

# RESHAPE THE Y-data (LHS, the og mortality rates)
y <- acast(uk_data, Age ~ Year, value.var = 'Male') # One col for each period in time
y_data <- array(data.matrix(y), dim=c(p,1,big_t)) # 3rd dim = time

# INITAILISE (EMPTY) GAMMA AND KAPPA MATRIX
gamma_matrix <- array(NA, dim=c(p, 1, big_t)) # DISSE VÆRDIER KOMMER FØRST FRA FFBS, they are just initalised here
kappa_matrix <- array(NA, dim=c(1, 1, big_t)) # DISSE VÆRDIER KOMMER FØRST FRA FFBS, they are just initialised here

# SET UP THE PARAMETERS IN THE STATE SPACE MODELS. SEE fUNG P. 13
# (to be used in the FFBS: Kalman filter and sampling part)
# The time dimension is ignored for now
alpha <- alpha_x_vector
B <- cbind(beta_x_vector, diag(beta_x_gamma_vector[1], nrow = p))
epsilon_cov <- diag(sigma_sq_epsilon, nrow = p)
top_2_rows <- matrix(c(1, rep(0, p+1-1), 0, lambda, rep(0, p+1-2)), ncol = p+1, nrow = 2, byrow = TRUE)
zero_vec <- matrix(rep(0, p+1-2), nrow = p+1-2, ncol = 1)
last_rows <- matrix(c(zero_vec, diag(1, nrow = p+1-2), zero_vec), nrow = p+1-2, ncol = p+1)
Gamma <- as.matrix(ldply(list(top_2_rows, last_rows)))
Theta <- matrix(c(theta, eta, rep(0, p+1-2)), nrow = p+1, ncol = 1)
Psi_cov <- matrix(diag(c(sigma_sq_kappa, sigma_sq_gamma, rep(0, p+1-2))), ncol = p+1, nrow = p+1)

# Initialize vector to store the 6 parameters in (theta, eta, lambda, 3*sigma)
results_vec <- matrix(NA, ncol = 6, nrow = iterations)

# INITLIAISE THINGS FOR THE KALMAN FILTER
# Starttig values for the distribution
mean_0 <- matrix(0, nrow = 32, ncol = 1)
sigma_0 <- diag(10, ncol = (p+1), nrow = (p+1))
# Initialise matrices to store values in
phis <- matrix(NA, nrow = p+1, ncol = big_t)
sigma_updates <- array(NA, dim=c((p+1),(p+1),big_t))
sigma_predictions <- array(NA, dim=c((p+1),(p+1),big_t))
phi_updates <- array(NA, dim=c((p+1),1,big_t))
phi_predictions <- array(NA, dim=c((p+1),1,big_t))
phi_array <- array(NA, dim=c((p+1),1,big_t))

# Remove unnecessary data
rm(list=setdiff(ls(), c("beta_x_vector", "beta_x_gamma_vector", "alpha_x_vector", 
                        "theta", "eta", "lambda", "sigma_sq_epsilon", "sigma_sq_kappa", "sigma_sq_gamma",
                        "y_data", "gamma_matrix", "kappa_matrix", "y", "y_data", 
                        "p", "big_t",
                        "alpha", "B", "epsilon_cov", "Gamma", "Theta", "Psi_cov", "iterations", "uk_data", 
                        "mean_0", "sigma_0", 
                        "phis", "sigma_updates", "sigma_predictions", "phi_updates", "phi_predictions", "phi_array"
                        )))




# ----- !! MC ITERATIONS START HERE !! ----

for(ite in 1:iterations){

# ---- KALMAN / FORWARD FILTERING ----

# Kalman filter iterations

# Kalman loop starts
for(i in 1:big_t){ # Loop through each time point
  
  # In the first iteration the inittial value is set
  thisIsTheFirstValue <- (i == 1)
  if(thisIsTheFirstValue){
    phi_upd <- mean_0
    sigma_upd <- sigma_0
  }

  # Prediction step
  phi_pre <- (Gamma %*% phi_upd) + Theta
  sigma_pre <- (Gamma %*% sigma_upd %*% t(Gamma)) + Psi_cov
  # if(!isSymmetric(sigma_pre)) {print(paste("problem in pred in ", i))}

  # Update step
  # Helper functions # VED JEG IKKE HVOR KOMMER FRA
  v <- y[, i] - (alpha + B %*% phi_pre) # y_t - f_t
  big_F <- (B %*% sigma_pre %*% t(B)) + epsilon_cov # Q_t in Fung, epsilon_cov = sigma * Idnetity matrix
  inv_F <- solve(big_F, tol = 1e-40)
  # Calculate updates
  phi_upd <- phi_pre + (sigma_pre %*% t(B) %*% inv_F %*% v)
  sigma_upd <- sigma_pre - (sigma_pre %*% t(B) %*% inv_F %*%  B %*% sigma_pre)
  # if(!isSymmetric(sigma_upd)) {print(paste("problem in upd in ", i))}

  # save values in the arrays (used later in backward sampling step)
  phis[, i] <- phi_upd
  phi_predictions[ , , i] <- phi_pre
  sigma_predictions[ , , i] <- sigma_pre
  phi_updates[ , , i] <- phi_upd
  sigma_updates[ , , i] <- sigma_upd

} # end for loop
# NOTE IT IS phi_updates and sigma_updates we are interested in: (Denoted C and M in Fungs paper)
# phi_updates
# sigma_updates

# # Nu har vi altså den underliggende psi-kæde, vi kan vel egentlig godt indsætte den i vores measurement equation og se, om y-værdierne nogenlunde kommer til at ligne de rigtige?
# # Calculate the "Kalman obtained y - values"
# kalman_y <- matrix(NA, nrow = p, ncol = big_t)
# for(i in 1:big_t){
#   kalman_y[, i] <- alpha + (B %*% phis[, i])
# }
# for(i in 1:ncol(y)){
#   no <- i
#   plot(y[, no])
#   lines(kalman_y[, no])
# }
# plot(y[,1])
# lines(kalman_y[, ncol(y)])
# # UMIDDELBART KOMMER LINJEN MÅSKE IKKE TIL AT SE PÆNERE UD OVER TID?

# alpha <- alpha_x_vector


# Remove unnecessary variables
rm(thisIsTheFirstValue, sigma_pre, sigma_upd, phi_upd, phi_pre)




# ---- BACKWARDS SAMPLING ----
# p 15 in FUng

# Find h_t and H_t by backwards sampling
loop_seq <- seq(from = (big_t - 1), to = 1, by = -1)
for(t in loop_seq){
  t <- (big_t - 1)

  # In the first iteration the inittial value is set
  thisIsTheSecondLastTimePoint <- (t == (big_t-1))
  if(thisIsTheSecondLastTimePoint){
    phi_next <- matrix(rmvnorm(1, mean = phi_updates[ , , big_t], sigma = (sigma_updates[ , ,  big_t])), nrow = (p+1), ncol = 1)
    # phi_next - phi_updates[ , , big_t] # ser ud til at lægge nogenlunde, hvor den skal
  # Save phi drawing
  phi_array[ , , big_t] <- phi_next
  }

  # Calculate mean value dep. on this value
  inv_sig_pred <- solve(sigma_predictions[ , , t+1]) #matlib::inv(sigma_predictions[ , , t+1]) #
  isSymmetric(inv_sig_pred)
  mean_t <- phi_updates[, , t] + (sigma_updates[, , t] %*% t(Gamma) %*% inv_sig_pred %*% (phi_array[ , , (t+1)] - phi_predictions[ , , t+1]))
  # Calculate covariance dep. on this value
  sigma_t <- sigma_updates[ , , t] - (sigma_updates[, , t] %*% t(Gamma) %*% inv_sig_pred %*% Gamma %*% sigma_updates[, , t])
  isSymmetric(sigma_updates[, , t])
  isSymmetric(sigma_t)
  test <- diag(sigma_t) # Burde være positive alle sammen
test <- sigma_updates[, , t]
test1
  # Make the drawing
  phi_next <- t(rmvnorm(1, mean = mean_t, sigma = sigma_t))

  # Save phi drawings
  phi_array[, , t] <- phi_next
  y
}
# https://github.com/cran/spate/blob/master/R/spateFcts.R
# Remove unnecessary variables
rm(phi_next, loop_seq, thisIsTheSecondLastTimePoint, inv_sig_pred, mean_t, sigma_t)

# The phi_array is the one we are interested in (the result from the FFBS)
# phi_array

# sigma_predictions[ , , 27] - this causes problems if the wrong initial are set for the Kalman filter (l. 87)

# Calcualte kappa_0 as this is to be used in som eof the posteriors
R1_inv <- solve(sigma_predictions[,,1])
h_0 <- mean_0 + (sigma_0 %*% t(Gamma) %*% R1_inv %*% (phi_array[,,1] - phi_predictions[,,1]) ) 
H_0 <- sigma_0 - (sigma_0 %*% t(Gamma) %*% R1_inv %*% Gamma %*% sigma_0)
# Draw kappa_0 and kappa_0
phi_0 <- t(rmvnorm(1, mean = h_0, sigma = H_0))
kappa_0 <- phi_0[1, ]
gamma_0 <- phi_0[(-1),]


# UPDATE KAPPA MATRIX # KAPPA is only the first element in the phi vector (p. 11 button i Fung Cohort)
kappa_matrix <- array(phi_array[1 , , ], dim=c(1,1,big_t))
# UPDATE GAMMA MATRIX # all elements in the phi vector except the first row
gamma_matrix <- array(phi_array[-1 , , ], dim=c(p,1,big_t))




# ---- IMPOSE KAPPA CONSTRAINT ---- 

# Beskrevet på side 13-14 i Cohort
# bemærk her, at phi'erne opfører sig som kappa'erne ud fra den "opr. cohort models ligning", p. 9

#  Change name
kappa_hat <- kappa_matrix
# mean across time
kappa_mean <- (1 / big_t) * rowSums(kappa_hat, dims = 2) # ER IKKE LIGE HELT OVERBEVIST OM, DET PASSER!
kappa_mean_array <- array(kappa_mean, dim=c(1,1,big_t))


# UPDATE KAPPA MATRIX
kappa_matrix <- kappa_hat - kappa_mean_array
rm(kappa_hat, kappa_mean_array, kappa_mean)




# ---- IMPOSE GAMMA CONSTRAINT ---- 

# # Change name
gamma_hat <- gamma_matrix
# Change the format of the gamma matrix: from (p x 1 x big_t) to (p x big_t)
gamma_big_matrix_hat <- matrix(gamma_hat, nrow = p, ncol = big_t)

# 
# # check same elements across time
# c(gamma_hat[1,1,1], gamma_hat[2,1,1]) # gamma_0 # PROBLEMATISK MED DE FØRSTE VÆRDIER
# c(gamma_hat[1,1,2], gamma_hat[2,1,3]) # HvORFOR ER DE IKKE PRÆCIS DE SAMME
# c(gamma_hat[1,1,40], gamma_hat[2,1,41]) # HvORFOR ER DE IKKE PRÆCIS DE SAMME

# I decide to tak the mean of two values since I do not know which to use (maybe it should be the last instead)

# Functions that returns a matrix of where the gamma values are placed in the gamma_big_matrix
makesGammaIndexMatrix <- function(big_t, p){
  
  gamma_seq <- c()
  
  startSeq <- seq(from = 0, to = -p+1, by = -1)
  for(startVal in startSeq){
    # startVal <- 0
    endVal <- startVal + big_t - 1
    row_seq <- paste("g_", seq(from = startVal, to = endVal), sep = "")
    
    # Add to the total matrix
    gamma_seq <- c(gamma_seq, row_seq)
  }
  gamma_index_matrix <- matrix(gamma_seq, nrow = p, ncol = big_t, byrow = TRUE)
  
  return(gamma_index_matrix)
}
# Function that loops through all the considered gamma values to get the averages
getGammaAvgVales <- function(index_names, eval_indexes, eval_matrix){
  # index_names <- gamma_values_vector
  # eval_indexes <- gammaIndexes
  # eval_matrix <- gamma_big_matrix
  
  for(i in 1:nrow(index_names)){
    # i <- 2
    name <- index_names[i, 1]
    # Values to this name
    val_vector <- eval_matrix[which(eval_indexes == name)]
    # Calculate mean
    meanVal <- mean(val_vector)
    # Replace value in vector
    index_names[i, 1] <- meanVal
  }
  
  # Turn into numeric values
  avg_vec <- apply(index_names, 2, as.numeric)
  
  return(avg_vec)
}

# Make a matrix with all gamma values (p x big_t)
gammaIndexes <- makesGammaIndexMatrix(big_t = big_t, p = p)

# Vector of all considered gamma values
gamma_values_vector <- matrix(paste("g_", seq(from = (big_t - 1), to = (-p+1)), sep = ""), nrow = (p + big_t - 1), ncol = 1)
# Calculate the mean values for each of the considered gamma names
gamma_avg_vector <- getGammaAvgVales(index_names = gamma_values_vector, 
                                     eval_indexes = gammaIndexes, eval_matrix = gamma_big_matrix_hat)
# Calculate the total mean (across all the gamma names)
gamma_mean <- (1/(p + big_t - 1)) * sum(gamma_avg_vector) # ca. -5 - men nu får jeg den til ca- - 0.04 eller 0.11

# Subtract the mean value: note the same for all entries
gamma_big_matrix_hat <- gamma_big_matrix_hat - gamma_mean


# UPDATE GAMMA MATRIX
gamma_big_matrix <- gamma_big_matrix_hat

# Hvis man har lyst kan man skifte tilbage til den oprindelige form for gamma matricen nu




# ---- INITIALISE VALUES THAT ARE TO BE USED IN THE POSTEIORS ----

# Parameters that are never changed
# Ns
mu_tilde <- 0 # p. 17
mu_alpha_tilde <- mu_beta_tilde <- mu_beta_gamma_tilde <- mu_theta_tilde <- 
  mu_eta_tilde <- mu_lambda_tilde <- mu_tilde # never changed
sigma_sq_tilde <- 10 # p. 17
sigma_sq_alpha_tilde <- sigma_sq_beta_tilde <- sigma_sq_gamma_tilde <- sigma_sq_theta_tilde <- 
  sigma_sq_eta_tilde <- sigma_sq_lambda_tilde <- sigma_sq_tilde # never changed
a <- 2.01 # p. 17
b <- 0.01 # p. 17
# IGs
a_epsilon_tilde <-a_kappa_tilde <- a_gamma_tilde <- a
b_epsilon_tilde <- b_kappa_tilde <- b_gamma_tilde <- b
n_epsilon <- big_t * p
n_kappa <- big_t
n_gamma <- big_t

# Function that makes a drawing from the normal distribution given input parameters (posteriors)
drawsFromN <- function(sigma_1, sum_term, mu, sigma_2, n_term, truncated){
  
  # sigma_1 <- sigma_sq_alpha_tilde
  # sum_term <- beta_x_sum_term
  # mu <- mu_alpha_tilde
  # sigma_2 <- sigma_sq_epsilon
  # n_term <- big_t
  # truncated <- FALSE
  
  # Calculate mean_value
  meanVal <- (sigma_1 * sum_term + (mu * sigma_2)) / ((sigma_1 * n_term) + sigma_2)
  # Calculate variance
  varVal <- (sigma_1 * sigma_2) / ((sigma_1 * n_term) + sigma_2)
  
  # Make drawing from normal distribution
  # Truncated drawing
  drawing <- rnorm(n = 1, mean = meanVal, sd = sqrt(varVal))
  # Truncated => valid values
  if(truncated == TRUE){
    while(drawing <= -1 | drawing >= 1){
      drawing <- rnorm(1, mean = meanVal, sd = sqrt(varVal))
    }
  }
  
  return(drawing)
}

# Function that calculates the shape parameter
calculateIGShape <- function(a, n){
  
  # a <- a_epsilon_tilde
  # n <- n_epsilon
  
  # Calculate the shae parameter
  shape <- a + (n / 2)
  
  return(shape)
}




# ---- BETA FROM ITS POSTERIOR ----

# Loop through all the indexes of the beta vector
for(i in 1:p){
  
  x_number <- i
  # x_number <- 1
  
  # Calculate the sum term and n term for beta_x posterior
  y_mat <- matrix(y_data[x_number,1, ], nrow = 1, ncol = big_t)
  alpha <- matrix(alpha_x_vector[x_number,1], nrow = 1, ncol = big_t)
  beta_gamma <- matrix(beta_x_gamma_vector[x_number, ], nrow = 1, ncol = big_t)
  gamma <- matrix(gamma_big_matrix[x_number , ], nrow = 1, ncol = big_t)
  kappa <- matrix(kappa_matrix, nrow = 1, ncol = big_t)
  
  sum_vector <- (y_mat - (alpha + beta_gamma * gamma )) * kappa
  x_sum_term <- sum(sum_vector)
  
  x_n_term <- sum(kappa^2)
  
  # Make drawing from posterior
  posterior <- drawsFromN(sigma_1 = sigma_sq_beta_tilde, sum_term = x_sum_term, 
                          mu = mu_beta_tilde, sigma_2 = sigma_sq_epsilon, 
                          n_term = x_n_term, truncated = FALSE)
  
  
  # UPDATE value in beta matrix
  beta_x_vector[x_number , 1] <- posterior
}




# ---- IMPOSE BETA_X CONSTRAINT ----

# Beskrevet på side 14 i Cohort
beta_sum <- sum(beta_x_vector)


# UPDATE BETA X
beta_x_vector <- beta_x_vector / beta_sum




# ---- BETA_GAMMA_X FROM ITS POSTERIOR ----

# NOTE: Calculations are very similar to above

# Loop through all the indexes of the beta gamma vector
for(i in 1:p){
  
  x_number <- i
  # x_number <- 1
  
  # Calculate the sum term and n term for beta_x_gamma posterior 
  # Everything is the same as for beta - except the beta and beta_gamma term and the order in the sum
  y_mat <- matrix(y_data[x_number,1, ], nrow = 1, ncol = big_t)
  alpha <- matrix(alpha_x_vector[x_number,1], nrow = 1, ncol = big_t)
  beta <- matrix(beta_x_vector[x_number , 1], nrow = 1, ncol = big_t) # BEMÆRK DET ER MED DET NYE NU!
  gamma <- matrix(gamma_big_matrix[x_number , ], nrow = 1, ncol = big_t)
  kappa <- matrix(kappa_matrix, nrow = 1, ncol = big_t)
  
  sum_vector <- (y_mat - (alpha + beta * kappa)) * gamma
  x_sum_term <- sum(sum_vector)
  
  x_n_term <- sum(gamma^2)
  
  # Make drawing from posterior
  posterior <- drawsFromN(sigma_1 = sigma_sq_beta_tilde, sum_term = x_sum_term, 
                          mu = mu_beta_tilde, sigma_2 = sigma_sq_epsilon, 
                          n_term = x_n_term, truncated = FALSE)
  
  
  # UPDATE value in beta matrix
  beta_x_gamma_vector[x_number , 1] <- posterior
}




# ---- IMPOSE BETA_GAMMA_X_CONSTRAINT ----

# # again: same procedure as above # Beskrevet på side 14 i Cohort
# beta_sum <- sum(beta_x_gamma_vector)
# 
# 
# # UPDATE BETA X GAMMA
# beta_x_gamma_vector <- beta_x_gamma_vector / beta_sum
# # Vi dividerer ca. med 10 i første omgang, måske de burde være tættere på hinanden, som tiden går - kunne måske være noget, der var rart at printe ud og dermed holde øje med!




# ---- ALPHA_X FROM ITS POSTERIOR ----

# NOTE: Calculations are very similar to above

# Loop through all the indexes of the beta gamma vector
for(i in 1:p){
  
  x_number <- i
  # x_number <- 1
  
  # Calculate the sum term and n term for alpha posterior 
  y_mat <- matrix(y_data[x_number,1, ], nrow = 1, ncol = big_t)
  beta <- matrix(beta_x_vector[x_number , 1], nrow = 1, ncol = big_t) # BEMÆRK DET ER MED DET NYE NU!
  kappa <- matrix(kappa_matrix, nrow = 1, ncol = big_t)
  beta_gamma <- matrix(beta_x_gamma_vector[x_number, ], nrow = 1, ncol = big_t)
  gamma <- matrix(gamma_big_matrix[x_number , ], nrow = 1, ncol = big_t)
  
  sum_vector <- (y_mat - beta * kappa - beta_gamma * gamma)
  x_sum_term <- sum(sum_vector)
  
  x_n_term <- p
  
  # Make drawing from posterior
  posterior <- drawsFromN(sigma_1 = sigma_sq_alpha_tilde, sum_term = x_sum_term, 
                          mu = mu_alpha_tilde, sigma_2 = sigma_sq_epsilon, 
                          n_term = x_n_term, truncated = FALSE)
  
  
  # UPDATE value in beta matrix
  alpha_x_vector[x_number , 1] <- posterior
}




# ---- THETA FROM ITS POSTERIOR ----

# Make a vector containing that kappa values for t = 0,...,t-1
kappa_tm1 <- matrix(c(kappa_0, kappa_matrix[,,(-big_t)]), nrow = 1, ncol = big_t)

sigma_sq_omega <- 10 # ERROR IN TEXT HERE! p. 15

# Calculate the sum term and n term for beta_x_gamma posterior 
kappa <- matrix(kappa_matrix, nrow = 1, ncol = big_t)

sum_vector <- (kappa - kappa_tm1)
x_sum_term <- sum(sum_vector)

x_n_term <- p

# Make drawing from posterior
posterior <- drawsFromN(sigma_1 = sigma_sq_theta_tilde, sum_term = x_sum_term, 
                        mu = mu_theta_tilde, sigma_2 = sigma_sq_omega, 
                        n_term = x_n_term, truncated = FALSE)


# UPDATE value in beta matrix
theta <- posterior




# ---- ETA FROM ITS POSTERIOR ----

# Make a vector containing that kappa values for t = 0,...,t-1
gamma_x1_tm1 <- matrix(c(gamma_0[1] , gamma_big_matrix[1,(-big_t)]), nrow = 1, ncol = big_t)
gamma_x1 <- matrix(gamma_big_matrix[1 , ], nrow = 1, ncol = big_t)

# Calculate the sum term and n term for beta_x_gamma posterior 
sum_vector <- (gamma_x1 - lambda * gamma_x1_tm1)
x_sum_term <- sum(sum_vector)

x_n_term <- p

# Make drawing from posterior
posterior <- drawsFromN(sigma_1 = sigma_sq_eta_tilde, sum_term = x_sum_term, 
                        mu = mu_eta_tilde, sigma_2 = sigma_sq_gamma, 
                        n_term = x_n_term, truncated = FALSE)


# UPDATE value in beta matrix
eta <- posterior




# ---- LAMBDA FROM ITS POSTERIOR ----

# # Make a vector containing that kappa values for t = 0,...,t-1: Already made above!
# gamma_x1_tm1 <- matrix(c(gamma_0[1] , gamma_big_matrix[1,(-big_t)]), nrow = 1, ncol = big_t)
# gamma_x1 <- matrix(gamma_big_matrix[1 , ], nrow = 1, ncol = big_t)

# Calculate the sum term and n term for beta_x_gamma posterior 
sum_vector <- ((gamma_x1 - eta) * gamma_x1_tm1)
x_sum_term <- sum(sum_vector)

x_n_term <- sum(gamma_x1_tm1^2)

# Make drawing from posterior
posterior <- drawsFromN(sigma_1 = sigma_sq_lambda_tilde, sum_term = x_sum_term, 
                        mu = mu_lambda_tilde, sigma_2 = sigma_sq_gamma, 
                        n_term = x_n_term, truncated = TRUE)


# UPDATE value in beta matrix
lambda <- posterior




# ---- SIGMA_EPSILON FROM ITS POSTERIOR ----

# Shape paramter
shape_epsilon <- calculateIGShape(a = a_epsilon_tilde, n = n_epsilon)

# Scale parameter
# calculate the double sum
innerSumVec <- matrix(NA, nrow = p, ncol = 1)
# Loop through all the indexes of the beta gamma vector
for(i in 1:p){
  
  x_number <- i
  # x_number <- 1
  
  # Calculate the sum term and n term for alpha posterior 
  y_mat <- matrix(y_data[x_number,1, ], nrow = 1, ncol = big_t)
  beta <- matrix(beta_x_vector[x_number , 1], nrow = 1, ncol = big_t) # BEMÆRK DET ER MED DET NYE NU!
  kappa <- matrix(kappa_matrix, nrow = 1, ncol = big_t)
  beta_gamma <- matrix(beta_x_gamma_vector[x_number, ], nrow = 1, ncol = big_t)
  gamma <- matrix(gamma_big_matrix[x_number , ], nrow = 1, ncol = big_t)
  alpha <- matrix(alpha_x_vector[x_number,1], nrow = 1, ncol = big_t)
  
  inner_sum_vector <- (y_mat - (alpha + (beta * kappa) + (beta_gamma * gamma)))^2
  inner_sum <- sum(inner_sum_vector)
  
  # update value
  innerSumVec[x_number, ]
}
scale_epsilon <- b_epsilon_tilde + (0.5 * (sum(inner_sum_vector)))


# UPDATE SIGMA_SQ_EPSILON
sigma_sq_epsilon <- rinvgamma(n = 1, shape = shape_epsilon, scale = scale_epsilon)




# ---- SIGMA_KAP FROM ITS POSTERIOR ----

# Make a vector containing that kappa values for t = 0,...,t-1
kappa_tm1 <- matrix(c(kappa_0, kappa_matrix[,,(-big_t)]), nrow = 1, ncol = big_t)
kappa <- matrix(kappa_matrix, nrow = 1, ncol = big_t)

# Draw a new sigma_eps
shape_kappa <- calculateIGShape(a = a_kappa_tilde, n = n_kappa)
vec <- kappa - (kappa_tm1 + theta)
sum <- sum(vec^2)
sum <- 40
scale_kappa <- b_kappa_tilde + (0.5 * sum) 

# print(paste("mean", (scale_kappa / (shape_kappa - 1))))

# UPDATE SIGMA_SQ_KAPPA
sigma_sq_kappa <- rinvgamma(n = 1, shape = shape_kappa, scale = scale_kappa)




# ---- SIGMA_GAM FROM ITS POSTERIOR ----

# Make a vector containing that kappa values for t = 0,...,t-1
gamma_x1_tm1 <- matrix(c(gamma_0[1] , gamma_big_matrix[1,(-big_t)]), nrow = 1, ncol = big_t)
gamma_x1 <- matrix(gamma_big_matrix[1 , ], nrow = 1, ncol = big_t)

# Draw a new sigma_gam
shape_gamma <- calculateIGShape(a = a_gamma_tilde, n = n_gamma)

vec <- gamma_x1 - (lambda * gamma_x1_tm1)
sum <- sum((vec)^2)
scale_gamma <- b_gamma_tilde + 0.5 * sum


# UPDATE SIGMA_SQ_GAMMA
sigma_sq_gamma <- rinvgamma(n = 1, shape = shape_gamma, scale = scale_gamma)





# # ---- SAVE VALUES FROM THIS ITERATION ---- 
results_vec[ite, 1] <- theta
results_vec[ite, 2] <- eta
results_vec[ite, 3] <- lambda
results_vec[ite, 4] <- sigma_sq_epsilon
results_vec[ite, 5] <- sigma_sq_kappa
results_vec[ite, 6] <- sigma_sq_gamma
# 
}
# ----- !! MC ITERATIONS END HERE !! ----


# ---- FINAL RESULTS ---- 
# model_estimates <- round(apply(results_vec, 2, mean), digits = 4)
model_estimates <- round(results_vec, digits = 4)
model_estimates[iterations , ]



# THE END!
