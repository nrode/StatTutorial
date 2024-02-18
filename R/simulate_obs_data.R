#' Simulate observed data to estimate repeatability
#' @description Simulate data for a trait measured on n_group groups with n0 replicates
#' @param seed seed used so that random values can found again (https://r-coder.com/set-seed-r/)
#' @param n_group Number of groups (e.g. number of individuals measured)
#' @param n0 Number of replicates
#' @param mean_trait Mean of the trait measured
#' @param var_among Variance among groups
#' @param var_within Variance among measurements of the trait within each individual
#' 
#' @return dataset with id of each group, the replicate and each simulated trait value
#' @export 
#'
#' @examples
#'simulate_obs_data(n_group = 30, n0 = 2, mean_trait=0, var_among=1, var_within=0.25)
#'
simulate_obs_data <- function(seed=123, n_group = 30, n0 = 2, mean_trait=0, var_among=1, var_within=0.25){
  
  ## Number of individual measured
  #n_group=30
  ## Number of replicate per individual
  #n0=2
  
  ## Mean of the trait in the population
  #mean_trait=10
  
  ## Variance of the trait among individuals
  #var_among=1
  
  ## Variance among measurement of the trait within each individual
  #var_within=0.1
  ## Set a seed so that results can be reproduced
  set.seed(seed)
  
  ## Create dataset
  data <- expand.grid(id=as.factor(1:n_group), replicate=paste0("rep", 1:n0))
  
  ## Simulate true value of the trait for each individual
  true_trait_value <- rnorm(n=n_group, mean = mean_trait, sd = sqrt(var_among))
  
  ## Add new column with true value of the trait for each individual
  data$true_trait_value <- rep(true_trait_value, n0)

  ## Add new column with measurement error
  data$measurement_error <- rnorm(n=nrow(data), mean = 0, sd = sqrt(var_within))
  
  ## Add new column with observed value of the trait for each replicate measurement of each individual
  data$observed_trait_value <- data$true_trait_value + data$measurement_error
  
  ## Fit model with ANOVA
  fit1 <- aov(observed_trait_value ~ id, data = data)
  summary(fit1)
  
  ## Within-group variance component = Residual variance
  VarWithin <-  MSW <- summary(fit1)[[1]]$`Mean Sq`[2]
  
  ## Among-groups (ie individual) Mean square
  MSA <- summary(fit1)[[1]]$`Mean Sq`[1]
  
  ## Among individual variance 
  VarAmong<- (MSA-MSW)/n0
  
  ## Observed repeatability
  Repeatability <- VarAmong/(VarAmong+VarWithin)
  
  
  return(Repeatability)

}