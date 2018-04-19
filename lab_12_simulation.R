generate_data <- function(n,p){
  return(list(covariates = matrix(rnorm(n*p,0,1), nrow = n, ncol = p), responses = rnorm(n,0,1)))
}

model_select <- function(covariates, responses, cutoff){
  # Get p-values from the regression, but don't include the intercept p-value
  original.lm.p.values <- summary(lm(responses ~ covariates))$coefficients[,4][-1]
  filtered.p.values <- original.lm.p.values[original.lm.p.values < cutoff]
  if(length(filtered.p.values) == 0){
    return(vector(length = 0, mode = "numeric"))
  }
  else{
    new.model <- lm(responses ~ covariates[,which(original.lm.p.values %in% filtered.p.values)])
    return(summary(new.model)$coefficients[,4][-1])
  }
}

make_plot <- function(datapath){
  p.values <- readRDS(datapath)
  hist(p.values)
}

run_simulation <- function(n_trials, n, p, cutoff){
  datapath = paste("p.values", as.character(n), ",", as.character(p), ",", as.character(cutoff), ",", as.character(n_trials), sep = "")
  p.values <- vector(mode = "numeric")
  for(i in 1:n_trials){
    data <- generate_data(n,p)
    p.values <- c(p.values, model_select(data$covariates, data$responses, cutoff))
  }
  saveRDS(p.values, file = datapath)
  make_plot(datapath)
}

for(n in c(100, 1000, 10000)){
  for(p in c(10,20,50)){
    run_simulation(n_trials = 100, n, p, cutoff = 0.05)
  }
}

# Now that the simulations have been run:
for(n in c(100, 1000, 10000)){
  for(p in c(10,20,50)){
    make_plot(paste("p.values", as.character(n), ",", as.character(p), ",", as.character(.05), ",", as.character(100), sep = ""))
  }
}

# P-values seem to be uniformly distributed from 0 to 1, but not after that
# Correction to 2c: I originally inputted p = 0.5 rather than 0.05. The p-values are not uniformly distributed