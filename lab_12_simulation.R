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

