generate_data <- function(n,p){
  return(list(covariates = matrix(rnorm(n*p,0,1), nrow = n, ncol = p), responses = rnorm(n,0,1)))
}
