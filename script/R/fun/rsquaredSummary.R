rsquaredSummary <- function ( data, lev = NULL, model = NULL){
  Rsquared <- 1 - (sum((data$obs - data$pred)^2) / sum((data$obs - mean(data$obs))^2))
  
  return(c(`Rsquared` = Rsquared)) 
}