library(magrittr)

main <- function(){
  use_rstan_packcage()
  data <- basics$read_interim("master")
  
  J_data <- extract_Japan_data(data) %>% 
    prepare_run_model()
  U_data <- extract_US_data(data) %>% 
    prepare_run_model()
  
  J_fit <- run_model(J_data)
  U_fit <- run_model(U_data)
  
  J_result <- rstan::extract(J_fit)
  U_result <- rstan::extract(U_fit)
  
  save_J_result(J_result)
  save_U_result(U_result)
}

use_rstan_packcage <- function(){
  install.packages("rstan")
  library(rstan)
}

extract_Japan_data <- function(input){
  output <- input %>% 
    dplyr::filter(country=="JPN")
  return(output)
}

extract_US_data <- function(input){
  output <- input %>% 
    dplyr::filter(country=="USA")
  return(output)
}

prepare_run_model <- function(input){
  output <- list(N=nrow(input),
                 Z=input$per_capita*10*9,
                 Y=input$gini)
  return(output)
}

run_model <- function(input){
  model_path <- "04_analyze/analyze/code/model.stan"
  fit <- rstan::stan(file=model_path,data=input,seed=123)
  return(fit)
}

save_J_result <-function(input){
  path <- "04_analyze/analyze/output/J_result.obj"
  saveRDS(input,file = path)
}

save_U_result<-function(input){
  path <- "04_analyze/analyze/output/U_result.obj"
  saveRDS(input,file =path)
}

main()