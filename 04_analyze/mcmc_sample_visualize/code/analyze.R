main <- function(){
  J_result <- readRDS("04_analyze/analyze/output/J_result.obj")
  U_result <- readRDS("04_analyze/analyze/output/U_result.obj")
  
  J_hist <- J_result %>% 
    lay_basic_J() %>% 
    lay_shape_J() %>% 
    lay_frame() %>% 
    lay_title_J()
  
  U_hist <- U_result %>% 
    lay_basic_U() %>% 
    lay_shape_U() %>% 
    lay_frame() %>% 
    lay_title_U()
  
  
    
  
  return(U_hist)
}


lay_basic_J<- function(input){
  output <- input$b %>% 
    dplyr::as_tibble() %>% 
    ggplot2::ggplot(mapping = ggplot2::aes(x=value))+
    ggplot2::xlim(-200,200)+
    ggplot2::
  return(output)
}

lay_basic_U<- function(input){
  output <- input$b %>% 
    dplyr::as_tibble() %>% 
    ggplot2::ggplot(mapping = ggplot2::aes(x=value))+
    ggplot2::xlim(-15,15)
  return(output)
}


lay_shape_J <- function(input){
  output <- input+
    ggplot2::geom_histogram(fill = "tomato")
  return(output)
}

lay_shape_U <- function(input){
  output <- input+
    ggplot2::geom_histogram(fill = "lightskyblue")
  return(output)
}


lay_frame <- function(input){
  output <- input+
    ggplot2::theme_minimal()
  return(output)
}


lay_title_J <- function(input){
  output <- input+
    ggplot2::labs(title = "Japan",
                  subtitle = "distribution of MCMC sample",
                  x = "b")
  return(output)
}

lay_title_U <- function(input){
  output <- input+
    ggplot2::labs(title = "USA",
                  subtitle = "distribution of MCMC sample",
                  x = "b")
  return(output)
}
main()


  

