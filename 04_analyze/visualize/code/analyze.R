main <- function(){
  data <- basics$read_interim("master")
  
  my_plot_gdp <- data %>% 
    lay_basic_gdp() %>% 
    lay_shape() %>% 
    lay_frame() %>% 
    lay_title_gdp() %>% 
    basics$save_my_plot( var_name = "gdp_trends",
                         folder_name = "visualize")
    
  
  my_plot_gini <- data %>% 
    lay_basic_gini() %>% 
    lay_shape() %>% 
    lay_frame() %>% 
    lay_title_gini() %>% 
    basics$save_my_plot( var_name = "gini_trends",
                         folder_name = "visualize")

}



lay_basic_gdp <- function(input_data){
  output<- input_data %>% 
    ggplot2::ggplot(mapping = ggplot2::aes(x=year,
                                           y=per_capita*10^5,
                                           color=country))
  return(output)
}
  

lay_shape <- function(input){
  output <- input+
    ggplot2::geom_line()+
    ggplot2::geom_point()
  return(output)
}

lay_frame <- function(input){
  output <- input+
    ggplot2::theme_minimal()
}

lay_title_gdp <- function(input){
  output <- input+
    ggplot2::labs(title = "Trends of GDP per capita",
                  y="GDP per capita" )
  return(output)
}
  

lay_basic_gini<- function(input_data){
  output<- input_data %>% 
    ggplot2::ggplot(mapping = ggplot2::aes(x=year,
                                           y=gini,
                                           color=country))
  return(output)
}


lay_title_gini <- function(input){
  output <- input+
    ggplot2::labs(title = "Trends of Gini Coefficient",
                  y="Gini Coefficient" )
  return(output)
}

box::use(`functions`/basics)
main()


