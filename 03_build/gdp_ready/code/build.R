library(magrittr)

main <- function(){
  my_folder <- "gdp"
  
  tidy_data <- basics$read_interim(my_folder, extension = "tidy")
  
  ready_data <- tidy_data %>% 
    add_gdp_per_capita() 
  
  basics$save_interim(ready_data, my_folder, extension = "ready")  
}


add_gdp_per_capita <- function(input_data){
  output_data <- input_data %>% 
    dplyr::mutate(per_capita = gdp/pop)
  return(output_data)
}

box::use(`functions`/basics)
data <- main()
