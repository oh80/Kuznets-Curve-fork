library(magrittr)

main <- function(){
  gini_data <- basics$read_interim("inequality", extension = "ready") 
  gdp_data <- basics$read_interim("gdp", extension = "ready")%>% 
    rename_to_USA()
  
  master_data <- master_gini_and_gdp(gdp_data,gini_data) %>% 
    extract_every_5years()
  
  basics$save_interim(master_data, "master")
}


rename_to_USA <- function(input_data){
  input_data$country <- input_data$country %>% 
    stringr::str_replace(pattern = "US",
                         replacement = "USA")
  output_data <- input_data
  return(output_data)
}

master_gini_and_gdp <- function(input_data1,input_data2){
  output_data <- input_data1 %>% 
    dplyr::left_join(input_data2,by=c("year","country"))
  return(output_data)
}


extract_every_5years <- function(input_data){
  output_data <- input_data %>% 
    dplyr::filter(year>=1990)
  return(output_data)
}


box::use(`functions`/basics)
main()
