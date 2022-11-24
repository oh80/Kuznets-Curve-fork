library(magritter)

main <- function(){
  my_folder <-"gdp"
  
  J_raw_data <- read_raw(my_folder,
                         file_name="japan.csv") 
  U_raw_data <- read_raw(my_folder,
                         file_name="United States.csv")
  
  J_data <- J_raw_data %>% 
    J_rename()  
  U_data <- U_raw_data %>% 
    U_rename() 
  
  tidy_data <- master_data(J_data,U_data) 

  basics$save_interim(tidy_data, my_folder, extension = "tidy")
}


read_raw <- function(folder_name,file_name){
  file_path <- here::here("02_raw",folder_name,"data",file_name)
  data <- readr::read_csv(file_path)
  return(data)
}


J_rename <- function(input_data){
  output_data <- input_data %>% 
    dplyr::rename("pop"=population,
                  "gdp"=GDP) %>% 
    dplyr::mutate("country"=rep("JPN"))
  return(output_data)
}


U_rename <- function(input_data){
  output_data <- input_data %>% 
    dplyr::rename("pop"=population,
                  "gdp"=GDP) %>% 
  dplyr::mutate("country"=rep("US"))
  return(output_data)
}


master_data <- function(input_data1,input_data2){
  output_data <- input_data1 %>% 
    dplyr::bind_rows(input_data2) %>% 
    dplyr::arrange(by=year)
  return(output_data)
}



box::use(`functions`/basics)
main()
