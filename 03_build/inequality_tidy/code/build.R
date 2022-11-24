library(magrittr)

main <- function(){
 my_folder <- "inequality" 
 
 raw_data <- read_raw(my_folder,
                      file_name = "Gini.xlsx") 
 
 tidy_data <-raw_data %>% 
   to_long() %>% 
   reprace_by_NA() %>% 
   to_numeric()
 
 basics$save_interim(tidy_data, my_folder, extension = "tidy")
}


read_raw <- function(folder_name,file_name){
  file_path <- here::here("02_raw",folder_name,"data",file_name)
  data <- readxl::read_excel(file_path,
                             skip=2,
                             col_names = FALSE)
  return(data)
}


to_long <- function(input_data){
  output_data <- input_data %>% 
    t() %>% 
    dplyr::as_tibble() %>% 
    dplyr::slice(-1) %>% 
    dplyr::rename("country"=V1,
                  "year"=V2,
                  "gini"=V3)
  return(output_data)
}


reprace_by_NA <- function(input_data){
  output_data <- input_data %>% 
    dplyr::na_if("missing")
  return(output_data)
}

to_numeric <- function(input_data){
  output_data <- input_data %>% 
    dplyr::mutate("year"=as.numeric(year),
                  "gini"=as.numeric(gini))
  return(output_data)
}


box::use(`functions`/basics)
main()



