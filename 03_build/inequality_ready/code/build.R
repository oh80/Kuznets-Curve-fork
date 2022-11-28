library(magrittr)

main <- function(){
  my_folder <-"inequality"
  
  tidy_data <- basics$read_interim(my_folder, extension = "tidy")
  J_tidy_data <-filter_J(tidy_data)
  U_tidy_data <-filter_U(tidy_data) 
  
  J_ready_data <-J_tidy_data %>% 
    add_year_J() %>% 
    fill_na_country_J() %>% 
    fill_na_gini_J()
     

  U_ready_data <-U_tidy_data %>% 
    add_year_U() %>% 
    fill_na_country_U() %>% 
    fill_na_gini_U() 
  
  ready_data <- master_data(J_ready_data, U_ready_data)
  
  basics$save_interim(ready_data, my_folder, extension = "ready")  
}

filter_J <- function(input_data){
  output_data <- input_data %>% 
    dplyr::filter(country=="JPN")
  return(output_data)
}


filter_U <- function(input_data){
  output_data <- input_data %>% 
    dplyr::filter(country=="USA")
  return(output_data)
}


add_year_J <- function(input_data){
  every_yaer <- c(1990:2017) %>% tidyr::as_tibble() %>% 
    dplyr::rename(year=value)
  output_data <- input_data %>% dplyr::full_join(every_yaer,by="year") %>% 
    dplyr::arrange(year)
  return(output_data)
}


fill_na_country_J <- function(input_data){
  output_data <- input_data %>% 
    tidyr::replace_na(replace = list(country="JPN"))
  return(output_data)
}


add_year_U <- function(input_data){
  every_yaer <- c(1974:2019) %>% tidyr::as_tibble() %>% 
    dplyr::rename(year=value)
  output_data <- input_data %>% dplyr::full_join(every_yaer,by="year") %>% 
    dplyr::arrange(year)
  return(output_data)
}

fill_na_country_U <- function(input_data){
  output_data <- input_data %>% 
    tidyr::replace_na(replace = list(country="USA"))
  return(output_data)
}


fill_na_gini_U <- function(input_data){
  observed_year <-input_data %>% 
    dplyr::filter(!is.na(gini)) %>% 
    dplyr::select(year)
  
  filled_gini <-c()
  
  for (i in 2:length(observed_year$year)-1) {
    if(observed_year$year[i+1]-observed_year$year[i]>=2){
      
      for (k in 1:(observed_year$year[i+1]-observed_year$year[i]-1)){
        dif <- observed_year$year[i+1]-observed_year$year[i]
        old_gini <-input_data$gini[input_data$year==observed_year$year[i]]
        gini_dif <-input_data$gini[input_data$year==observed_year$year[i]]-
          input_data$gini[input_data$year==observed_year$year[i+1]]
        new_gini <- old_gini+k/dif*gini_dif
        filled_gini <-append(filled_gini,new_gini)
      }
    }else{
      gini <- input_data$gini[input_data$year==observed_year$year[i]]
      filled_gini <-append(filled_gini,gini)
    }
  }
  
  filled_gini <- filled_gini %>% 
    append(input_data$gini[40:46]) 
  
  output_data <- input_data %>% 
    dplyr::mutate("gini"=filled_gini)
  
  return(output_data)
}


fill_na_gini_J<- function(input_data){
  for (i in 1:length(input_data$gini)){
    if (i%%3==1){
      input_data$gini[i] <- input_data$gini[i]
    }
    else if(i%%3==2){
      input_data$gini[i] <- (input_data$gini[i-1]*2/3+input_data$gini[i+2]*1/3)
    }
    else if(i%%3==0){
      input_data$gini[i] <- (input_data$gini[i-2]*1/3+input_data$gini[i+1]*1/3)
    }
  }
  output_data <- input_data
  return(output_data)
}

master_data <- function(input_data1,input_data2){
  output_data <-input_data1 %>% 
    dplyr::bind_rows(input_data2)
  return(output_data)
}


box::use(`functions`/basics)
main()

