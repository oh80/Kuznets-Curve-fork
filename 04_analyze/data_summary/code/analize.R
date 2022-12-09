library(magrittr)

main <- function(){
  data <- basics$read_interim("master")
  
  data_summary <- summaraize(data)
  
  my_table <- modify_order(data_summary) %>% 
    plot_summary_table()
  
  saveRDS(my_table,
          file = "04_analyze/data_summary/figure/summary_table.obj")
}


summaraize <- function(input_data){
  input_data <- input_data %>% 
    dplyr::mutate(per_capita = per_capita*10^5)
  
  data_90 <- input_data %>% 
    dplyr::filter(year==1990 | year==1995) %>% 
    dplyr::group_by(country)
  
  data_00 <- input_data %>% 
    dplyr::filter(year==2000 | year==2005) %>% 
    dplyr::group_by(country)
  
  data_10 <- input_data %>% 
    dplyr::filter(year==2010 | year==2015) %>% 
    dplyr::group_by(country)
  
  summary_90 <- data_90 %>% 
    dplyr::summarise(GDP_per_capita_90 = mean(per_capita),
                     gini_90 = mean(gini))
  summary_00 <- data_00 %>% 
    dplyr::summarise(GDP_per_capita_00 = mean(per_capita),
                     gini_00=mean(gini))
  summary_10 <- data_10 %>% 
    dplyr::summarise(GDP_per_capita_10 = mean(per_capita),
                     gini_10=mean(gini))
  
  output <-summary_90 %>% 
    dplyr::full_join(summary_00) %>% 
    dplyr::full_join(summary_10)
  
  return(output)
}

modify_order <- function(input_data){
  output <- input_data %>% 
    dplyr::select(country,
                  GDP_per_capita_90,GDP_per_capita_00,GDP_per_capita_10,
                  gini_90,gini_00,gini_10)
  return(output)
}


plot_summary_table <- function(input_data){
  
  col_name <- list("country","1990's","2000's","2010's",
                   "1990's","2000's","2010's")
  output <- input_data %>% 
    kableExtra::kbl(col.names = col_name,
                    digits = 3,
                    booktabs = TRUE) %>% 
    kableExtra::kable_styling(font_size = 15) %>% 
    kableExtra::add_header_above(c(" "=1,"GDP_per_capita"=3,"Gini"=3)) %>% 
    kableExtra::column_spec(2:4,
                            background = "lightblue") %>% 
    kableExtra::column_spec(5:7,
                            background = "lightyellow") %>% 
    
  
  return(output)
}


box::use(`functions`/basics)
main()

