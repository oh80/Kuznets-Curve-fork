main <- function(){
  J_result <- readRDS("04_analyze/analyze/output/J_result.obj")
  U_result <- readRDS("04_analyze/analyze/output/U_result.obj")
  
  J_hist <- J_result %>% 
    lay_basic_J() %>% 
    lay_shape_J() %>% 
    lay_frame() %>% 
    lay_title_J() %>% 
    basics$save_my_plot( var_name = "J_hist",
                        folder_name = "mcmc_sample_visualize")
  
  U_hist <- U_result %>% 
    lay_basic_U() %>% 
    lay_shape_U() %>% 
    lay_frame() %>% 
    lay_title_U()%>% 
    basics$save_my_plot( var_name = "U_hist",
                         folder_name = "mcmc_sample_visualize")
  
  J_summary <- J_result %>% 
    extract_beta() %>% 
    summaraize()
  
  U_summary <- U_result %>% 
    extract_beta() %>% 
    summaraize()
  
  summary_table <- master_tables(J_summary,U_summary) %>% 
    plot_summary_table() %>% 
    save_table()
}


lay_basic_J<- function(input){
  output <- input$b %>% 
    dplyr::as_tibble() %>% 
    ggplot2::ggplot(mapping = ggplot2::aes(x=value))+
    ggplot2::xlim(-200,200)
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
    ggplot2::geom_histogram(fill = "tomato",binwidth = 5)
  return(output)
}

lay_shape_U <- function(input){
  output <- input+
    ggplot2::geom_histogram(fill = "lightskyblue",binwidth = 0.5)
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


extract_beta <- function(input){
  output <- input$b %>% 
    as.data.frame() %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename("b"=".")

  return(output)
}

summaraize <- function(input){
  output <- input %>% 
    dplyr::summarise(mean=mean(b),
                     sd = sd(b),
                     median=median(b),
                     q_2.5=quantile(b,0.025),
                     q_97.5=quantile(b,0.975))
  return(output)
}

master_tables <- function(input_1,input_2){
  output <- input_1 %>% 
    dplyr::bind_rows(input_2) %>% 
    dplyr::mutate("country"=c("JPN","USA")) %>% 
    dplyr::select("country","mean","sd","q_2.5","q_97.5")
  return(output)
}


plot_summary_table <- function(input){
  output <- input %>% 
    kableExtra::kbl(
                    digits = 2,
                    booktabs = TRUE,
                    format = "latex") %>% 
    kableExtra::kable_styling(font_size = 15)
  return(output)
}


save_table <- function(input){
  path <- "04_analyze/mcmc_sample_visualize/table/table.txt"
  writeLines(input,
             path)
}
box::use(`functions`/basics)
main()

