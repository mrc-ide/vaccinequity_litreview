OR_over_time <- function(meta_in, cols_to_use, axistitle){
  to_plot <- meta_in$escalc_out %>%
    rowwise() %>%
    mutate(mean_data_year = mean(c(year_of_data_min, year_of_data_max)))
  to_plot$size <- rowSums(to_plot[, cols_to_use])
  
  fitted <- lm(yi ~ mean_data_year, data = to_plot)
  pval <- summary(fitted)$coefficients[,4][2]
  
  to_plot %>%
    ggplot()+
    aes(x = mean_data_year,
        xmin = year_of_data_min,
        xmax = year_of_data_max,
        y = yi)+
    geom_point()+
    geom_smooth(method = "lm", 
                colour = viridis::cividis(9)[8], fill = viridis::cividis(9)[8],
    )+
    geom_errorbarh(alpha = 0.6)+
    theme_minimal()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    labs(x = "Year/s of data",
         y = axistitle)+
    ggtitle(paste0("Relationship between year of data and adjusted odds ratio (pvalue = ", 
                   round(pval,3), ")"))
}