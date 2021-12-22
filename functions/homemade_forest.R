homemade_forest <- function(tmp, axistitle, maxvalue){
  tmp %>% 
    unique() %>%
    filter(!is.na(yi)) %>%
    ggplot() +
    geom_point(aes(x = yi,
                   y = reorder(study_age, -age_mean),
                   shape = rem,
                   colour = rem,
                   size = number_covid_positive)) +
    geom_errorbarh(aes(xmin = ifelse(ci.lb<0,0,ci.lb),
                       xmax = ifelse(ci.ub>1,1,ci.ub),
                       y = reorder(study_age, -age_mean),
                       colour = rem),
                   alpha = 0.4)+
    geom_text(aes(label = paste0(round(yi, 2), 
                                 " [", ifelse(ci.lb<0,0,round(ci.lb,2)), 
                                 ", ", 
                                 ifelse(ci.ub>1,1,round(ci.ub,2)), "]"),
                  x = maxvalue + 0.2,
                  y = reorder(study_age, -age_mean),
                  colour = rem),
              size = 4)+
    geom_vline(xintercept = rem$yi, colour = cividis[9], linetype = "dashed")+
    
    coord_cartesian(xlim = c(0,maxvalue+0.3))+
    labs(y = "", x = axistitle)+
    theme_minimal() +
    scale_shape_manual(values = c(15, 18)) +
    theme(legend.position = "none", axis.text = element_text(size = 12)) +
    scale_y_discrete(breaks = tmp$study_age, labels = toupper(tmp$study)) +
    scale_size(range = c(0.1, 5))+
    scale_colour_manual(values = c("black", cividis[9]) )+
    scale_x_continuous(breaks = seq(0,1,0.2), labels = seq(0,1,0.2))
  
}