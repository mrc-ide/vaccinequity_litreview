homemade_forest <- function(meta_in, cols_to_use, axistitle){
  # split out summary and rma
  tmp_escalc <- summary(meta_in$escalc_out) %>% 
    mutate_at(vars(cols_to_use), as.numeric)
  tmp_rma <- meta_in$metan
  
  #get range for plots
  maxvalue <- max(tmp_escalc$ci.ub)
  minvalue <- min(tmp_escalc$ci.lb)
  
  #pick out rma
  rem <- data.frame(yi = as.numeric(tmp_rma$beta),
                    ci.lb = as.numeric(tmp_rma$ci.lb),
                    ci.ub = as.numeric(tmp_rma$ci.ub),
                    rem = TRUE,
                    year_of_data_min = 1900,
                    pval = tmp_rma$pval)
  
  #stick it together and tidy
  tmp <- tmp_escalc %>%
    select( yi, ci.lb, ci.ub,covidence_id, year_of_article, first_author_surname, 
            year_of_data_min, year_of_data_max,
            !!cols_to_use) %>%
    mutate(rem = FALSE) %>%
    bind_rows(rem) %>%
    rowwise() %>%
    mutate(data_years = ifelse(year_of_data_min == year_of_data_max, 
                               paste0(year_of_data_max),
                               paste0(year_of_data_min, "-", year_of_data_max))) %>%
    mutate(study = paste0(first_author_surname, " ", 
                          year_of_article, " (", data_years, ")")) %>%
    mutate(study = ifelse(rem %in% TRUE, " Random Effects Model", study)) 
  

  #get sizes as vaccinated + unvaxxed
  tmp$size <- rowSums(tmp[, cols_to_use], na.rm = TRUE) 
  tmp <- tmp %>% mutate(size = ifelse(study == " Random Effects Model", 5000, size))
  
  #colour depends on whether fully negative/ positive or neither
  tmp <- tmp %>% mutate(col = case_when(ci.lb > 0 ~ "A",
                                        yi >= 0 ~ "B",
                                        ci.ub < 0 ~ "D",
                                        yi < 0 ~ "C")) %>%
    mutate(col = factor(col, levels = c("A", "B", "C", "D")))
  
  
  tmp %>% 
    unique() %>%
    filter(!is.na(yi)) %>%
    ggplot() +
    geom_point(aes(x = yi,
                   y = reorder(study, year_of_data_min),
                   shape = rem,
                   colour = col,
                   size = size,
                   alpha = rem)) +
    geom_errorbarh(aes(xmin = ci.lb,
                       xmax = ci.ub,
                       y = reorder(study, year_of_data_min),
                       colour = col,
                       alpha = rem), 
                   size=1)+
    geom_text(aes(label = paste0(round(yi, 2), 
                                 " [", round(ci.lb,2), 
                                 ", ", 
                                 round(ci.ub,2), "]"),
                  x = maxvalue + 0.3,
                  y = reorder(study, year_of_data_min),
                  colour = col,
                  alpha = rem),
              size = 4)+
    geom_vline(xintercept = 0, colour = viridis::turbo(9)[8], linetype = "dashed", size = 1)+
    
    coord_cartesian(xlim = c(minvalue,maxvalue+0.6))+
    labs(y = "", x = axistitle)+
    theme_minimal() +
    scale_shape_manual(values = c(15, 18)) +
    scale_alpha_discrete(range = c(0.5, 1))+
    theme(legend.position = "none", axis.text = element_text(size = 12)) +
    scale_size(range = c(1, 5))+
    scale_colour_manual(values = c("A" = viridis::turbo(10)[10],
                                   "B" = viridis::turbo(10)[8],
                                   "C" = viridis::turbo(10)[2],
                                   "D" = viridis::turbo(10)[1]))
  
 }
