run_meta <- function(df, cols_to_use, outp = "RR"){
  tmp <- df %>% filter_at(vars(cols_to_use), all_vars(!is.na(.))) %>% 
    mutate_at(vars(cols_to_use), as.numeric)
  
  unv <- cols_to_use[grep("unv", cols_to_use)]
  v <- cols_to_use[grep("_v", cols_to_use)]
  
  #aggregate 
  tmp <- tmp %>% 
    select(year_of_data_min, year_of_data_max, year_of_article, first_author_surname, covidence_id,
           !!cols_to_use) %>%
    group_by(year_of_data_min, year_of_data_max, year_of_article, first_author_surname, covidence_id) %>%
    summarise_all(sum, na.rm = TRUE)
  
  metan <- metafor::rma(n1i = rowSums(tmp[,v], na.rm = TRUE),
                        n2i = rowSums(tmp[,unv], na.rm = TRUE),
                        ai  = tmp %>% pull(!!v[1]),
                        bi  = tmp %>% pull(!!v[2]), #reference group
                        ci  = tmp %>% pull(!!unv[1]),
                        di  = tmp %>% pull(!!unv[2]),
                        measure = outp)
  
  escalc_out <- metafor::escalc(n1i = rowSums(tmp[,v], na.rm = TRUE),
                                n2i = rowSums(tmp[,unv], na.rm = TRUE),
                                ai  = tmp %>% pull(!!v[1]),
                                bi  = tmp %>% pull(!!v[2]), #reference group
                                ci  = tmp %>% pull(!!unv[1]),
                                di  = tmp %>% pull(!!unv[2]),
                                measure = outp,
                                data = tmp)
  
  metan
  forest(metan, atransf = "exp",
         slab = paste(tmp$first_author_surname, tmp$year_of_article))
  
  return(list(metan = metan,  ref_grp = v[2], escalc_out = escalc_out))
}
