



fun_ch.Gene_by.Ancestry_SPLINE_2.0 = function(data, DEPENDENT = 'ch.DNMT3A',
                                              mat_confounding, condition_term,
                                              v_confounders_glm){
  
  
  ####################################################################
  ####### ch.GENE (DEPENDENT) ~ Ancestry : Reference on EUR ####################
  
  ## Creating dummy df with tags by ancestry:
  colnames(mat_confounding) = c(condition_term, 'order_ancestry', 'tags_ancestry')
  # mat_confounding = as.data.frame(rbind(
  #   mat_confounding %>% filter(get(condition_term)==ref_label),
  #   mat_confounding %>% filter(get(condition_term)!=ref_label)
  # ))
  mat_confounding = mat_confounding %>%
    mutate(order_ancestry = paste0('__', seq(1, nrow(mat_confounding), by=1),
                                   '_', as.character(mat_confounding[,1])))
  
  
  ## Selecting data:
  
  # DEPENDENT = 'ch.DNMT3A'
  # print(paste('##@@@@--', DEPENDENT, '--@@@@##', sep=''))
  
  data_1 = data  %>% 
    # filter(get(DEPENDENT) == 1 | functional_ch == 1 ) %>%
    filter(get(DEPENDENT) == 1 | functional_ch == 1 | functional_ch == 0) %>%
    select(all_of(DEPENDENT), all_of(condition_term), all_of(v_confounders_glm)) %>%
    left_join(mat_confounding)
  
  ## Extracting number of patients by ancestry (CONDITION) in DEPENDENT:
  
  v_ancestry_label = as.character(mat_confounding[,1])
  names(v_ancestry_label) = as.character(mat_confounding[,3])
  
  df_numbers_by.ancesty = as.data.frame(do.call(rbind, lapply(1:length(v_ancestry_label), function(i){
    CONDITION <-  v_ancestry_label[i]
    # print(paste('##--', CONDITION, '--##', sep=''))
    
    data2 = data_1 %>%
      mutate(new=ifelse(get(condition_term)==CONDITION, 1, 0))
    colnames(data2)[ncol(data2)] = CONDITION
    
    m <- data2 %>% select(all_of(DEPENDENT), all_of(CONDITION)) %>% table
    m2 <- c(CONDITION, DEPENDENT,
            sum(m),
            sum(m[,2]),
            sum(m[2,]),
            m[2,2],
            sum(m[2,])/sum(m),
            m[2,2]/sum(m[,2])) %>%
      as.data.frame %>% t()
    colnames(m2) <- c('Condition', 'Dependent', 'Total', 'n_Condition', 'n_Dependent',
                      'n_Dependent_in_Condition', 'f_Dependent_in_Total', 'f_Dependent_in_Condition')
    # m2 
    
    Glm.test = glm(get(DEPENDENT) ~ get(CONDITION) + ns(age,3) + Sex + smoking_bin + binary_therapy + Major.Cancer.Type,
                   data=data2, family = binomial(link=logit))
    b_multi <- broom::tidy(Glm.test, exponentiate = T, conf.int=T) %>% as.data.frame %>%
      select(term, estimate, p.value, conf.low, conf.high) 
    
    b_multi_2 = b_multi %>%
      select(term, estimate, p.value, conf.low, conf.high) %>%
      filter(grepl('CONDITION', term)) %>%
      mutate(term=CONDITION) %>%
      mutate(order_ancestry = paste0('__', i, '_', term)) %>%
      left_join(mat_confounding) %>%
      select(-c(term, order_ancestry, all_of(condition_term)))
    colnames(b_multi_2) = c('glm.odds.ratio', 'glm.p.value', 'glm.conf.low', 'glm.conf.high', 'Condition_tags')
    
    cbind(m2, b_multi_2)
    
  })))
  
  
  b_multi_3 = df_numbers_by.ancesty
  
  # ## Export:
  # 
  # NAME <- paste(out.dir_data.folder, DEPENDENT, '_by.Ancestries_assocs_', Sys.Date(), '.tsv', sep='')
  # write.table(b_multi_3, NAME, col.names=T, row.names=F, quote=F, sep='\t')
  # 
  # NAME 
  
  b_multi_3
}
