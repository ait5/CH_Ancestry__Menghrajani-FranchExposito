

fun_ch.Gene_by.Ancestry = function(df_ano, DEPENDENT = 'ch.DNMT3A', v_confounders_glm){
  
  
  ####################################################################
  ####### ch.GENE (DEPENDENT) ~ Ancestry : Reference on EUR ####################
  
  ## Creating dummy df with tags by ancestry:
  
  sub_ancestry = c('nonASJ-EUR', 'AFR', 'ASJ-EUR', 'EAS', 'NAM', 'SAS', 'ADMIX_OTHER')
  order_sub_ancestry = paste0('__', seq(1, length(sub_ancestry), by=1), '_', sub_ancestry)
  tags_sub_ancestry = c('EUR', 'AFR', 'ASJ', 'EAS', 'NAM', 'SAS', 'ADM')
  
  mat_confounding = as.data.frame(
    cbind(sub_ancestry, order_sub_ancestry, tags_sub_ancestry)
  )
  colnames(mat_confounding) = c('ancestry_label', 'order_ancestry', 'tags_ancestry')
  
  
  ## Selecting data:
  
  # DEPENDENT = 'ch.DNMT3A'
  # print(paste('##@@@@--', DEPENDENT, '--@@@@##', sep=''))

  data = df_ano  %>% 
    filter(get(DEPENDENT) == 1 | functional_ch == 1 ) %>%
    select(all_of(DEPENDENT), ancestry_label, all_of(v_confounders_glm)) %>%
    left_join(mat_confounding, by='ancestry_label')
  
  ## Extracting number of patients by ancestry (CONDITION) in DEPENDENT:
  
  v_ancestry_label = sub_ancestry
  names(v_ancestry_label) = tags_sub_ancestry
  
  df_numbers_by.ancesty = as.data.frame(do.call(rbind, lapply(1:length(v_ancestry_label), function(i){
    CONDITION <-  v_ancestry_label[i]
    # print(paste('##--', CONDITION, '--##', sep=''))
    
    data2 = data %>%
      mutate(new=ifelse(ancestry_label==CONDITION, 1, 0))
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
    m2 
  }))) %>% mutate(Condition_tags = names(v_ancestry_label))
  
  
  ## Computing association odds-ratio values: Multivariable test
  data2 = data %>% select(all_of(DEPENDENT), order_ancestry, all_of(v_confounders_glm))
  
  # Glm.test = glm(get(DEPENDENT) ~ order_ancestry + age + Sex + smoking_bin + binary_therapy,
  #                data=data)
  # Glm.test = glm(data2) # use all variables (special identifier that one can use in a formula to mean all the variables, it is the . identifier)
  Glm.test = glm(data2, family = binomial(link=logit))
  b_multi <- broom::tidy(Glm.test, exponentiate = T, conf.int=T) %>% as.data.frame %>%
    select(term, estimate, p.value, conf.low, conf.high) 
  
  b_multi_2 = b_multi %>%
    select(term, estimate, p.value, conf.low, conf.high) %>%
    filter(grepl('order_ancestry__', term)) %>%
    mutate(term=gsub('order_ancestry', '', term)) %>%
    mutate(order_ancestry = term) %>%
    left_join(mat_confounding) %>%
    select(-c(term, order_ancestry, ancestry_label))
  b_multi_2 = as.data.frame(rbind(
    c(1,1,1,1, 'EUR'),
    b_multi_2
  ))
  colnames(b_multi_2) = c('glm.odds.ratio', 'glm.p.value', 'glm.conf.low', 'glm.conf.high', 'Condition_tags')
  
  
  ## Preparing results to export:
  
  b_multi_3 = df_numbers_by.ancesty %>%
    filter(Dependent == DEPENDENT) %>%
    filter(Condition_tags %in% tags_sub_ancestry) %>%
    select(Condition, Dependent, Total, n_Condition, n_Dependent, n_Dependent_in_Condition, f_Dependent_in_Total, f_Dependent_in_Condition, Condition_tags) %>%
    left_join(b_multi_2, by='Condition_tags')
  
  # ## Export:
  # 
  # NAME <- paste(out.dir_data.folder, DEPENDENT, '_by.Ancestries_assocs_', Sys.Date(), '.tsv', sep='')
  # write.table(b_multi_3, NAME, col.names=T, row.names=F, quote=F, sep='\t')
  # 
  # NAME 
  
  b_multi_3
}