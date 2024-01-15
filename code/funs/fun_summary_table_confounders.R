

fun_summary_table_confounders = function(data_mat){
  
   v_confounders = c('age_bin', 'Sex', 'ancestry_label', 'smoking_bin', 'binary_therapy', 'Major.Cancer.Type')
  names(v_confounders) = c('Age (bins)', 'Sex', 'Inferred ancestry', 'Smoking', 'Treatment', 'Cancer type')
  
  
  ## Creating matrix by confounding and sublevels:
    # age_bin
      sub_age = c(seq(0, 100, by=10), NA)
      order_sub_age = paste0('__', seq(1, length(sub_age), by=1), '_', sub_age)
      order_sub_age[6] = gsub('__6_', '__0_', order_sub_age[6])
      tags_sub_age = paste0(sub_age, '-', sub_age+10)
      name_variable_age = 'age_bin'
      tag_age = names(v_confounders[which(v_confounders==name_variable_age)])
    # Sex
      sub_sex = c('Male', 'Female', NA)
      order_sub_sex = paste0('__', seq(1, length(sub_sex), by=1), '_', sub_sex)
      tags_sub_sex = sub_sex
      name_variable_sex = 'Sex'
      tag_sex = names(v_confounders[which(v_confounders==name_variable_sex)])
    # ancestry_label
      sub_ancestry = c('nonASJ-EUR', 'AFR', 'ASJ-EUR', 'EAS', 'NAM', 'SAS', 'ADMIX_OTHER', NA)
      order_sub_ancestry = paste0('__', seq(1, length(sub_ancestry), by=1), '_', sub_ancestry)
      tags_sub_ancestry = c('European (not Ashkenazi Jews)', 'African', 'European (Ashkenazi Jews)', 'East Asian',
                            'Native American', 'South Asian', 'Other', 'NA')
      name_variable_ancestry = 'ancestry_label'
      tag_ancestry = names(v_confounders[which(v_confounders==name_variable_ancestry)])
    # smoking_bin
      sub_smoking = c('0', '1', NA)
      order_sub_smoking = paste0('__', seq(1, length(sub_smoking), by=1), '_', sub_smoking)
      tags_sub_smoking = c('Non smoker', 'Smoker', 'NA')
      name_variable_smoking = 'smoking_bin'
      tag_smoking = names(v_confounders[which(v_confounders==name_variable_smoking)])
    # binary_therapy
      sub_therapy = c('0', '1', NA)
      order_sub_therapy = paste0('__', seq(1, length(sub_therapy), by=1), '_', sub_therapy)
      tags_sub_therapy = c('Untreated', 'Treated', 'NA')
      name_variable_therapy = 'binary_therapy'
      tag_therapy = names(v_confounders[which(v_confounders==name_variable_therapy)])
    # Major.Cancer.Type
      sub_cancer = c(colnames(
        data_mat)[grep('Non.Small.Cell.Lung.Cancer', colnames(data_mat)):grep('Other.cancer.type', colnames(data_mat))], NA)
      sub_cancer = gsub('\\ ', '\\.', sub_cancer)
      order_sub_cancer = paste0('__', seq(1, length(sub_cancer), by=1), '_', sub_cancer)
      order_sub_cancer[21] = gsub('__21_', '__0_', order_sub_cancer[21])
      tags_sub_cancer = gsub('\\.', '\\ ', sub_cancer)
      name_variable_cancer = 'Major.Cancer.Type'
      tag_cancer = names(v_confounders[which(v_confounders==name_variable_cancer)])
    
    mat_confounding = as.data.frame(rbind(
      cbind(name_variable_age, sub_age, order_sub_age, tags_sub_age, tag_age),
      cbind(name_variable_sex, sub_sex, order_sub_sex, tags_sub_sex, tag_sex),
      cbind(name_variable_ancestry, sub_ancestry, order_sub_ancestry, tags_sub_ancestry, tag_ancestry),
      cbind(name_variable_smoking, sub_smoking, order_sub_smoking, tags_sub_smoking, tag_smoking),
      cbind(name_variable_therapy, sub_therapy, order_sub_therapy, tags_sub_therapy, tag_therapy),
      cbind(name_variable_cancer, sub_cancer, order_sub_cancer, tags_sub_cancer, tag_cancer)
    ))
    colnames(mat_confounding) = c('name_variable', 'sub', 'order', 'sub_tags', 'tags')
    mat_confounding = mat_confounding %>%
      mutate(name_order = paste0(name_variable, order)) %>%
      mutate(var_aggregate = paste0(name_variable, '__', sub))
  
    
  ## CH 
  mm = c('Total',
         '',
         data_mat %>% filter(functional_ch==0) %>% nrow,
         paste0( '(', round( data_mat %>% filter(functional_ch==0) %>% nrow / nrow(data_mat) *100, 2), ')'),
         data_mat %>% filter(functional_ch==1) %>% nrow,
         paste0( '(', round( data_mat %>% filter(functional_ch==1) %>% nrow / nrow(data_mat) *100, 2), ')'))
  
  dt = as.data.frame(do.call(rbind, lapply(1:length(v_confounders), function(i){
    
    CONFOUNDER = as.character(v_confounders[i])
    name_Confounder = names(v_confounders)[i]
    print(CONFOUNDER)
    LEVELS = mat_confounding %>% filter(name_variable == CONFOUNDER) %>%
      select(sub_tags) %>% unique %>% pull %>% as.character
    
    m_confounder = data_mat %>%
      filter(functional_ch==0) %>%
      select(all_of(CONFOUNDER)) %>% table(useNA='always') %>% as.data.frame() %>%
      mutate(Percent=round(Freq/sum(Freq)*100, 2))
    colnames(m_confounder) = c('Terms', 'Freq_NOT_CH', 'Percent_NOT_CH')
    
    m_confounder_CH = data_mat %>%
      filter(functional_ch==1) %>%
      select(all_of(CONFOUNDER)) %>% table(useNA='always') %>% as.data.frame() %>%
      mutate(Percent=round(Freq/sum(Freq)*100, 2))
    colnames(m_confounder_CH) = c('Terms', 'Freq_CH', 'Percent_CH') 
    
    m_confounder = m_confounder %>%
      full_join(m_confounder_CH, by='Terms') %>%
      mutate(Confounder = CONFOUNDER) %>%
      select(Confounder, Terms, Freq_NOT_CH, Percent_NOT_CH, Freq_CH, Percent_CH) %>%
      mutate(Terms = as.character(Terms))
    colnames(m_confounder) = c('name_variable', 'sub', 'N.all', 'percent.all', 'N.CH', 'percent.CH')
    
    if (CONFOUNDER == 'Major.Cancer.Type'){
      m_confounder$sub = gsub('\\ ', '\\.', m_confounder$sub)
    }
    m_confounder2 = mat_confounding %>%
      filter(name_variable==CONFOUNDER) %>%
      left_join(m_confounder %>%
                  mutate(var_aggregate = paste0(name_variable, '__', sub)) %>%
                  select(-name_variable, -sub)) %>%
      mutate(Levels = LEVELS)
    # m_confounder2[1:(nrow(m_confounder2)-1),'Levels'] = LEVELS[1:(nrow(m_confounder2)-1)]
    m_confounder2
  })))
  dt_final = dt
  
  ANS = list(dt_final)
  names(ANS) = 't_freq_ch_confounders'
  
  v_confounders = c('age_bin', 'Sex', 'ancestry_label', 'smoking_bin', 'binary_therapy', 'Major.Cancer.Type')
  
  data = data_mat %>%
    select(functional_ch, all_of(v_confounders))
  
    data$age_bin = as.character(data$age_bin)
    data$smoking_bin = as.character(data$smoking_bin)
    data$binary_therapy = as.character(data$binary_therapy)
    data$Major.Cancer.Type = gsub('\\ ', '\\.', as.character(data$Major.Cancer.Type))
  
  l_b = list()
  for (i in 1:length(v_confounders)){
    CONFOUNDER=v_confounders[i]
    print(CONFOUNDER)
    d_CONFOUNDER = paste0('d_', CONFOUNDER)
    
    dummy_mat = mat_confounding %>%
      filter(name_variable==CONFOUNDER) %>%
      select(sub, order)
    colnames(dummy_mat) = c(CONFOUNDER, d_CONFOUNDER)
    
    data = data  %>%
      left_join(dummy_mat, by=CONFOUNDER)
    
    ## Univariable test:
    Glm.test = glm(functional_ch ~  get(d_CONFOUNDER), data=data)
    b = broom::tidy(Glm.test, exponentiate = T, conf.int=T) %>% as.data.frame %>%
      select(term, estimate, p.value, conf.low, conf.high) %>%
      mutate(name_variable = CONFOUNDER)
    
    l_b[[paste(CONFOUNDER)]] = b[-1,] %>%
      mutate(term=gsub('get\\(d_CONFOUNDER\\)', '', as.character(term))) %>%
      rename(order=term)
  }
  mat_uni = as.data.frame(do.call(rbind, l_b)) %>%
    mutate(name_order=paste0(name_variable, order)) %>%
    left_join(mat_confounding %>% select(-name_variable, -order))
  
  ANS = append(ANS, list(mat_uni))
  names(ANS)[length(ANS)] = 't_uni_test'
  
  ## Multivariable test:
  Glm.test = glm(functional_ch ~ d_age_bin + d_Sex + d_ancestry_label + d_smoking_bin + d_binary_therapy + d_Major.Cancer.Type,
                 data=data)
  b_multi <- broom::tidy(Glm.test, exponentiate = T, conf.int=T) %>% as.data.frame %>%
    select(term, estimate, p.value, conf.low, conf.high) 
  b_multi2 = b_multi[-1,] %>%
    separate(term, into=c('first', 'order'), '__', remove=T) %>%
    mutate(name_variable=gsub('d_', '', first)) %>%
    select(-first) %>%
    mutate(order=paste0('__', order)) %>%
    mutate(name_order=paste0(name_variable, order)) %>%
    left_join(mat_confounding %>% select(-order))
  # colnames(b_multi2)[1:(ncol(b_multi2)-1)] = paste0(colnames(b_multi2), '_multi')
  
  ANS = append(ANS, list(b_multi2))
  names(ANS)[length(ANS)] = 't_multi_test'
  
  ANS
  
}
