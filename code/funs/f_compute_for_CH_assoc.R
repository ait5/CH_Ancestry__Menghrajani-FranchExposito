
f_compute_for_CH_assoc = function(data, CONDITION){
  
  # CONDITION = 'germ_event'
  # print(paste0('- ', CONDITION))
  
  ## CH vs Germline event
  data2 = data
  DEPENDENT = 'functional_ch'
  # print(paste0('-- ', DEPENDENT))

  m <- data2 %>% select(all_of(DEPENDENT), all_of(CONDITION)) %>% table
  Glm.test <- glm(as.factor(get(DEPENDENT)) ~ get(CONDITION)+age+Sex+smoking_bin+binary_therapy ,
                  data = data2, family = "binomial")
  b <- broom::tidy(Glm.test, exponentiate = T, conf.int=T)[2,c('estimate', 'p.value', 'conf.low', 'conf.high')] %>%
    as.data.frame()
  colnames(b) <- c('glm.odds-ratio', 'glm.p.value', 'glm.conf.low', 'glm.conf.high')
  m2 <- c(CONDITION, DEPENDENT,
          sum(m),
          sum(m[,2]),
          sum(m[2,]),
          m[2,2],
          sum(m[2,])/sum(m),
          m[2,2]/sum(m[,2])) %>%
    as.data.frame %>% t()
  colnames(m2) <- c('Condition', 'Dependent', 'Total', 'n_Condition', 'n_Dependent', 'n_Dependent_in_Condition', 'f_Dependent_in_Total', 'f_Dependent_in_Condition')
  
  ans_ch <- as.data.frame( cbind(m2,b) )
  
  
  ## >=2 CH vs Germline event
  data2 = data %>% 
    filter(more_than_one_ch_muts_functional == 1 | functional_ch == 0) # filtering out those patients with only one CH mutation
  DEPENDENT = 'more_than_one_ch_muts_functional'
  # print(paste0('-- ', DEPENDENT))
  
  m <- data2 %>% select(all_of(DEPENDENT), all_of(CONDITION)) %>% table
  Glm.test <- glm(as.factor(get(DEPENDENT)) ~ get(CONDITION)+age+Sex+smoking_bin+binary_therapy ,
                  data = data2, family = "binomial")
  b <- broom::tidy(Glm.test, exponentiate = T, conf.int=T)[2,c('estimate', 'p.value', 'conf.low', 'conf.high')] %>%
    as.data.frame()
  colnames(b) <- c('glm.odds-ratio', 'glm.p.value', 'glm.conf.low', 'glm.conf.high')
  m2 <- c(CONDITION, DEPENDENT,
          sum(m),
          sum(m[,2]),
          sum(m[2,]),
          m[2,2],
          sum(m[2,])/sum(m),
          m[2,2]/sum(m[,2])) %>%
    as.data.frame %>% t()
  colnames(m2) <- c('Condition', 'Dependent', 'Total', 'n_Condition', 'n_Dependent', 'n_Dependent_in_Condition', 'f_Dependent_in_Total', 'f_Dependent_in_Condition')
  
  ans_2ch <- as.data.frame( cbind(m2,b) )
  
  
  ## CH-PD vs Germline event
  data2 = data %>%
    filter(functional_ch_pd == 1 | functional_ch == 0) # 
  DEPENDENT = 'functional_ch_pd'
  # print(paste0('-- ', DEPENDENT))
  
  m <- data2 %>% select(all_of(DEPENDENT), all_of(CONDITION)) %>% table
  Glm.test <- glm(as.factor(get(DEPENDENT)) ~ get(CONDITION)+age+Sex+smoking_bin+binary_therapy ,
                  data = data2, family = "binomial")
  b <- broom::tidy(Glm.test, exponentiate = T, conf.int=T)[2,c('estimate', 'p.value', 'conf.low', 'conf.high')] %>%
    as.data.frame()
  colnames(b) <- c('glm.odds-ratio', 'glm.p.value', 'glm.conf.low', 'glm.conf.high')
  m2 <- c(CONDITION, DEPENDENT,
          sum(m),
          sum(m[,2]),
          sum(m[2,]),
          m[2,2],
          sum(m[2,])/sum(m),
          m[2,2]/sum(m[,2])) %>%
    as.data.frame %>% t()
  colnames(m2) <- c('Condition', 'Dependent', 'Total', 'n_Condition', 'n_Dependent', 'n_Dependent_in_Condition', 'f_Dependent_in_Total', 'f_Dependent_in_Condition')
  
  ans_ch_pd <- as.data.frame( cbind(m2,b) )
  
  
  ## CH-high-VAF vs Germline event
  data2 = data %>%
    filter(HighVAF_CH == 1 | functional_ch == 0) # 
  DEPENDENT = 'HighVAF_CH'
  # print(paste0('-- ', DEPENDENT))
  
  m <- data2 %>% select(all_of(DEPENDENT), all_of(CONDITION)) %>% table
  Glm.test <- glm(as.factor(get(DEPENDENT)) ~ get(CONDITION)+age+Sex+smoking_bin+binary_therapy ,
                  data = data2, family = "binomial")
  b <- broom::tidy(Glm.test, exponentiate = T, conf.int=T)[2,c('estimate', 'p.value', 'conf.low', 'conf.high')] %>%
    as.data.frame()
  colnames(b) <- c('glm.odds-ratio', 'glm.p.value', 'glm.conf.low', 'glm.conf.high')
  m2 <- c(CONDITION, DEPENDENT,
          sum(m),
          sum(m[,2]),
          sum(m[2,]),
          m[2,2],
          sum(m[2,])/sum(m),
          m[2,2]/sum(m[,2])) %>%
    as.data.frame %>% t()
  colnames(m2) <- c('Condition', 'Dependent', 'Total', 'n_Condition', 'n_Dependent', 'n_Dependent_in_Condition', 'f_Dependent_in_Total', 'f_Dependent_in_Condition')
  
  ans_ch.highVaf <- as.data.frame( cbind(m2,b) )
  
  
  
  
  ANS = as.data.frame(rbind(ans_ch, ans_2ch, ans_ch_pd, ans_ch.highVaf)) %>%
    rename('glm.odds.ratio' = 'glm.odds-ratio')
  
  MAIN <- ANS %>% mutate(glm.signif=fifelse(round(glm.p.value, 2)<=0.001, '***',
                                            fifelse(round(glm.p.value, 2)<=0.01, '**', 
                                                    fifelse(round(glm.p.value, 2)<=0.05, '*', 
                                                            fifelse(round(glm.p.value, 2)<0.1, 't', ''))))) 
  
  
  ##### Adjusting Pvalue for Multiple Testing (between all genes by Dependent):
  dd = MAIN
  dd <- as.data.frame(do.call(rbind, lapply(
    as.character(unique(dd$Dependent)), function(dependent_term){
      tt = dd %>% filter(Dependent == dependent_term)
      tt$Adj.glm.p.value <- p.adjust(tt$glm.p.value, method="fdr")
      tt = tt %>% 
        mutate(Adj.glm.signif = fifelse(
          round(Adj.glm.p.value,2)<=0.001, '***',
          fifelse(round(Adj.glm.p.value, 2)<=0.01, '**',
                  fifelse(round(Adj.glm.p.value, 2)<=0.05, '*',
                          fifelse(round(Adj.glm.p.value, 2)<0.1, 't', '')))))
      tt
    })))
  dd
}