

## Data wrangle ------

df_ano_2 = df_ano %>%
  mutate(ancestry_label_2 = ifelse(ancestry_label == 'nonASJ-EUR' | ancestry_label == 'ASJ-EUR', 'EUR', ancestry_label)) 

term_ancestry = c('nonASJ.EUR', 'ASJ.EUR', 'ADMIX_OTHER', 'EUR', 'EAS', 'AFR', 'SAS', 'NAM')
mat_ancestry = as.data.frame(cbind(
  term_ancestry,
  tag_ancestry = c('nonASJ.EUR', 'ASJ', 'ADM', 'EUR', 'EAS', 'AFR', 'SAS', 'NAM'),
  order_ancestry = 1:length(term_ancestry),
  color_ancestry = c(v_colors[1:length(term_ancestry)])
))

mat_ancestry = mat_ancestry %>%
  filter(tag_ancestry %in% c('ADM', 'EUR', 'EAS', 'AFR', 'SAS', 'NAM'))

df_ano_2 = df_ano_2 %>% filter(ancestry_label_2 != "ADMIX_OTHER")
mat_ancestry = mat_ancestry %>%
  filter(tag_ancestry %in% c('EUR', 'EAS', 'AFR', 'SAS', 'NAM'))

print('df_ano_2 ready!')




## CH~Ancestry association forest plot (glm model with spline for age)
source(paste0(dir_funs, 'fun_main.assoc_by.Ancestry_SPLINE.R'))

## CH ~ Ancestry ----
data=df_ano_2
DEPENDENT = 'functional_ch'
DEPENDENT_tag = 'CH'
condition_term = 'ancestry_label_2'
v_confounders_glm = c('age', 'Sex', 'smoking_bin', 'binary_therapy', 'Major.Cancer.Type')

mat_confounding = mat_ancestry %>%
  select(term_ancestry, order_ancestry, tag_ancestry)
ref_label = 'EUR'

data <- fun_main.assoc_by.Ancestry_SPLINE(data, DEPENDENT, condition_term, ref_label,
                                          mat_confounding, v_confounders_glm)

## Export:
NAME <- paste(out_dir, 'SPLINE_main.assoc_by.Ancestry__', Sys.Date(), '.tsv', sep='')
write.table(data, NAME, col.names=T, row.names=F, quote=F, sep='\t')






## ch.GENE ~ Ancestry association -------
source(paste0(dir_funs, 'fun_ch.Gene_by.Ancestry_SPLINE_2.0.R'))

## Computing associations ch.GENE ~ ancestry:
v_dependents <- rownames(df_ano_2 %>%
                           select(all_of(ch.genes.cols)) %>%
                           apply(2, sum) %>% sort(decreasing=T) %>%
                           as.data.frame() %>% slice(1:15)) 

condition_term = 'ancestry_label_2'
data = df_ano_2
mat_confounding = mat_ancestry %>%
  select(term_ancestry, order_ancestry, tag_ancestry)

DF_all__ch.Gene.v.Ancestry = as.data.frame(do.call(rbind, lapply(v_dependents, function(DEPENDENT){
  v_confounders_glm = c('age', 'Sex', 'smoking_bin', 'binary_therapy', 'Major.Cancer.Type') ## FIXED!! DO NOT CHANGE!
  
  dat_by.ch.Gene <- fun_ch.Gene_by.Ancestry_SPLINE_2.0(data, DEPENDENT,
                                                       mat_confounding, condition_term,
                                                       v_confounders_glm)
  dat_by.ch.Gene
})))

## Export:
NAME <- paste(out_dir_by.ch.Gene, 'SPLINE_ch.genes_by.Ancestries__', Sys.Date(), '.tsv', sep='')
write.table(DF_all__ch.Gene.v.Ancestry, NAME, col.names=T, row.names=F, quote=F, sep='\t')




## [among CH] ch.GENE ~ Ancestry association -------
source(paste0(dir_funs, 'fun_ch.Gene_by.Ancestry_SPLINE_2.0.R'))

## Computing associations ch.GENE ~ ancestry:
v_dependents <- rownames(df_ano_2 %>%
                           select(all_of(ch.genes.cols)) %>%
                           apply(2, sum) %>% sort(decreasing=T) %>%
                           as.data.frame() %>% slice(1:15)) 

condition_term = 'ancestry_label_2'
data = df_ano_2 |> filter(functional_ch == 1)
mat_confounding = mat_ancestry %>%
  select(term_ancestry, order_ancestry, tag_ancestry)

onlyCH_DF_all__ch.Gene.v.Ancestry = as.data.frame(do.call(rbind, lapply(v_dependents, function(DEPENDENT){
  v_confounders_glm = c('age', 'Sex', 'smoking_bin', 'binary_therapy', 'Major.Cancer.Type') ## FIXED!! DO NOT CHANGE!
  
  dat_by.ch.Gene <- fun_ch.Gene_by.Ancestry_SPLINE_2.0(data, DEPENDENT,
                                                       mat_confounding, condition_term,
                                                       v_confounders_glm)
  dat_by.ch.Gene
})))

## Export:
NAME <- paste(out_dir_by.ch.Gene, 'onlyCH_SPLINE_ch.genes_by.Ancestries__', Sys.Date(), '.tsv', sep='')
write.table(onlyCH_DF_all__ch.Gene.v.Ancestry, NAME, col.names=T, row.names=F, quote=F, sep='\t')






## ch.GENE ~ Ancestry association EUR_sub -----

source(paste0(dir_funs, 'fun_ch.Gene_by.Ancestry_SPLINE_2.0.R'))

## Computing associations ch.GENE ~ ancestry:
v_dependents <- rownames(df_ano_2 %>%
                           select(all_of(ch.genes.cols)) %>%
                           apply(2, sum) %>% sort(decreasing=T) %>%
                           as.data.frame() %>% slice(1:15)) 

condition_term = 'EUR_sub'

data = df_ano_2 %>%
  mutate(EUR_sub = ifelse(ancestry_label == 'ASJ-EUR', 'ASJ',
                          ifelse(ancestry_label == 'nonASJ-EUR', 'non-ASJ', ancestry_label))) %>%
  filter(EUR_sub == 'ASJ' | EUR_sub == 'non-ASJ')

mat_confounding = mat_seq_var_bins = as.data.frame(cbind(
  term_ancestry = c('ASJ'),
  order_ancestry = c(1),
  tag_ancestry = c('ASJ')))

DF_all__ch.Gene.v.Ancestry = as.data.frame(do.call(rbind, lapply(v_dependents, function(DEPENDENT){
  v_confounders_glm = c('age', 'Sex', 'smoking_bin', 'binary_therapy', 'Major.Cancer.Type') ## FIXED!! DO NOT CHANGE!
  print(DEPENDENT)
  dat_by.ch.Gene <- fun_ch.Gene_by.Ancestry_SPLINE_2.0(data, DEPENDENT,
                                                       mat_confounding, condition_term,
                                                       v_confounders_glm)
  dat_by.ch.Gene
})))


## Export:
NAME <- paste(out_dir_by.ch.Gene, 'SPLINE_ch.genes_by.EUR__', Sys.Date(), '.tsv', sep='')
write.table(DF_all__ch.Gene.v.Ancestry, NAME, col.names=T, row.names=F, quote=F, sep='\t')






## CH~ ASJ vs non-ASJ association ----
source(paste0(dir_funs, 'fun_main.assoc_by.Ancestry_SPLINE.R'))



## Computing associations CH ~ ancestry (SPLINE):
data= df_ano_2 %>%
  mutate(EUR_sub = ifelse(ancestry_label == 'ASJ-EUR', 'ASJ',
                          ifelse(ancestry_label == 'nonASJ-EUR', 'non-ASJ', ancestry_label))) %>%
  filter(EUR_sub == 'ASJ' | EUR_sub == 'non-ASJ')

DEPENDENT = 'functional_ch'
DEPENDENT_tag = 'CH'
condition_term = 'EUR_sub'
v_confounders_glm = c('age', 'Sex', 'smoking_bin', 'binary_therapy', 'Major.Cancer.Type')

mat_confounding = mat_seq_var_bins = as.data.frame(cbind(
  term_ancestry = c('non-ASJ', 'ASJ'),
  order_ancestry = c(1,2),
  tag_ancestry = c('non-ASJ', 'ASJ')
  
))
ref_label = 'non-ASJ'

data <- fun_main.assoc_by.Ancestry_SPLINE(data, DEPENDENT, condition_term, ref_label,
                                          mat_confounding, v_confounders_glm)

## Export:
NAME <- paste(out_dir, 'SPLINE_main.assoc_by.Ancestry_EUR_', Sys.Date(), '.tsv', sep='')
write.table(data, NAME, col.names=T, row.names=F, quote=F, sep='\t')
