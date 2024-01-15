

fun_table_to_export = function(ANS){
  
  dt = ANS[['t_freq_ch_confounders']]
  head(dt)
  mat_uni = ANS[['t_uni_test']]
  head(mat_uni)
  b_multi2 = ANS[['t_multi_test']]
  head(b_multi2)
  
  dt2 = dt %>%
    select(name_variable, sub, N.all, percent.all, N.CH, percent.CH, Levels, var_aggregate, tags)
  colnames(dt2) = c('Variable', 'sub', 'N.all', '(%).all', 'N.ch', '(%).ch', 'Levels', 'var_aggregate', 'tags')
    
  dt2 = dt2 %>%
    left_join(mat_uni %>%
                select(estimate, conf.low, conf.high, p.value, sub_tags, var_aggregate) %>%
                rename(Levels = sub_tags), by='var_aggregate') %>%
    left_join(b_multi2 %>%
                select(estimate, conf.low, conf.high, p.value, sub_tags, var_aggregate)%>%
                rename(Levels = sub_tags), by='var_aggregate') %>% unique %>%
    select(-Levels.y, -Levels) %>%
    rename(Levels=Levels.x)
    
  
  dt3 = dt2 %>%
    mutate(Odds.ratio.x= round(estimate.x,2)) %>%
    mutate(Levels = ifelse( Levels=='NA-NA', NA, Levels)) %>%
    mutate(Odds.ratio.x = ifelse(is.na(estimate.x) & !is.na(Levels), '(ref.)', 
                                 ifelse( is.na(estimate.x) & is.na(Levels), '-', Odds.ratio.x))) %>%
    mutate(Conf.int.x=ifelse(!is.na(estimate.x), paste0('[', round(conf.low.x,2), '-', round(conf.high.x,2), ']'), '')) %>%
    mutate(Odds.ratio.y= round(estimate.y,2)) %>%
    mutate(Odds.ratio.y = ifelse(is.na(estimate.x) & !is.na(Levels), '(ref.)', 
                                 ifelse( is.na(estimate.x) & is.na(Levels), '-', Odds.ratio.y))) %>%
    mutate(Conf.int.y= ifelse(!is.na(estimate.y), paste0('[', round(conf.low.y,2), '-', round(conf.high.y,2), ']'), '')) %>%
    mutate(signif.x=fifelse(round(p.value.x, 3)<=0.001, '***',
                            fifelse(round(p.value.x, 3)<=0.01, '**', 
                                    fifelse(round(p.value.x, 3)<=0.05, '*','ns')))) %>%
    mutate(signif.x=ifelse(is.na(signif.x), '', signif.x)) %>%
    mutate(P.value.x = ifelse(is.na(p.value.x), '', round(p.value.x,3) )) %>%
    mutate(P.value.x = ifelse(P.value.x==0, '<0.001', P.value.x)) %>%
    mutate(signif.y=fifelse(round(p.value.y, 3)<=0.001, '***',
                            fifelse(round(p.value.y, 3)<=0.01, '**', 
                                    fifelse(round(p.value.y, 3)<=0.05, '*','ns')))) %>%
    mutate(signif.y=ifelse(is.na(signif.y), '', signif.y)) %>%
    mutate(P.value.y = ifelse(is.na(p.value.y), '', round(p.value.y,3) )) %>%
    mutate(P.value.y = ifelse(P.value.y==0, '<0.001', P.value.y)) %>%
    select('tags', 'Levels', 'N.all', '(%).all', 'N.ch', '(%).ch',
           'Odds.ratio.x', 'Conf.int.x', 'P.value.x', 'signif.x',
           'Odds.ratio.y', 'Conf.int.y', 'P.value.y', 'signif.y')
  
  dt4=dt3
  colnames(dt4) = c('', mm[-1], rep('', 8))
  # header_1 = c('Variable', 'Levels', 'N', '(%)', 'N', '(%)',
  # 'OR', 'Conf.Interval', 'pVal', 'OR', 'Conf.Interval', 'pVal')
  
  table = dt4 %>%
    kbl(caption = "") %>%
    kable_styling(full_width = T, html_font = "Times New Roman") %>%
    column_spec(1, bold = T, border_left = T, border_right = T) %>%
    column_spec(2, italic = T, border_right = T) %>%
    column_spec(4, border_right = T) %>%
    column_spec(6, border_right = T) %>%
    column_spec(14, border_right = T) %>%
    row_spec(4, hline_after = T) %>%
    collapse_rows(columns = 1, valign = "middle", longtable_clean_cut=T) %>%
    # add_header_above(c('Variable', 'Levels', 'N', '(%)', 'N', '(%)',
    # 'OR'=2, 'pVal'=2, 'OR'=2, 'pVal'=2)) %>%
    add_header_above(c(' '=2, 'N', '(%)', 'N', '(%)',
                       'OR'=2, 'pVal'=2, 'OR'=2, 'pVal'=2)) %>%
    add_header_above(c('', '', 'Not CH'=2, 'CH'=2,
                       'Univariable test'=4, 'Multivariable test'=4))
  
  table
  
  

}
