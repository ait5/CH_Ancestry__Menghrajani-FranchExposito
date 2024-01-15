

fun_CH.rates.by.CONFOUNDER = function(data, conf_var_term, x_term='functional_ch', x_term_tag='CH',
                                 rate_color='red4', rest_color='orange',
                                 p_y_lim=c(0,100), decreasing=TRUE){
  
  general_fraction = 100 * (data %>% filter(get(x_term)==1) %>% nrow() / nrow(data))
  
  if (conf_var_term == 'age_bin'){
    
    seq_var_bins =  seq(0, 90, by=10)
    seq_var_tags =  paste0( seq_var_bins, '-', seq_var_bins+10)
    mat_seq_var_bins = as.data.frame(cbind(
      as.character(seq_var_bins),
      as.character(seq_var_tags),
      seq(from=20,length.out=length(seq_var_bins), by=1)))
    colnames(mat_seq_var_bins) = c('conf_var', 'var_tags', 'order_bins')
    
  } else if (conf_var_term == 'Sex'){
    
    seq_var_bins =  c('Male', 'Female')
    seq_var_tags =  c('Male', 'Female')
    mat_seq_var_bins = as.data.frame(cbind(
      as.character(seq_var_bins),
      as.character(seq_var_tags),
      seq(from=20,length.out=length(seq_var_bins), by=1)))
    colnames(mat_seq_var_bins) = c('conf_var', 'var_tags', 'order_bins')
    
  } else if (conf_var_term == 'ancestry_label'){
    
    seq_var_bins =  c('nonASJ-EUR', 'AFR', 'ASJ-EUR', 'EAS', 'NAM', 'SAS', 'ADMIX_OTHER')
    seq_var_tags =  c('EUR', 'AFR', 'ASJ', 'EAS', 'NAM', 'SAS', 'ADM')
    mat_seq_var_bins = as.data.frame(cbind(
      as.character(seq_var_bins),
      as.character(seq_var_tags),
      seq(from=20,length.out=length(seq_var_bins), by=1)))
    colnames(mat_seq_var_bins) = c('conf_var', 'var_tags', 'order_bins')
    
  } else if (conf_var_term == 'smoking_bin'){
    
    seq_var_bins =  c(0, 1)
    seq_var_tags =  c('Non smoker', 'Smoker')
    mat_seq_var_bins = as.data.frame(cbind(
      as.character(seq_var_bins),
      as.character(seq_var_tags),
      seq(from=20,length.out=length(seq_var_bins), by=1)))
    colnames(mat_seq_var_bins) = c('conf_var', 'var_tags', 'order_bins')
    
  } else if (conf_var_term == 'binary_therapy'){
    
    seq_var_bins =  c(0, 1)
    seq_var_tags =  c('Untreated', 'Treated')
    mat_seq_var_bins = as.data.frame(cbind(
      as.character(seq_var_bins),
      as.character(seq_var_tags),
      seq(from=20,length.out=length(seq_var_bins), by=1)))
    colnames(mat_seq_var_bins) = c('conf_var', 'var_tags', 'order_bins')
    
  }
  
  
  data2 = data %>%
    mutate(conf_var = get(conf_var_term)) 
  
    mm0 = data2 %>%
      filter(!is.na(conf_var)) %>%
      filter(get(x_term)==0) %>%
      count(conf_var, .drop=F) %>% as.data.frame %>%
      left_join(data2 %>%
                  filter(get(x_term)==1) %>%
                  count(conf_var, .drop=F) %>% as.data.frame, by='conf_var') %>%
      rename(n.not.CH=n.x) %>%
      rename(n.CH=n.y) %>%
      mutate(n.CH=ifelse(is.na(n.CH), 0, n.CH)) %>%
      mutate(percent.not.CH= round((n.not.CH/(n.not.CH+n.CH))*100, 2) ) %>%
      mutate(percent.CH= round((n.CH/(n.not.CH+n.CH))*100, 2) )
  
  
  mm = rbind(
    mm0 %>% select(conf_var, percent.CH) %>%
      rename(percent=percent.CH) %>%
      mutate(type='1_CH'),
    mm0 %>% select(conf_var, percent.not.CH) %>%
      rename(percent=percent.not.CH) %>%
      mutate(type='2_not.CH')
  ) %>% as.data.frame
  
  # All the cancer.types here?
  all_types = unique(mat_seq_var_bins$conf_var)
  types_not_here = all_types[which(! all_types %in% mm$conf_var)]
  
  if (length(types_not_here)>0){
    new_rows = rbind(
      cbind(types_not_here, rep(0, length(types_not_here)), rep('1_CH', length(types_not_here))),
      cbind(types_not_here, rep(0, length(types_not_here)), rep('2_not.CH', length(types_not_here)))
    )
    colnames(new_rows) = colnames(mm)
    mm = rbind(mm, new_rows) %>%
      arrange(type, conf_var)
  }
  
  mm = mm %>%
    mutate(conf_var=gsub(' Cancer', '', conf_var)) %>%
    left_join(mat_seq_var_bins, by='conf_var') %>%
    filter(!is.na(conf_var))
  mm$percent = as.numeric(as.character(mm$percent))
  
  if (decreasing==TRUE){
    mm$order_bins = as.numeric(as.character(mm$order_bins))
  } else if (decreasing==FALSE){
    mm$order_bins = as.numeric(as.character(mm$order_bins)) * -1
  }

  
  v_colors_fill = c(rest_color, rate_color)
  names(v_colors_fill) = c('2_not.CH', '1_CH')
  
  pp = ggplot(data=mm, aes(x=reorder(var_tags, order_bins),
                           y=-percent,
                           fill=type)) +
    geom_bar(stat="identity", position=position_stack()) + 
    geom_hline(aes(yintercept = general_fraction-100), size = .25, linetype = "dashed") + 
    theme_bw() +
    theme(panel.border = element_blank(),
          axis.line = element_line('gray'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(angle=60, hjust=1)
    ) + ylab('% patients') + xlab('') +
    scale_fill_manual(name='',
                      values = v_colors_fill,
                      labels=c(paste0('not ', x_term_tag), x_term_tag)) +
    scale_y_continuous(position='left', lim=p_y_lim-100,
                       breaks = seq(p_y_lim[1]-100, p_y_lim[2]-100, by=25),
                       labels = seq(p_y_lim[1], p_y_lim[2], by=25))

    pp
}