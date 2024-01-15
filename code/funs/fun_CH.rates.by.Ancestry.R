


fun_CH.rates.by.Ancestry = function(data, x_term, x_term_tag, y_term, mat_seq_var_bins,
                                      rate_color='red4', rest_color='orange',
                                      p_y_lim=c(0,100), decreasing=TRUE){
  

    

  colnames(mat_seq_var_bins) = c('conf_var', 'var_tags', 'order_bins')
  
  vline_value = (nrow(data %>% filter(get(x_term)==1))/nrow(data) ) * 100
  
  data2 = data %>%
    mutate(conf_var = get(y_term)) 
  
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
    mm0 %>% select(conf_var, n=n.CH, percent=percent.CH) %>%
      mutate(type='1_CH'),
    mm0 %>% select(conf_var, n=n.not.CH, percent=percent.not.CH) %>%
      mutate(type='2_not.CH')
  ) %>% as.data.frame
  
  ## All ancestry tags here?
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
    geom_hline(aes(yintercept = vline_value-100), size = .25, linetype = "dashed") + 
    theme_bw() +
    theme(panel.border = element_blank(),
          axis.line = element_line('gray'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(angle=0, hjust=0.5, size=SIZE_TEXT*3)
    ) + ylab('Patients with solid tumors (%)') + xlab('Inferred ancestry') +
    scale_fill_manual(name='',
                      values = v_colors_fill,
                      labels=c( x_term_tag, paste0('not ', x_term_tag) ) ) +
    scale_y_continuous(position='left', lim=p_y_lim-100,
                       breaks = seq(p_y_lim[1]-100, p_y_lim[2]-100, by=25),
                       labels = seq(p_y_lim[1], p_y_lim[2], by=25))
  
  return(list(pp, mm))
}