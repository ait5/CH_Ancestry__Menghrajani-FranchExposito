

fun_CH.rates.by.Cancer.Types = function(data, x_term='functional_ch', x_term_tag='CH',
                                 rate_color='red4', rest_color='orange',
                                 p_y_lim=c(0,100)){
  
  general_fraction = 100 * (data %>% filter(get(x_term)==1) %>% nrow() / nrow(data))
    
    mm0 = data %>%
      filter(get(x_term)==0) %>%
      count(Major.Cancer.Type, .drop=F) %>% as.data.frame %>%
      left_join(data %>%
                  filter(get(x_term)==1) %>%
                  count(Major.Cancer.Type, .drop=F) %>% as.data.frame, by='Major.Cancer.Type') %>%
      rename(n.not.CH=n.x) %>%
      rename(n.CH=n.y) %>%
      mutate(n.CH=ifelse(is.na(n.CH), 0, n.CH)) %>%
      mutate(percent.not.CH= round((n.not.CH/(n.not.CH+n.CH))*100, 2) ) %>%
      mutate(percent.CH= round((n.CH/(n.not.CH+n.CH))*100, 2) )
 
  
  mm = rbind(
    mm0 %>% select(Major.Cancer.Type, percent.CH) %>%
      rename(percent=percent.CH) %>%
      mutate(type='1_CH'),
    mm0 %>% select(Major.Cancer.Type, percent.not.CH) %>%
      rename(percent=percent.not.CH) %>%
      mutate(type='2_not.CH')
  ) %>% as.data.frame
  
  # All the cancer.types here?
  all_types = c('Non Small Cell Lung Cancer', 'Glioma', 'Colorectal Cancer',
                'Hepatobiliary Cancer', 'Melanoma', 'Breast Cancer', 'Endometrial Cancer',
                'Other cancer type', 'Soft Tissue Sarcoma', 'Head and Neck Cancer',
                'Bladder Cancer', 'Cancer of Unknown Primary', 'Gastrointestinal Stromal Tumor',
                'Pancreatic Cancer', 'Esophagogastric Cancer', 'Renal Cell Carcinoma',
                'Thyroid Cancer', 'Ovarian Cancer', 'Germ Cell Tumor', 'Prostate Cancer', 'Bone Cancer')
  types_not_here = all_types[which(! all_types %in% mm$Major.Cancer.Type)]

  
  if (length(types_not_here)>0){
    new_rows = rbind(
      cbind(types_not_here, rep(0, length(types_not_here)), rep('1_CH', length(types_not_here))),
      cbind(types_not_here, rep(0, length(types_not_here)), rep('2_not.CH', length(types_not_here)))
    )
    colnames(new_rows) = colnames(mm)
    mm = rbind(mm, new_rows) %>%
      arrange(type, Major.Cancer.Type)
  }
  
  mm = mm %>%
    mutate(Major.Cancer.Type=gsub(' Cancer', '', Major.Cancer.Type))
  mm$percent = as.numeric(as.character(mm$percent))
  

  
  v_colors_fill = c(rest_color, rate_color)
  names(v_colors_fill) = c('2_not.CH', '1_CH')
  
  pp = ggplot(data=mm, aes(x=Major.Cancer.Type,
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