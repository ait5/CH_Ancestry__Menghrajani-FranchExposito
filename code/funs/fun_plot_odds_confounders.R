
fun_plot_odds_confounders = function(mat, conf_var_term, limits_p=NULL, decreasing=TRUE){
  
    if(is.null(limits_p)){
      limits_p <- c(-0.4, 0.4)
    } else {limits_p = limits_p}
  
  if (conf_var_term == 'age_bin'){
    
    seq_var_bins =  seq(0, 110, by=10)
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
    seq_var_tags =  c('European (not Ashkenazi Jews)', 'African', 'European (Ashkenazi Jews)', 'East Asian',
                      'Native American', 'South Asian', 'Other')
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
  
  
  
  if (conf_var_term == 'Major.Cancer.Type'){
    
    mat2 = mat %>%
      select(sub_tags.x, estimate, conf.low, conf.high, p.value) %>%
      rename(var_tags = sub_tags.x) %>%
      filter(!is.na(var_tags)) %>%
      mutate(estimate = ifelse(is.na(estimate), log10(1), log10(estimate))) %>%
      mutate(conf.low = ifelse(is.na(conf.low), log10(1), log10(conf.low))) %>%
      mutate(conf.high = ifelse(is.na(conf.high), log10(1), log10(conf.high))) %>%
      mutate(p.value = ifelse(is.na(p.value), 1, p.value)) %>%
      mutate(estimate = ifelse(estimate < limits_p[1], limits_p[1], 
                               ifelse(estimate > limits_p[2], limits_p[2], estimate))) %>%
      mutate(conf.low = ifelse(conf.low < limits_p[1], limits_p[1], 
                               ifelse(conf.low > limits_p[2], limits_p[2], conf.low))) %>%
      mutate(conf.high = ifelse(conf.high < limits_p[1], limits_p[1], 
                                ifelse(conf.high > limits_p[2], limits_p[2], conf.high))) %>%
      mutate(signif.text=ifelse(p.value<=0.001, '***',
                                          ifelse(p.value<=0.01, '**',
                                                  ifelse(p.value<=0.05, '*', ''))))
    
    pp = ggplot(mat2, aes(x = estimate, y = var_tags)) +
      geom_point(size=1, color='gray25') +
      geom_errorbarh(aes(xmax = conf.high, xmin = conf.low),
                     color='gray25', size = 0.5, height=0) +
      geom_vline(aes(xintercept = log10(1)), size = .25, linetype = "dashed") + 
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=10),
        plot.margin=unit(c(0.5,0,0.3,0.5), 'cm'),
        axis.title.y = element_blank(),
        axis.text.y=element_text(size=10, color = 'gray20', face = "bold"),
        axis.ticks.y=element_blank()) +
      scale_x_continuous(position = "bottom", limit=limits_p) +
      annotate(geom = 'segment',
               x= limits_p[1], xend = limits_p[2], y = -Inf, yend = -Inf) +
      geom_text(aes(label=signif.text), size=3.5,
                position = position_nudge(y = +.15))
      
    
  } else {
    
    mat2 = mat %>%
      select(sub_tags.x, estimate, conf.low, conf.high, p.value) %>%
      rename(var_tags = sub_tags.x) %>%
      left_join(mat_seq_var_bins, by='var_tags') %>%
      filter(!is.na(conf_var)) %>%
      mutate(estimate = ifelse(is.na(estimate), log10(1), log10(estimate))) %>%
      mutate(conf.low = ifelse(is.na(conf.low), log10(1), log10(conf.low))) %>%
      mutate(conf.high = ifelse(is.na(conf.high), log10(1), log10(conf.high))) %>%
      mutate(p.value = ifelse(is.na(p.value), 1, p.value)) %>%
      mutate(estimate = ifelse(estimate < limits_p[1], limits_p[1], 
                               ifelse(estimate > limits_p[2], limits_p[2], estimate))) %>%
      mutate(conf.low = ifelse(conf.low < limits_p[1], limits_p[1], 
                               ifelse(conf.low > limits_p[2], limits_p[2], conf.low))) %>%
      mutate(conf.high = ifelse(conf.high < limits_p[1], limits_p[1], 
                                ifelse(conf.high > limits_p[2], limits_p[2], conf.high))) %>%
      mutate(signif.text=ifelse(p.value<=0.001, '***',
                                ifelse(p.value<=0.01, '**',
                                       ifelse(p.value<=0.05, '*', ''))))
    
    if (decreasing==TRUE){
      mat2$order_bins = as.numeric(as.character(mat2$order_bins))
    } else if (decreasing==FALSE){
      mat2$order_bins = as.numeric(as.character(mat2$order_bins)) * -1
    }
    
    pp = ggplot(mat2, aes(x = estimate, y = reorder(var_tags, order_bins))) +
      geom_point(size=1, color='gray25') +
      geom_errorbarh(aes(xmax = conf.high, xmin = conf.low),
                     color='gray25', size = 0.5, height=0) +
      geom_vline(aes(xintercept = log10(1)), size = .25, linetype = "dashed") + 
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=10),
        plot.margin=unit(c(0.5,0,0.3,0.5), 'cm'),
        axis.title.y = element_blank(),
        axis.text.y=element_text(size=10, color = 'gray20', face = "bold"),
        axis.ticks.y=element_blank()) +
      scale_x_continuous(position = "bottom", limit=limits_p) +
      geom_text(aes(label=signif.text), size=3.5,
                position = position_nudge(y = +.15))
    
  }
  
 pp  +
   xlab('log(OR)')
}