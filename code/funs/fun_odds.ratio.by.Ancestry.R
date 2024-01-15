

fun_odds.ratio.by.Ancestry = function(data, DEPENDENT, 
                                      size_text=2, color_p='gray',
                                      odds_limits=c(-0.3, 0.3), by_scale=0.1){
  
  ## Creating dummy df with tags by ancestry:
  # ancestry_label = c('nonASJ-EUR', 'AFR', 'ASJ-EUR', 'EAS', 'NAM', 'SAS', 'ADMIX_OTHER')
  # tags_ancestry = c('EUR', 'AFR', 'ASJ', 'EAS', 'NAM', 'SAS', 'ADM')
  # mat_confounding = as.data.frame( cbind(ancestry_label, tags_ancestry) )
  mat_confounding = mat_confounding %>% select(term_ancestry, tag_ancestry)
  colnames(mat_confounding) = c('ancestry_label', 'tags_ancestry')
  
  ## Arranging data for plot:
  dd = data %>%
    mutate(ancestry_label = Condition) %>%
    left_join(mat_confounding) %>%
    mutate(Total = as.numeric(Total)) %>%
    mutate(n_Condition = as.numeric(n_Condition)) %>%
    mutate(n_Dependent = as.numeric(n_Dependent)) %>%
    mutate(n_Dependent_in_Condition = as.numeric(n_Dependent_in_Condition)) %>%
    mutate(f_Dependent_in_Total = as.numeric(f_Dependent_in_Total)) %>%
    mutate(f_Dependent_in_Condition = as.numeric(f_Dependent_in_Condition)) %>%
    mutate(glm.odds.ratio = as.numeric(glm.odds.ratio)) %>%
    mutate(glm.p.value = as.numeric(glm.p.value)) %>%
    mutate(glm.conf.low = as.numeric(glm.conf.low)) %>%
    mutate(glm.conf.high = as.numeric(glm.conf.high)) %>%
    select(tags_ancestry, Dependent, n_Condition, n_Dependent_in_Condition,
           glm.odds.ratio, glm.conf.low, glm.conf.high,
           glm.p.value) %>%
    mutate(glm.signif=fifelse(glm.p.value<=0.001, '***',
                              fifelse(glm.p.value<=0.001, '***', 
                                      fifelse(glm.p.value<=0.01, '**', 
                                              fifelse(glm.p.value<0.05, '*', ''))))) %>%
    mutate(signif2=paste('p=',round(glm.p.value,3), sep='')) %>%
    mutate(color_signif2 = fifelse(glm.signif=='*' | glm.signif=='**' | glm.signif=='***', 'yes', 'no')) %>%
    mutate(text_dependent = paste0(n_Dependent_in_Condition, '/', n_Condition)) %>%
    mutate(values.plot=log(glm.odds.ratio)) %>%
    mutate(values.low=log(glm.conf.low)) %>%
    mutate(values.high=log(glm.conf.high)) %>%
    mutate(adj_p.value = p.adjust(glm.p.value, method="fdr")) %>%
    mutate(adj_signif2=paste('Qval=',round(adj_p.value,3), sep='')) %>%
    arrange(desc(n_Condition))
  
  dd$order <- 1:nrow(dd)
  
  
  min_y = odds_limits[1]
  max_y = odds_limits[2]
  by_scale = by_scale
  
  if (max(dd$values.high)>max_y){
    dd[which(dd$values.high > max_y), 'values.high'] = max_y
  }
  if (min(dd$values.low)<min_y | is.na(min(dd$values.low))){
    dd[which(dd$values.low < min_y), 'values.low'] = min_y
  }
  if (max(dd$values.plot)>max_y){
    dd[which(dd$values.plot > max_y), 'values.plot'] = max_y
  }
  if (min(dd$values.plot)<min_y){
    dd[which(dd$values.plot < min_y), 'values.plot'] = min_y
  }
  
  colors_to_signif <- c('black', 'gray20')
  names(colors_to_signif) <- c('yes', 'no')
  
  ## Odds.ratio plot:
  p_odds <- ggplot(dd, aes(y = reorder(tags_ancestry, -order), x = values.plot) ) +
    geom_point(color=color_p, size = size_text/1 ) +
    geom_errorbarh(aes(xmax = values.high, xmin = values.low),
                   color=color_p , size=size_text/3, height=0) +
    geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") + 
    labs(x = "log(OR)", y = "") +
    theme_bw() +
    theme(
      # legend.position = c(1.15, 0.6),
      plot.title = element_text(hjust=0.5, size=size_text*3),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size=size_text*2),
      axis.title.x = element_text(hjust=0.5, size=size_text*3),
      plot.margin=unit(c(1, 0.5, 1, 0.5), 'cm'),
      axis.text.y=element_text(size=size_text*3, color = 'gray20', face = "bold"),
      axis.ticks.y=element_blank()) +
    scale_x_continuous(breaks=seq(min_y,max_y, by=by_scale), labels=round(seq(min_y,max_y, by=by_scale),2)) +
    # scale_y_discrete(labels=rev(labels_Dependents)) +
    coord_cartesian(xlim=c(min_y, max_y), clip = "off") +
    scale_color_manual(values=colors_to_signif, name='') +
    annotate(geom = 'segment',
             x= min_y, xend = max_y, y = -Inf, yend = -Inf) +
    geom_text(dd,
              mapping=aes(x=min_y-by_scale, y=tags_ancestry,
                          label= as.character(text_dependent)),
              size=size_text,
              position = position_nudge(y = -0.20)) +
    geom_text(dd, show.legend = FALSE,
              mapping=aes(x=values.plot, y=tags_ancestry ,label=glm.signif, color=color_signif2),
              size=size_text*2,
              position = position_nudge(x=+0., y = +0.15)) +
    geom_text(dd, show.legend = FALSE,
              mapping=aes(x=values.plot, y=tags_ancestry ,label=signif2, color='gray25'),
              size=size_text*0.75,
              position = position_nudge(x=+0.45, y = +0.15)) +
    geom_text(dd, show.legend = FALSE,
              mapping=aes(x=values.plot, y=tags_ancestry ,label=adj_signif2, color='gray25'),
              size=size_text*0.75,
              position = position_nudge(x=+0.15, y = -0.15))
  
  list(p_odds, dd)
  
  
  
}