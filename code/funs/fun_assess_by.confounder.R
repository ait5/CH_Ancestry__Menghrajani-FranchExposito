
fun_assess_by.confounder = function(df_ano, CONFOUNDER){


  # print(CONFOUNDER)
  
  data = df_ano %>% select(ancestry_label, all_of(CONFOUNDER))
  levels_confounder = sort( data %>% select(all_of(CONFOUNDER)) %>% unique %>%  pull %>% as.character , decreasing=T)
  if (CONFOUNDER=='age_bin'){
    levels_confounder = c('100', '90', '80', '70', '60', '50', '40', '30', '20', '10', '0')
  }
  
  ## Colors:
  v_colors = c(
    wes_palette(name="Cavalcanti1", n=5, type="discrete" ),
    wes_palette(name="Rushmore1", n=5, type="discrete" ),
    wes_palette(name="GrandBudapest1", n=4, type="discrete" ),
    wes_palette(name="Royal1", n=4, type="discrete" ))
  
  
  # random_numbers = sample(seq(from = 1,
  #                             to = length(v_colors), by = 1),
  #                         size = length(levels_confounder), replace = TRUE)
  random_numbers = seq(from = 1,
                              to = length(levels_confounder), by = 1)
  
  p_colors <- v_colors[random_numbers]
  names(p_colors) = levels_confounder
  
  ##### By ancestry #####
  
  ANCESTRY = 'nonASJ-EUR'
  ANCESTRY_tag = 'EUR'
  
  n_table = data %>% select(all_of(CONFOUNDER)) %>% table()
  n_table
  percent_table = n_table / sum(n_table)
  percent_table
  
  n_table_sub = data %>% filter(ancestry_label==ANCESTRY) %>% select(all_of(CONFOUNDER)) %>% table()
  n_table_sub
  percent_table_sub = n_table_sub / sum(n_table_sub)
  percent_table_sub
  
  mat = cbind(
    n_table %>% as.data.frame %>% rename(n = Freq) %>% mutate(x = 'All') %>% mutate(order_x=1) %>% select(-1),
    percent_table %>% as.data.frame %>% rename(Percent = Freq) %>% select(Percent)
  )
  mat_sub =   cbind(
    n_table_sub %>% as.data.frame %>% rename(n = Freq) %>% mutate(x = ANCESTRY_tag) %>% mutate(order_x=2),
    percent_table_sub %>% as.data.frame %>% rename(Percent = Freq) %>% select(Percent)
  )
  
  mat = cbind(mat_sub[,1], mat)
  colnames(mat)[1] = CONFOUNDER
  
  colnames(mat_sub)[1] = CONFOUNDER
  
  mm = rbind(
    mat,
    mat_sub
  )
  
  v_ancestry_label = c('nonASJ-EUR', 'AFR', 'ASJ-EUR', 'EAS', 'NAM', 'SAS', 'ADMIX_OTHER')
  names(v_ancestry_label) = c('EUR', 'AFR', 'ASJ', 'EAS', 'NAM', 'SAS', 'ADM')
  
  
  for (i in 2:length(v_ancestry_label)){
    ANCESTRY  = v_ancestry_label[i]
    ANCESTRY_tag = names(v_ancestry_label[i])
    
    n_table_sub = data %>% filter(ancestry_label==ANCESTRY) %>% select(all_of(CONFOUNDER)) %>% table()
    percent_table_sub = n_table_sub / sum(n_table_sub)
    
    mat_sub =   cbind(
      n_table_sub %>% as.data.frame %>% rename(n = Freq) %>% mutate(x = ANCESTRY_tag) %>% mutate(order_x=i+1),
      percent_table_sub %>% as.data.frame %>% rename(Percent = Freq) %>% select(Percent)
    )
    colnames(mat_sub)[1] = CONFOUNDER
    
    mm = rbind(
      mm,
      mat_sub
    )
    
  }
  
  mat_order = as.data.frame(cbind(levels_confounder, seq(1:length(levels_confounder))))
  colnames(mat_order) = c(CONFOUNDER, 'order_stack')
  
  mm2 = mm %>%
    left_join(mat_order, by=CONFOUNDER) %>% arrange(order_x, order_stack)
  
  mm3 = mm2 %>%
    mutate(sig.square = ifelse(x=='All', TRUE, FALSE))
  colors_sq = c("gray", "white")
  names(colors_sq) = c(TRUE, FALSE)
  sizes_sq = c(1, 0)
  names(sizes_sq) = c(TRUE, FALSE)
  
  ## Barplot:
  bp = ggplot(mm3, aes(x=reorder(x, order_x), y=Percent, fill=factor(get(CONFOUNDER), levels=levels_confounder))) +
    geom_bar(colour=mm3$sig.square, size=mm3$sig.squar, stat="identity", position=position_stack())+ 
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(name=paste(CONFOUNDER), values=p_colors) + 
    xlab('') + ylab('%') +
    ggtitle(paste0(CONFOUNDER, ' by ancestries')) 
  
  list(bp, mm3)
  
}

