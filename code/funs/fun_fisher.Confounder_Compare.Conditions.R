
fun_fisher.Confounder_Compare.Conditions = function(dd,
                                                CONFOUNDER, CONFOUNDER_tag,
                                                DEPENDENT, DEPENDENT_tag,
                                                CONDITION_1, CONDITION_1_tag,
                                                CONDITION_2, CONDITION_2_tag,
                                                v_colors){
  
  names(v_colors) = c(CONDITION_1_tag, CONDITION_2_tag)
  

  
  dd2 = dd %>%
    filter(get(CONDITION_1) == 1 | get(CONDITION_2) == 1) %>%
    mutate(C1.or.C2 = ifelse( get(CONDITION_2) == 1, 1, 0)) %>%
    select(all_of(DEPENDENT), all_of(CONDITION_1), all_of(CONDITION_2), all_of(CONFOUNDER), C1.or.C2)
  
  data = rbind(dd2 %>%
                 filter(get(DEPENDENT)==1) %>%
                 select(all_of(CONFOUNDER), C1.or.C2) %>%
                 table() %>%
                 as.data.frame() %>%
                 filter(C1.or.C2 == 0) %>%
                 select(all_of(CONFOUNDER), Freq) %>%
                 mutate(type=CONDITION_1_tag) %>%
                 tibble::rownames_to_column(var='order_x') %>%
                 mutate(x =as.character( get( all_of(CONFOUNDER) ) ) ),
               dd2 %>%
                 filter(get(DEPENDENT)==1) %>%
                 select(all_of(CONFOUNDER), C1.or.C2) %>%
                 table() %>%
                 as.data.frame() %>%
                 filter(C1.or.C2 == 1) %>%
                 select(all_of(CONFOUNDER), Freq) %>%
                 mutate(type=CONDITION_2_tag) %>%
                 tibble::rownames_to_column(var='order_x') %>%
                 mutate(x =as.character( get( all_of(CONFOUNDER) ) ) )
  ) %>%
    # filter(age_bin!='NA') %>%
    rename(n = Freq) %>%
    mutate(percent = NA) %>%
    mutate(percent = '')
  
  v_confounder_bins = data %>% select(all_of(CONFOUNDER)) %>% unique %>% pull %>% as.character
                                      
  df_by.confounder.bin = as.data.frame(do.call(rbind, lapply(1:length(v_confounder_bins), function(i){
    CONFOUNDER_BIN = v_confounder_bins[i]
    # print(CONFOUNDER_BIN)
    
    m = dd2 %>% 
      filter(get(CONFOUNDER) == CONFOUNDER_BIN) %>%
      select(all_of(DEPENDENT), C1.or.C2) %>% table()
    
    if(ncol(m)==2 & nrow(m)==2){
      f = fisher.test(m)
      val = round(f$p.value,2)
      
        if (val <= 0.001){
          val2 <- paste("***\np=", val, sep=" ")
        } else if (val <= 0.01){
          val2 <- paste("**\np=", val, sep=" ")
        } else if (val <= 0.05){
          val2 <- paste("*\np=", val, sep=" ")
        } else if (val > 0.05){
          val2 <- paste("(ns)\np=", val, sep=" ")
        } else {
          val2 <- val
        }
      
      n_1 = m[2,1]
      n_percent_1 = m[2,1]/(m[2,1]+m[1,1])
      
      n_2 = m[2,2]
      n_percent_2 = m[2,2]/(m[2,2]+m[1,2])
      
      ans = data %>% filter(get(CONFOUNDER) == CONFOUNDER_BIN) %>%
        mutate(percent = c(n_percent_1, n_percent_2)) %>%
        mutate(pvals = c('', val2))
      ans
        
    } else {print('Error!')}
    

  }))) %>%
    mutate(percent = as.numeric(percent) * 100) %>%
    mutate(x_text=paste0('(',round(percent,1),'%)')) %>%
    mutate(order_x = as.numeric(order_x)) %>%
    mutate(y=percent) %>%
    mutate(n_text = paste0(n, '\n', x_text))

  data2 = df_by.confounder.bin
  
  ## Plot:
  y_max = max(data2$y)
  y_value_text = y_max*0.1
  
  bp = ggplot(data2,
              aes(x=reorder(x, order_x), y=y, fill=type)) +
    geom_bar(stat="identity", position=position_dodge(), color='gray25') +
    geom_text(data2, mapping=aes(y=y*1.005+y_value_text,label=n_text), size=2.6,
              colour = "gray55", fontface = "bold", position = position_dodge(width = 0.9)) +
    geom_text(data2, mapping=aes(y=y*1.005+y_value_text+6, label=pvals),
              size=2.5, colour = "gray25", position = position_dodge(width = 0)) +
    ylim(c(0, y_max*1.4)) +
    theme_bw() +
    theme(panel.border = element_blank(),
          axis.line = element_line('gray'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 0,
                                     hjust = 0.5),
          axis.title.y = element_text(angle = 90, vjust=0.5, size=8.5),
          axis.title.y.right = element_text(angle = 0, vjust=0.5, size=8.5)) + 
    scale_fill_manual(values=v_colors, name='') +
    ylab(paste0('% patients with ', DEPENDENT_tag)) +
    xlab(CONFOUNDER_tag)
  
}



