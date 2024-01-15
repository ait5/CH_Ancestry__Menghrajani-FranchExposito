
fun_bp.AgeBin <- function(data, size_text=2, ch.term, ch.color, age_column,
                          fill.color='gray', text.color='gray50'){
  
  df_all <- data %>% as.data.frame %>%
    filter(get(age_column)!=100) %>%
    select(all_of(age_column)) %>% table() %>% as.data.frame() %>%
    rename(age=1) %>%
    mutate(rates=(Freq/sum(Freq))*100) %>%
    mutate(text=
             paste(Freq, '\n(', round(rates,1), '%)', sep='')) %>%
    mutate(age_factor = row_number()) %>%
    mutate(age = as.numeric(as.character(age))) %>%
    mutate(age_tags = paste0(age, '-', age+10))
  
  
  ## CH
  m0 <- data %>% as.data.frame %>%
    filter(get(age_column)!=100) %>%
    filter(get(ch.term)==1) %>%
    select(all_of(age_column)) %>% table() %>%
    as.data.frame() %>%
    rename(age=1) %>%
    mutate(age = as.numeric(as.character(age))) %>%
    mutate(rates=(Freq/sum(Freq))*100) %>%
    left_join(df_all %>% select(age, Freq), by='age') %>%
    mutate(bin_rates=(Freq.x/Freq.y)*100) %>%
    select(-Freq.y) %>%
    rename(Freq=Freq.x)  %>%
    mutate(age_tags = paste0(age, '-', age+10)) %>%
    left_join(df_all %>% select(age_tags, age_factor))
  
  m0$cum_rates <- 0
  for (i in 1:nrow(m0)){
    if (i>1){
      m0[i,'cum_rates'] <- m0[i,'rates']+m0[i-1,'cum_rates']
    } else { m0[i,'cum_rates'] <- m0[i,'rates'] }
  }
  # m0$text <- paste(as.character(m0$Freq), '\n(', round(m0$rates,1), '%)', sep='')
  m0$text <- ''
  m0[nrow(m0),'text'] <- paste('(', round(m0[nrow(m0),'cum_rates'],2), '%)', sep='')
  m1 <- m0
  m1$color <- ch.color
  # m1$age_factor <- as.factor(1:nrow(m1))
  
  df_ch <- m1 
  df_ch$group <- 'CH'
  
  df2 = df_ch
  
  coeff <- 1
  max_value = ceiling(max(c(df_all$rates*coeff, df2$bin_rates)))
  
  df_all <- df_all %>% filter(age_factor != 100) 
  df2 <- df2 %>% filter(age_factor != 100)
  
  
  p_barplot <- ggplot(data=df_all, aes(x=reorder(age_tags, age), y=rates*coeff)) +
    geom_bar(stat="identity", position=position_dodge(), fill=fill.color, size=3) + 
    geom_line(data = df2, aes(x=age_tags, y=bin_rates, group=group), color=df2$color, size=size_text/3) +
    geom_point(data = df2, aes(x=age_tags, y=bin_rates, group=group), color=df2$color, size=size_text/2) +
    geom_text(df_all, mapping=aes(y=(rates*coeff)+3,label=text), size=size_text*0.7,
              colour = text.color, fontface = "bold", position = position_dodge(width = 0.9)) +
    theme_bw() +
    theme(panel.border = element_blank(),
          plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
          axis.line = element_line('gray'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(angle = 0, vjust=0.5, size=size_text*2.5),
          axis.title.y = element_text(angle = 0, vjust=0.5, size=size_text*2.5),
          axis.text.x = element_text(angle = 0, hjust = 0.5, size=size_text*2),
          axis.text.y = element_text(angle = 0, hjust = 0.5, size=size_text*2),
          axis.title.y.right = element_text(angle = 0, vjust=0.5, size=size_text*2.5)
    ) +
    ylab("Patients\nwith\nsolid tumors (%)") +
    xlab('Patient age when blood was drawn\n(years)') +
    scale_y_continuous(breaks=seq(0,max_value, by=10), labels=seq(0,max_value, by=10), limits=c(0,max_value+2)) 

  
  p_barplot
}