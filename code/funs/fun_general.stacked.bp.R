
fun_general.stacked.bp <- function(data, size_text=1, fill_colors=c('gray', 'firebrick3'),
                                   color_color='white', size_color=NULL,
                                   color_text=c('black', 'white'),
                                   text_vars = c(NA, 'CH'),
                                   color_text_vars = c('gray', 'firebrick3')){
  
  
  m0 <- data %>% table %>%
    as.data.frame
  colnames(m0) <- c('Var1', 'Freq')
  m0$y <- 'var'
  m0$rates <- m0$Freq/sum(m0$Freq)
  m0$text <- paste(as.character(m0$Freq), '  \n(', round(m0$rates*100,1), '%)', sep='')
  m0$text2 = text_vars
  
  df <- m0
  df$order <- 1:nrow(df)
  colors <- fill_colors
  names(colors) <- c(0,1)
  
  p_barplot <- ggplot(data=df, aes(x=y, y=rates, fill=Var1)) +
    # geom_bar(stat="identity", position=position_dodge()) + # Dodging stacked bars
    geom_bar(stat="identity", position='stack',
             color=color_color, size=size_color) + # Stacking bars
    # ggtitle(paste('Fraction of patients with CH', sep='')
    # subtitle="[N patients (%)]") 
    # )+
    theme_bw() +
    theme(panel.border = element_blank(),
          plot.margin=unit(c(0,0.2,0,0.2), 'cm'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # axis.text.x = element_text(angle = 50, hjust = 1),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    geom_text(df, mapping=aes(y=rates,label=text),
              color=color_text, angle=0,
              size=size_text,position = position_stack(vjust=0.5)) +
    geom_text(df, mapping=aes(y=0.1, x=1.51, label=text2),
              color=color_text_vars, angle=0,
              size=size_text*0.9, fontface = "bold") +
    scale_fill_manual(name='', values=colors, labels=text_vars) +
    guides(fill='none') +
    coord_flip()
  p_barplot
}