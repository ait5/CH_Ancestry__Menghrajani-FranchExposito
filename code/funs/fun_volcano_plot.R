library(ggrepel)

fun_volcano_plot = function(x_file,
                            fill_colors=c("darkgoldenrod","mintcream","firebrick"),
                            text_color='aquamarine4',
                            n_range=1.5,
                            de_value=0.2,
                            n_line_pval=0.05,
                            genes_to_label=''){
  
  
  ### Reading associations tables:
  df_file = read.delim(x_file) %>% 
    mutate(Gene = Dependent) %>%
    filter(Gene %in% as.character(ch.genes)) %>% 
    select(Condition_tags, n_Condition, Gene, n_Dependent_in_Condition,
           glm.odds.ratio, glm.p.value)
  
  m1 = df_file %>%
    mutate(odds.ratio = glm.odds.ratio) %>%
    mutate(p.value = glm.p.value)
  
  m1[which(is.na(m1$odds.ratio)),'odds.ratio'] <- NA
  #########################################################
  ## Conversion to log(odds-ratio):
  m2 <- m1 %>%
    # filter(text2>0) %>%
    mutate(values.plot=log(odds.ratio))
  m2[which(is.na(m2$odds.ratio.low)), 'values.plot'] <- NA
  n.max <- n_range
  MAX.max <- ceiling(max(m2$values.plot, na.rm=T))
  if (MAX.max=='Inf' | MAX.max > n.max){
    # MAX.max = ceiling(sort(unique(dd$Ftest.odds.ratio.MAX),decreasing=T))[2]
    MAX.max = n.max
    m2[which(m2$values.plot>n.max & !is.na(m2$values.plot)),'values.plot'] <- n.max
  }
  n.min <- n_range*-1
  MIN.min <- floor(min(m2$values.plot, na.rm=T))
  if (MIN.min < n.min){
    # MAX.max = ceiling(sort(unique(dd$Ftest.odds.ratio.MAX),decreasing=T))[2]
    MIN.min = n.min
    m2[which(m2$values.plot<n.min & !is.na(m2$values.plot)),'values.plot'] <- n.min
  } else if (MIN.min=='-Inf') {
    m2[which(m2$values.plot<'-Inf'),'values.plot'] <- NA
  }
  
  df = m2 %>% 
    mutate(diffexpressed=ifelse(values.plot > de_value & p.value <= n_line_pval, 'or_3', 
                                ifelse(values.plot < (de_value *-1) & p.value <= n_line_pval, 'or_1', 'or_2'))) %>%
    mutate(Gene_to_label = Gene)
  
  fill_colors <- fill_colors %>%
    setNames(c('or_1', 'or_2', 'or_3'))
  fill_colors <- fill_colors[order(factor(names(fill_colors),
                                          levels = rev(c('or_1', 'or_2', 'or_3'))))]
  
  plot <- ggplot(data=df,
                 aes(x=values.plot, y=-log10(p.value),
                     col=diffexpressed,
                     label=Gene_to_label)) +
    geom_vline(xintercept=c(0), col="gray20") +
    geom_hline(yintercept=-log10(n_line_pval), col="darkorange4",
               linetype = 2, size=0.5) +
    geom_point(size=2) + 
    theme_light() +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.box = "horizontal",
      legend.text.align = 0.5,
      legend.title.align = 0.5,
      plot.background = element_rect(fill='white', color='white'),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
    ggrepel::geom_text_repel(size=5,max.time = 1,
                             box.padding = 1.2, 
                             point.padding = 1,
                             seed = 123) +
    scale_color_manual(values=fill_colors,
                       labels= c("Significantly lower in ASJ",
                                 "Not significant",
                                 "Significantly higher in ASJ"),
                       name="CH-mutated gene presence in ASJ\n(compared to non-ASJ)") +
    guides(color = guide_legend(override.aes = list(size = 3),
                                title.position = "top",
                                label.position = "right",
                                )) +
    xlab('log(OR)') + ylab('-log10(P.value)') +
    xlim(c((n_range*-1), n_range))
  plot
  return(list(plot, df))
  
}
