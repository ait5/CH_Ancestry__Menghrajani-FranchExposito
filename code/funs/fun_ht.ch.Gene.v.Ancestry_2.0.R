
fun_ht.ch.Gene.v.Ancestry_2.0 <- function(x_file, v_conditions, ch.genes,
                                      fill_colors=c("darkgoldenrod","mintcream","firebrick"),
                                      text_color='aquamarine4',
                                      n_range=1.5,
                                      x.text_angle=50, add_numbers=FALSE){
  
  ### Reading associations tables:
  df_file = read.delim(x_file) %>% 
    mutate(Gene = Dependent) %>%
    filter(Gene %in% as.character(ch.genes)) %>% 
    select(Condition_tags, n_Condition, Gene, n_Dependent_in_Condition,
           glm.odds.ratio, glm.p.value)
  
  ## Multiple-testing correction BY ch.GENE:
  by.germ.gene_df <- as.data.frame(do.call(rbind, lapply(ch.genes, function(ch.GENE){
    ans = df_file %>%
      filter(Gene == ch.GENE) %>%
      mutate(odds.ratio = glm.odds.ratio) %>%
      mutate(p.value = glm.p.value) %>%
      mutate(adj.p.value = p.adjust(p.value, method="fdr")) %>%
      mutate(signif.text = fifelse(p.value<=0.001, '***',
                                   fifelse( p.value<=0.01, '**',
                                            fifelse( p.value<=0.05, '*', ''))))
    if (length(is.na(ans$odds.ratio))==nrow(ans)){
      ans = ans %>%
        filter(!is.na(odds.ratio)) # If all genes interactions resulted NA... go NEXT!
    }
    ans
  })))
  
  ## Filtering out non-informative rows:
  m1 <- by.germ.gene_df %>%
    mutate(text.Condition_tags = paste0(Condition_tags, ' (', n_Condition, ')')) %>%
    arrange(desc(n_Condition), n_Dependent_in_Condition) %>%
    filter(n_Condition >= 5) %>%
    mutate(odds.ratio = ifelse(odds.ratio<1e-04 & p.value>0.95 & n_Dependent_in_Condition<1,
                               NA, odds.ratio))
  m1[which(is.na(m1$odds.ratio)),'odds.ratio'] <- NA
  
  
  ## Conversion to log10(odds-ratio):
  m2 <- m1 %>%
    mutate(values.plot=log(odds.ratio))
  
  n.max <- n_range
  MAX.max <- ceiling(max(m2$values.plot, na.rm=T))
  if (MAX.max=='Inf' | MAX.max > n.max){
    MAX.max = n.max
    m2[which(m2$values.plot>n.max & !is.na(m2$values.plot)),'values.plot'] <- n.max
  }
  
  n.min <- n_range*-1
  MIN.min <- floor(min(m2$values.plot, na.rm=T))
  if (MIN.min < n.min){
    MIN.min = n.min
    m2[which(m2$values.plot<n.min & !is.na(m2$values.plot)),'values.plot'] <- n.min
  } else if (MIN.min=='-Inf') {
    m2[which(m2$values.plot<'-Inf'),'values.plot'] <- NA
  }
  
  
  ## ARRANGING BY CH terms!
  m_ch_order <- as.data.frame(cbind(ch.genes, 1:length(ch.genes)))
  colnames(m_ch_order) <- c('ch.gene', 'order_ch')
  m3 = m2 %>%
    mutate(ch.gene = Gene) %>%
    left_join(m_ch_order, by='ch.gene') %>%
    mutate(order_ch = as.numeric(order_ch))
  
  
  ## FDR annotation in ht:
  m3 = m3 %>%
    mutate(sig.square = ifelse(adj.p.value<=0.1, TRUE, FALSE))
  colors_fdr = c("darkorange2", "white")
  names(colors_fdr) = c(TRUE, FALSE)
  sizes_fdr = c(1, 0)
  names(sizes_fdr) = c(TRUE, FALSE)
  
  
  ## Heatmap 
  ht = ggplot(m3 ,
              aes(y=reorder(text.Condition_tags, n_Condition), x=reorder(Gene, order_ch), fill= values.plot)) + 
    geom_tile(color = "white",
              lwd = 0.75,
              linetype = 1) +
    scale_fill_gradientn(name='log(OR)',
                         colors=fill_colors,
                         na.value = "gray95",
                         values=scales::rescale(c(n_range*-1, 0, n_range)),
                         limits=c(n_range*-1, n_range)
    ) +
    geom_text(aes(label=signif.text), size=SIZE_TEXT,
              position = position_nudge(y = .15), color=text_color) +
    xlab(paste0('')) +
    ylab('') +
    # ggtitle('') +
    theme_minimal() + 
    scale_x_discrete(position = "top") +
    theme(
      plot.background = element_rect(fill='white', color='white'),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      axis.text.x = element_text(angle = x.text_angle, hjust = 0))
  if (add_numbers == TRUE) {
    ht = ht +
      geom_text(aes(label=n_Dependent_in_Condition), size=SIZE_TEXT/2,
                position = position_nudge(y = -0.2), color='gray20') 
  }
  ht = ht + 
    geom_tile(data=m3,
              aes(colour=sig.square, size=sig.square), alpha=0) + 
    scale_colour_manual("", values=colors_fdr, labels=c('', 'Q.val<=0.1')) + 
    scale_size_manual("", values=sizes_fdr, labels=c('', 'Q.val<=0.1'))
  PLOT = ht
  TABLE_plot = m3
  ans = list(PLOT, TABLE_plot)
  names(ans) <- c('PLOT', 'TABLE_plot')
  ans
}