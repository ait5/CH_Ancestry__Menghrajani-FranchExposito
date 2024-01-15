
fun_density.plot = function(data, x_term, x_term_tag=x_term, x_lim=NULL,
                            color_outline='gray50',
                            color_filling='gray',
                            color_vline_median='gray15',
                            size_text = SIZE_TEXT){
  
  v_median = median(as.numeric(as.character(data[,x_term])), na.rm=T)
  
  dp = ggplot(data, aes(x=get(x_term))) +
    geom_density(color=color_outline, size = 1, width = 0.6, fill=color_filling)+
    geom_vline(xintercept = v_median, color=color_vline_median, size=1) +
    geom_vline(xintercept = 50, color=color_outline, size=1, linetype = "dashed") +
    xlab(x_term_tag) +
    ylab('') +
    theme(legend.position="none",
          plot.background = element_rect(fill='white', color='white'),
          text = element_text(size=size_text+2),
          # legend.position=c(0.8,0.8),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          title=element_text(size=size_text*2),
          # axis.line = element_line(colour = "black"),
          axis.text.y=element_blank(),
          axis.title.x=element_text(size=SIZE_TEXT*3),
          axis.ticks.y=element_blank(),
          axis.line = element_blank()
    )
  
  if (!is.null(x_lim)){
    dp = dp + xlim(x_lim)
  }
  
  dp
}