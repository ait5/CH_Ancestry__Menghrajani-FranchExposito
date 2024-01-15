
fun_plot_confounders_by_data = function(data, rest_color='gray') {

  ## Plots rates
  l_plots_rates_by_conf = lapply(1:length(v_confounders), function(j){
    conf_var = v_confounders[j]
    # conf_var = 'age_bin'
    print(conf_var)
    
    ## For CH rates
    conf_var_term = conf_var
    rate_color='red4'
    rest_color=rest_color
    p_y_lim=c(0,100)
    decreasing=FALSE 
    
    
    if (conf_var!='Major.Cancer.Type') {
      plot_conf = fun_CH.rates.by.CONFOUNDER(data, conf_var_term,
                                             rate_color, rest_color, p_y_lim, decreasing) +
        theme(axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank())
      
    } else if (conf_var=='Major.Cancer.Type') {
      
      plot_conf = fun_CH.rates.by.Cancer.Types(data, rate_color, rest_color, p_y_lim)
      
    }
    plot_rates = plot_conf + coord_flip()
    
    plot_rates
  })
  # l_plots_rates_by_conf = align_patches(l_plots_rates_by_conf)
  
  
  
  ## For odds plot
  data_mat = data
  ANS = fun_summary_table_confounders(data_mat)
  
  freq_table = as.data.frame(ANS[['t_freq_ch_confounders']])
  uni_test_table = ANS[['t_uni_test']]
  multi_test_table = as.data.frame(ANS[['t_multi_test']])
  
  mat_gene = freq_table %>%
    left_join(uni_test_table, by='var_aggregate') %>%
    left_join(multi_test_table, by='var_aggregate')
  

  l_plots_odds_by_conf = lapply(1:length(v_confounders), function(j){
    conf_var = v_confounders[j]
    # conf_var = 'age_bin'
    print(conf_var)
    
    ## Odds plot_uni
    mat = freq_table %>%
      filter(name_variable == conf_var) %>%
      left_join(uni_test_table, by='var_aggregate')
    
    conf_var_term = conf_var
    plot_odds_uni = fun_plot_odds_confounders(mat, conf_var_term, limits_p=NULL, decreasing=FALSE) 
    
    if (conf_var!='Major.Cancer.Type') {
      plot_odds_uni = plot_odds_uni +
        theme(axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank())
      
    } 
    plot_odds_uni = plot_odds_uni +
      theme(axis.text.y=element_blank())
    
    ## Odds plot_multi
    mat = freq_table %>%
      filter(name_variable == conf_var) %>%
      left_join(multi_test_table, by='var_aggregate')
    
    conf_var_term = conf_var
    plot_odds_multi = fun_plot_odds_confounders(mat, conf_var_term, limits_p=NULL, decreasing=FALSE) 
    
    if (conf_var!='Major.Cancer.Type') {
      plot_odds_multi = plot_odds_multi +
        theme(axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank())
      
    } 
    plot_odds_multi = plot_odds_multi +
      theme(axis.text.y=element_blank())
    
    p_gene_odds = ggarrange(plot_odds_uni, plot_odds_multi,
                            ncol=2, nrow=1, widths = c(1,1))
    p_gene_odds
  })
  # l_plots_odds_by_conf = align_patches(l_plots_odds_by_conf)
  
  ## Arranging:
  p_gene_rates = ggarrange(plotlist = l_plots_rates_by_conf, align='v',
                           ncol=1, nrow=length(v_confounders), heights=c(8, 3, 6, 3, 3, 12),
                           label.x=v_confounders, common.legend = T, legend='none') 
  
  p_gene_odds = ggarrange(plotlist = l_plots_odds_by_conf , align='v',
                          ncol=1, nrow=length(v_confounders), heights=c(8, 3, 6, 3, 3, 12),
                          label.x=v_confounders, common.legend = T, legend='none') 
  
  p_gene = ggarrange(p_gene_rates, p_gene_odds, '',
                     align='hv', widths=c(5.5,4,0.5),
                     ncol=3, nrow=1)
  p_t_gene = list(mat_gene, p_gene)
  names(p_t_gene) = c('table', 'plot')
  p_t_gene
}