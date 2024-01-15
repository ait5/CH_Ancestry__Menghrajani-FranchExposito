
fun_ch_mutated_genes_bp_by_ancestry <- function(df_mat,
                                                genes_to_plot = c("ch.DNMT3A", "ch.TET2", "ch.PPM1D", "ch.ASXL1", "ch.TP53")){
  
  df_counts_by_ancestry <- df_mat %>%
    # filter(functional_ch == 1) %>%
    select(all_of(genes_to_plot), functional_ch, ancestry_label_2) %>%
    group_by(ancestry_label_2) %>% 
    summarize(n_patients = n(),
              across(all_of(c(genes_to_plot, 'functional_ch')), ~ sum(as.numeric(.)))) %>% 
    ungroup %>% 
    mutate(ch.other = functional_ch - rowSums(across(all_of(genes_to_plot)))) %>%
    mutate(ancestry_label_2 = factor(ancestry_label_2,
                                     levels=c('EUR', 'EAS', 'AFR', 'SAS', 'NAM'))) %>%
    arrange(ancestry_label_2)
  
  mat <- df_counts_by_ancestry %>%
    tidyr::pivot_longer(cols = c(genes_to_plot, ch.other),
                        names_to = "Gene",
                        values_to = "Counts_gene") %>% 
    mutate(Percent_gene = round( (Counts_gene/n_patients)*100, 2)) %>% 
    mutate(Gene = factor(Gene, levels=rev(c(genes_to_plot, "ch.other")))) %>%
    mutate(x_text = paste0(ancestry_label_2, "\n(", n_patients, ")")) %>%
    mutate(x_text = factor(x_text, levels=unique(.data$x_text)))

  # Define the LaCroix color palette
  my_palette <- c(wesanderson::wes_palette("Royal2",
                                            type = "continuous",
                                            n=length(genes_to_plot)),
                                            COLOR_ch) %>% 
    # c(LaCroixColoR::lacroix_palette("Pamplemousse",
    #                                             n=length(c(genes_to_plot)),
    #                                             type = "discrete"), "gray") %>%
    setNames(c(genes_to_plot, "ch.other"))
  
  # ch.DNMT3A
  n_ch_dnmt3a <- df_mat %>%
    # filter(functional_ch == 1) %>%
    filter(ch.DNMT3A==1) %>% nrow() 
  overall_percent_ch_dnmt3a <- n_ch_dnmt3a / df_mat %>%
    # filter(functional_ch == 1) %>%
    nrow()
  
  # Create the stacked bar plot
  plot <- ggplot(mat, aes(x = x_text,
                          y = Percent_gene, fill = Gene)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = my_palette) +
    labs(x = "", y = "Percentage of\npatients\n(%)", fill = "Gene") +
    theme_light() +
    theme(plot.background = element_rect(fill='white', color='white'),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(angle=0, vjust = 0.5)
    )
  # +
  #   geom_hline(yintercept=overall_percent_ch_dnmt3a*100, col="darkolivegreen",
  #              linetype = 1, size=0.5)
  plot
}



# df_ano_2 %>%
#   fun_ch_mutated_genes_bp_by_ancestry(.)
# 
# df_ano_2 %>%
#   filter(functional_ch == 1) %>%
#   fun_ch_mutated_genes_bp_by_ancestry(.)



# n_ch_dnmt3a <- df_ano_2 %>%
#   # filter(functional_ch == 1) %>%
#   filter(ch.DNMT3A==1) %>% nrow() 
# overall_percent_ch_dnmt3a <- n_ch_dnmt3a / df_ano_2 %>%
#   # filter(functional_ch == 1) %>%
#   nrow()
