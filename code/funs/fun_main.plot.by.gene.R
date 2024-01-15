

fun_main.plot.by.gene = function(data, DEPENDENT, size_text=5, genes.to.plot, term_plot, CH.cancer.rate,
                     pvalue_term, signif_term, fill_colors, odds_range=c(-1,1), n_genes=15){

dd = data %>%
  mutate(Total = as.numeric(Total)) %>%
  mutate(n_Condition = as.numeric(n_Condition)) %>%
  mutate(n_Dependent = as.numeric(n_Dependent)) %>%
  mutate(n_Dependent_in_Condition = as.numeric(n_Dependent_in_Condition)) %>%
  mutate(f_Dependent_in_Total = as.numeric(f_Dependent_in_Total)) %>%
  mutate(f_Dependent_in_Condition = as.numeric(f_Dependent_in_Condition)) %>%
  mutate(glm.odds.ratio = as.numeric(glm.odds.ratio)) %>%
  mutate(glm.p.value = as.numeric(glm.p.value)) %>%
  mutate(glm.conf.low = as.numeric(glm.conf.low)) %>%
  mutate(glm.conf.high = as.numeric(glm.conf.high))


top_genes = genes.to.plot
CH_cancer_rate = CH.cancer.rate
size_title_x = size_text*3
size_text_x = size_text*2
size_text_y = size_text*2

# pvalue_term = c('glm.p.value', 'glm.FDR.adj.Pvalue')

		## Arranging data for plotting:
		dd <- dd %>%
		 filter(Dependent == DEPENDENT) %>%
		 # filter(Gene%in%top_genes) %>%
		 filter(!is.na(glm.p.value)) %>% droplevels() %>%
		 mutate(Gene.germ.mut.Rate=n_Condition/Total) %>%
		 arrange(desc(Gene.germ.mut.Rate)) %>%
		 slice(1:n_genes) %>%
		 filter(n_Dependent_in_Condition>0) %>%
		mutate(values.plot=log(glm.odds.ratio)) %>%
		mutate(values.low=log(glm.conf.low)) %>%
		mutate(values.high=log(glm.conf.high)) %>%
		mutate(glm.FDR.adj.Pvalue = p.adjust(glm.p.value, method="fdr")) %>%
		mutate(signif.Adj.Pvalue=fifelse(glm.FDR.adj.Pvalue<=0.001, '***',
				fifelse(glm.FDR.adj.Pvalue<=0.01, '***', 
					fifelse(glm.FDR.adj.Pvalue<=0.05, '**', 
						fifelse(glm.FDR.adj.Pvalue<0.1, '*', ''))))) %>%
		mutate(text_adj.p.value = fifelse(glm.FDR.adj.Pvalue<0.1,
		                                  paste0('Adj.P=', round(glm.FDR.adj.Pvalue,3)), ''))


min_y = odds_range[1]
max_y = odds_range[2]
by_scale = max_y/4

	if (max(dd$values.high)>max_y){
		dd[which(dd$values.high > max_y), 'values.high'] = max_y
	}
	if (min(dd$values.high)<min_y){
		dd[which(dd$values.low < min_y), 'values.low'] = min_y
	}
	if (max(dd$values.plot)>max_y){
		dd[which(dd$values.plot > max_y), 'values.plot'] = max_y
	}

		# dd <- dd %>% mutate(y=paste(Gene, '\n(', round(values.plot,2), ')', sep=''))
		dd <- dd %>% mutate(y=paste(Gene, ' (', round(values.plot,2), ')', sep=''))

		dd$order <- 1:nrow(dd)


		###Odds ratio plot 
			p_odds <- ggplot(dd, aes(y = reorder(y, -order), x = values.plot) ) +
				geom_point(size = 2, color = fill_colors[3]) +
				geom_errorbarh(aes(xmax = values.high, xmin = values.low), size = 1,
				               color=fill_colors[3], height=0) +
				geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") + 
			  geom_text(dd, mapping=aes(x=values.plot, y=y ,label=get(signif_term)),
			            colour = "gray35", fontface = "bold", position = position_nudge(y = +0.4)) +
			  # geom_text(dd, mapping=aes(x=values.plot, y=y ,label=text_adj.p.value),
			            # colour = "gray35", fontface = "plain", position = position_nudge(x = +0.4)) +
				labs(x = "\nlog(OR)", y = "") +
				theme_bw() +
				theme(panel.border = element_blank(),
					axis.text.y = element_text(size=size_text_y, vjust=0.5),
					axis.title.x = element_text(size=size_title_x, hjust = 0.5),
					axis.line = element_line('gray'),
					panel.grid.major = element_blank(),
					panel.grid.minor = element_blank(),
					axis.text.x = element_text(angle = 0, hjust = 0.5, size=size_text_x)) +
			  	scale_x_continuous(position='top',
			  		breaks=seq(min_y,max_y, by=by_scale),
			  		labels=round(seq(min_y,max_y, by=by_scale),2), limit=c(min_y, max_y))

				#p_odds



		### Barplot for CH rates when gene mutated

		dd <- dd %>%
		  # mutate(y_ch=paste0(Gene, '\n(', n_Dependent_in_Condition, '/', n_Condition, ')'))
		# mutate(y_ch=paste0(Gene, ' (', n_Dependent_in_Condition, '/', n_Condition, ')'))
		mutate(y_ch=paste0(Gene, ' (', n_Dependent_in_Condition, ')'))

		# CH_cancer_rate <- data %>% filter(functional_ch==1) %>% nrow() / data %>% nrow

			p_ch_rate <- ggplot(dd, aes(y = reorder(y_ch, -order), x = f_Dependent_in_Condition*100)) +
			  geom_bar(stat="identity", position=position_dodge(), fill=fill_colors[2],
			           # color=fill_colors[1] # This would get a color frame (yellow) for each bar
			           color='white') + # 
			  geom_vline(aes(xintercept = CH_cancer_rate*100), size = 1, linetype = "dashed", color='violetred4') + 
			   xlab(paste0("% of patients with ", term_plot)) + ylab("") +
			  theme_bw() + 
			  theme(panel.border = element_blank(),
				  axis.text.y = element_text(size=size_text_y, vjust=0.5),
				  axis.title.x = element_text(size=size_title_x, hjust = 0.5),
				  	axis.line = element_line('gray'),
				  panel.grid.major = element_blank(),
				  panel.grid.minor = element_blank(),
				  axis.text.x = element_text(angle = 0, hjust = 0.5, size=size_text_x)) +
			  scale_x_continuous(position = "top")

			  # p_ch_rate

		### Barplot for Germline mutation rates

		dd <- dd %>%
		 # mutate(y_germ=paste0(Gene, '\n(', n_Condition, '/', Total, ')')) 
		# mutate(y_germ=paste0(Gene, ' (', n_Condition, '/', Total, ')')) 
		mutate(y_germ=paste0(Gene, ' (', n_Condition, ')')) 

			p_germ_rate <- ggplot(dd, aes(y = reorder(y_germ, -order), x = Gene.germ.mut.Rate*100)) +
			  geom_bar(stat="identity", position=position_dodge(), fill=fill_colors[1]) + # 
			   xlab("% of patients") + ylab("") +
			  theme_bw() + 
			  theme(panel.border = element_blank(),
				axis.text.y = element_text(size=size_text_y, vjust=0.5),
				axis.title.x = element_text(size=size_title_x, hjust = 0.5),
			  	axis.line = element_line('gray'),
			  	panel.grid.major = element_blank(),
			  	panel.grid.minor = element_blank(),
			  	axis.text.x = element_text(angle = 0, hjust = 0.5, size=size_text_x))+
			  scale_x_continuous(position = "top")

			  # p_germ_rate


			### Joining plots

			p_all <- ggarrange(p_germ_rate, p_ch_rate, p_odds, ncol=3, nrow=1,
				widths=c(0.8,0.8,0.8), heights=c(1)) +
			  theme(
			    plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), 'cm')
			    )
			# print('Done!')

			list(p_all, dd)
		   # annotate_figure(p_all,
		   #                 top = text_grob(paste('TOP germline-mutated genes', sep=''),
		   #                                 color = "darkgray", size = 14),
		   #                 bottom=text_grob(paste0('Genes ordered by % patients germline mutated\n\n [germline SNVs] ', pvalue_term),
		   #                                  hjust=0, vjust=0.2, x=0.1, color = "black",
		   #                                  face = "plain", size = 6))

}




