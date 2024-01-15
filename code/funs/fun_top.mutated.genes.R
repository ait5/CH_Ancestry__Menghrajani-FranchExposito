
fun_top.mutated.genes <- function(data, genes_to_count, n_genes=NULL, x_lab=NULL, fill_colors=NULL, color_color=NULL, size_color=NULL, term_change=NULL, dist_text=NULL){

if(is.null(n_genes)){
	n_genes = 10
} else {n_genes = n_genes}

if(is.null(x_lab)){
	x_lab = ''
} else {x_lab = x_lab}

if(is.null(fill_colors)){
	fill_colors = c('firebrick4', 'firebrick1')
} else {fill_colors = fill_colors}

if(is.null(color_color)){
	color_color = 'gray'
} else {color_color = color_color}

if(is.null(size_color)){
	size_color = 1
} else {size_color = size_color}

if(is.null(size_color)){
	size_color = 1
} else {size_color = size_color}

if(is.null(dist_text)){
	dist_text = 2
} else {dist_text = dist_text}



top_genes <- data %>%
 select(all_of(genes_to_count)) %>%
  apply(2, sum) %>% sort(decreasing=T) %>%
  as.data.frame() %>% slice(1:n_genes) %>%
   tibble::rownames_to_column(var='Gene')
colnames(top_genes) <- c('Gene', 'N_patients')

 if (!is.null(term_change)){
 	top_genes$Gene <- gsub(term_change, '', as.character(top_genes$Gene))
 }

	distinguish_colors <- fill_colors
	colors_genes <- colorRampPalette(distinguish_colors)(n_genes)
	names(colors_genes) <- top_genes$Gene

n_patients_total <- data %>% nrow()



# Barplot

		mm2 <- top_genes
		mm2$order_by <- 1:nrow(mm2)
		mm2$N_patients <- as.numeric(as.character(mm2$N_patients))

		mm2 <- mm2 %>% mutate(y = (N_patients/sum(n_patients_total))*100) %>% mutate(text=paste(N_patients, '\n(', round(y,1), ')', sep='' ))

			p <- ggplot(data=mm2, aes(x=reorder(Gene, +order_by), y=y, fill=Gene)) +
				geom_bar(stat="identity", position=position_dodge(),
				color=color_color, size=size_color) + # Dodging stacked bars
				# geom_col() + # Stacking bars
				geom_text(mm2, mapping=aes(y=y+dist_text,label=text), size=2.5, colour = "darkgray", fontface = "bold", position = position_dodge(width = 0.9)) +
				# ggtitle(paste('Top-10 genes CH mutated', sep=''),
					# subtitle=(paste('(N=', n_patients_total, ')', sep=''))) +
				xlab("") + ylab('% patients') + labs(fill = "") +
				theme_bw() + 
				theme(panel.border = element_blank(),
					axis.line = element_line('gray'),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				#legend.position='none',
				 axis.text.x = element_text(angle = 0, hjust = 0.5)
				)
			p2 <- p +
			scale_fill_manual(values=colors_genes) +
			guides(fill='none') # Do not print legend
	p2
}