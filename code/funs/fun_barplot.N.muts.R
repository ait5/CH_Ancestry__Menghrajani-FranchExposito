
fun_barplot.N.muts <- function(data, x_lab=NULL, fill_color=NULL, color_color=NULL, size_color=NULL, title_p=NULL, subtitle_p=NULL, y_lim=NULL, dist_text=NULL){
if(is.null(x_lab)){
	x_lab = ''
} else {x_lab = x_lab}

if(is.null(fill_color)){
	fill_color = 'gray'
} else {fill_color = fill_color}

if(is.null(color_color)){
	color_color = 'white'
} else {color_color = color_color}

if(is.null(size_color)){
	size_color = 1
} else {size_color = size_color}

if(is.null(title_p)){
	title_p = ''
} else {title_p = title_p}

if(is.null(subtitle_p)){
	subtitle_p = ''
} else {subtitle_p = subtitle_p}

if(is.null(y_lim)){
	y_lim = c(0, 100)
} else {y_lim = y_lim}

if(is.null(dist_text)){
	dist_text = 10
} else {dist_text = dist_text}



	m0 <- data

	m0$rates <- (m0$Freq/sum(m0$Freq))*100
	m0$text <- paste(as.character(m0$Freq), '\n(', round(m0$rates,1), '%)', sep='')
	m1 <- m0

	df <- m1
		df$order <- 1:nrow(df)


	p_barplot <- ggplot(data=df, aes(x=reorder(N_mutations, order), y=rates)) +
	  geom_bar(stat="identity", position=position_dodge(), fill=fill_color, color=color_color, size=size_color) + 
	  # ggtitle(title_p, subtitle=subtitle_p) +
	  theme_bw() +
	  	  theme(panel.border = element_blank(),
	  	 axis.line = element_line('gray'),
	   panel.grid.major = element_blank(),
	   panel.grid.minor = element_blank(),
	   axis.text.x = element_text(angle = 0),
	   # axis.title.x=element_blank(),
    #    axis.text.x=element_blank(),
    #    axis.ticks.x=element_blank(),
       # axis.title.y=element_blank(),
       # axis.text.y=element_blank(),
       # axis.ticks.y=element_blank()
       ) + ylab('% patients') + xlab(x_lab) +
	  geom_text(df, mapping=aes(y=rates+dist_text,label=text), size=2.3, colour = "darkgray", fontface = "bold", position = position_dodge(width = 0.9)) +
	  ylim(y_lim) 

	 p_barplot
}