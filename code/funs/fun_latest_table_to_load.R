fun_latest_table_to_load <- function(dir_to_look, pattern) {
  ## Example:
  # dir_to_look <- paste0(out_dir_results, 'tables_assocs/')
  
  ## Pattern possibilities:
  # pattern <- 'ch.features.v.germ.event' # Main associations CH (and CH-features) ~ germ.event
  # pattern <- 'ch.fearures.v.g.genes' # Associations CH ~ g.Gene
  # pattern <- 'ch.gene.v.g.gene' # Associations between CH ~ g.Gene
  
  
  
  files_in_folder <- list.files(dir_to_look)
  files_with_pattern_sorted <- sort( files_in_folder[grep(pattern, files_in_folder, fixed = TRUE)], decreasing = TRUE ) # Grep & sort by date
  ans <- files_with_pattern_sorted[1] # Select the latest file
  print(paste0('This is the table name: ', ans, '!'))
  
  ans
}
