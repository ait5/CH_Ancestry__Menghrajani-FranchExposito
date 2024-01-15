

# LOADING FILES ----

## ANO : (data frame ~50k cancer patients) ----
ano <- read.delim(ANO_FILE, header=T, sep='\t')
# dim(ano)

df_ano <- ano 
# df_ano <- ano %>% filter(fully_annotated == 1) # USING ONLY THOSE PATIENTS FULLY ANNOTATED!

patients_in_df_ano = unique(as.character(df_ano$DMP_PATIENT_ID))
ch.genes.cols <- colnames(df_ano)[grep('ch.DNMT3A',
                                       colnames(df_ano))[1]:grep('ch.HIST1H3E', colnames(df_ano))[1]]
MajorCancerTypes_cols <- colnames(df_ano)[grep('Non.Small.Cell.Lung.Cancer',
                                               colnames(df_ano)):grep('Other.cancer.type', colnames(df_ano))]


## OTHER files ----
## Self-reported race/ethnicity:
df_self_ancestry <- read.delim(SELF_ANCESTRY, header=T, sep=',')

## Clinical data (Kelly's paper):
df_nature_clinical <- read.delim(NATURE_CLINICAL, header=T, sep='\t')

## Ancestry continous values (by Kanika):
df_ancestry_continous <- read.delim(ANCESTRY_CONTINOUS, header=T, sep='\t')

print('All files are loaded!')
