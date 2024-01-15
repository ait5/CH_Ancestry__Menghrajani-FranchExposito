

## Load libraries:
library(data.table)
library(dplyr)
library(tidyverse)
library(ggmosaic)

library(ggplot2)
library(doBy)
# library(maftools)
library(ggpubr)
library(cowplot)
library(ggforce)
library(VennDiagram)
library(kableExtra)
library(wesanderson)
library(patchwork)
library(survival)
library(survminer)
library(EnvStats)
library(magick)
library(splines)



##### FILES NAMES: (CHECK dates FOR FILES!!!) ----
in_dir <- './data_deid/'

## Anonymized patient matrix file:
name = 'deid_patient_matrix_2023-08-03.tsv'
ANO_FILE = paste0(in_dir, name)


### OTHER files ----
## Self-reported race/ethnicity:
name <- "deid_self_ancestry_2023-08-03.tsv"
SELF_ANCESTRY = paste0(in_dir, name)

## Clinical data (Kelly's paper):
name <- "deid_nature_clinical_2023-08-03.tsv"
NATURE_CLINICAL = paste0(in_dir, name)

## Ancestry continous values (by Kanika):
name <- "deid_ancestry_continous_2023-08-03.tsv"
ANCESTRY_CONTINOUS = paste0(in_dir, name)



out_dir <- './Results_2.0/'
dir.create(out_dir)

out_dir_by.ch.Gene = paste0(out_dir, 'by.ch.Gene/')
dir.create(out_dir_by.ch.Gene)



dir_funs = './code/funs/'

COLOR_germ = 'lightgoldenrod'
COLOR_ch = 'indianred2'
COLOR_ch_2 = 'indianred4'

COLOR_plain = 'cornsilk2'

COLOR_min = 'darkslategray4'
COLOR_mid = 'cornsilk'
COLOR_max = COLOR_ch


SIZE_TEXT = 4
or_xlim = c(-1.5,1.5)
or_x.byscale = 0.25

## Colors:
v_colors = c(
  wes_palette(name="Cavalcanti1", n=5, type="discrete" ),
  wes_palette(name="Rushmore1", n=5, type="discrete" ),
  wes_palette(name="GrandBudapest1", n=4, type="discrete" ),
  wes_palette(name="Royal1", n=4, type="discrete" ))





###### Workflow:

## 1. Load files:
source('code/1__load_files.R')

## 2. Analyze CH~Germline associations
# source('code/2__assess_associations.R')

### 2.1 Search for latest tables to load after Step 2
source(paste0(dir_funs, 'fun_latest_table_to_load.R'))

NAME_tbl_SPLINE_main.assoc_by.Ancestry <- paste0(out_dir, fun_latest_table_to_load(out_dir, 'SPLINE_main.assoc_by.Ancestry__'))

NAME_tbl_SPLINE_ch.genes_by.Ancestries <- paste0(out_dir_by.ch.Gene, fun_latest_table_to_load(out_dir_by.ch.Gene, 'SPLINE_ch.genes_by.Ancestries__'))
NAME_tbl_onlyCH_SPLINE_ch.genes_by.Ancestries <- paste0(out_dir_by.ch.Gene, fun_latest_table_to_load(out_dir_by.ch.Gene, 'onlyCH_SPLINE_ch.genes_by.Ancestries__'))

NAME_tbl_SPLINE_ch.genes_by.EUR <- paste0(out_dir_by.ch.Gene, fun_latest_table_to_load(out_dir_by.ch.Gene, 'SPLINE_ch.genes_by.EUR__'))

NAME_tbl_SPLINE_main.assoc_by.Ancestry_EUR <- paste0(out_dir, fun_latest_table_to_load(out_dir, 'SPLINE_main.assoc_by.Ancestry_EUR_'))


## 3. Run RMD file (format: HTML)
params <- list(
  ANO_FILE=ANO_FILE,
  SELF_ANCESTRY=SELF_ANCESTRY, 
  NATURE_CLINICAL=NATURE_CLINICAL, 
  ANCESTRY_CONTINOUS=ANCESTRY_CONTINOUS,
  out_dir=out_dir, out_dir_by.ch.Gene=out_dir_by.ch.Gene,
  dir_funs=dir_funs,
  COLOR_germ=COLOR_germ, COLOR_ch=COLOR_ch, COLOR_ch_2=COLOR_ch_2, COLOR_plain=COLOR_plain, 
  COLOR_min=COLOR_min, COLOR_mid=COLOR_mid, COLOR_max=COLOR_max, COLOR_ch=COLOR_ch,
  SIZE_TEXT=SIZE_TEXT, or_xlim=or_xlim, or_x.byscale=or_x.byscale,
  v_colors=v_colors,
  NAME_tbl_SPLINE_main.assoc_by.Ancestry=NAME_tbl_SPLINE_main.assoc_by.Ancestry,
  NAME_tbl_SPLINE_ch.genes_by.Ancestries=NAME_tbl_SPLINE_ch.genes_by.Ancestries,
  NAME_tbl_onlyCH_SPLINE_ch.genes_by.Ancestries=NAME_tbl_onlyCH_SPLINE_ch.genes_by.Ancestries,
  NAME_tbl_SPLINE_ch.genes_by.EUR=NAME_tbl_SPLINE_ch.genes_by.EUR,
  NAME_tbl_SPLINE_main.assoc_by.Ancestry_EUR=NAME_tbl_SPLINE_main.assoc_by.Ancestry_EUR
)


### Run!

rmarkdown::render(input = '2.0_CH~Ancestry.Rmd',
                  output_file = paste0("CH~Ancestry_report_", Sys.Date(), ".html"),
                  params = params)
