#sudo apt install r-base r-base-core r-recommended

#ad.test
library(nortest)

#str_pad
library(stringr)

#ggplot
library(ggplot2)

#geom_signif
library("ggsignif")


source('const.R')
source('utils.R')
source('report_paper2.R')

REPORT_PAPER = TRUE

###############################################################3
df = load_data(filename_dataset)
df = load_isimitvideo(df)
df_pvals = load_data(filename_dataset_pvals)

confidence = 0.95
prompt     = 0
inputFile  = filename_dataset
outputDir  = "saida"
outputFile = "report_paper2.html"

if (REPORT_PAPER)
{
  report_paper2(inputFile, outputFile, outputDir, df, df_pvals, confidence, prompt)
}

###############################################################3
