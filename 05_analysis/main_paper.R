#sudo apt install r-base r-base-core r-recommended

#ad.test
library(nortest)

#str_pad
library(stringr)

#ggplot
library(ggplot2)

source('const.R')
source('report_paper.R')

REPORT_PAPER = TRUE

###############################################################3
df = load_data(filename_dataset)
df = load_isimitvideo(df)
df_pvals = load_data(filename_dataset_pvals)

confidence = 0.95
prompt     = 0
inputFile  = filename_dataset
outputDir  = "saida"
outputFile = "report.html"

outputFile     = "report_paper.html"
if (REPORT_PAPER)
{
  report_paper(inputFile, outputFile, outputDir, df, df_pvals, confidence, prompt)
}

###############################################################3
