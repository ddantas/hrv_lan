
library(poolr)

source('report_poolr.R')

REPORT   = TRUE

###############################################################3
df_pvals = load_data("dataset_pvals.tsv")

confidence = 0.95
prompt     = 0
inputFile     = filename_dataset
outputDir      = "saida"
outputFile     = "report_poolr.html"

if (REPORT)
{
  report_poolr(inputFile, outputFile, outputDir, df_pvals)
}

###############################################################3
