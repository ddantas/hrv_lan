
library(poolr)

source('report_poolr.R')

REPORT   = TRUE

###############################################################3
df_pvals = load_data("dataset_pvals.tsv")

confidence = 0.95
prompt     = 0
inputFile     = filename_dataset
outputDir      = "saida"
outputFile1     = "report_poolr1.html"
outputFile2     = "report_poolr2.html"

if (REPORT)
{
  report_poolr(inputFile, outputFile1, outputDir, df_pvals)
  summary = 1
  report_poolr(inputFile, outputFile2, outputDir, df_pvals, summary)
}

###############################################################3
