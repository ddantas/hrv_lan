#sudo apt install r-base r-base-core r-recommended

#ad.test
library(nortest)

#str_pad
library(stringr)

library(ggplot2)

source('report.R')

REPORT   = TRUE

###############################################################3
df = load_data(filename_dataset)
df_stack = load_data(filename_dataset_stack)
df_role = load_data(filename_dataset_role)

confidence = 0.95
prompt     = 0
inputFile     = filename_dataset
outputDir      = "saida"
outputFile     = "report.html"

if (REPORT)
{
  report(inputFile, outputFile, outputDir, df, df_stack, df_role, confidence, prompt)
}

###############################################################3
