
GENERATE = TRUE

source("utils.R")
source('dataset_stack.R')
source('dataset_role.R')

folders = commandArgs(trailingOnly=TRUE)
print("Getting datasets from folders")
print(folders)

if (length(folders) == 0)
{
  folders = c("../data/b001",
              "../data/b002",
              "../data/b003",
              "../data/b004",
              "../data/b009",
              "../data/b010",
              "../data/b011",
              "../data/b012",
              "../data/b013",
              "../data/b014",
              "../data/b015",
              "../data/b016",
              "../data/b017",
              "../data/b018",
              "../data/b019",
              "../data/b020",
              "../data/b021",
              "../data/b022",
              "../data/b023",
              "../data/b024",
              "../data/b025",
              "../data/b026",
              "../data/b027",
              "../data/b028",
              "../data/b029",
              "../data/b030",
              "../data/b031",
              "../data/b032",
              "../data/b033",
              "../data/b034",
              "../data/b035",
              "../data/b036",
              "../data/b037",
              "../data/b038",
              "../data/b039",
              "../data/b040",
              "../data/b041",
              "../data/b042",
              "../data/b043",
              "../data/b044",
              "../data/b045",
              "../data/b046",
              "../data/b047",
              "../data/b048")
}

ds_files = c("02_preprocess/dataset_dd.tsv", "02_preprocess/dataset_jf.tsv")
ds_flow_files = c("04_optical_flow_filter/subj1_flow_subs.tsv", "04_optical_flow_filter/subj2_flow_subs.tsv")
#ds_flow_files = NULL
#ds_files = c("02_preprocess/dataset_dd.tsv")
#ds_files = c("02_preprocess/dataset_jf.tsv")

filename_dataset = "dataset.tsv"
if (GENERATE)
{
  concatenate_datasets(folders, filename_dataset, ds_files, ds_flow_files)
}

###############################################################3
filename_dataset_stack = "dataset_stack.tsv"
if (GENERATE)
{
  dataset_stack(filename_dataset, filename_dataset_stack)
}

###############################################################3
filename_dataset_role = "dataset_role.tsv"
if (GENERATE)
{
  dataset_role(filename_dataset, filename_dataset_role)
}

###############################################################3
