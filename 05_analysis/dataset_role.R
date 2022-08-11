dataset_role <- function(filename_input, filename_output)
{
  df = load_data(filename_input)

  df1 = df[!is.na(df$Imitator) & df$Imitator == 1,
           c("folder", "annotator", "block", "label", "time", "IsImit", "IsSync", "Imitator", "Model",
             "hr_subj1_linear", "hr_subj2_linear", "hr_subj1_nearest", "hr_subj2_nearest",
             "hr_subj1_ecg_linear", "hr_subj2_ecg_linear", "hr_subj1_ecg_nearest", "hr_subj2_ecg_nearest", "msg1", "msg2", "Gender",
             "d_hr_subj1_linear", "d_hr_subj2_linear", "d_hr_subj1_nearest", "d_hr_subj2_nearest",
             "d_hr_subj1_ecg_linear", "d_hr_subj2_ecg_linear", "d_hr_subj1_ecg_nearest", "d_hr_subj2_ecg_nearest")]

  df2 = df[!is.na(df$Imitator) & df$Imitator == 2,
           c("folder", "annotator", "block", "label", "time", "IsImit", "IsSync", "Imitator", "Model",
             "hr_subj2_linear", "hr_subj1_linear", "hr_subj2_nearest", "hr_subj1_nearest",
             "hr_subj2_ecg_linear", "hr_subj1_ecg_linear", "hr_subj2_ecg_nearest", "hr_subj1_ecg_nearest", "msg2", "msg1", "Gender",
             "d_hr_subj2_linear", "d_hr_subj1_linear", "d_hr_subj2_nearest", "d_hr_subj1_nearest",
             "d_hr_subj2_ecg_linear", "d_hr_subj1_ecg_linear", "d_hr_subj2_ecg_nearest", "d_hr_subj1_ecg_nearest")]

  colnames_new = c("folder", "annotator", "block", "label", "time", "IsImit", "IsSync", "Imitator", "Model",
             "hr_subjI_linear", "hr_subjM_linear", "hr_subjI_nearest", "hr_subjM_nearest",
             "hr_subjI_ecg_linear", "hr_subjM_ecg_linear", "hr_subjI_ecg_nearest", "hr_subjM_ecg_nearest", "msgI", "msgM", "Gender",
             "d_hr_subjI_linear", "d_hr_subjM_linear", "d_hr_subjI_nearest", "d_hr_subjM_nearest",
             "d_hr_subjI_ecg_linear", "d_hr_subjM_ecg_linear", "d_hr_subjI_ecg_nearest", "d_hr_subjM_ecg_nearest")


  colnames(df1) = colnames_new
  colnames(df2) = colnames_new
  df_new = rbind(df1, df2)
  df_new = df_new[order(as.numeric(row.names(df_new))), ]

  write.table(df_new, file=filename_output, sep="\t", row.names=FALSE)
}


