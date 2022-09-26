dataset_role <- function(filename_input, filename_output)
{
  df = load_data(filename_input)

  df1 = df[!is.na(df$Imitator) & df$Imitator == 1,
           c("folder", "annotator", "block", "label", "time", "IsImit", "IsSync", "Imitator", "Model",
             "hr_subj1_linear", "hr_subj2_linear", "hr_subj1_nearest", "hr_subj2_nearest",
             "hr_subj1_ecg_linear", "hr_subj2_ecg_linear", "hr_subj1_ecg_nearest", "hr_subj2_ecg_nearest", "msg1", "msg2", "Gender", "Exp",
             "d_hr_subj1_linear", "d_hr_subj2_linear", "d_hr_subj1_nearest", "d_hr_subj2_nearest",
             "d_hr_subj1_ecg_linear", "d_hr_subj2_ecg_linear", "d_hr_subj1_ecg_nearest", "d_hr_subj2_ecg_nearest",
             "subj1_flow_l_cx", "subj1_flow_l_cy", "subj1_flow_l_vx", "subj1_flow_l_vy", "subj1_flow_l_npix", "subj1_flow_l_npix_filt",
             "subj1_flow_r_cx", "subj1_flow_r_cy", "subj1_flow_r_vx", "subj1_flow_r_vy", "subj1_flow_r_npix", "subj1_flow_r_npix_filt",
             "subj2_flow_l_cx", "subj2_flow_l_cy", "subj2_flow_l_vx", "subj2_flow_l_vy", "subj2_flow_l_npix", "subj2_flow_l_npix_filt",
             "subj2_flow_r_cx", "subj2_flow_r_cy", "subj2_flow_r_vx", "subj2_flow_r_vy", "subj2_flow_r_npix", "subj2_flow_r_npix_filt")]

  df2 = df[!is.na(df$Imitator) & df$Imitator == 2,
           c("folder", "annotator", "block", "label", "time", "IsImit", "IsSync", "Imitator", "Model",
             "hr_subj2_linear", "hr_subj1_linear", "hr_subj2_nearest", "hr_subj1_nearest",
             "hr_subj2_ecg_linear", "hr_subj1_ecg_linear", "hr_subj2_ecg_nearest", "hr_subj1_ecg_nearest", "msg2", "msg1", "Gender", "Exp",
             "d_hr_subj2_linear", "d_hr_subj1_linear", "d_hr_subj2_nearest", "d_hr_subj1_nearest",
             "d_hr_subj2_ecg_linear", "d_hr_subj1_ecg_linear", "d_hr_subj2_ecg_nearest", "d_hr_subj1_ecg_nearest",
             "subj2_flow_l_cx", "subj2_flow_l_cy", "subj2_flow_l_vx", "subj2_flow_l_vy", "subj2_flow_l_npix", "subj2_flow_l_npix_filt",
             "subj2_flow_r_cx", "subj2_flow_r_cy", "subj2_flow_r_vx", "subj2_flow_r_vy", "subj2_flow_r_npix", "subj2_flow_r_npix_filt",
             "subj1_flow_l_cx", "subj1_flow_l_cy", "subj1_flow_l_vx", "subj1_flow_l_vy", "subj1_flow_l_npix", "subj1_flow_l_npix_filt",
             "subj1_flow_r_cx", "subj1_flow_r_cy", "subj1_flow_r_vx", "subj1_flow_r_vy", "subj1_flow_r_npix", "subj1_flow_r_npix_filt")]

  colnames_new = c("folder", "annotator", "block", "label", "time", "IsImit", "IsSync", "Imitator", "Model",
             "hr_subjI_linear", "hr_subjM_linear", "hr_subjI_nearest", "hr_subjM_nearest",
             "hr_subjI_ecg_linear", "hr_subjM_ecg_linear", "hr_subjI_ecg_nearest", "hr_subjM_ecg_nearest", "msgI", "msgM", "Gender", "Exp",
             "d_hr_subjI_linear", "d_hr_subjM_linear", "d_hr_subjI_nearest", "d_hr_subjM_nearest",
             "d_hr_subjI_ecg_linear", "d_hr_subjM_ecg_linear", "d_hr_subjI_ecg_nearest", "d_hr_subjM_ecg_nearest",
             "subjI_flow_l_cx", "subjI_flow_l_cy", "subjI_flow_l_vx", "subjI_flow_l_vy", "subjI_flow_l_npix", "subjI_flow_l_npix_filt",
             "subjI_flow_r_cx", "subjI_flow_r_cy", "subjI_flow_r_vx", "subjI_flow_r_vy", "subjI_flow_r_npix", "subjI_flow_r_npix_filt",
             "subjM_flow_l_cx", "subjM_flow_l_cy", "subjM_flow_l_vx", "subjM_flow_l_vy", "subjM_flow_l_npix", "subjM_flow_l_npix_filt",
             "subjM_flow_r_cx", "subjM_flow_r_cy", "subjM_flow_r_vx", "subjM_flow_r_vy", "subjM_flow_r_npix", "subjM_flow_r_npix_filt")


  colnames(df1) = colnames_new
  colnames(df2) = colnames_new
  df_new = rbind(df1, df2)
  df_new = df_new[order(as.numeric(row.names(df_new))), ]

  write.table(df_new, file=filename_output, sep="\t", row.names=FALSE)
}


