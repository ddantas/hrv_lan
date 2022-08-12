dataset_stack <- function(filename_input, filename_output)
{
  df = load_data(filename_input)


  df1 = df[c("folder", "annotator", "block", "label", "time", "IsImit", "IsSync", "Imitator", "Model",
             "hr_subj1_linear", "hr_subj1_nearest", "hr_subj1_ecg_linear", "hr_subj1_ecg_nearest", "msg1", "Gender",
             "d_hr_subj1_linear", "d_hr_subj1_nearest", "d_hr_subj1_ecg_linear", "d_hr_subj1_ecg_nearest",
             "subj1_flow_l_cx", "subj1_flow_l_cy", "subj1_flow_l_vx", "subj1_flow_l_vy", "subj1_flow_l_npix", "subj1_flow_l_npix_filt",
             "subj1_flow_r_cx", "subj1_flow_r_cy", "subj1_flow_r_vx", "subj1_flow_r_vy", "subj1_flow_r_npix", "subj1_flow_r_npix_filt")]
  df2 = df[c("folder", "annotator", "block", "label", "time", "IsImit", "IsSync", "Imitator", "Model",
             "hr_subj2_linear", "hr_subj2_nearest", "hr_subj2_ecg_linear", "hr_subj2_ecg_nearest", "msg2", "Gender",
             "d_hr_subj2_linear", "d_hr_subj2_nearest", "d_hr_subj2_ecg_linear", "d_hr_subj2_ecg_nearest",
             "subj2_flow_l_cx", "subj2_flow_l_cy", "subj2_flow_l_vx", "subj2_flow_l_vy", "subj2_flow_l_npix", "subj2_flow_l_npix_filt",
             "subj2_flow_r_cx", "subj2_flow_r_cy", "subj2_flow_r_vx", "subj2_flow_r_vy", "subj2_flow_r_npix", "subj2_flow_r_npix_filt")]

  colnames_new = c("folder", "annotator", "block", "label", "time", "IsImit", "IsSync", "Imitator", "Model",
             "hr_linear", "hr_nearest", "hr_ecg_linear", "hr_ecg_nearest", "msg", "Gender",
             "d_hr_linear", "d_hr_nearest", "d_hr_ecg_linear", "d_hr_ecg_nearest",
             "flow_l_cx", "flow_l_cy", "flow_l_vx", "flow_l_vy", "flow_l_npix", "flow_l_npix_filt",
             "flow_r_cx", "flow_r_cy", "flow_r_vx", "flow_r_vy", "flow_r_npix", "flow_r_npix_filt")

  colnames(df1) = colnames_new
  colnames(df2) = colnames_new
  df_new = rbind(df1, df2)

  write.table(df_new, file=filename_output, sep="\t", row.names=FALSE)
}




