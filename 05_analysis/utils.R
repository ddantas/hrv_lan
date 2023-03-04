load_data <- function(filename){

  data = read.table(filename, header=TRUE, sep='	')

  return(data)
}

save_data <- function(df_new, filename){

  write.table(df_new, file=filename, sep="\t", row.names=FALSE)

  return(data)
}

concatenate_datasets <- function(folders, filename_output, ds_files, ds_flow_files = NULL) {

  columns = c("folder", "annotator", "block", "label", "time",
              "IsImit", "IsSync", "Imitator", "Model",
              "hr_subj1_linear",     "hr_subj2_linear",     "hr_subj1_nearest",     "hr_subj2_nearest",
              "hr_subj1_ecg_linear", "hr_subj2_ecg_linear", "hr_subj1_ecg_nearest", "hr_subj2_ecg_nearest",
              "rr_subj1_linear",     "rr_subj2_linear",     "rr_subj1_nearest",     "rr_subj2_nearest",
              "rr_subj1_ecg_linear", "rr_subj2_ecg_linear", "rr_subj1_ecg_nearest", "rr_subj2_ecg_nearest",
              "nn_subj1_ecg_linear", "nn_subj2_ecg_linear", "nn_subj1_ecg_nearest", "nn_subj2_ecg_nearest",
              "msg1", "msg2", "Gender", "Exp")
  set_male = c("b005", "b006", "b007", "b008",
               "b009", "b010", "b011", "b012",
               "b013", "b014", "b015", "b016",
               "b021", "b022", "b023", "b024",
               "b025", "b026", "b027", "b028",
               "b033", "b034", "b035", "b036")
  columns_flow = c("flow_l_cx", "flow_l_cy", "flow_l_vx", "flow_l_vy", "flow_l_npix", "flow_l_npix_filt",
                   "flow_r_cx", "flow_r_cy", "flow_r_vx", "flow_r_vy", "flow_r_npix", "flow_r_npix_filt")

  print(columns)

  n_cols_deriv = 8
  if (is.null(ds_flow_files))
  {
    final_df = data.frame(matrix(nrow=0, ncol=length(columns) + n_cols_deriv))
  }
  else
  {
    final_df = data.frame(matrix(nrow=0, ncol=length(columns) + n_cols_deriv + 2*length(columns_flow)))
  }
  #colnames(final_df) = columns
  for (default_ds_subpath in ds_files)
  {
    writeLines(paste("File: ", default_ds_subpath))

    for (f in folders){
      writeLines(paste("Folder: ", f))
      ds_path = file.path(f, default_ds_subpath)
      options(stringsAsFactors=FALSE)
      df = load_data(ds_path)
      options(stringsAsFactors=TRUE)

      # Changing NA to ""
      #df2[is.na(df2$IsImit), 'IsImit'] = ""
      #df2[is.na(df2$IsSync), 'IsSync'] = ""
      #df2[is.na(df2$Imitator), 'Imitator'] = ""
      #df2[is.na(df2$Model), 'Model'] = ""

      # If IsImit is TRUE, Imitator must not be NA
      n = length(df[(!is.na(df$IsImit) & df$IsImit == TRUE & is.na(df$Imitator)), "IsSync"])
      if (n > 0)
      {
        writeLines(paste("Found", n, "lines with IsImit = TRUE and Imitator == NA. Fatal error. Please review annotation..."))
        writeLines("")
        stop
      }
      # If IsImit is TRUE, IsSync must not be NA
      n = length(df[(!is.na(df$IsImit) & df$IsImit == TRUE & is.na(df$IsSync)), "IsSync"])
      if (n > 0)
      {
        writeLines(paste("Found", n, "lines with IsImit = TRUE and IsSync == NA. Correcting..."))
        writeLines("")
        df[(!is.na(df$IsImit) & df$IsImit == FALSE & is.na(df$IsSync)), "IsSync"] = FALSE
      }
      # If IsImit is FALSE, IsSync must be NA
      n = length(df[(!is.na(df$IsImit) & df$IsImit == FALSE & !is.na(df$IsSync)), "IsSync"])
      if (n > 0)
      {
        writeLines(paste("Found", n, "lines with IsImit = FALSE and IsSync != NA. Correcting..."))
        writeLines("")
        df[(!is.na(df$IsImit) & df$IsImit == FALSE & !is.na(df$IsSync)), "IsSync"] = NA
      }
      # If IsImit is FALSE, Imitator must be NA
      n = length(df[(!is.na(df$IsImit) & df$IsImit == FALSE & !is.na(df$Imitator)), "IsSync"])
      if (n > 0)
      {
        writeLines(paste("Found", n, "lines with IsImit = FALSE and Imitator != NA. Correcting..."))
        writeLines("")
        df[(!is.na(df$IsImit) & df$IsImit == FALSE & !is.na(df$Imitator)), "Imitator"] = NA
      }
      # If IsImit is FALSE, Model must be NA
      n = length(df[(!is.na(df$IsImit) & df$IsImit == FALSE & !is.na(df$Model)), "IsSync"])
      if (n > 0)
      {
        writeLines(paste("Found", n, "lines with IsImit = FALSE and Model != NA. Correcting..."))
        writeLines("")
        df[(!is.na(df$IsImit) & df$IsImit == FALSE & !is.na(df$Model)), "Model"] = NA
      }
      # If IsImit is NA, Imitator must be NA
      n = length(df[(is.na(df$IsImit) & !is.na(df$Imitator)), "IsSync"])
      if (n > 0)
      {
        writeLines(paste("Found", n, "lines with IsImit = NA and Imitator != NA. Correcting..."))
        writeLines("")
        df[(is.na(df$IsImit) & !is.na(df$Imitator)), "Imitator"] = NA
      }
      # If IsImit is NA, Model must be NA
      n = length(df[(is.na(df$IsImit) & !is.na(df$Model)), "IsSync"])
      if (n > 0)
      {
        writeLines(paste("Found", n, "lines with IsImit = NA and Model != NA. Correcting..."))
        writeLines("")
        df[(is.na(df$IsImit) & !is.na(df$Model)), "Model"] = NA
      }
      # Fill Model column
      df$Model = 3 - df$Imitator
      # Fill Gender column
      if (df$folder %in% set_male)
      {
        df$Gender = "M"
      }
      else
      {
        df$Gender = "F"
      }
      # Update label with Video
      df[df$label == "NVNM" & df$time < 100, "label"] = "Video"
      # Fill Exp column
      df$Exp = sprintf("%02d", floor((as.integer(substr(df$folder, 2, 5)) - 1) / 4) + 1)
      # Fill derivative  columns
      df$d_hr_subj1_linear      = derivate(df$hr_subj1_linear)
      df$d_hr_subj2_linear      = derivate(df$hr_subj2_linear)
      df$d_hr_subj1_nearest     = derivate(df$hr_subj1_nearest)
      df$d_hr_subj2_nearest     = derivate(df$hr_subj2_nearest)
      df$d_hr_subj1_ecg_linear  = derivate(df$hr_subj1_ecg_linear)
      df$d_hr_subj2_ecg_linear  = derivate(df$hr_subj2_ecg_linear)
      df$d_hr_subj1_ecg_nearest = derivate(df$hr_subj1_ecg_nearest)
      df$d_hr_subj2_ecg_nearest = derivate(df$hr_subj2_ecg_nearest)

      df$d_rr_subj1_linear      = derivate(df$rr_subj1_linear)
      df$d_rr_subj2_linear      = derivate(df$rr_subj2_linear)
      df$d_rr_subj1_nearest     = derivate(df$rr_subj1_nearest)
      df$d_rr_subj2_nearest     = derivate(df$rr_subj2_nearest)
      df$d_rr_subj1_ecg_linear  = derivate(df$rr_subj1_ecg_linear)
      df$d_rr_subj2_ecg_linear  = derivate(df$rr_subj2_ecg_linear)
      df$d_rr_subj1_ecg_nearest = derivate(df$rr_subj1_ecg_nearest)
      df$d_rr_subj2_ecg_nearest = derivate(df$rr_subj2_ecg_nearest)

      df$d_nn_subj1_ecg_linear  = derivate(df$nn_subj1_ecg_linear)
      df$d_nn_subj2_ecg_linear  = derivate(df$nn_subj2_ecg_linear)
      df$d_nn_subj1_ecg_nearest = derivate(df$nn_subj1_ecg_nearest)
      df$d_nn_subj2_ecg_nearest = derivate(df$nn_subj2_ecg_nearest)

      # Fill columns with optical flow data
      if (!is.null(ds_flow_files))
      {
        n_rows = nrow(df)
        subj = 1
        for (flow_file in ds_flow_files)
        {
          str_subj = paste("subj", subj, sep="")
          if (!grep(str_subj, flow_file))
          {
            writeLines(paste("Fatal error in concatenate_datasets: expected", str_subj, "in filename", flow_file))
            writeLines("")
            stop
          }
          writeLines(paste("Flow file: ", flow_file))
          columns_flow_subj = paste(str_subj, "_", columns_flow, sep="")
          flow_path = file.path(f, flow_file)
          if (file.exists(flow_path))
          {
            df_flow = load_data(flow_path)
            df_flow = df_flow[1:n_rows,]
          }
          else
          {
            writeLines(paste("File", flow_path, "does not exist. Filling with zeroes."))
            mat = matrix(0, nrow=n_rows, ncol=length(columns_flow_subj))
            df_flow = as.data.frame(mat)
          }
          colnames(df_flow) = columns_flow_subj
          df = cbind(df, df_flow)
          subj = subj + 1
        }
      }
      # End
      # Fill columns with optical flow data

      final_df = rbind(final_df, df)
    }
  }
  final_df$block <- as.character(final_df$block)
  final_df$block[final_df$label == "Prelude"] = "block0"
  final_df$block[final_df$block == "block2"] = "block3"
  final_df$block[final_df$label == "Interlude"] = "block2"
  final_df$block <- as.factor(final_df$block)

  #final_df[is.na(final_df$IsImit), 'IsImit'] = ""
  #final_df[is.na(final_df$IsSync), 'IsSync'] = ""
  #final_df[is.na(final_df$Imitator), 'Imitator'] = ""
  #final_df[is.na(final_df$Model), 'Model'] = ""
  #df2[is.na(df2$IsImit), 'IsImit'] = ""
  #df2[is.na(df2$IsSync), 'IsSync'] = ""
  #df2[is.na(df2$Imitator), 'Imitator'] = ""
  #df2[is.na(df2$Model), 'Model'] = ""

  print(colnames(final_df))
  
  write.table(final_df, file=filename_output, sep="\t", row.names=FALSE)
}

get_folder_names <- function(df) {
  return(unique(df$folder))
}


# Return derivative or input array
derivate <- function(arr)
{
  arr2 = c(arr[-1], tail(arr, n=1))
  result = arr2 - arr
  return(result)
}
