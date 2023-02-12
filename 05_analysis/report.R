report <- function(inputFile, outputFile, outputDir, df, df_stack, df_role, confidence=0.95, prompt=1)
{
  source('print_separator.R')
  source('generate_summary.R')
  source('plot_tc_distribution.R')
  source('plot_tc_correlation.R')
  source('report_tc_test.R')
  source('plot_correlationts.R')
  source('report_pdc.R')
  source('report_pdc_new.R')
  source('plot_pdc_df.R')
  source('plot_pdc.R')
  source('print_table_html.R')
  source('pval_color.R')
  source('corr_color.R')
  source('script.R')
  source('plot_hist.R')

  DANTAS = FALSE
  DANTAS_PDC = TRUE
  DANTAS_CORR = TRUE
  DANTAS_REGR = TRUE
  FELIPE = FALSE
  
  outputFullname = paste(outputDir, "/", outputFile, sep="")
  sink(file=NULL)
  sink(file=outputFullname, append=FALSE, split=TRUE)
  #outputDir = ""

  ##########
  writeLines("<tt><pre>");
  print_separator()

  writeLines(paste("input file:  ", inputFile));
  writeLines(paste("output file: ", outputFile));
  writeLines(paste("output dir:  ", outputDir));
  writeLines("...")
  writeLines("")

  ##########
  options(width = 200)
  prompt = 0

  ##########
  print_separator()
  writeLines(paste("<h2>Descriptive statistics in original dataset</h2>", sep=""))

  cols = c(seq(10, 25), seq(30, 45))

  ##########
  writeLines("...")
  writeLines("Descriptive statistics from each column in original dataset")
  writeLines("")

  print(generate_summary(df[cols]))
  writeLines("")

  ##########
  print_separator()
  writeLines(paste("<h2>Distributions of Test and Control groups in original dataset</h2>", sep=""))

  #rows
  conditions = rbind(
    c("df$folder != ''",                                  "all",             "Subject 1", "Subject 2"),

    c("df$label == 'NVNM'",                               "label_nvnm",      "Subject 1", "Subject 2"),
    c("df$label == 'NVM'",                                "label_nvm",       "Subject 1", "Subject 2"),
    c("df$label == 'Prelude'",                            "label_prelude",   "Subject 1", "Subject 2"),
    c("df$label == 'Interlude'",                          "label_interlude", "Subject 1", "Subject 2"),

    c("df$folder %in% c('b001', 'b002', 'b003', 'b004')", "ex01",            "Subject 1", "Subject 2"),
    c("df$folder %in% c('b009', 'b010', 'b011', 'b012')", "ex03",            "Subject 1", "Subject 2"),
    c("df$folder %in% c('b013', 'b014', 'b015', 'b016')", "ex04",            "Subject 1", "Subject 2"),
    c("df$folder %in% c('b017', 'b018', 'b019', 'b020')", "ex05",            "Subject 1", "Subject 2"),
    c("df$folder %in% c('b021', 'b022', 'b023', 'b024')", "ex06",            "Subject 1", "Subject 2"),
    c("df$folder %in% c('b025', 'b026', 'b027', 'b028')", "ex07",            "Subject 1", "Subject 2"),
    c("df$folder %in% c('b029', 'b030', 'b031', 'b032')", "ex08",            "Subject 1", "Subject 2"),
    c("df$folder %in% c('b033', 'b034', 'b035', 'b036')", "ex09",            "Subject 1", "Subject 2"),
    c("df$folder %in% c('b037', 'b038', 'b039', 'b040')", "ex10",            "Subject 1", "Subject 2"),
    c("df$folder %in% c('b041', 'b042', 'b043', 'b044')", "ex11",            "Subject 1", "Subject 2"),
    c("df$folder %in% c('b045', 'b046', 'b047', 'b048')", "ex12",            "Subject 1", "Subject 2")  )

  cond_folder = rbind(
    c("df$folder %in% c('b001', 'b002', 'b003', 'b004')", "ex01"),
    c("df$folder %in% c('b009', 'b010', 'b011', 'b012')", "ex03"),
    c("df$folder %in% c('b013', 'b014', 'b015', 'b016')", "ex04"),
    c("df$folder %in% c('b017', 'b018', 'b019', 'b020')", "ex05"),
    c("df$folder %in% c('b021', 'b022', 'b023', 'b024')", "ex06"),
    c("df$folder %in% c('b025', 'b026', 'b027', 'b028')", "ex07"),
    c("df$folder %in% c('b029', 'b030', 'b031', 'b032')", "ex08"),
    c("df$folder %in% c('b033', 'b034', 'b035', 'b036')", "ex09"),
    c("df$folder %in% c('b037', 'b038', 'b039', 'b040')", "ex10"),
    c("df$folder %in% c('b041', 'b042', 'b043', 'b044')", "ex11"),
    c("df$folder %in% c('b045', 'b046', 'b047', 'b048')", "ex12") )
  cond_label = rbind(
    c("df$label == 'SI'",                                 "si"),
    c("df$label == 'IImitator1'",                         "iimitator1"),
    c("df$label == 'IImitator2'",                         "iimitator2"),
    c("df$label == 'Video'",                              "video"),
    c("df$label == 'NVNM'",                               "nvnm"),
    c("df$label == 'NVM'",                                "nvm"),
    c("df$label == 'Prelude'",                            "prelude"),
    c("df$label == 'Interlude'",                          "interlude")  )
  cond_annot = rbind(
    c("df$annotator == 'dd'",                             "dd"),
    c("df$annotator == 'jf'",                             "jf")  )

  df_pvals = data.frame(folder = numeric(0),
                        label = numeric(0),
                        col = numeric(0),
                        to_col = numeric(0),
                        val = numeric(0))

  ic_min = nrow(conditions) + 1
  ica = 1
  #for (icf in seq(1))
  for (icf in seq(1, nrow(cond_folder)))
  {
    for (icl in seq(1, nrow(cond_label)))
    {
      cond_new = c(paste(cond_folder[icf, 1], ' & ', cond_label[icl, 1], ' & ',  cond_annot[ica, 1], sep=''),
                   paste(cond_folder[icf, 2],  '_',  cond_label[icl, 2],  '_',   cond_annot[ica, 2], sep=''),
                   "Subject 1", "Subject 2")
      conditions = rbind(conditions, cond_new)
    }
  }
  rownames(conditions) = NULL

  ## First experiment only
  #for ( ic in seq(ic_min, ic_min + nrow(cond_label) - 1) )
  ## All 11 experiments
  for (ic in seq(1, dim(conditions)[1]))
  {
    rows = eval(parse(text=conditions[ic, 1]))
    exp_label = conditions[ic, 2]
    str1 = conditions[ic, 3]
    str2 = conditions[ic, 4]
    col_hand1 = "subj1_flow_l_cx"
    col_hand2 = "subj2_flow_l_cx"
    hand_pos_data1 = df[rows, col_hand1, drop=FALSE]
    hand_pos_data2 = df[rows, col_hand2, drop=FALSE]
    subj = c(1, 2)

    IsImit = df[rows, "IsImit", drop=FALSE]
    IsImit[is.na(IsImit)] = FALSE
    n_imit = sum(IsImit*1)
    val_imit = n_imit / nrow(IsImit)
    IsSync = df[rows, "IsSync", drop=FALSE]
    IsSync[is.na(IsSync)] = FALSE
    n_sync = sum(IsSync*1)
    val_sync = n_sync / n_imit
    
    for (i in 1)
    #for (i in c(1, 17))
    #for (i in seq(1, length(cols), by = 2))
    {
      col1 = names(df)[cols[i]]
      col2 = names(df)[cols[i + 1]]
      data1 = df[rows, col1, drop=FALSE]
      data2 = df[rows, col2, drop=FALSE]
      str_title = paste("exp_", exp_label, "__", col1, "_vs_", col2, sep="")
      writeLines("...")
      writeLines(paste("<h3>", str_title, "</h3>", sep=""))

      if (n_imit == 0)
      {
        val_sync = 0
      }

      str_title = paste("exp_", exp_label, "__", col1, "_vs_", col2, sep="")
      if (DANTAS)
      {
        report_tc_test(data1, data2, str_title, confidence=0.95, str1, str2)
        plot_tc_distribution(data1, data2, str_title, prompt, outputDir, "Heart rate", str1, str2)
      }
      if ( (DANTAS_PDC | DANTAS_CORR) & ic >= ic_min)
      {
      }
      if (DANTAS_CORR & ic >= ic_min)
      {
        folder = strsplit(exp_label, '_')[[1]][1]
        label = strsplit(exp_label, '_')[[1]][2]
        #cor_val = orig <- abs(cor(data1, data2, method="spearman"))
        plot_tc_correlation(data1, data2, str_title, prompt, outputDir)
        result = correlationts(data1[,1], data2[,1])
        df_corr = cbind(data1, data2, hand_pos_data1, hand_pos_data2)
        result2 = covar(df_corr);
        print(result2)
        writeLines(paste("Regarding correlation between Subject ", subj[1], " and Subject ", subj[2], ":\n", sep = ""))
        writeLines(paste("Correlation      = ", result$coef))
        writeLines(paste("Abs. Correlation = ", result$abs_coef))
        writeLines(paste("P-value          = ", result$p.value))
        writeLines(paste("IsImit           = ", val_imit))
        writeLines(paste("IsSync           = ", val_sync))
        writeLines(paste("Covar            = ", abs(result2$P[1, 2])))
        writeLines(paste("T-value          = ", abs(result2$tvalue[1, 2])))
        df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col1, "corr12", result$coef)
        df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col1, "abscorr12", result$abs_coef)
        df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col1, "pval12", result$p.value)
        df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col1, "IsImit", val_imit)
        df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col1, "IsSync", val_sync)
        df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col1, "covar12", abs(result2$P[1, 2]))
        df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col1, "tvalue12", abs(result2$tvalue[1, 2]))
      }
      if (DANTAS_REGR & ic >= ic_min)
      {
        folder = strsplit(exp_label, '_')[[1]][1]
        label = strsplit(exp_label, '_')[[1]][2]
        str_title = paste("exp_", exp_label, "__", col1, "_vs_", col2, "_residual", sep="")
        writeLines("...")
        writeLines(paste("<h3>", str_title, "</h3>", sep=""))

        hand_regr_data1 = df[rows, c("subj1_flow_l_cx", "subj1_flow_l_cy", "subj1_flow_r_cx", "subj1_flow_r_cy"), drop=FALSE]
        hand_regr_data2 = df[rows, c("subj2_flow_l_cx", "subj2_flow_l_cy", "subj2_flow_r_cx", "subj2_flow_r_cy"), drop=FALSE]
        df_regr = cbind(data1, data2, hand_regr_data1, hand_regr_data2)

        formula_data1 = "hr_subj1_linear ~ subj1_flow_l_cx + subj1_flow_l_cy + subj1_flow_r_cx + subj1_flow_r_cy + subj2_flow_l_cx + subj2_flow_l_cy + subj2_flow_r_cx + subj2_flow_r_cy"
        glm_data1 = glm(formula_data1, df_regr, family=gaussian())
        residual1 = glm_data1$residuals

        formula_data2 = "hr_subj2_linear ~ subj1_flow_l_cx + subj1_flow_l_cy + subj1_flow_r_cx + subj1_flow_r_cy + subj2_flow_l_cx + subj2_flow_l_cy + subj2_flow_r_cx + subj2_flow_r_cy"
        glm_data2 = glm(formula_data2, df_regr, family=gaussian())
        residual2 = glm_data2$residuals

        #formula2_data1 = "hr_subj1_linear ~ subj1_flow_l_cx + subj1_flow_l_cy + subj2_flow_l_cx + subj2_flow_l_cy"
        #glm2_data2 = glm(formula2_data1, df_regr, family=gaussian())

        plot_tc_correlation(data.frame(residual1), data.frame(residual2), str_title, prompt, outputDir)

        result = correlationts(residual1, residual2)
        writeLines(paste("Regarding correlation of residuals between Subject ", subj[1], " and Subject ", subj[2], ":\n", sep = ""))
        writeLines(paste("Residual Correlation      = ", result$coef))
        writeLines(paste("Residual Abs. Correlation = ", result$abs_coef))
        writeLines(paste("Residual P-value          = ", result$p.value))
        df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col1, "resid_corr12", result$coef)
        df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col1, "resid_abscorr12", result$abs_coef)
        df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col1, "resid_pval12", result$p.value)

      }
      if (DANTAS_PDC & ic >= ic_min)
      {
        if (!grepl("lude", str_title)) # Not prelude or interlude
        {
          df_pdc = cbind(data1, data2, hand_pos_data2)
        } else {
          df_pdc = cbind(data1, data2)
        }
        str_title_1 = paste(str_title, "_1", sep="")
        res = plot_pdc_df(df_pdc, str_title_1, outputDir)
        folder = strsplit(exp_label, "_")[[1]][1]
        label = strsplit(exp_label, "_")[[1]][2]

        report_pdc_new(res, c(1, 2), subj, str_title_1)
        df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col1, "granger12h", res$p.value[1, 2])
        if (!grepl("lude", str_title)) # Not prelude or interlude
        {
          report_pdc_new(res, c(1, 3), subj, str_title_1)
          df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col1, "granger12f", res$p.value[1, 3])
        }

        subj = c(2, 1)
        hand_pos_data2 = df[rows, col_hand2, drop=FALSE]
        if (!grepl("lude", str_title)) # Not prelude or interlude
        {
          df_pdc = cbind(data2, data1, hand_pos_data1)
        } else {
          df_pdc = cbind(data2, data1)
        }
        str_title_2 = paste(str_title, "_2", sep="")
        res = plot_pdc_df(df_pdc, str_title_2, outputDir)
        folder = strsplit(exp_label, "_")[[1]][1]
        label = strsplit(exp_label, "_")[[1]][2]

        report_pdc_new(res, c(1, 2), subj, str_title_2)
        df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col2, "granger21h", res$p.value[1, 2])
        if (!grepl("lude", str_title)) # Not prelude or interlude
        {
          report_pdc_new(res, c(1, 3), subj, str_title_2)
          df_pvals[nrow(df_pvals) + 1,] = list(folder, label, col2, "granger21f", res$p.value[1, 3])
        }
      }
    }
  }
  if (DANTAS_CORR)
  {
    writeLines(paste("<h3>Wilcoxon paired P-values for every combination of label</h3>", sep=""))
    arr_label = c("iimitator1", "iimitator2", "si", "video", "nvm", "nvnm", "interlude", "prelude")
    writeLines(paste("<table border=1>", sep=""))
    writeLines(paste("  <tr>", sep=""))
    writeLines(paste("    <td></td>", sep=""))
    for (j in seq(length(arr_label)))
    {
      str_j = arr_label[j]
      writeLines(paste("    <td>", str_j, "</td>", sep=""))
    }
    writeLines(paste("  </tr>", sep=""))
    for (i in seq(length(arr_label)))
    {
      ##########
      str_i = arr_label[i]
      writeLines(paste("  <tr>", sep=""))
      writeLines(paste("    <td>", str_i, "</td>", sep=""))
      arr_i = df_pvals[df_pvals$to_col=="abscorr12" & df_pvals$label==str_i, "val"]
      for (j in seq(length(arr_label)))
      {
        if (i <= j)
        {
          writeLines(paste("    <td>-</td>", sep=""))
          next
        }
        str_j = arr_label[j]
        arr_j = df_pvals[df_pvals$to_col=="abscorr12" & df_pvals$label==str_j, "val"]
        result = wilcox.test(arr_i, arr_j, paired=TRUE)
        pval = result$p.value
        pval_color
        writeLines(paste("    <td><div style='color:", pval_color(pval), "'>", pval, "</div></td>", sep=""))
      }
      writeLines(paste("  </tr>", sep=""))
    }
    writeLines(paste("</table>", sep=""))
    ##########
    str_title = "boxplot_every_label_combination"
    outputSubdir = "plot_report"
    if (outputDir != "")
    {
      outputFile     = paste("boxplot_report_", str_title, ".png", sep="")
      outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
      png(outputFullname, width=640);
    }
    x = ggplot(df_pvals[df_pvals$to_col == "abscorr12",], aes(x=label, y=.data$val, color=label)) + geom_boxplot()
    grid::grid.draw(x)
    if (outputDir != "")
    {
      writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
      writeLines("")
      writeLines("")
      dev.off()
    }
    ##########
  }

  #print(df_pvals)
  
  if (DANTAS_PDC | DANTAS_CORR | DANTAS_REGR)
  {
    df_pvals_wide = reshape(df_pvals, v.names="val", timevar="folder", idvar=c("label", "col", "to_col"), direction="wide")
    #print(df_pvals_wide)
    writeLines("...")
    writeLines(paste("<h3>P-values</h3>", sep=""))
    #print_table_html(df_pvals_wide, pval_color)
    print_table_html(df_pvals_wide, corr_color)
    #for (i in seq(1, length(cols), by = 2))
    outputSubdir = "plot_hist"
    for (i in seq(1))
    {
      col1 = names(df)[cols[i]]
      col2 = names(df)[cols[i+1]]

      if (DANTAS_PDC | DANTAS_CORR)
      {
        writeLines("...")
        writeLines(paste("<h3>Spearman correlation from ", col1, " to ", col2, "</h3>", sep=""))
        df_tmp = df_pvals_wide[df_pvals_wide$col == col1 & df_pvals_wide$to_col == "corr12",]
        print_table_html(df_tmp, corr_color)
        df_hist = data.frame(df_pvals[df_pvals$to_col=="corr12", "val"])
        str_title = paste(col1, "_", "corr12", sep="")
        plot_hist(df_hist, str_title, prompt, outputDir, outputSubdir, xlabel="Spearman correlation", label1="Data")

        writeLines("...")
        writeLines(paste("<h3>Abs. Spearman correlation from ", col1, " to ", col2, "</h3>", sep=""))
        df_tmp = df_pvals_wide[df_pvals_wide$col == col1 & df_pvals_wide$to_col == "abscorr12",]
        print_table_html(df_tmp, corr_color)
        df_hist = data.frame(df_pvals[df_pvals$to_col=="abscorr12", "val"])
        str_title = paste(col1, "_", "abscorr12", sep="")
        plot_hist(df_hist, str_title, prompt, outputDir, outputSubdir, xlabel="Abs. Spearman correlation", label1="Data")

        writeLines("...")
        writeLines(paste("<h3>P-values of Spearman correlation from ", col1, " to ", col2, "</h3>", sep=""))
        df_tmp = df_pvals_wide[df_pvals_wide$col == col1 & df_pvals_wide$to_col == "pval12",]
        print_table_html(df_tmp, pval_color)
        df_hist = data.frame(df_pvals[df_pvals$to_col=="pval12", "val"])
        str_title = paste(col1, "_", "pval12", sep="")
        plot_hist(df_hist, str_title, prompt, outputDir, outputSubdir, xlabel="P-values of Spearman correlation", label1="Data")

        writeLines("...")
        writeLines(paste("<h3>P-values of Granger causality from ", col1, " to ", col2, "</h3>", sep=""))
        df_tmp = df_pvals_wide[df_pvals_wide$col == col1 & df_pvals_wide$to_col == "granger12h",]
        print_table_html(df_tmp, pval_color)

        writeLines("...")
        writeLines(paste("<h3>P-values of Granger causality from ", col2, " to ", col1, "</h3>", sep=""))
        df_tmp = df_pvals_wide[df_pvals_wide$col == col2 & df_pvals_wide$to_col == "granger21h",]
        print_table_html(df_tmp, pval_color)

        writeLines("...")
        writeLines(paste("<h3>P-values of Granger causality from ", col1, " to ", col_hand2, "</h3>", sep=""))
        df_tmp = df_pvals_wide[df_pvals_wide$col == col1 & df_pvals_wide$to_col == "granger12f",]
        print_table_html(df_tmp, pval_color)

        writeLines("...")
        writeLines(paste("<h3>P-values of Granger causality from ", col2, " to ", col_hand1, "</h3>", sep=""))
        df_tmp = df_pvals_wide[df_pvals_wide$col == col2 & df_pvals_wide$to_col == "granger21f",]
        print_table_html(df_tmp, pval_color)

        writeLines("...")
        writeLines(paste("<h3>COVAR</h3>", sep=""))
        df_tmp = df_pvals_wide[df_pvals_wide$col == col1 & df_pvals_wide$to_col == "covar12",]
        print_table_html(df_tmp, corr_color)

        writeLines("...")
        writeLines(paste("<h3>T-value</h3>", sep=""))
        df_tmp = df_pvals_wide[df_pvals_wide$col == col1 & df_pvals_wide$to_col == "tvalue12",]
        print_table_html(df_tmp, corr_color)
      }

      if (DANTAS_REGR)
      {
        writeLines("...")
        writeLines(paste("<h3>Residual Spearman correlation from ", col1, " to ", col2, "</h3>", sep=""))
        df_tmp = df_pvals_wide[df_pvals_wide$col == col1 & df_pvals_wide$to_col == "resid_corr12",]
        print_table_html(df_tmp, corr_color)
        df_hist = data.frame(df_pvals[df_pvals$to_col=="resid_corr12", "val"])
        str_title = paste(col1, "_", "resid_corr12", sep="")
        plot_hist(df_hist, str_title, prompt, outputDir, outputSubdir, xlabel="Spearman correlation", label1="Data")

        writeLines("...")
        writeLines(paste("<h3>Residual Abs. Spearman correlation from ", col1, " to ", col2, "</h3>", sep=""))
        df_tmp = df_pvals_wide[df_pvals_wide$col == col1 & df_pvals_wide$to_col == "resid_abscorr12",]
        print_table_html(df_tmp, corr_color)
        df_hist = data.frame(df_pvals[df_pvals$to_col=="resid_abscorr12", "val"])
        str_title = paste(col1, "_", "resid_abscorr12", sep="")
        plot_hist(df_hist, str_title, prompt, outputDir, outputSubdir, xlabel="Abs. Spearman correlation", label1="Data")

        writeLines("...")
        writeLines(paste("<h3>Residual P-values of Spearman correlation from ", col1, " to ", col2, "</h3>", sep=""))
        df_tmp = df_pvals_wide[df_pvals_wide$col == col1 & df_pvals_wide$to_col == "resid_pval12",]
        print_table_html(df_tmp, pval_color)
        df_hist = data.frame(df_pvals[df_pvals$to_col=="resid_pval12", "val"])
        str_title = paste(col1, "_", "resid_pval12", sep="")
        plot_hist(df_hist, str_title, prompt, outputDir, outputSubdir, xlabel="P-values of Spearman correlation", label1="Data")
      }
    }
  }

  save_data(df_pvals, "dataset_pvals.tsv")


  ##########
  print_separator()
  writeLines(paste("<h2>Descriptive statistics in stacked dataset</h2>", sep=""))

  ##########
  writeLines("...")
  writeLines("Descriptive statistics from each column in stacked dataset")
  writeLines("")

  cols_stack = c(seq(10, 13), seq(17, 20))
  print(generate_summary(df_stack[cols_stack]))
  writeLines("")

  ##########
  print_separator()
  writeLines(paste("<h2>Distributions of Test and Control groups in stacked dataset</h2>", sep=""))

  #rows
  conditions = rbind(
    c("df_stack$Gender == 'M'",         "df_stack$Gender == 'F'",         "gender",    "Male",       "Female",     "male",      "female"),

    c("df_stack$block == 'block0'",     "df_stack$block == 'block1'",     "block",     "Block 0",    "Block 1",    "block0",    "block1"),
    c("df_stack$block == 'block0'",     "df_stack$block == 'block2'",     "block",     "Block 0",    "Block 2",    "block0",    "block2"),
    c("df_stack$block == 'block0'",     "df_stack$block == 'block3'",     "block",     "Block 0",    "Block 3",    "block0",    "block3"),
    c("df_stack$block == 'block1'",     "df_stack$block == 'block2'",     "block",     "Block 1",    "Block 2",    "block1",    "block2"),
    c("df_stack$block == 'block1'",     "df_stack$block == 'block3'",     "block",     "Block 1",    "Block 3",    "block1",    "block3"),
    c("df_stack$block == 'block2'",     "df_stack$block == 'block3'",     "block",     "Block 2",    "Block 3",    "block2",    "block3"),

    c("df_stack$label == 'IImitator1'", "df_stack$label == 'IImitator2'", "label",     "Imitator 1", "Imitator 2", "imitator1", "imitator2"),
    c("df_stack$label == 'IImitator1'", "df_stack$label == 'SI'",         "label",     "Imitator 1", "SI",         "imitator1", "si"),
    c("df_stack$label == 'IImitator2'", "df_stack$label == 'SI'",         "label",     "Imitator 2", "SI",         "imitator2", "si"),
    c("df_stack$label == 'Prelude'",    "df_stack$label == 'Interlude'",  "label",     "Prelude",    "Interlude",  "prelude",   "interlude"),
    c("df_stack$label == 'NVNM'",       "df_stack$label == 'NVM'",        "label",     "NVNM",       "NVM",        "nvnm",      "nvm"),

    c("df_stack$IsImit == TRUE",        "df_stack$IsImit == FALSE",       "IsImit",    "True",       "False",      "true",      "false"),

    c("df_stack$IsSync == TRUE",        "df_stack$IsSync == FALSE",       "IsSync",    "True",       "False",      "true",      "false"),

    c("df_stack$annotator == 'dd'",     "df_stack$annotator == 'jf'",     "annotator", "dd",         "jf",         "dd",        "jf")         )

  for (ic in seq(1))
  #for (ic in seq(1, dim(conditions)[1]))
  {
    rows1 = eval(parse(text=conditions[ic, 1]))
    rows2 = eval(parse(text=conditions[ic, 2]))
    rows1[is.na(rows1)] = FALSE
    rows2[is.na(rows2)] = FALSE
    exp_label = conditions[ic, 3]
    str1 = conditions[ic, 4]
    str2 = conditions[ic, 5]
    label1 = conditions[ic, 6]
    label2 = conditions[ic, 7]
    
    for (i in seq(1, length(cols_stack)))
    {
      col_name = names(df_stack)[cols_stack[i]]
      data1 = df_stack[rows1, col_name, drop=FALSE]
      data2 = df_stack[rows2, col_name, drop=FALSE]
      str_title = paste("exp_", exp_label, "__", col_name, "__", label1, "_vs_", label2, sep="")
      writeLines("...")
      writeLines(paste("<h3>", str_title, "</h3>", sep=""))

      if (DANTAS)
      {
        report_tc_test(data1, data2, str_title, confidence=0.95, str1, str2)
        plot_tc_distribution(data1, data2, str_title, prompt, outputDir, "Heart rate", str1, str2)
      }
    }
  }

  ##########
  print_separator()
  writeLines(paste("<h2>Descriptive statistics in role dataset</h2>", sep=""))

  #cols = c(seq(10, 17), seq(21, 28))

  ##########
  writeLines("...")
  writeLines("Descriptive statistics from each column in role dataset")
  writeLines("")

  cols_role = c(seq(10, 17), seq(22, 29))
  print(generate_summary(df_role[cols_role]))
  writeLines("")

  ##########
  print_separator()
  writeLines(paste("<h2>Distributions of Test and Control groups in role dataset</h2>", sep=""))

  #rows
  conditions = rbind(
    c("df_role$folder != ''",           "all",              "Imitator", "Model"),
    c("df_role$Gender == 'M'",          "gender_male",      "Imitator", "Model"),
    c("df_role$Gender == 'F'",          "gender_female",    "Imitator", "Model"),

    c("df_role$label == 'SI'",          "label_si",         "Imitator", "Model"),
    c("df_role$label == 'IImitator1'",  "label_iimitator1", "Imitator", "Model"),
    c("df_role$label == 'IImitator2'",  "label_iimitator2", "Imitator", "Model"),

    #c("df_role$block == 'block0'",      "block0",           "Imitator", "Model"),
    c("df_role$block == 'block1'",      "block1",           "Imitator", "Model"),
    #c("df_role$block == 'block2'",      "block2",           "Imitator", "Model"),
    c("df_role$block == 'block3'",      "block3",           "Imitator", "Model"),

    c("df_role$IsSync == TRUE",         "issync_true",      "Imitator", "Model"),
    c("df_role$IsSync == FALSE",        "issync_false",     "Imitator", "Model"),

    c("df_role$annotator == 'dd'",      "annotator_dd",     "Imitator", "Model"),
    c("df_role$annotator == 'jf'",      "annotator_jf",     "Imitator", "Model")         )

  for (ic in seq(1))
  #for (ic in seq(1, dim(conditions)[1]))
  {
    rows = eval(parse(text=conditions[ic, 1]))
    rows[is.na(rows)] = FALSE
    exp_label = conditions[ic, 2]
    str1 = conditions[ic, 3]
    str2 = conditions[ic, 4]

    for (i in seq(1, length(cols), by = 2))
    {
      col1 = names(df_role)[cols[i]]
      col2 = names(df_role)[cols[i + 1]]
      data1 = df_role[rows, col1, drop=FALSE]
      data2 = df_role[rows, col2, drop=FALSE]
      str_title = paste("exp_", exp_label, "__", col1, "_vs_", col2, sep="")
      writeLines("...")
      writeLines(paste("<h3>", str_title, "</h3>", sep=""))

      if (DANTAS)
      {
        report_tc_test(data1, data2, str_title, confidence=0.95, str1, str2)
        plot_tc_distribution(data1, data2, str_title, prompt, outputDir, "Heart rate", str1, str2)
      }
    }
  }

  ##########
  print_separator()

  conditions = rbind(
    c("df$folder != ''"                                 , "all"),
    c("df$folder %in% c('b001', 'b002', 'b003', 'b004')", "ex01"),
    c("df$folder %in% c('b009', 'b010', 'b011', 'b012')", "ex03"),
    c("df$folder %in% c('b013', 'b014', 'b015', 'b016')", "ex04"),
    c("df$folder %in% c('b017', 'b018', 'b019', 'b020')", "ex05"),
    c("df$folder %in% c('b021', 'b022', 'b023', 'b024')", "ex06"),
    c("df$folder %in% c('b025', 'b026', 'b027', 'b028')", "ex07"),
    c("df$folder %in% c('b029', 'b030', 'b031', 'b032')", "ex08"),
    c("df$folder %in% c('b033', 'b034', 'b035', 'b036')", "ex09"),
    c("df$folder %in% c('b037', 'b038', 'b039', 'b040')", "ex10"),
    c("df$folder %in% c('b041', 'b042', 'b043', 'b044')", "ex11") )

  writeLines(paste("<h2>Correlation between time series on different phases of the experiment</h2>", sep=""))

  cols = c(seq(10, 17, 2))
  labels = unique(df$label)

  for (j in cols) {

    str_title = names(df)[j]
    str_title = gsub("hr_subj1_", "", str_title)

    writeLines(paste("<h3>Correlation for ", str_title, " data</h3>", sep=""))
    writeLines("...")

    for (ic in seq(1, dim(conditions)[1])) {

      rows = eval(parse(text=conditions[ic, 1]))
      exp = conditions[ic, 2]
      df_data = df[rows, ]
      col1 = names(df)[j]
      col2 = names(df)[j+1]
      data1 = df_data[, c(col1, "label")]
      data2 = df_data[, c(col2, "label")]
      str_title = paste(str_title, exp)
      # plot_correlationts(data1, data2, labels, exp, str_title, outputDir)

    }
  }

  ##########
  print_separator()

  cols = c(seq(10, 17), seq(21, 28))
  conditions = rbind(
    c("df$annotator == 'dd' & df$folder != ''",                  "all",  "Subject 1", "Subject 2"),
    c("df$annotator == 'dd' & df$folder %in% c('b002', 'b004')", "ex01", "Subject 1", "Subject 2"),
    c("df$annotator == 'dd' & df$folder %in% c('b010', 'b012')", "ex03", "Subject 1", "Subject 2"),
    c("df$annotator == 'dd' & df$folder %in% c('b014', 'b016')", "ex04", "Subject 1", "Subject 2"),
    c("df$annotator == 'dd' & df$folder %in% c('b018', 'b020')", "ex05", "Subject 1", "Subject 2"),
    c("df$annotator == 'dd' & df$folder %in% c('b022', 'b024')", "ex06", "Subject 1", "Subject 2"),
    c("df$annotator == 'dd' & df$folder %in% c('b026', 'b028')", "ex07", "Subject 1", "Subject 2"),
    c("df$annotator == 'dd' & df$folder %in% c('b030', 'b032')", "ex08", "Subject 1", "Subject 2"),
    c("df$annotator == 'dd' & df$folder %in% c('b034', 'b036')", "ex09", "Subject 1", "Subject 2"),
    c("df$annotator == 'dd' & df$folder %in% c('b038', 'b040')", "ex10", "Subject 1", "Subject 2"),
    c("df$annotator == 'dd' & df$folder %in% c('b042', 'b044')", "ex11", "Subject 1", "Subject 2") )
  
  for (ic in seq(2, dim(conditions)[1]))
  #for (ic in seq(1))
  {
    rows = eval(parse(text=conditions[ic, 1]))
    exp_label = conditions[ic, 2]
    str1 = conditions[ic, 3]
    str2 = conditions[ic, 4]

    hand_pos_data1 = df[rows, c("subj1_flow_l_cx", "subj1_flow_l_cy"), drop=FALSE]
    hand_pos_data2 = df[rows, c("subj2_flow_l_cx", "subj2_flow_l_cy"), drop=FALSE]
    
    for (i in seq(1, length(cols), by = 2))
    {
      col1 = names(df)[cols[i]]
      col2 = names(df)[cols[i + 1]]
      data1 = df[rows, col1, drop=FALSE]
      data2 = df[rows, col2, drop=FALSE]

      str_title = paste("exp_", exp_label, "__", col1, "_vs_", col2, sep="")
      writeLines("...")
      writeLines(paste("<h3>", str_title, "</h3>", sep=""))

      ## PDC
      if (FELIPE)
      {
        res = plot_pdc(data1, data2, hand_pos_data1, hand_pos_data2, str_title, outputDir)
        report_pdc(res, str_title)
      }
    }
  }
  ##########

  ###plot_tc_dispersion(test, control, cols_dispersion, prompt, outputDir)
  #plot_tc_dispersion(test, control, cols, prompt, outputDir)


  

  sink(file=NULL)
}
