report_paper2 <- function(inputFile, outputFile, outputDir, df, df_pvals, df_pdc_pvals, confidence=0.95, prompt=1)
{

make_table_pool_pvals <- function(df_pdc_pvals)
{
  colnames_poolr = colnames(df_pdc_pvals)
  for (i in seq(ncol(df_pdc_pvals)))
  {
    pool = df_pdc_pvals[, i]

    df_poolr[i, "label"] = colnames_poolr[i]

    poolr_f = fisher(pool, batchsize = 10000)
    print(poolr_f)
    df_poolr[i, "fisher"] = poolr_f$p

    poolr_s = stouffer(pool, batchsize = 10000)
    print(poolr_s)
    df_poolr[i, "stoufer"] = poolr_s$p
  }
  print_table_html(df_poolr, pval_color)
  print_table_latex(df_poolr, pval_color, col_list=c(1,2,3))
}



  source('print_separator.R')
  source('same_mean_test.R')
  source('report_tc_test.R')
  source('pval_color.R')
  source('pval_star.R')
  source('print_table_html.R')
  source('print_table_latex.R')
  
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
  FIG1 = TRUE
  FIG12 = TRUE
  FIG3 = TRUE
  FIG4 = TRUE

  text_size = 20
  THEME = theme_bw()

  outputSubdir = "plot_paper2"
  if (outputDir != "")
  {
    dir.create(paste(outputDir, "/", outputSubdir, sep=""))
  }


  ##########
  arr_pval = c(0.15939771623433,  #video1
                 0.163697438246427, #nvm1
                 0.325250835004095, #nvnm11
                 0.005096150670058, #si1
                 0.253062542639005, #nvnm12
                 0.044655606679524, #ii1
                 0.029558487136295, #video2
                 0.227730096674434, #nvm2
                 0.858063531029954, #nvnm21
                 0.002378113900150, #si2
                 0.000727984292684, #nvnm21
                 1.16018083495e-06  #ii2
                 )

  ##########
  if (FIG1)
  {
    df_pvals$Step = ""
    df_pvals[df_pvals$label == "video1", "Step"] = "Video"
    df_pvals[df_pvals$label == "video2", "Step"] = "Video"
    df_pvals[df_pvals$label == "nvm1", "Step"] = "NVM"
    df_pvals[df_pvals$label == "nvm2", "Step"] = "NVM"
    df_pvals[df_pvals$label == "nvnm11", "Step"] = "NVNM 1"
    df_pvals[df_pvals$label == "nvnm12", "Step"] = "NVNM 2"
    df_pvals[df_pvals$label == "nvnm21", "Step"] = "NVNM 1"
    df_pvals[df_pvals$label == "nvnm22", "Step"] = "NVNM 2"
    df_pvals[df_pvals$label == "si1", "Step"] = "S.Imit."
    df_pvals[df_pvals$label == "si2", "Step"] = "S.Imit."
    df_pvals[df_pvals$label == "iimitator1", "Step"] = "I.Imit."
    df_pvals[df_pvals$label == "iimitator2", "Step"] = "I.Imit."
    ##########
    labels_fig1 = c("video1", "nvm1", "nvnm11", "si1", "nvnm12", "iimitator1")
    labels_fig2 = c("video2", "nvm2", "nvnm21", "si2", "nvnm22", "iimitator2")
    levels_fig1 = c("Video", "NVM", "NVNM 1", "S.Imit.", "NVNM 2", "I.Imit.")
    levels_fig2 = c("Video", "NVM", "NVNM 1", "S.Imit.", "NVNM 2", "I.Imit.")

    ##########
    df_fig = df_pvals[df_pvals$to_col == "abscorr12" & df_pvals$label %in% labels_fig1,]
    df_fig$Step = as.factor(df_fig$Step)
    df_fig$Step = factor(df_fig$Step, levels = levels_fig1)
    ##########
    str_title = "boxplot_fig1"
    writeLines("...")
    writeLines(paste("<h3>", str_title, "</h3>", sep=""))
    if (outputDir != "")
    {
      outputFile     = paste("plot_paper_", str_title, ".png", sep="")
      outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
      png(outputFullname, width=640);
    }
    p = ggplot(df_fig, aes(x=Step, y=.data$val, color=Step)) +
      theme(legend.position="none") +
      geom_boxplot() + ylab("Correlation") + xlab("Step") + theme(text = element_text(size = text_size))
    grid::grid.draw(p)
    if (outputDir != "")
    {
      writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
      writeLines("")
      writeLines("")
      dev.off()
    }
    ##########
    df_fig = df_pvals[df_pvals$to_col == "resid_abscorr12" & df_pvals$label %in% labels_fig1,]
    df_fig$Step = as.factor(df_fig$Step)
    df_fig$Step = factor(df_fig$Step, levels = levels_fig1)
    ##########
    str_title = "boxplot_fig1_residual"
    writeLines("...")
    writeLines(paste("<h3>", str_title, "</h3>", sep=""))
    if (outputDir != "")
    {
      outputFile     = paste("plot_paper_", str_title, ".png", sep="")
      outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
      png(outputFullname, width=640);
    }
    p = ggplot(df_fig, aes(x=Step, y=val)) + THEME
    p = p + theme(legend.position="none") +
      geom_boxplot(fill="gray", alpha=1.0) + ylab("Correlation") + xlab("Capture Block 1") + theme(text = element_text(size = text_size))
    for (i in seq(length(arr_pval)/2))
    {
      str_pval = pval_star(arr_pval[i])
      p = p + geom_text(x=i, y=0.4, label=str_pval, size=text_size/2, color="azure4")
    }
    grid::grid.draw(p)
    if (outputDir != "")
    {
      writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
      writeLines("")
      writeLines("")
      dev.off()
    }

    ##########
    df_fig = df_pvals[df_pvals$to_col == "abscorr12" & df_pvals$label %in% labels_fig2,]
    df_fig$Step = as.factor(df_fig$Step)
    df_fig$Step = factor(df_fig$Step, levels = levels_fig2)
    ##########
    str_title = "boxplot_fig2"
    writeLines("...")
    writeLines(paste("<h3>", str_title, "</h3>", sep=""))
    if (outputDir != "")
    {
      outputFile     = paste("plot_paper_", str_title, ".png", sep="")
      outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
      png(outputFullname, width=640);
    }
    p = ggplot(df_fig, aes(x=Step, y=.data$val, color=Step)) +
      theme(legend.position="none") +
      geom_boxplot() + ylab("Correlation") + xlab("Step") + theme(text = element_text(size = text_size))
    grid::grid.draw(p)
    if (outputDir != "")
    {
      writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
      writeLines("")
      writeLines("")
      dev.off()
    }
    ##########
    df_fig = df_pvals[df_pvals$to_col == "resid_abscorr12" & df_pvals$label %in% labels_fig2,]
    df_fig$Step = as.factor(df_fig$Step)
    df_fig$Step = factor(df_fig$Step, levels = levels_fig2)
    ##########
    str_title = "boxplot_fig2_residual"
    writeLines("...")
    writeLines(paste("<h3>", str_title, "</h3>", sep=""))
    if (outputDir != "")
    {
      outputFile     = paste("plot_paper_", str_title, ".png", sep="")
      outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
      png(outputFullname, width=640);
    }
    p = ggplot(df_fig, aes(x=Step, y=val)) + THEME
    p = p + theme(legend.position="none") +
      geom_boxplot(fill="gray", alpha=1.0) + ylab("Correlation") + xlab("Capture Block 2") + theme(text = element_text(size = text_size))
    for (i in seq(length(arr_pval)/2))
    {
      str_pval = pval_star(arr_pval[i + length(arr_pval)/2])
      p = p + geom_text(x=i, y=0.4, label=str_pval, size=text_size/2, color="azure4")
    }
    grid::grid.draw(p)
    if (outputDir != "")
    {
      writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
      writeLines("")
      writeLines("")
      dev.off()
    }
    ##########
  }
  
  ##########
  if (FIG12)
  {
    df_pvals$Step = ""
    df_pvals[df_pvals$label == "video1", "Step"] = "Video 1"
    df_pvals[df_pvals$label == "video2", "Step"] = "Video 2"
    df_pvals[df_pvals$label == "nvm1", "Step"] = "NVM 1"
    df_pvals[df_pvals$label == "nvm2", "Step"] = "NVM 2"
    df_pvals[df_pvals$label == "nvnm11", "Step"] = "NVNM 1.1"
    df_pvals[df_pvals$label == "nvnm12", "Step"] = "NVNM 1.2"
    df_pvals[df_pvals$label == "nvnm21", "Step"] = "NVNM 2.1"
    df_pvals[df_pvals$label == "nvnm22", "Step"] = "NVNM 2.2"
    df_pvals[df_pvals$label == "si1", "Step"] = "S.Imit. 1"
    df_pvals[df_pvals$label == "si2", "Step"] = "S.Imit. 2"
    df_pvals[df_pvals$label == "iimitator1", "Step"] = "I.Imit. 1"
    df_pvals[df_pvals$label == "iimitator2", "Step"] = "I.Imit. 2"
    ##########
    labels_fig12 = c("video1", "nvm1", "nvnm11", "si1", "nvnm12", "iimitator1",
                     "video2", "nvm2", "nvnm21", "si2", "nvnm22", "iimitator2")
    levels_fig12 = c("Video 1", "NVM 1", "NVNM 1.1", "S.Imit. 1", "NVNM 1.2", "I.Imit. 1",
                     "Video 2", "NVM 2", "NVNM 2.1", "S.Imit. 2", "NVNM 2.2", "I.Imit. 2")

    ##########
    df_fig = df_pvals[df_pvals$to_col == "resid_abscorr12" & df_pvals$label %in% labels_fig12,]
    df_fig$Step = as.factor(df_fig$Step)
    df_fig$Step = factor(df_fig$Step, levels = levels_fig12)
    ##########
    str_title = "boxplot_fig12_residual"
    writeLines("...")
    writeLines(paste("<h3>", str_title, "</h3>", sep=""))
    if (outputDir != "")
    {
      outputFile     = paste("plot_paper_", str_title, ".png", sep="")
      outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
      png(outputFullname, width=960);
    }
    p = ggplot(df_fig, aes(x=Step, y=.data$val, color=Step)) +
      theme(legend.position="none") +
      geom_boxplot() + ylab("Correlation") + xlab("Step") + theme(text = element_text(size = text_size))
    for (i in seq(length(arr_pval)))
    {
      str_pval = pval_star(arr_pval[i])
      p = p + geom_text(x=i, y=0.4, label=str_pval, size=text_size/4, color="azure4")
    }
    grid::grid.draw(p)
    if (outputDir != "")
    {
      writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
      writeLines("")
      writeLines("")
      dev.off()
    }
    ##########
  }
  
  ##########
  if (FIG3)
  {
    labels_fig3 = c("interludem1", "interludem2")
    ##########
    df_fig3 = df_pvals[df_pvals$to_col == "abscorr12" & df_pvals$label %in% labels_fig3,]
    df_fig3$Experiment = substring(df_fig3$folder, 3, 4)
    ##########
    str_title = "lineplot_fig3"
    writeLines("...")
    writeLines(paste("<h3>", str_title, "</h3>", sep=""))
    if (outputDir != "")
    {
      outputFile     = paste("plot_paper_", str_title, ".png", sep="")
      outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
      png(outputFullname, width=640);
    }

    df_fig3[df_fig3$label == "interludem1", "Step"] = "Interlude 1st half"
    df_fig3[df_fig3$label == "interludem2", "Step"] = "Interlude 2nd half"
    p = ggplot(df_fig3, aes(x=Step, y=val, color=Experiment, group=Experiment)) +
      theme(legend.position="none") +
      geom_point() + geom_line() + geom_path() + ylab("Correlation") + xlab("Step") + theme(text = element_text(size = text_size))
    grid::grid.draw(p)
    if (outputDir != "")
    {
      writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
      writeLines("")
      writeLines("")
      dev.off()
    }
    ##########
    ##########
    df_fig3 = df_pvals[df_pvals$to_col == "resid_abscorr12" & df_pvals$label %in% labels_fig3,]
    df_fig3$Experiment = substring(df_fig3$folder, 3, 4)
    ##########
    str_title = "lineplot_fig3_residual"
    writeLines("...")
    writeLines(paste("<h3>", str_title, "</h3>", sep=""))
    if (outputDir != "")
    {
      outputFile     = paste("plot_paper_", str_title, ".png", sep="")
      outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
      png(outputFullname, width=640);
    }

    df_fig3[df_fig3$label == "interludem1", "Step"] = "Interlude 1st half"
    df_fig3[df_fig3$label == "interludem2", "Step"] = "Interlude 2nd half"
    p = ggplot(df_fig3, aes(x=Step, y=val, group=Experiment)) + THEME
    p = p + theme(legend.position="none") +
      geom_point() + geom_line() + geom_path() + ylab("Correlation") + xlab("") + theme(text = element_text(size = text_size))
    grid::grid.draw(p)
    if (outputDir != "")
    {
      writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
      writeLines("")
      writeLines("")
      dev.off()
    }
    ##########
    str1 = "interludem1"
    str2 = "interludem2"
    x = df_fig3[df_fig3$label == str1, "val", drop=FALSE]
    y = df_fig3[df_fig3$label == str2, "val", drop=FALSE]
    report_tc_test(x, y, str_title, confidence=0.95, str1, str2)
    ##########
    ##########
    labels_fig3ii = c("iimitator1", "iimitator2")
    df_fig3 = df_pvals[df_pvals$to_col == "resid_abscorr12" & df_pvals$label %in% labels_fig3ii,]
    df_fig3$Experiment = substring(df_fig3$folder, 3, 4)
    ##########
    str_title = "lineplot_fig3ii_residual"
    writeLines("...")
    writeLines(paste("<h3>", str_title, "</h3>", sep=""))
    if (outputDir != "")
    {
      outputFile     = paste("plot_paper_", str_title, ".png", sep="")
      outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
      png(outputFullname, width=640);
    }

    df_fig3[df_fig3$label == "iimitator1", "Step"] = "Induced Imit. 1"
    df_fig3[df_fig3$label == "iimitator2", "Step"] = "Induced Imit. 2"
    p = ggplot(df_fig3, aes(x=Step, y=val, group=Experiment)) + THEME
    p = p + theme(legend.position="none") +
      geom_point() + geom_line() + geom_path() + ylab("Correlation") + xlab("") + theme(text = element_text(size = text_size))
    grid::grid.draw(p)
    if (outputDir != "")
    {
      writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
      writeLines("")
      writeLines("")
      dev.off()
    }
    str1 = "iimitator1"
    str2 = "iimitator2"
    x = df_fig3[df_fig3$label == str1, "val", drop=FALSE]
    y = df_fig3[df_fig3$label == str2, "val", drop=FALSE]
    report_tc_test(x, y, str_title, confidence=0.95, str1, str2)
  }

  ##########
  if (FIG4)
  {
    df_pvals$Step = ""
    df_pvals[df_pvals$label == "video1", "Step"] = "Video 1"
    df_pvals[df_pvals$label == "video2", "Step"] = "Video 2"
    df_pvals[df_pvals$label == "si1", "Step"] = "S.Imit. 1"
    df_pvals[df_pvals$label == "si2", "Step"] = "S.Imit. 2"
    ##########
    labels_video_fig4 = c("video1", "video2")
    to_col_video_fig4 = c("IsImitVideo_subj1", "IsImitVideo_subj2")
    labels_si_fig4 = c("si1", "si2")
    to_col_si_fig4 = c("IsImit")
    ##########
    df_video = df_pvals[df_pvals$to_col %in% to_col_video_fig4 & df_pvals$label %in% labels_video_fig4,]
    df_si    = df_pvals[df_pvals$to_col %in% to_col_si_fig4    & df_pvals$label %in% labels_si_fig4,]
    df_fig   = rbind(df_si, df_video)
    ##########
    str_title = "boxplot_fig4"
    writeLines("...")
    writeLines(paste("<h3>", str_title, "</h3>", sep=""))
    if (outputDir != "")
    {
      outputFile     = paste("plot_paper_", str_title, ".png", sep="")
      outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
      png(outputFullname, width=640);
    }
    p = ggplot(df_fig, aes(x=Step, y=.data$val, color=Step)) +
      theme(legend.position="none") +
      geom_boxplot() + ylab("Correlation") + xlab("Step") + theme(text = element_text(size = text_size)) 
    grid::grid.draw(p)
    if (outputDir != "")
    {
      writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
      writeLines("")
      writeLines("")
      dev.off()
    }
  }
  ##########
  df_poolr = data.frame(label = numeric(0),
                       fisher = numeric(0),
                       stoufer = numeric(0))


  colnames_poolr = colnames(df_pdc_pvals)
  for (i in seq(ncol(df_pdc_pvals)))
  {
    pool = df_pdc_pvals[, i]

    df_poolr[i, "label"] = colnames_poolr[i]

    poolr_f = fisher(pool, batchsize = 10000)
    print(poolr_f)
    df_poolr[i, "fisher"] = poolr_f$p

    poolr_s = stouffer(pool, batchsize = 10000)
    print(poolr_s)
    df_poolr[i, "stoufer"] = poolr_s$p
  }
  print_table_html(df_poolr, pval_color)
  print_table_latex(df_poolr, pval_color, col_list=c(1,2,3))

  ##########
  print_separator()

  for (i in seq(0, 2))
  {
    if (i == 0)
      str_subject = ""
    else
      str_subject = paste0(i)
    end

    writeLines(paste0("<h3>NVM", str_subject, "</h3>"))
    df_pca = load_data(paste0("dataset_pdc_pvals_NVM", str_subject, ".tsv"))
    make_table_pool_pvals(df_pca)

    writeLines(paste("<h3>SI", str_subject, "</h3>", sep=""))
    df_pca = load_data(paste0("dataset_pdc_pvals_SI", str_subject, ".tsv"))
    make_table_pool_pvals(df_pca)

    writeLines(paste("<h3>IImitator", str_subject, "</h3>", sep=""))
    df_pca = load_data(paste0("dataset_pdc_pvals_IImitator", str_subject, ".tsv"))
    make_table_pool_pvals(df_pca)
  }

  ##########
  sink(file=NULL)
}
