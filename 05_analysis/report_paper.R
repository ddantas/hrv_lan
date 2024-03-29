report_paper <- function(inputFile, outputFile, outputDir, df, df_pvals,confidence=0.95, prompt=1)
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
  FIG3 = TRUE
  FIG4 = TRUE

  text_size = 20

  outputSubdir = "plot_paper"
  if (outputDir != "")
  {
    dir.create(paste(outputDir, "/", outputSubdir, sep=""))
  }
  ##########
  if (FIG1)
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
    labels_fig1 = c("video1", "nvm1", "nvnm11", "si1", "nvnm12", "iimitator1")
    labels_fig2 = c("video2", "nvm2", "nvnm21", "si2", "nvnm22", "iimitator2")
    levels_fig1 = c("Video 1", "NVM 1", "NVNM 1.1", "S.Imit. 1", "NVNM 1.2", "I.Imit. 1")
    levels_fig2 = c("Video 2", "NVM 2", "NVNM 2.1", "S.Imit. 2", "NVNM 2.2", "I.Imit. 2")

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
    x = ggplot(df_fig, aes(x=Step, y=.data$val, color=Step)) + geom_boxplot() + ylab("Correlation") + xlab("Step") + theme(text = element_text(size = text_size))
    grid::grid.draw(x)
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
    x = ggplot(df_fig, aes(x=Step, y=.data$val, color=Step)) + geom_boxplot() + ylab("Correlation") + xlab("Step") + theme(text = element_text(size = text_size))
    grid::grid.draw(x)
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
    x = ggplot(df_fig, aes(x=Step, y=.data$val, color=Step)) + geom_boxplot() + ylab("Correlation") + xlab("Step") + theme(text = element_text(size = text_size))
    grid::grid.draw(x)
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
    x = ggplot(df_fig, aes(x=Step, y=.data$val, color=Step)) + geom_boxplot() + ylab("Correlation") + xlab("Step") + theme(text = element_text(size = text_size))
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
    x = ggplot(df_fig3, aes(x=Step, y=val, color=Experiment, group=Experiment)) + geom_point() + geom_line() + geom_path() + ylab("Correlation") + xlab("Step") + theme(text = element_text(size = text_size))
    grid::grid.draw(x)
    if (outputDir != "")
    {
      writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
      writeLines("")
      writeLines("")
      dev.off()
    }
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
    x = ggplot(df_fig3, aes(x=Step, y=val, color=Experiment, group=Experiment)) + geom_point() + geom_line() + geom_path() + ylab("Correlation") + xlab("Step") + theme(text = element_text(size = text_size))
    grid::grid.draw(x)
    if (outputDir != "")
    {
      writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
      writeLines("")
      writeLines("")
      dev.off()
    }
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
    x = ggplot(df_fig, aes(x=Step, y=.data$val, color=Step)) + geom_boxplot() + ylab("Correlation") + xlab("Step") + theme(text = element_text(size = text_size)) 
    grid::grid.draw(x)
    if (outputDir != "")
    {
      writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
      writeLines("")
      writeLines("")
      dev.off()
    }
  }

  sink(file=NULL)
}
