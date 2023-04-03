plot_tc_correlation <- function(df_test, df_control, df_imit, str_title, prompt=1, outputDir="", xlabel="", label1="Test", label2="Control")
{
  ## Reference:
  ## http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization

  outputFullname = ""
  outputFile     = ""
  outputSubdir   = "plot_tc_correlation"
  if (outputDir != "")
  {
    dir.create(paste(outputDir, "/", outputSubdir, sep=""))
  }

  time = seq(nrow(df_test))
  df_data <- cbind(time, df_test, df_control)
  col1 = colnames(df_data[2])
  col2 = colnames(df_data[3])

  if (outputDir != "")
  {
    writeLines(paste("<table><tr>", sep=""))
  }
  ##########
  if (outputDir != "")
  {
    outputFile     = paste("corr_scatter_tc_", str_title, ".png", sep="")
    outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
    png(outputFullname, width=640);
  }
  x = ggplot(df_data, aes_string(x=col1, y=col2)) + geom_point()
  grid::grid.draw(x)
  if (outputDir != "")
  {
    writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
    writeLines("")
    writeLines("")
    dev.off()
  }
  ##########
  if (outputDir != "")
  {
    outputFile     = paste("corr_series_tc_", str_title, ".png", sep="")
    outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
    png(outputFullname, width=640);
  }
  df_data_long = reshape(df_data, timevar="Subject", varying=c(col1, col2), v.name="value", direction="long")
  df_data_long[df_data_long$Subject==1,]$Subject = "Subject 1"
  df_data_long[df_data_long$Subject==2,]$Subject = "Subject 2"
  x = ggplot(df_data_long, aes(x=time, color=Subject, group=Subject)) +
      geom_line(aes_string(y="value"))
  grid::grid.draw(x)
  if (outputDir != "")
  {
    writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
    writeLines("")
    writeLines("")
    dev.off()
  }
  ##########
  if (outputDir != "")
  {
    outputFile     = paste("corr_roll_tc_", str_title, ".png", sep="")
    outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
    png(outputFullname, width=640);
  }
  radius = 25
  diam = 2 * radius + 1
  roll_df = data.frame(df_test, df_control)
  result = rollapply(roll_df, diam, rollapply_correlationts, by.column=FALSE)
  result2 = result[,-c(2,3)]
  result3 = data.frame(p.value=unlist(result2[,1]), coef=unlist(result2[,2]), abs_coef=unlist(result2[,3]))
  zeros1 = result3[seq(25),] * 0
  zeros2 = result3[seq(nrow(df_imit) - 25 - nrow(result3)),] * 0
  result4 = rbind(zeros1, result3, zeros2)
  time = seq(nrow(result4))
  result5 = data.frame(time, result4, df_imit)
  result5[is.na(result5$IsSync), "IsSync"] = 0
  result5[is.na(result5$IsImit), "IsImit"] = 0
  result5[result5$IsSync == TRUE, "IsSync"] = 1
  result5[result5$IsImit == TRUE, "IsImit"] = 1
  df_data_long = reshape(result5, timevar="Series", varying=c("p.value", "coef", "abs_coef", "IsImit", "IsSync"), v.name="value", direction="long")
  df_data_long[df_data_long$Series==1,]$Series = "P-value"
  df_data_long[df_data_long$Series==2,]$Series = "Correlation"
  df_data_long[df_data_long$Series==3,]$Series = "Abs. Correlation"
  df_data_long[df_data_long$Series==4,]$Series = "Is Imitating"
  df_data_long[df_data_long$Series==5,]$Series = "Is Synchronized"
  #save_data(result5, paste("/tmp/df_data_long_", str_title, ".tsv", sep=''))
  x = ggplot(df_data_long, aes(x=time, color=Series, group=Series)) +
    geom_line(aes_string(y="value")) +
    annotate("rect", xmin = 0, xmax = nrow(result5), ymin = 0.0, ymax = 0.05, alpha = .5)
  grid::grid.draw(x)
  if (outputDir != "")
  {
    writeLines(paste("<td><img src='", outputSubdir, "/", outputFile, "'></td>", sep=""))
    writeLines("")
    writeLines("")
    dev.off()
  }
  ##########
  if (outputDir != "")
  {
    writeLines(paste("</tr></table>", sep=""))
  }

  if (prompt)
  {
    readline(prompt=paste("Showing plots of",  str_title))
    writeLines("\n...")
    writeLines("")
  }
}
