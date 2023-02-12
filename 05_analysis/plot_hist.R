plot_hist <- function(df_data, str_title, prompt=1, outputDir="", outputSubdir, xlabel="", label1="Data")
{
  ## Reference:
  ## https://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
  
  outputFullname = ""
  outputFile     = ""

  colnames(df_data) = "x"
  #colnames(df_control) = "x"
  df_data$Group = label1
  #df_control$Group = label2
  #df_data <- rbind(df_data, df_control)

  if (outputDir != "")
  {
    writeLines(paste("<table><tr>", sep=""))
  }
  ##########
  if (outputDir != "")
  {
    outputFile     = paste("hist_smooth_data_", str_title, ".png", sep="")
    outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
    png(outputFullname, width=640);
  }
  x = ggplot(df_data, aes(.data$x, fill=Group)) +
    geom_density(alpha = 0.2) + xlab(xlabel)
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
    outputFile     = paste("hist_bar_data_", str_title, ".png", sep="")
    outputFullname = paste(outputDir, "/", outputSubdir, "/", outputFile, sep="")
    png(outputFullname, width=640);
  }
  #df_data <- rbind(df_data, df_control)
  x = ggplot(df_data, aes(.data$x, fill=Group)) +
    geom_histogram(alpha = 0.5, position = 'identity')  + xlab(xlabel)
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
