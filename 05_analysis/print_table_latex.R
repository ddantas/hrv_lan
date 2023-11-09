# Print table in latex format.
# Input:
#   tab: Table to print
#   cs: color specifier function
print_table_latex <- function(tab, cs=NULL, col_list=NULL)
{
  source("const.R")

  col_titles = names(tab)
  w = length(tab)
  h = length(tab[[1]])
  if (is.null(col_list))
  {
    col_list = seq(1, w)
  }
  else
  {
    w = length(col_list)
  }
  

  writeLines("<table border=1>")
  writeLines("\\begin{table}[!ht]")
  writeLines("  \\caption{Latex table caption.}")
  writeLines("  \\centering")
  writeLines(paste0("  \\begin{tabular}{|", strrep("l|", w), "}"))
  writeLines("    \\hline")
  
  if (h > 0)
  {
    for (j in col_list)
    {
      colsep = "& "
      if (j == 1)
      {
        colsep = "" 
      }
      writeLines(paste0("      ", colsep, col_titles[j]))
    }
    writeLines("    \\\\ \\hline")

    for (i in seq(1, h))
    {
      for (j in col_list)
      {
        d = tab[[j]][[i]]
        s = paste(d)
        colsep = "& "
        if (j == 1)
        {
          colsep = "" 
        }
        if (!is.null(cs) & is.numeric(d))
        {
          s = paste(round(d, 4))
          writeLines(paste0("      ", colsep, "\\textcolor", cs(d, confidence=0.95, format=LATEX), "{", s, "}"))
        }
        else
        {
          s = gsub("_", "\\\\_", s)
          writeLines(paste0("      ", colsep, s))
        }
      }
      writeLines("    \\\\ \\hline")
    }
  }
  writeLines("  \\end{tabular}")
  writeLines("\\end{table}")

}
