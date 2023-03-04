report_poolr <- function(inputFile, outputFile, outputDir, df_pvals)
{
  ##########
  outputFullname = paste(outputDir, "/", outputFile, sep="")
  sink(file=NULL)
  sink(file=outputFullname, append=FALSE, split=TRUE)

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
  arr_to_col = c("granger12h", "granger21h", "granger12f", "granger21f", "pval12")
  
  df_poolr = data.frame(label = numeric(0),
                         col = numeric(0),
                         to_col = numeric(0),
                         alg = numeric(0),
                         val = numeric(0))


  for (i_to_col in arr_to_col)
  {
    print(i_to_col)
    df_tmp = df_pvals[df_pvals$to_col == i_to_col,]
    arr_col = unique(df_tmp$col)
    
    for (i_col in arr_col)
    {
      print(i_col)
      df_tmp = df_pvals[df_pvals$to_col == i_to_col & df_pvals$col == i_col,]
      arr_label = unique(df_tmp$label)

      for (i_label in arr_label)
      {
        print(i_label)
        df_tmp = df_pvals[df_pvals$to_col == i_to_col & df_pvals$col == i_col & df_pvals$label == i_label,]
        pool = df_tmp$val

        REMOVE_MIN = FALSE
        REMOVE_MAX = FALSE
        if (REMOVE_MIN)
        {
          pool = sort(pool)
          pool = pool[-1]
        }
        if (REMOVE_MAX)
        {
          pool = sort(pool)
          pool = pool[-length(pool)]
        }

        pool[pool < 0.001] = 0.001
        print(pool)
        
        poolr_f = fisher(pool, batchsize = 10000)
        print(poolr_f)
        df_poolr[nrow(df_poolr) + 1,] = list(i_label, i_col, i_to_col, "fisher", poolr_f$p)
        
        poolr_s = stouffer(pool, batchsize = 10000)
        print(poolr_s)
        df_poolr[nrow(df_poolr) + 1,] = list(i_label, i_col, i_to_col, "stouffer", poolr_s$p)
      }
    }
  }

  save_data(df_poolr, "dataset_poolr.tsv")
  df_poolr_wide = reshape(df_poolr, v.names="val", timevar="alg", idvar=c("label", "col", "to_col"), direction="wide")
  print_table_html(df_poolr_wide, pval_color)

  sink(file=NULL)
}
