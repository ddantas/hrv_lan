report_poolr <- function(inputFile, outputFile, outputDir, df_pvals, summary=0)
{
  source("print_table_html.R")
  source("print_table_latex.R")
  source("pval_color.R")

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
  df_pvals = load_data(inputFile)
  METHOD1 = TRUE

  ##########
  if (summary)
  {
    df_pvals$label_alt = ""
    arr_label = c()
    label = "0   Prelude"
    df_pvals[df_pvals$label == "prelude", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "11  Video 1"
    df_pvals[df_pvals$label == "video1", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "31  Video 2"
    df_pvals[df_pvals$label == "video2", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "12  NVM 1"
    df_pvals[df_pvals$label == "nvm1", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "32  NVM 2"
    df_pvals[df_pvals$label == "nvm2", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "13  NVNM 1.1"
    df_pvals[df_pvals$label == "nvnm11", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "15  NVNM 1.2"
    df_pvals[df_pvals$label == "nvnm12", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "33  NVNM 2.1"
    df_pvals[df_pvals$label == "nvnm21", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "35  NVNM 2.2"
    df_pvals[df_pvals$label == "nvnm22", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "14  SI 1"
    df_pvals[df_pvals$label == "si1", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "34  SI 2"
    df_pvals[df_pvals$label == "si2", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "16  IImitator 1"
    df_pvals[df_pvals$label == "iimitator1", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "36  IImitator 2"
    df_pvals[df_pvals$label == "iimitator2", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "20  Interlude"
    df_pvals[df_pvals$label == "interlude", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "21  Interlude 1/2"
    df_pvals[df_pvals$label == "interludem1", "label_alt"] = label
    arr_label = rbind(arr_label, label)
    label = "22  Interlude 2/2"
    df_pvals[df_pvals$label == "interludem2", "label_alt"] = label
    arr_label = rbind(arr_label, label)

    df_pvals$label = df_pvals$label_alt
  }

  ##########
  arr_to_col = c("granger12h", "granger21h", "granger12f", "granger21f", "pval12", "resid_pval12")
  if (FALSE)
  {
    arr_to_col = c("abscorr_F1handAheartA", "corr_F1handAheartA", "pval_F1handAheartA",
                   "abscorr_F1handBheartB", "corr_F1handBheartB", "pval_F1handBheartB",
                   "abscorr_F2handAheartB", "corr_F2handAheartB", "pval_F2handAheartB",
                   "abscorr_F2handBheartA", "corr_F2handBheartA", "pval_F2handBheartA",
                   "abscorr_F3handAhandB",  "corr_F3handAhandB",  "pval_F3handAhandB",
                   "abscorr_F4heartAheartB", "corr_F4heartAheartB", "pval_F4heartAheartB")
  }
  else
  {
    print(df_pvals)
    df_pvals$to_col <- as.character(df_pvals$to_col)

    df_pvals[df_pvals$to_col == "abscorr_F1handAheartA", "to_col"] = "abscorr_F1handheart_same"
    df_pvals[df_pvals$to_col == "corr_F1handAheartA", "to_col"] = "corr_F1handheart_same"
    df_pvals[df_pvals$to_col == "pval_F1handAheartA", "to_col"] = "pval_F1handheart_same"
    df_pvals[df_pvals$to_col == "abscorr_F1handBheartB", "to_col"] = "abscorr_F1handheart_same"
    df_pvals[df_pvals$to_col == "corr_F1handBheartB", "to_col"] = "corr_F1handheart_same"
    df_pvals[df_pvals$to_col == "pval_F1handBheartB", "to_col"] = "pval_F1handheart_same"

    df_pvals[df_pvals$to_col == "abscorr_F2handAheartB", "to_col"] = "abscorr_F2handheart_diff"
    df_pvals[df_pvals$to_col == "corr_F2handAheartB", "to_col"] = "corr_F2handheart_diff"
    df_pvals[df_pvals$to_col == "pval_F2handAheartB", "to_col"] = "pval_F2handheart_diff"
    df_pvals[df_pvals$to_col == "abscorr_F2handBheartA", "to_col"] = "abscorr_F2handheart_diff"
    df_pvals[df_pvals$to_col == "corr_F2handBheartA", "to_col"] = "corr_F2handheart_diff"
    df_pvals[df_pvals$to_col == "pval_F2handBheartA", "to_col"] = "pval_F2handheart_diff"

    df_pvals$to_col <- as.factor(df_pvals$to_col)

    arr_to_col = c("abscorr_F1handheart_same", "corr_F1handheart_same", "pval_F1handheart_same",
                   "abscorr_F2handheart_diff", "corr_F2handheart_diff", "pval_F2handheart_diff",
                   "abscorr_F3handAhandB",  "corr_F3handAhandB",  "pval_F3handAhandB",
                   "abscorr_F4heartAheartB", "corr_F4heartAheartB", "pval_F4heartAheartB")

    print(df_pvals)
  }
  if (METHOD1)
  {
    arr_to_col = c("corr12", "abscorr12", "pval12")
  }

  if (summary)
  {
    #arr_to_col = c("resid_pval12")
  }
  
  df_poolr = data.frame(label = numeric(0),
                         col = numeric(0),
                         to_col = numeric(0),
                         alg = numeric(0),
                         val = numeric(0))


  for (i_to_col in arr_to_col)
  {
    print(paste("i_to_col = ", i_to_col, sep=""))
    df_tmp = df_pvals[df_pvals$to_col == i_to_col,]
    arr_col = unique(df_tmp$col)
    
    for (i_col in arr_col)
    {
      print(paste("  i_col = ", i_col, sep=""))
      df_tmp = df_pvals[df_pvals$to_col == i_to_col & df_pvals$col == i_col,]
      if (summary == 0)
      {
        arr_label = unique(df_tmp$label)
      }

      for (i_label in arr_label)
      {
        print(paste("    i_label = ", i_label, sep=""))
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
    df_save = reshape(df_pvals[df_pvals$to_col == i_to_col, ], v.names="val", timevar="label", idvar=c("folder", "col", "to_col"), direction="wide")
    save_data(df_save, paste0("data_fujita_poolr/dataset_fujita_", i_to_col, ".tsv"))
  }

  save_data(df_poolr, "dataset_poolr.tsv")
  df_poolr_wide = reshape(df_poolr, v.names="val", timevar="alg", idvar=c("label", "col", "to_col"), direction="wide")
  print_table_html(df_poolr_wide, pval_color)
  print_table_latex(df_poolr_wide, pval_color, col_list=c(1,4,5))

  sink(file=NULL)
}
