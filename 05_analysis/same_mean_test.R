same_mean_test <- function(x, y, str_title, confidence=0.95, label1="Test", label2="Control", paired=FALSE)
{
  significance = 1 - confidence

  if (paired)
  {
    str_paired = "paired"
  }
  else
  {
    str_paired = "unpaired"
  }

  pad = max(nchar(label1), nchar(label2))
  label1_pad = str_pad(label1, pad, side="right")
  label2_pad = str_pad(label2, pad, side="right")
  writeLines(paste0("Testing (", str_paired, ") if column ", str_title, " have the same mean..."))
  writeLines(paste0(label1_pad, " (Mean &plusmn; SD) = (", mean(x), " &plusmn; ", sd(x), ")"))
  writeLines(paste0(label2_pad, " (Mean &plusmn; SD) = (", mean(y), " &plusmn; ", sd(y), ")"))

  if (paired)
  {
    if (length(x) != length(y))
    {
      writeLines("Unable to make a paired test. All columns must have the same length.")
    }
  }

  result <- list(p.value=NaN)
  tryCatch(
    expr = {
      result = t.test(x, y, conf.level=confidence, var.equal=TRUE, paired=paired)
    },
    error = function(e){
      writeLines(paste0("All values are identical"))
    }
  )
  writeLines(paste0("Student ", str_paired, " equal variances t-test (parametric)"))
  writeLines(paste0("P-value = ", result$p.value))
  if (!is.nan(result$p.value))
  {
    if (result$p.value < significance){
      writeLines(paste0("P-value smaller than", significance, ": Sample means are DIFFERENT.\n"))
    }
    else{
      writeLines(paste0("P-value greater than", significance, ": Sample means are EQUAL.\n"))
    }
  }
  ##########
  result2 <- list(p.value=NaN)
  tryCatch(
    expr = {
      result2 = t.test(x, y, conf.level=confidence, var.equal=FALSE, paired=paired)
    },
    error = function(e){
      writeLines(paste("All values are identical"))
    }
  )
  writeLines(paste0("Welch ", str_paired, " unequal variances t-test (parametric)"))
  writeLines(paste0("P-value = ", result2$p.value))
  if (!is.nan(result2$p.value))
  {
    if (result2$p.value < significance){
      writeLines(paste0("P-value smaller than ", significance, ": Sample means are DIFFERENT.\n"))
    }
    else{
      writeLines(paste0("P-value greater than ", significance, ": Sample means are EQUAL.\n"))
    }
  }
  ##########
  writeLines(paste0("Mann-Whitney ", str_paired, " U-test (nonparametric)"))
  result3 = wilcox.test(x, y, paired=paired)
  writeLines(paste0("P-value = ", result3$p.value))
  if (!is.nan(result3$p.value))
  {
    if (result3$p.value < significance){
      writeLines(paste0("P-value smaller than ", significance, ": Sample means are DIFFERENT.\n"))
    }
    else{
      writeLines(paste0("P-value greater than ", significance, ": Sample means are EQUAL.\n"))
    }
  }
}


