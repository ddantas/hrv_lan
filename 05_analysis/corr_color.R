corr_color <- function(corr, format=HTML)
{
  source("const.R")

  if (is.na(corr))
  {
    result = "gray"
    return(result)
  }
  if (FALSE)
  {
    hi_r = 0x05
    hi_g = 0x30
    hi_b = 0x61
    lo_r = 0x67
    lo_g = 0x00
    lo_b = 0x1f
    zr_r = 0xc0
    zr_g = 0xc0
    zr_b = 0xc0
  }
  else
  {
    hi_r = 30
    hi_g = 30
    hi_b = 255
    lo_r = 255
    lo_g = 30
    lo_b = 30
    zr_r = 200
    zr_g = 200
    zr_b = 200
  }

  corr_abs = abs(corr)
  if (corr >= 0)
  {
    r = hi_r
    g = hi_g
    b = hi_b
  }
  else
  {
    r = lo_r
    g = lo_g
    b = lo_b
  }
  if (format == HTML)
  {
    result = paste("rgb(",
                   as.integer( (pval_abs * r) + ((1-pval_abs) * zr_r) ), ",",
                   as.integer( (pval_abs * g) + ((1-pval_abs) * zr_g) ), ",",
                   as.integer( (pval_abs * b) + ((1-pval_abs) * zr_b) ), ")",
                   sep = "")
  }
  else
  {
    result = paste("[RGB]{",
                   as.integer( (pval_abs * r) + ((1-pval_abs) * zr_r) ), ",",
                   as.integer( (pval_abs * g) + ((1-pval_abs) * zr_g) ), ",",
                   as.integer( (pval_abs * b) + ((1-pval_abs) * zr_b) ), "}",
                   sep = "")
  }
  return(result)
}
