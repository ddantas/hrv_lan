pval_color <- function(pval, confidence=0.95, format=HTML)
{
  source("const.R")

  significance = 1 - confidence

  if (is.na(pval) | pval < 0.0 | pval > 1.0)
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
    zr_r = 120
    zr_g = 120
    zr_b = 120
  }

  if (pval <= significance)
  {
    pval_min = 1E-3
    pval_abs = (log(max(pval, pval_min)) - log(significance) ) / (log(pval_min) - log(significance))
    r = hi_r
    g = hi_g
    b = hi_b
  }
  else
  {
    pval_abs = (log(pval) - log(significance) ) / (log(1) - log(significance))
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
