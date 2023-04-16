pval_star <- function(pval, significance=0.05)
{

  if (pval > significance)
  {
    return("ns")
  }
  else if (pval > 0.01)
  {
    return("*")
  }
  else if (pval > 0.001)
  {
    return("**")
  }
  else if (pval > 0.0001)
  {
    return("***")
  }
  return("****")

}
