#ggplot
library(ggplot2)

if (FALSE)
{
  ex01 = load_data("df_data_long_exp_ex02_interlude_dd__nn_subj1_ecg_linear_vs_nn_subj2_ecg_linear_residual.tsv")
  ex03 = load_data("df_data_long_exp_ex03_interlude_dd__nn_subj1_ecg_linear_vs_nn_subj2_ecg_linear_residual.tsv")
  ex04 = load_data("df_data_long_exp_ex04_interlude_dd__nn_subj1_ecg_linear_vs_nn_subj2_ecg_linear_residual.tsv")
  ex05 = load_data("df_data_long_exp_ex05_interlude_dd__nn_subj1_ecg_linear_vs_nn_subj2_ecg_linear_residual.tsv")
  ex06 = load_data("df_data_long_exp_ex06_interlude_dd__nn_subj1_ecg_linear_vs_nn_subj2_ecg_linear_residual.tsv")
  ex07 = load_data("df_data_long_exp_ex07_interlude_dd__nn_subj1_ecg_linear_vs_nn_subj2_ecg_linear_residual.tsv")
  ex08 = load_data("df_data_long_exp_ex08_interlude_dd__nn_subj1_ecg_linear_vs_nn_subj2_ecg_linear_residual.tsv")
  ex09 = load_data("df_data_long_exp_ex09_interlude_dd__nn_subj1_ecg_linear_vs_nn_subj2_ecg_linear_residual.tsv")
  ex10 = load_data("df_data_long_exp_ex10_interlude_dd__nn_subj1_ecg_linear_vs_nn_subj2_ecg_linear_residual.tsv")
  ex11 = load_data("df_data_long_exp_ex11_interlude_dd__nn_subj1_ecg_linear_vs_nn_subj2_ecg_linear_residual.tsv")
  ex12 = load_data("df_data_long_exp_ex12_interlude_dd__nn_subj1_ecg_linear_vs_nn_subj2_ecg_linear_residual.tsv")

  df_fujita_interlude_corr = data.frame(ex01$coef, ex03$coef, ex04$coef, ex05$coef, ex06$coef, ex07$coef, ex08$coef, ex09$coef, ex10$coef, ex11$coef, ex12$coef)
  save_data(df_fujita_interlude_corr, "fujita_interlude_corr.tsv")
  saveRDS(df_fujita_interlude_corr, file="fujita_interlude_corr.rds")
}

df_fujita_interlude_corr = load_data("fujita_interlude_corr.tsv")
time = 26:576

p <- ggplot() + xlim(0, 600) + xlab("Time") + ylim(-0.4, 0.4) + ylab("Correlation")
for (i in 1:ncol(df_fujita_interlude_corr))
{
  data = abs(df_fujita_interlude_corr[time,i])
  data = df_fujita_interlude_corr[time,i]
  model = lm(data ~ time)
  intercept = model$coefficients[[1]]
  slope = model$coefficients[[2]]
  print(paste("i", i))
  print(paste("intercept =", intercept, "slope =", slope))
  p <- p + geom_abline(intercept=intercept, slope=slope)
}
grid::grid.draw(p)


data = df_fujita_interlude_corr[time,]
coef.orig <- array(0, ncol(data))
for (i in 1:ncol(data))
{
    coef.orig[i] <- summary(lm(data[,i] ~ time))$coef[2]
}

window <- 10
maxBoot <- 1000
tmp <- array(0, ncol(data))
coef.boot <- matrix(0, maxBoot, ncol(data))

for (boot in 1:maxBoot)
{
  bl <- sample(c(1:(nrow(data) - window)), nrow(data) / window, replace=TRUE)
  x <- as.matrix(data[1,])
  for (j in 1:length(bl))
  {
    x <- rbind(x, as.matrix(data[c(bl[j]:(bl[j]+window-1)),]))
  }
  for (i in 1:ncol(data))
  {
    coef.boot[boot,i] <- summary(lm(x[,i]~time))$coef[2]	
  }
  print(boot)
}

arr_pool <- array(0, ncol(data))
for(i in 1:ncol(data)) {
  p[i] <- length(which(coef.boot[,i] < coef.orig[i])) / maxBoot
}

library(poolr)
arr_pool[which(arr_pool == 0)] <-1/maxBoot
fisher(arr_pool)
stouffer(arr_pool)
