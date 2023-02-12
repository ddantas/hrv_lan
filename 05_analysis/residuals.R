

#filename_dataset = "dataset.tsv"
#df = load_data(filename_dataset)

i = 1
cols = c(seq(10, 25), seq(30, 45))

col1 = names(df)[cols[i]]
col2 = names(df)[cols[i + 1]]

data1 = df[rows, col1, drop=FALSE]
data2 = df[rows, col2, drop=FALSE]
hand_regr_data1 = df[rows, c("subj1_flow_l_cx", "subj1_flow_l_cy", "subj1_flow_r_cx", "subj1_flow_r_cy"), drop=FALSE]
hand_regr_data2 = df[rows, c("subj2_flow_l_cx", "subj2_flow_l_cy", "subj2_flow_r_cx", "subj2_flow_r_cy"), drop=FALSE]
df_regr = cbind(data1, data2, hand_regr_data1, hand_regr_data2)

formula = "hr_subj1_linear ~ subj1_flow_l_cx + subj1_flow_l_cy + subj1_flow_r_cx + subj1_flow_r_cy + subj2_flow_l_cx + subj2_flow_l_cy + subj2_flow_r_cx + subj2_flow_r_cy"
glm_model = glm(formula, df_regr, family=gaussian())

formula2 = "hr_subj1_linear ~ subj1_flow_l_cx + subj1_flow_l_cy + subj2_flow_l_cx + subj2_flow_l_cy"
glm_model2 = glm(formula2, df_regr, family=gaussian())

png("saida/residuals.png")
par(mfrow = c(3, 1)); plot(glm_model2$residuals); plot(glm_model$residuals); plot(data1[,1])
