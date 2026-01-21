# create df w/ all columns of interest, for now wo/ age since it needs to be revised
install.packages("Hmisc")
library(Hmisc)
install.packages("gt")
library(gt)
install.packages("dplyr")

correlation_data <- subset(data, select= -age)
correlation_data_by_group = split(correlation_data, factor(correlation_data$group, levels = unique(correlation_data$group)))




correlations_with_score_KS22_r <- rcorr(as.matrix(correlation_data_by_group[[1]][c(1:49, 51)]))$r[, "score"]
correlations_with_score_KS22_p <- rcorr(as.matrix(correlation_data_by_group[[1]][c(1:49, 51)]))$P[, "score"]
correlations_with_score_KS22_n <- as.integer(colSums(!is.na(as.matrix(correlation_data_by_group[[1]][c(1:49, 51)]))))

correlations_with_score_PD22_r <- rcorr(as.matrix(correlation_data_by_group[[2]][c(1:49, 51)]))$r[, "score"]
correlations_with_score_PD22_p<- rcorr(as.matrix(correlation_data_by_group[[2]][c(1:49, 51)]))$P[, "score"]
correlations_with_score_PD22_n <- colSums(!is.na(as.matrix(correlation_data_by_group[[2]][c(1:49, 51)])))

correlations_with_score_KS23_r<- rcorr(as.matrix(correlation_data_by_group[[3]][c(1:49, 51)]))$r[, "score"]
correlations_with_score_KS23_p<- rcorr(as.matrix(correlation_data_by_group[[3]][c(1:49, 51)]))$P[, "score"]
correlations_with_score_KS23_n <- colSums(!is.na(as.matrix(correlation_data_by_group[[3]][c(1:49, 51)])))

correlations_with_score_PD23_r<- rcorr(as.matrix(correlation_data_by_group[[4]][c(1:49, 51)]))$r[, "score"]
correlations_with_score_PD23_p<- rcorr(as.matrix(correlation_data_by_group[[4]][c(1:49, 51)]))$P[, "score"]
correlations_with_score_PD23_n <- colSums(!is.na(as.matrix(correlation_data_by_group[[4]][c(1:49, 51)])))

correlations_df <- data.frame(correlations_with_score_KS22_r, 
                              correlations_with_score_KS22_p,
                              correlations_with_score_KS22_n,
                              correlations_with_score_PD22_r,
                              correlations_with_score_PD22_p,
                              correlations_with_score_KS22_n,
                              correlations_with_score_KS23_r,
                              correlations_with_score_KS23_p,
                              correlations_with_score_KS22_n,
                              correlations_with_score_PD23_r,
                              correlations_with_score_PD23_p,
                              correlations_with_score_KS22_n)
cbind(correlations_df, )

correlations_df_transposed <- data.frame(t(correlations_df))

correlations_table <- gt(correlations_df_transposed,  
                           rowname_col = "used_script_digital") %>% fmt_number(drop_trailing_zeros = T, decimals = 3)
      
correlations_table


