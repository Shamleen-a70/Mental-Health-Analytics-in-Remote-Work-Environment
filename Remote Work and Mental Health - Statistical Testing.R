install.packages("dplyr")
library("dplyr")
df<-read.csv("Cleaned_Mental_Health_Remote_Work.csv")

#Basic Data Overview
View(df)
head(df)
tail(df)
summary(df)

## T-test:Social Isolation Rating by Stress Level
df_filtered <- df %>% 
  filter(Stress_Level %in% c("High","Low"))

table(df_filtered$Stress_Level)

# Run the t-test
result <- t.test(Social_Isolation_Rating ~ Stress_Level, data = df_filtered)
print(result)
if (result$p.value < 0.05) {
  print("The difference is statistically significant (p < 0.05)\n")
} else {
  print("The difference is NOT statistically significant (p = 0.05)\n")
}


## Chi Square Test:Job role and Mental health Condition
table_data <- table(df$Job_Role, df$Mental_Health_Condition)

# Perform the Chi-Square Test
chisq_result <- chisq.test(table_data)

# Print test result
print(chisq_result)

# View result

print(chisq_result)
if (chisq_result$p.value < 0.05) {
  print("The difference is statistically significant (p < 0.05)\n")
} else {
  print("The difference is NOT statistically significant (p = 0.05)\n")
}


# ANOVA Test:Comparing Years of Experience across Access to Mental Health Resources

anova_result <- aov(Years_of_Experience ~ Access_to_Mental_Health_Resources, data = df)
summary(anova_result)
pval <- summary(anova_result)[[1]][["Pr(>F)"]][1]
if (pval < 0.05) {
  print("The difference is statistically significant (p < 0.05)\n")
} else {
  print("The difference is NOT statistically significant (p = 0.05)\n")
}

# Z-test:Remote Work Status & Satisfaction

df$Is_Remote <- ifelse(df$Work_Location == "Remote", "Yes", "No")

# Satisfied = Yes, others = No
df$Remote_Satisfied <- ifelse(df$Satisfaction_with_Remote_Work == "Satisfied", "Yes", "No")

tab <- table(df$Is_Remote, df$Remote_Satisfied)

# Run Z-test
z_result <- prop.test(tab[, "Yes"], rowSums(tab), correct = FALSE)

# result
print(z_result)
if (z_result$p.value < 0.05) {
  cat("Statistically significant difference in satisfaction based on work location (p < 0.05)\n")
} else {
  cat("No significant difference (p = 0.05)\n")
}


# binary support column 
df$Support_Binary <- ifelse(df$Company_Support_for_Remote_Work > 3, "Yes", "No")

# run the F-test
f_result=var.test(Company_Support_for_Remote_Work ~ Support_Binary, data = df)
print(f_result)
if (f_result$p.value < 0.05) {
  cat("Statistically significant difference in satisfaction based on work location (p < 0.05)\n")
} else {
  cat("No significant difference (p = 0.05)\n")
}

