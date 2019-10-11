library(survival)
library(survminer)

fit <- survfit(Surv(days, pheno$`Thyroid Tumor`) ~ pheno$'groups', data = pheno)
# Visualize with survminer
ggsurvplot(fit, data = pheno, risk.table = TRUE, conf.int = F, pval= T)



newdata <- subset(pheno, pheno$`Parathyroid Adenoma`=='1')
fit2 <- survfit(Surv(days) ~ groups, data = newdata)
ggsurvplot(fit2, data = newdata, risk.table = TRUE, conf.int = F, pval= T)
print(fit2, print.rmean=TRUE)

rm(fit2)



library(survival)
library(survminer)

fit <- survfit(Surv(Age) ~ data$'Group', data = data)
# Visualize with survminer
ggsurvplot(fit, data = data, risk.table = TRUE, conf.int = F, pval= T)



newdata <- subset(data, data$`Parathyroid Adenoma`=='1')
fit2 <- survfit(Surv(days) ~ groups, data = newdata)
ggsurvplot(fit2, data = newdata, risk.table = TRUE, conf.int = F, pval= T)
print(fit2, print.rmean=TRUE)

rm(fit2)
