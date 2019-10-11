
library(ggplot2)
library(gridExtra)
library(readxl)
library(ggpubr)

###Generate Data

### ALB  
my_mean = aggregate(data$ALB, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$ALB , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### ALB Plot
ALB <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Albumin") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$ALB), width = 0.1)+
  theme(axis.title.x=element_blank())


### ALP
my_mean = aggregate(data$ALP, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$ALP , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### ALP plot
ALP <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "ALP") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$ALP), width = 0.1)+
  theme(axis.title.x=element_blank())


### ALT
my_mean = aggregate(data$ALT, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$ALT , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### ALT plot
ALT <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "ALT") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$ALT), width = 0.1)+
  theme(axis.title.x=element_blank())

### BUN 
my_mean = aggregate(data$BUN, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$BUN , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### BUN plot
BUN <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "BUN") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$BUN), width = 0.1)+
  theme(axis.title.x=element_blank())

### CREATININE
my_mean = aggregate(data$CRE, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$CRE , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### CREATININE plot
CRE <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Creatinine") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$CRE), width = 0.1)+
  theme(axis.title.x=element_blank())

### GLU 
my_mean = aggregate(data$GLU, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$GLU , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### GLU plot
GLU <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Glucose") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$GLU), width = 0.1)+
  theme(axis.title.x=element_blank())

### TP
my_mean = aggregate(data$TP, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$TP , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### TP plot
TP <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Total Protein") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$TP), width = 0.1)+
  theme(axis.title.x=element_blank())

### GLOB
my_mean = aggregate(data$GLOB, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$GLOB , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### GLOB plot
GLOB <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Globulins") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$GLOB), width = 0.1)+
  theme(axis.title.x=element_blank())

### Ca
my_mean = aggregate(data$Ca, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$Ca , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Ca plot
Ca <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Ca") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$Ca), width = 0.1)+
  theme(axis.title.x=element_blank())

### PHOS
my_mean = aggregate(data$PHOS, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$PHOS , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### PHOS plot
PHOS <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "PHOS") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$PHOS), width = 0.1)+
  theme(axis.title.x=element_blank())

### NaPlus
my_mean = aggregate(data$NaPlus, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$NaPlus , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### NaPlus plot
NaPlus <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Na+") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$NaPlus), width = 0.1)+
  theme(axis.title.x=element_blank())

### KPlus
my_mean = aggregate(data$KPlus, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$KPlus , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### KPlus plot
KPlus <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "K+") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$KPlus), width = 0.1)+
  theme(axis.title.x=element_blank())

### TBIL
my_mean = aggregate(data$TBIL, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$TBIL , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### TBIL plot
TBIL <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Total Bilirubin") +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$TBIL), width = 0.1)+
  theme(axis.title.x=element_blank())

### Generate Multiplots
#ggarrange(TP, GLOB, ALB, ALP, ALT, BUN, CRE, TBIL,
#         labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
#          ncol = 2, nrow = 4)

#ggarrange(GLU, Ca, PHOS, NaPlus, KPlus,
#          labels = c("A", "B", "C", "D", "E"),
#          ncol = 2, nrow = 4)

#ggarrange(TP, GLOB, ALB, ALP, ALT, BUN, CRE, TBIL, GLU, Ca, PHOS, NaPlus, KPlus,
 #         labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"),
  #        ncol = 3, nrow = 5)

ggarrange(TP, GLOB, ALB, ALP, ALT, BUN, CRE, GLU, Ca, PHOS, NaPlus, KPlus,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
          ncol = 3, nrow = 4)





### FOR ONLY 1 OB


### ALB  
my_mean = aggregate(data$ALB, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$ALB , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### ALB Plot
ALB <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Albumin") +
  #geom_errorbar(data = my_mean, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$ALB), width = 0.1)+
  theme(axis.title.x=element_blank())


### ALP
my_mean = aggregate(data$ALP, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$ALP , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### ALP plot
ALP <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "ALP") +
  #geom_errorbar(data = my_mean, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$ALP), width = 0.1)+
  theme(axis.title.x=element_blank())


### ALT
my_mean = aggregate(data$ALT, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$ALT , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### ALT plot
ALT <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "ALT") +
  #geom_errorbar(data = my_mean, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$ALT), width = 0.1)+
  theme(axis.title.x=element_blank())

### BUN 
my_mean = aggregate(data$BUN, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$BUN , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### BUN plot
BUN <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "BUN") +
  #geom_errorbar(data = my_mean, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$BUN), width = 0.1)+
  theme(axis.title.x=element_blank())

### CREATININE
my_mean = aggregate(data$CRE, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$CRE , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### CREATININE plot
CRE <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Creatinine") +
  #geom_errorbar(data = my_mean, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$CRE), width = 0.1)+
  theme(axis.title.x=element_blank())

### GLU 
my_mean = aggregate(data$GLU, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$GLU , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### GLU plot
GLU <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Glucose") +
  #geom_errorbar(data = my_mean, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$GLU), width = 0.1)+
  theme(axis.title.x=element_blank())

### TP
my_mean = aggregate(data$TP, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$TP , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### TP plot
TP <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Total Protein") +
  #geom_errorbar(data = my_mean, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$TP), width = 0.1)+
  theme(axis.title.x=element_blank())

### GLOB
my_mean = aggregate(data$GLOB, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$GLOB , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### GLOB plot
GLOB <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Globulins") +
  #geom_errorbar(data = my_mean, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$GLOB), width = 0.1)+
  theme(axis.title.x=element_blank())

### Ca
my_mean = aggregate(data$Ca, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$Ca , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Ca plot
Ca <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Ca") +
  #geom_errorbar(data = my_mean, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$Ca), width = 0.1)+
  theme(axis.title.x=element_blank())

### PHOS
my_mean = aggregate(data$PHOS, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$PHOS , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### PHOS plot
PHOS <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "PHOS") +
  #geom_errorbar(data = my_mean, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$PHOS), width = 0.1)+
  theme(axis.title.x=element_blank())

### NaPlus
my_mean = aggregate(data$NaPlus, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$NaPlus , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### NaPlus plot
NaPlus <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Na+") +
  #geom_errorbar(data = my_mean, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$NaPlus), width = 0.1)+
  theme(axis.title.x=element_blank())

### KPlus
my_mean = aggregate(data$KPlus, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$KPlus , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### KPlus plot
KPlus <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "K+") +
  #geom_errorbar(data = my_mean, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$KPlus), width = 0.1)+
  theme(axis.title.x=element_blank())

### TBIL
my_mean = aggregate(data$TBIL, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$TBIL , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### TBIL plot
TBIL <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Total Bilirubin") +
  #geom_errorbar(data = my_mean, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$TBIL), width = 0.1)+
  theme(axis.title.x=element_blank())

### Generate Multiplots
#ggarrange(TP, GLOB, ALB, ALP, ALT, BUN, CRE, TBIL,
#         labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
#          ncol = 2, nrow = 4)

#ggarrange(GLU, Ca, PHOS, NaPlus, KPlus,
#          labels = c("A", "B", "C", "D", "E"),
#          ncol = 2, nrow = 4)

ggarrange(TP, GLOB, ALB, ALP, ALT, BUN, CRE, TBIL, GLU, Ca, PHOS, NaPlus, KPlus,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"),
          ncol = 3, nrow = 5)
