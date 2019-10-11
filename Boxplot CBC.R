
library(ggplot2)
library(gridExtra)
library(readxl)
library(ggpubr)

###Generate Data

### WBC  
my_mean = aggregate(data$WBC, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$WBC , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### WBC Plot
WBC <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "White Blood Cells") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$WBC), width = 0.1)+
  theme(axis.title.x=element_blank())


### Neutrophils
my_mean = aggregate(data$NE, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$NE , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Nuetrophils plot
NE <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Neutrophils") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$NE), width = 0.1)+
  theme(axis.title.x=element_blank())


### Lymphocyes
my_mean = aggregate(data$LY, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$LY , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Lymphocyes plot
LY <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Lymphocyes") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$LY), width = 0.1)+
  theme(axis.title.x=element_blank())

### Monocytes 
my_mean = aggregate(data$MO, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$MO , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Monocytes plot
MO <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Monocytes") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$MO), width = 0.1)+
  theme(axis.title.x=element_blank())

### Eosinophils
my_mean = aggregate(data$EO, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$EO , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Eosinophils plot
EO <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Eosinophils") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$EO), width = 0.1)+
  theme(axis.title.x=element_blank())

### Basophils 
my_mean = aggregate(data$BA, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$BA , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Basophils plot
BA <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Basophils") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$BA), width = 0.1)+
  theme(axis.title.x=element_blank())

### Hematocrit
my_mean = aggregate(data$HCT, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$HCT , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Hematocrit plot
HCT <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Hematocrit") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$HCT), width = 0.1)+
  theme(axis.title.x=element_blank())

### Platelets
my_mean = aggregate(data$PLT, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$PLT , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Platelets plot
PLT <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Platelets") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$PLT), width = 0.1)+
  theme(axis.title.x=element_blank())


### Generate Multiplots
ggarrange(HCT, PLT, WBC, NE, LY, MO, EO, BA,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
          ncol = 2, nrow = 4)



























## ONLY 1 OB


### WBC  
my_mean = aggregate(data$WBC, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$WBC , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### WBC Plot
WBC <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "White Blood Cells") +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$WBC), width = 0.1)+
  theme(axis.title.x=element_blank())


### Neutrophils
my_mean = aggregate(data$NE, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$NE , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Nuetrophils plot
NE <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Neutrophils") +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$NE), width = 0.1)+
  theme(axis.title.x=element_blank())


### Lymphocyes
my_mean = aggregate(data$LY, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$LY , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Lymphocyes plot
LY <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Lymphocyes") +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$LY), width = 0.1)+
  theme(axis.title.x=element_blank())

### Monocytes 
my_mean = aggregate(data$MO, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$MO , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Monocytes plot
MO <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Monocytes") +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$MO), width = 0.1)+
  theme(axis.title.x=element_blank())

### Eosinophils
my_mean = aggregate(data$EO, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$EO , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Eosinophils plot
EO <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Eosinophils") +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$EO), width = 0.1)+
  theme(axis.title.x=element_blank())

### Basophils 
my_mean = aggregate(data$BA, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$BA , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Basophils plot
BA <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Basophils") +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$BA), width = 0.1)+
  theme(axis.title.x=element_blank())

### Hematocrit
my_mean = aggregate(data$HCT, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$HCT , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Hematocrit plot
HCT <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Hematocrit") +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$HCT), width = 0.1)+
  theme(axis.title.x=element_blank())

### Platelets
my_mean = aggregate(data$PLT, by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$PLT , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Platelets plot
PLT <- ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "Platelets") +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$PLT), width = 0.1)+
  theme(axis.title.x=element_blank())


### Generate Multiplots
ggarrange(HCT, PLT, WBC, NE, LY, MO, EO, BA,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
          ncol = 2, nrow = 4)




