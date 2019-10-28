
library(plyr)
library(dplyr)
library(readr)


path = "C:/Users/edmondsonef/Desktop/QuPath/Zheng/PHL 154543/annotations/"
#path = "P:/archive/PHL/Edmondson/QuPath/NCL/ADME Tox 180/iNOS/"

setwd(path)
outFile <-"Result.csv"
#### Replace .txt with whatever identifier will pick up all of the files you want to analyze. Detections or Annotations are common choices
Annotationfiles <- dir(path,pattern = ".txt")
Measurements <- data.frame()
for(i in 1:length(Annotationfiles)){
  data.raw <- read_delim(Annotationfiles[i],"\t", escape_double = FALSE, trim_ws = TRUE)
  Sample = tools::file_path_sans_ext(Annotationfiles[i])
  data.raw[1,2]<-Sample
  Measurements<-bind_rows(Measurements, data.raw)
}
write.csv(Measurements, outFile, row.names=T)





library(ggplot2)
library(gridExtra)
library(readxl)
library(ggpubr)
library(Rmisc)

###########
###########
###########
###########

my_mean1 = aggregate(data$'CD45: Num Positive per mm^2', by=list(data$Group), mean) ; colnames(my_mean1)=c("Group" , "mean")
my_CI1 = aggregate(data$'CD45: Num Positive per mm^2', by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI1)=c("Group" , "CI")
my_info1 = merge(my_mean1, my_CI, by.x=1 , by.y=1)
my_info1$CIdiff = ((my_CI1$CI[,2] - my_CI1$CI[,1])/2)

CD45 <- ggplot(data) + 
  geom_point(data = my_info1, aes(x = Group, y = my_info1$mean), color = "Grey", size = 5) +
  scale_y_continuous(name = "CD45+ per mm^2") +
  geom_errorbar(data = my_info1, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'CD45: Num Positive per mm^2'), width = 0.2, size = 4) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))

#

my_mean = aggregate(data$'CD11b: Num Positive per mm^2', by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'CD11b: Num Positive per mm^2', by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean, my_CI, by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

CD11b <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group, y = my_info$mean), color = "Grey", size = 5) +
  scale_y_continuous(name = "CD11b+ per mm^2") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'CD11b: Positive per mm^2'), width = 0.2, size = 4) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))

#

my_mean3 = aggregate(data$'Microvessel Density -- Number of vessels per unit area (um2)', by=list(data$Group), mean) ; colnames(my_mean3)=c("Group" , "mean")
my_CI3 = aggregate(data$'Microvessel Density -- Number of vessels per unit area (um2)', by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI3)=c("Group" , "CI")
my_info3 = merge(my_mean3, my_CI3, by.x=1 , by.y=1)
my_info3$CIdiff = ((my_CI3$CI[,2] - my_CI3$CI[,1])/2)

MVD <- ggplot(data) + 
  geom_point(data = my_info3, aes(x = Group, y = my_info3$mean), color = "Grey", size = 5) +
  scale_y_continuous(name = "Vessels per um2") +
  geom_errorbar(data = my_info3, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'Microvessel Density -- Number of vessels per unit area (um2)'), width = 0.2, size = 4) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))

#

my_mean4 = aggregate(data$'Murine PD-L1: H-score', by=list(data$Group), mean) ; colnames(my_mean4)=c("Group" , "mean")
my_CI4 = aggregate(data$'Murine PD-L1: H-score', by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI4)=c("Group" , "CI")
my_info4 = merge(my_mean4, my_CI4, by.x=1 , by.y=1)
my_info4$CIdiff = ((my_CI4$CI[,2] - my_CI4$CI[,1])/2)

muPDL1 <- ggplot(data) + 
  geom_point(data = my_info4, aes(x = Group, y = my_info4$mean), color = "Grey", size = 5) +
  scale_y_continuous(name = "Murine PD-L1: H-score") +
  geom_errorbar(data = my_info4, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'Murine PD-L1: H-score'), width = 0.2, size = 4) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))

#

my_mean5 = aggregate(data$'Human PD-L1: H-score', by=list(data$Group), mean) ; colnames(my_mean5)=c("Group" , "mean")
my_CI5 = aggregate(data$'Human PD-L1: H-score', by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI5)=c("Group" , "CI")
my_info5 = merge(my_mean5, my_CI5, by.x=1 , by.y=1)
my_info5$CIdiff = ((my_CI5$CI[,2] - my_CI5$CI[,1])/2)

huPDL1 <- ggplot(data) + 
  geom_point(data = my_info5, aes(x = Group, y = my_info5$mean), color = "Grey", size = 5) +
  scale_y_continuous(name = "Human PD-L1: H-score") +
  geom_errorbar(data = my_info5, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'Human PD-L1: H-score'), width = 0.2, size = 4) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))

#

ggarrange(CD45, CD11b, huPDL1, muPDL1, MVD, 
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

########
###########

my_mean = aggregate(data$'Metastatic Density (% area, visual estimate)', by=list(data$'Group'), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Metastatic Density (% area, visual estimate)', by=list(data$'Group') , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean, my_CI, by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

ggplot(data) + 
  geom_point(data = my_info, aes(x = my_info$'Group', y = my_info$mean), color = "Grey", size = 5) +
  scale_y_continuous(name = "Metastatic Density (% area, visual estimate)") +
  geom_errorbar(data = my_info, aes(x = my_info$'Metastatic Density (% area, visual estimate)', y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$'Group', y = data$'Metastatic Density (% area, visual estimate)', color = data$Group), width = 0.2, size = 4) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))

###########
###########
###########


###########
###########
###########
###########
###ONLY1###
 



my_mean = aggregate(data$'Metastatic Density (% area, visual estimate)', by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$"Metastatic Density (% area, visual estimate)", by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

ggplot(data) + 
  geom_point(data = my_mean, aes(x = Group , y = mean), color = "grey", size = 5) +
  scale_y_continuous(name = "Metastatic Density (% area, visual estimate)") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$"Metastatic Density (% area, visual estimate)"), width = 0.2, size = 4) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6)) #+
  #expand_limits(y=c(0,250))
###########
###########
###########
###########



### Generate Multiplots
ggarrange(first, Second,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)



data1 <- summarySE(data, measurevar="Xenograft, Abdominal involvement", groupvars=c("Group"), na.rm = TRUE)
tgc2 <- tgc
tgc2$dose <- factor(tgc2$dose)

ggplot(data1, aes(x=data1$'Group', y=data1$'Xenograft, Abdominal involvement')) + 
  geom_errorbar(aes(ymin=data1$'Xenograft, Abdominal involvement'-se, ymax=data1$'Xenograft, Abdominal involvement'+se))


ggplot(data) 
  geom_point(data, aes(x=data$'Group', y=data$'Total Tumor Area / Total Tissue Area (%)'), color = "grey", size = 3) +
  scale_y_continuous(name = "Xeno") +
  geom_errorbar(aes(ymin=data1$'Xenograft, Abdominal involvement'-se, ymax=data1$'Xenograft, Abdominal involvement'+se), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$ALB), width = 0.1)+
  theme(axis.title.x=element_blank())
  


ggplot(data, aes(x="Group", y="Organ Weight", color="Organ", shape="Sex")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 16, 17))+ 
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  theme(legend.position="top")

ggplot(data, aes(x=data$"Group", y=data$"Organ Weight")) 

########
my_mean = aggregate(data$'iNOS to CD206 ratio', by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'iNOSt to CD206 ratio' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean, my_CI, by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)



ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = my_info$mean), color = "grey", size = 5) +
  scale_y_continuous(name = "iNOS to CD206 ratio") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'iNOS to CD206 ratio'), width = 0.2, size = 4) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))


############SD

my_mean=aggregate(data$'First' , by=list(data$Group) , mean, na.rm=TRUE) ; colnames(my_mean)=c("Group" , "mean")
my_sd=aggregate(data$'First' , by=list(data$Group) , sd, na.rm=TRUE) ; colnames(my_sd)=c("Group" , "sd")
my_info=merge(my_mean , my_sd , by.x=1 , by.y=1)


ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = my_info$mean), color = "grey", size = 3) +
  scale_y_continuous(name = "MKPV: PCR CT Value", limits = c(15, 50)) +
  geom_errorbar(data = my_info, aes(x = Group, y = sd, ymin = mean - sd, ymax = mean + sd), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'First'), width = 0.2, size = 3) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))

### Generate Multiplots
ggarrange(First, Second,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)




########
my_mean = aggregate(data$'Second', by=list(data$Group), mean, na.rm=TRUE) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Second' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

Second <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "MKPV: PCR CT Value") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = Group, y = data$'Second'), width = 0.2, size = 3) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 



###Scatterplots, tumor size over time, etc###


plot(data$Age, data$'xe', main="Scatterplot Example", 
     xlab="Age (days) ", ylab="Pulmonary Metastatic Density", pch=19)
abline(lm(data$'PCR CT Value'~data$Age), col="red") # regression line (y~x) 
lines(lowess(data$'PCR CT Value',data$Age), col="blue") # lowess line (x,y)

scatterplot(data$'CD45: Num Positive per mm^2' ~ data$"Murine PD-L1: H-score" | data$Group, data=data, 
            xlab="Days", ylab="Allograft Metastatic Density", 
            main="Enhanced Scatter Plot")


scatterplot(data$'Metastatic Density (% area, visual estimate)' ~ data$Age | data$Group, data=data,
            xlab="Days", ylab="Allograft Metastatic Density", 
            main="Enhanced Scatter Plot")


qplot(data$Age, data$'Metastatic Density (% area, visual estimate)', data = data, colour = data$Group, geom = "line")



plot(data$Urine, data$`Dry Fecal Pellet`, main="Urine vs Fecal Pellet", 
     xlab="Urine PCR ", ylab="Fecal PCR ", pch=19)

abline(lm(data$`Dry Fecal Pellet`~data$Urine), col="red") # regression line (y~x) 
lines(lowess(data$Urine,data$`Dry Fecal Pellet`), col="blue") # lowess line (x,y)

###3D scatterplot
library(scatterplot3d)
colors <- c("#999999", "#E69F00", "#56B4E9")
x <- data$Urine
y <- data$`Dry Fecal Pellet`
z <- data$swab
grps <- as.factor(data$Species)
scatterplot3d(data$Urine, data$`Dry Fecal Pellet`, data$swab, pch = 16, color = colors[grps],
              grid = TRUE, box = FALSE, xlab = "Urine PCR", 
              ylab = "Fecal Pellet PCR", zlab = "Cage Swab PCR")



library(ggplot2)
library(ggpmisc)
library(ggplot2)
library(gridExtra)
library(readxl)
library(ggpubr)
library(Rmisc)


my.formula <- y ~ x
ggplot(data = data, aes(x = data$'CD45: Num Positive per mm^2', y = data$'y'), na.rm=TRUE) +
  geom_smooth(method = "lm", se=FALSE, color="red", formula = my.formula, na.rm=TRUE) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, na.rm=TRUE) +  
  geom_point(na.rm=TRUE)+
  scale_y_continuous(name = "Murine PD-L1") +
  scale_x_continuous(name = "CD45+ cells per mm^2") + #, limits = c(20, 36)) +
    theme_bw(base_size = 16)      

pswab1 <- ggplot(data = data1, aes(x = data1$'Urine.log', y = data1$'Swab.log'), na.rm=TRUE) +
  #geom_smooth(method = "lm", se=FALSE, color="red", formula = my.formula, na.rm=TRUE) +
  stat_smooth(method="lm",formula=y~log(x),fill="red") +  
  geom_point(na.rm=TRUE)+
  scale_y_continuous(name = "Swab qPCR (Copy Number)") +
  scale_x_continuous(name = "Urine qPCR (Copy Number)") +
                  theme_bw(base_size = 16) 

pblood1 <- ggplot(data = data1, aes(x = data1$'Urine.log', y = data1$'Blood.log'), na.rm=TRUE) +
  #geom_smooth(method = "lm", se=FALSE, color="red", formula = my.formula, na.rm=TRUE) +
  stat_smooth(method="lm",formula=y~log(x),fill="red") +  
  geom_point(na.rm=TRUE)+
  scale_y_continuous(name = "Blood qPCR (Copy Number)") +
  scale_x_continuous(name = "Urine qPCR (Copy Number)") + #, limits = c(20, 36)) +
  theme_bw(base_size = 16)  

pother1 <- ggplot(data = data1, aes(x = data1$'Fecal Pellet.log', y = data1$'Swab.log'), na.rm=TRUE) +
  #geom_smooth(method = "lm", se=FALSE, color="red", formula = my.formula, na.rm=TRUE) +
  stat_smooth(method="lm",formula=y~log(x),fill="red") +  
  geom_point(na.rm=TRUE)+
  scale_y_continuous(name = "Swab qPCR (Copy Number)") +
  scale_x_continuous(name = "Fecal qPCR (Copy Number)") +
  theme_bw(base_size = 16)  

ggarrange(pfecal1, pswab1, pblood1, pother1, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


library(ggplot2)
library(ggpmisc)
library(ggplot2)
library(gridExtra)
library(readxl)
library(ggpubr)
library(Rmisc)


my.formula <- y ~ x
pswab1 <- ggplot(data = data1, aes(x = data1$'Urine', y = data1$'Dry Fecal Pellet'), na.rm=TRUE) +
  geom_smooth(method = "lm", se=FALSE, color="red", formula = my.formula, na.rm=TRUE) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, na.rm=TRUE) +  
  geom_point(na.rm=TRUE)+
  scale_y_continuous(name = "Swab PCR (CT Values)") +
  scale_x_continuous(name = "Urine PCR (CT Values)", xlim=c(20,32.5)) +
  theme_bw(base_size = 18)       

pfecal1

ggarrange(pfecal, pswab1, pother1, pblood, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


plot(data$'PD-L1 (mouse): Positive %', data$'CD45 Positive %', main="", 
     xlab="PD-L1 (mouse): Positive % ", ylab="CD45 Positive % ", pch=19)

abline(lm(data$'CD45 Positive %'~data$'PD-L1 (mouse): Positive %'), col="red") # regression line (y~x) 
lines(lowess(data$'PD-L1 (mouse): Positive %',data$'CD45 Positive %'), col="blue") # lowess line (x,y)
