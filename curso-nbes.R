#limpiar el ambiente ----
rm(list = ls(all.names = TRUE))#objetos
gc()#reporte de la memoria
options(max.print = .Machine$integer.max, scipen = 999, stringsAsFactors = F, dplyr.summarise.inform=F)#evita datos truncos

#directorio de trabajo ----
setwd("~/Desktop")

#cargar datos ----
data <- read.table(file = "datos-glucosa-curso-r.csv",
                         header = T, 
                         sep = ",", 
                         stringsAsFactors = F)

#rango intercuartil (IQR) #boxplot ----
GlucPRE <- boxplot(data$GlucPRE ~ data$Sexo, #estableces el análisis #basededatos$columna ~ basededatos$columna
                   col = c("red","blue"), #defines colores de tus grupos
                   main = "Rango Intercuartílico GPA", #escribes título principal
                   xlab = " ", #escribes título para eje X
                   ylab = "Glucosa (mg/dl)") #escribes título para eje Y

GlucPOST <- boxplot(data$GlucPOST ~ data$Sexo, #estableces el análisis #basededatos$columna ~ basededatos$columna
                   col = c("red","blue"), #defines colores de tus grupos
                   main = "Rango Intercuartílico Glucosa Postpandrial", #escribes título principal
                   xlab = " ", #escribes título para eje X
                   ylab = "Glucosa (mg/dl)") #escribes título para eje Y


#normalidad ----
#grupos
Masculino <- data[data$Sexo == "Masculino",]
Femenino <- data[data$Sexo == "Femenino",]

#Prueba de normalidad de los datos
#se aplica Shapiro-Wilk porque tenemos n<50
#H0 los datos son normales
#por tanto si p>0.05 se acepta H0

shapiro.test(Masculino$GlucPRE)
#Shapiro-Wilk normality test
#data:  Masculino$GlucPRE
#p-value = 0.07433

shapiro.test(Masculino$GlucPOST)
#Shapiro-Wilk normality test
#data:  Masculino$GlucPOST
#p-value = 0.07386

shapiro.test(Femenino$GlucPRE)
#Shapiro-Wilk normality test
#data:  Femenino$GlucPRE
#p-value = 0.6732

shapiro.test(Femenino$GlucPOST)
#Shapiro-Wilk normality test
#data:  Femenino$GlucPOST
#p-value = 0.6728

#homocedasticidad ----
# se aplica la prueba de Bartlett, cumpliendo los siguientes supuestos
#"n" entre grupos diferente
# 2 o más poblaciones
# H0 la varianza es igual entre los grupos, p-value > 0.05

bartlett.test(data$GlucPRE ~ data$Sexo)
#Bartlett test of homogeneity of variances
#data:  data$GlucPRE by data$Sexo
#Bartlett's K-squared = 0.018308, df = 1, p-value = 0.8924

bartlett.test(data$GlucPOST ~ data$Sexo)
#Bartlett test of homogeneity of variances
#data:  data$GlucPOST by data$Sexo
#Bartlett's K-squared = 0.018599, df = 1, p-value = 0.8915

# media y error estándar de la media ----
#Cargar paquetería
library(plotrix)

#Grupo:Masculino

#Variable de medición GPA (GlucPRE)
#media 
mean(Masculino$GlucPRE,na.rm=TRUE) #[1] 90
#error estándar de la media
std.error(Masculino$GlucPRE) #[1] 4.441933

#Variable de medición GPA (GlucPOST)
#media 
mean(Masculino$GlucPOST,na.rm=TRUE) #[1] 153.1538
#error estándar de la media
std.error(Masculino$GlucPOST) #[1] 7.560734


#Grupo:Femenino

#Variable de medición GPA (GlucPRE)
#media 
mean(Femenino$GlucPRE,na.rm=TRUE) #[1] 97.36364
#error estándar de la media
std.error(Femenino$GlucPRE) #[1] 4.630478

#Variable de medición GPA (GlucPOST)
#media 
mean(Femenino$GlucPOST,na.rm=TRUE) #[1] 165.5455
#error estándar de la media
std.error(Femenino$GlucPOST) #[1] 7.879044


#Student's t-Test ----
t.test(Masculino$GlucPRE, Femenino$GlucPRE, paired = FALSE, var.equal = TRUE)
#Welch Two Sample t-test
#data:  Masculino$GlucPRE and Femenino$GlucPRE
#t = -1.1476, df = 21.618, p-value = 0.2637
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -20.684405   5.957133
#sample estimates:
#  mean of x mean of y 
#90.00000  97.36364 

t.test(Masculino$GlucPOST, Femenino$GlucPOST, paired = FALSE, var.equal = TRUE)
#Welch Two Sample t-test
#data:  Masculino$GlucPOST and Femenino$GlucPOST
#t = -1.1306, df = 22, p-value = 0.2704
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -35.12160  10.33839
#sample estimates:
#  mean of x mean of y 
#153.1538  165.5455

#reportar ----
#table = read.table("clipboard", header = TRUE)

table <- read.table(file = "resultados-datos-glucosa-curso-r.csv",
                   header = T, 
                   sep = ",", 
                   stringsAsFactors = F)

library(ggplot2)

ggplot(table, aes(x=Grupo, y=Media, fill=Grupo))+
  geom_bar(stat="identity", width = 0.4) +
  ggtitle("GPA")+ 
  xlab("Grupo")+
  ylab("Glucosa (mg/dl)")+
  geom_errorbar(aes(ymin=Media-Error, ymax=Media+Error), width=0.2, position = position_dodge(0.9))

# Figura 1. Dimorfismo sexual en la GPA. Media ± error estándar. Prueba Shapiro-Wilk P>0.05. 
# Prueba de Bartlett P>0.05. Prueba T, *P<0.05. Masculino n=13, Femenino n=11.

#La notación estándar es:
#Se escribe t(gl) = valor, p = valor.
#El intervalo de confianza se reporta como IC 95% [límite inferior, límite superior].
#El valor de p se escribe en minúscula (p), no con mayúscula.

#ejemplo# t(22) = -1.13, p = 0.27, IC 95% [-35.12, 10.34]





