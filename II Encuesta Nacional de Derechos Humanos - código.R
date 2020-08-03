*** Código de post "Una exploración a los datos de la II Encuesta Nacional de Derechos Humanos (Perú)"***

A) Sobre aquellos que no se sintieron discriminados, según NSE

# Data.frame de la pregunta 6, sobre la base del documento de Excel
'Tabla de resultados por edad, sexo y NSE'. Datos de los que no se han sentido
discriminados (en los últimos doce meses) según nivel socioeconómico.

data1 = data.frame(NSE=c("A-B", "C", "D-E"), Porcentaje=c(68, 66, 63))

# Cargar librería ggplot

library(ggplot2)

# Gráfico

a = ggplot(data1, aes(x=NSE, y=Porcentaje))+
geom_bar(stat = "identity", width=0.3, fill = "red", color = "grey40")+ 
geom_text(aes(label=scales::percent(Porcentaje, scale = 1, accuracy = 1)), 
vjust=1.5, color="white", size=3)+
ggtitle("No se sintieron discriminados en los últimos 
doce meses, según NSE")+labs(y="", x="")+
theme(plot.title = element_text(color="black", size=15, face="bold"))+
ylim(0,100)

a

B) Confianza en FFAA y nivel de estudios [NO SE INCLUYÓ EN EL POST]

# Cargar archivo con la base de datos

DH = read.csv("II-ENDHcsv.csv")

# Distribución de respuestas según nivel de estudios

table(DH$P44, DH$P38_7_Rp)

# Ajustar los valores de la distribución aplicando factor de ponderación

sum((subset(DH, DH$P44==1 & DH$P38_7_Rp==99))$WTVAR)
sum((subset(DH, DH$P44==1 & DH$P38_7_Rp==98))$WTVAR)
sum((subset(DH, DH$P44==1 & DH$P38_7_Rp==2))$WTVAR)
sum((subset(DH, DH$P44==1 & DH$P38_7_Rp==1))$WTVAR)

sum((subset(DH, DH$P44==2 & DH$P38_7_Rp==1))$WTVAR
sum((subset(DH, DH$P44==2 & DH$P38_7_Rp==2))$WTVAR)
sum((subset(DH, DH$P44==2 & DH$P38_7_Rp==98))$WTVAR)
sum((subset(DH, DH$P44==2 & DH$P38_7_Rp==99))$WTVAR)

sum((subset(DH, DH$P44==3 & DH$P38_7_Rp==1))$WTVAR)
sum((subset(DH, DH$P44==3 & DH$P38_7_Rp==2))$WTVAR)
sum((subset(DH, DH$P44==3 & DH$P38_7_Rp==98))$WTVAR)
sum((subset(DH, DH$P44==3 & DH$P38_7_Rp==99))$WTVAR)

sum((subset(DH, DH$P44==4 & DH$P38_7_Rp==1))$WTVAR)
sum((subset(DH, DH$P44==4 & DH$P38_7_Rp==2))$WTVAR)
sum((subset(DH, DH$P44==4 & DH$P38_7_Rp==98))$WTVAR)
sum((subset(DH, DH$P44==4 & DH$P38_7_Rp==99))$WTVAR)

sum((subset(DH, DH$P44==5 & DH$P38_7_Rp==99))$WTVAR)
sum((subset(DH, DH$P44==5 & DH$P38_7_Rp==98))$WTVAR)
sum((subset(DH, DH$P44==5 & DH$P38_7_Rp==2))$WTVAR)
sum((subset(DH, DH$P44==5 & DH$P38_7_Rp==1))$WTVAR)

sum((subset(DH, DH$P44==6 & DH$P38_7_Rp==1))$WTVAR)
sum((subset(DH, DH$P44==6 & DH$P38_7_Rp==2))$WTVAR)
sum((subset(DH, DH$P44==6 & DH$P38_7_Rp==98))$WTVAR)
sum((subset(DH, DH$P44==6 & DH$P38_7_Rp==99))$WTVAR)

# Reemplazar los valores de la distribución inicial con los
valores ponderados

      1       2     98     99
  1 15.2    6.77    1.94   1.73
  2 218.05  216.19  41.59  18.69
  3 793.92  830.25  21.87  58.57
  4 329.18  304.2   9.03   18.03
  5 178.29  217.48  5.81   6.06
  6 15.52   3.22    0      0

# Para la pregunta que interesa, sacar todos los porcentajes
de la columna dos.

# Data.frame de la pregunta 38. Datos de confianza en las FFAA según nivel 
de estudios.

data2 = data.frame(Niv.Estudios=c("Primaria", "Secundaria", 
"Superior no Universitaria/ Técnica", "Superior Universitaria"), 
D=c("", "", "", ""), Porcentaje=c(43.72, 48.71, 
46.06, 53.35))

# Gráfico

b = ggplot(data2, aes(x=D, y=Porcentaje, fill=Niv.Estudios))+
geom_bar(stat="identity", position=position_dodge(width=0.9),width=0.5)+ 
geom_text(aes(label=scales::percent(Porcentaje, scale = 1, accuracy = 1)), 
vjust=-1, color="black", 
position = position_dodge(0.9), size=2.8)+scale_fill_brewer(palette="Set1")+
theme_minimal()+ggtitle("Confianza en FFAA, según nivel educativo ")+
labs(y="", x="")

b

C) Confianza de mujeres en las FFAA

# Data.frame de la pregunta 38, sobre la base del documento de Excel
'Tabla de resultados por edad, sexo y NSE'. Datos apoyo a FFAA y otras 
instituciones por parte de mujeres.

data3 = data.frame(Institución=c("FFAA", "Medios de Com.", "ONGs", "PNP", 
"Minist. Púb.", "Defens. del Pueblo", "Muni Distrital", "Empresas", 
"Poder Jud.", "Gobierno", "Trib. Constituc.", "Gob. Regional", "Congreso"), 
D=c("", "", "", "", "", "", "", 
"", "", "", "", "", ""), 
Porcentaje=c(42, 41, 37, 27, 26, 25, 25, 23, 17, 17, 16, 16, 8))

# Gráfico

c = ggplot(data=data3, aes(x=reorder(Institución, Porcentaje), 
y=Porcentaje))+ geom_bar(stat="identity")+
geom_text(aes(label=scales::percent(Porcentaje, scale = 1, accuracy = 1)), 
vjust=1.6, color="white", position = position_dodge(0.9), size=4)+
ggtitle("Confianza de mujeres sobre si cada institución los derechos humanos")+ 
labs(y="", x="")+theme(plot.title = element_text(color="black", size=13,
face="bold"))+ylim(0,45)

c

# Data.frame de la pregunta 38, sobre la base del documento de Excel
'Tabla de resultados por edad, sexo y NSE'. Datos apoyo a FFAA de mujeres
y hombres.

data4 = data.frame(Sexo=c("Mujer", "Mujer", "Mujer", "Hombre", "Hombre",
"Hombre"), Confianza=c("Confía", "Desconfía", "No precisa/ no sabe", 
"Confía", "Desconfía", "No precisa/ no sabe"), 
Porcentaje=c(42, 52, 6, 54, 41, 5))

# Gráfico

d = ggplot(data=data4, aes(x=Sexo, y=Porcentaje, fill=Confianza))+
geom_bar(stat="identity", position=position_dodge(), width=0.3)+
geom_text(aes(label=scales::percent(Porcentaje, scale = 1, accuracy = 1)), 
vjust=1.6, color="white", 
position = position_dodge(0.3), size=5)+
ggtitle("Confianza en FFAA, según sexo")+ labs(y="", x="")+
theme(plot.title = element_text(color="black", size=15, face="bold"))+
ylim(0,60)

d

D) Derechos humanos y empresas

# Creación de tres conjuntos de datos: uno de quienes consideran que
grandes empresas sí respetan derechos humanos, otro de quienes
consideran que dichas empresas no respetan derechos humanos, y otro de
no precisa.

DH1 = subset(DH, P42_1_Rp==1) 
DH2 = subset(DH, P42_1_Rp==2)
DH3 = subset(DH, P42_1_Rp==99)

# Sacar porcentajes de quienes consideran que grandes empresas sí 
respetan derechos humanos vs quienes consideran que estas
no respetan derechos humanos, según el encuestado vive en Lima 
Metropolitana fuera de ella ("fuera de ella" incluye Lima provincias).
Las operaciones incluyen ponderación de resultados.

sum((subset(DH1, PA==1))$WTVAR)*100/sum((subset(DH, PA==1))$WTVAR)
sum((subset(DH1, PA>1))$WTVAR)*100/sum((subset(DH, PA>1))$WTVAR)

sum((subset(DH2, PA==1))$WTVAR)*100/sum((subset(DH, PA==1))$WTVAR)
sum((subset(DH2, PA>1))$WTVAR)*100/sum((subset(DH, PA>1))$WTVAR)

sum((subset(DH3, PA==1))$WTVAR)*100/sum((subset(DH, PA==1))$WTVAR)
sum((subset(DH3, PA>1))$WTVAR)*100/sum((subset(DH, PA>1))$WTVAR)

# Con las cifras obtenidas, armar el data frame de dicha distribución
porcentual

data5 = data.frame(Dónde=c("Lima", "Lima", "Lima", "Resto del Perú", 
"Resto del Perú", "Resto del Perú"), Percepción=c("Respetan", "No respetan",
"No precisa","Respetan", "No respetan", "No precisa"),
Porcentaje=c(28.09, 68.91, 3.00, 18.74, 76.18, 5.078704))

# Visualizar en columnas dicha distribución

head(data5)

# Gráfico

e = ggplot(data=data5, aes(x=Dónde, y=Porcentaje, fill=Percepción))+
geom_bar(stat="identity", position=position_dodge(),width=0.6)+
ggtitle("Percepción de si grandes empresas respetan o no derechos humanos")+ 
labs(y="", x="")+
geom_text(aes(label=scales::percent(Porcentaje, scale = 1, accuracy = 1)), 
vjust=-0.6, color="black", 
position = position_dodge(0.6), size=5)+scale_fill_brewer(palette="Set1")+
theme_minimal()+
theme(plot.title = element_text(color="black", size=15, face="bold"))

e

# Porcentajes según departamentos para armar otro data frame. 
Resultados ponderados al factor de ponderación.

sum((subset(DH1, PA==1))$WTVAR)*100/sum((subset(DH, PA==1))$WTVAR)
sum((subset(DH1, PA==2))$WTVAR)*100/sum((subset(DH, PA==2))$WTVAR)
sum((subset(DH1, PA==3))$WTVAR)*100/sum((subset(DH, PA==3))$WTVAR)
sum((subset(DH1, PA==4))$WTVAR)*100/sum((subset(DH, PA==4))$WTVAR)
sum((subset(DH1, PA==5))$WTVAR)*100/sum((subset(DH, PA==5))$WTVAR)
sum((subset(DH1, PA==6))$WTVAR)*100/sum((subset(DH, PA==6))$WTVAR)
sum((subset(DH1, PA==7))$WTVAR)*100/sum((subset(DH, PA==7))$WTVAR)
sum((subset(DH1, PA==8))$WTVAR)*100/sum((subset(DH, PA==8))$WTVAR)
sum((subset(DH1, PA==9))$WTVAR)*100/sum((subset(DH, PA==9))$WTVAR)
sum((subset(DH1, PA==10))$WTVAR)*100/sum((subset(DH, PA==10))$WTVAR)
sum((subset(DH1, PA==11))$WTVAR)*100/sum((subset(DH, PA==11))$WTVAR)
sum((subset(DH1, PA==12))$WTVAR)*100/sum((subset(DH, PA==12))$WTVAR)
sum((subset(DH1, PA==13))$WTVAR)*100/sum((subset(DH, PA==13))$WTVAR)
sum((subset(DH1, PA==14))$WTVAR)*100/sum((subset(DH, PA==14))$WTVAR)
sum((subset(DH1, PA==15))$WTVAR)*100/sum((subset(DH, PA==15))$WTVAR)
sum((subset(DH1, PA==16))$WTVAR)*100/sum((subset(DH, PA==16))$WTVAR)
sum((subset(DH1, PA==17))$WTVAR)*100/sum((subset(DH, PA==17))$WTVAR)
sum((subset(DH1, PA==18))$WTVAR)*100/sum((subset(DH, PA==18))$WTVAR)
sum((subset(DH1, PA==19))$WTVAR)*100/sum((subset(DH, PA==19))$WTVAR)
sum((subset(DH1, PA==20))$WTVAR)*100/sum((subset(DH, PA==20))$WTVAR)
sum((subset(DH1, PA==21))$WTVAR)*100/sum((subset(DH, PA==21))$WTVAR)
sum((subset(DH1, PA==22))$WTVAR)*100/sum((subset(DH, PA==22))$WTVAR)
sum((subset(DH1, PA==23))$WTVAR)*100/sum((subset(DH, PA==23))$WTVAR)
sum((subset(DH1, PA==24))$WTVAR)*100/sum((subset(DH, PA==24))$WTVAR)
sum((subset(DH1, PA==300))$WTVAR)*100/sum((subset(DH, PA==300))$WTVAR)

# Con las cifras obtenidas para Lima, Piura, Huánuco y Ayacucho, 
armar el data frame de dicha distribución porcentual

data6 = data.frame(Dónde=c("Piura", "Lima", "Huánuco", "Ayacucho"), 
Porcentaje=c(30.57, 28.09, 6.14, 5.15))

# Gráfico

f = ggplot(data6, aes(x=Dónde, y=Porcentaje))+
geom_bar(stat = "identity", width=0.4, fill = "brown", color = "grey40", 
alpha = 5)+ 
geom_text(aes(label=scales::percent(Porcentaje, scale = 1, accuracy = 1)), 
vjust=1.5, color="white", size=5)+
ggtitle("Confianza en que empresas respetan derechos humanos, en cuatro
departamentos")+labs(y="", x="")+
theme(plot.title = element_text(color="black", size=15, face="bold"))+
ylim(0,40)

f


