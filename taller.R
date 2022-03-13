library(quantmod)
library("moments")

hembras<-c(183.2,184.1,183.0,204.3,176.5,179.0,188.3,186.8,202.2,182.5,190.0,
           178.1,193.2,180.4,184.3,189.2,189.1,203.1,166.8,196.3,193.3,187.3,
           185.8,189.3,195.5,202.4,210.8)

machos<-c(140.9,121.7,173.8,154.5,109.2,150.7,203.3,163.0,137.7,173.9,177.4,
          154.8,177.5,153.4,138.7,136.7,165.3,126.7,118.9,140.0,192.7,134.4,
          175.0,169.8,153.9,176.7,150.0)

#1.a) Haga un histograma con cinco clases y determine la distribuciÃ³n de los 
#datos para cada sexo. Explique acerca de la distribuciÃ³n del tamaÃ±o para cada gÃ©nero. 
#par(mfrow = c(1,2))
ancho_barras <- (max(hembras)-min(hembras))/5
ancho_barrasm <- (max(machos)-min(machos))/5

nbarras <- seq(min(hembras),
               max(hembras), by = ancho_barras)

nbarrasm <- seq(min(machos),
               max(machos), by = ancho_barrasm)
par(mfrow = c(1,2))

hist(hembras,prob = TRUE, breaks = nbarras, main = "Histograma de frecuencias", 
     ylab = "Frecuencia")
lines(density(hembras), lwd = 2, col = 'red')

hist(machos, prob = TRUE, breaks = nbarrasm, main = "Histograma de frecuencias", 
     ylab = "Frecuencia")
lines(density(machos), lwd = 2, col = 'red')


#========================================================
           #---------datos machos---------------
#========================================================

m<-mean(machos, na.rm = FALSE)# esta es la media o el promedio de machos
m
varia<-var(machos, na.rm = FALSE)# esta es la varianza
varia
des<-sd(machos, na.rm = FALSE)#  esta es la desviacion estandar
des
skewness(machos, na.rm = FALSE)# coeficiente de simetria
kurtosis(machos, na.rm = FALSE)# coeficiente de curtosis

#_________________________intervalo de nivel de confianza__
n<-length(machos) #tamaño de muestra
n
cm<-0.97;a<-1-cm #confianza y valor de alfa
t<-qt(a/2,n-1,lower.tail=F) #punto crítico
li<-m-t*des/sqrt(n) #límite inferior
li
ls<-m+t*des/sqrt(n) #límite superior
ls
c(li,ls)

#========================================================
          #---------datos hembras---------------
#========================================================
m1<-mean(hembras, na.rm = FALSE)#  esta es la media o el promedio de hembras
m1
varia1<-var(hembras, na.rm = FALSE)# esta es la varianza
varia1
des1<-sd(hembras, na.rm = FALSE)#  esta es la desviacion estandar
des1
skewness(hembras, na.rm = FALSE)# coeficiente de simetria
kurtosis(hembras, na.rm = FALSE)# coeficiente de curtosis

#_________________________intervalo de nivel de confianza__
n1<-length(hembras) #tamaño de muestra
n1
ch1<-0.97;a1<-1-ch1 #confianza y valor de alfa
t1<-qt(a1/2,n1-1,lower.tail=F) #punto crítico
li1<-m1-t*des1/sqrt(n1) #límite inferior
li1
ls1<-m1+t1*des1/sqrt(n1) #límite superior
ls1
c(li1,ls1)


##summary(hembras)

