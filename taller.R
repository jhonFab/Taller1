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

#####-------------cajas y bigotes-------------
boxplot(machos,
        main = "Tamaño de los 
    langostinos",
        xlab = "Largo en cm",
        ylab = "Machos",
        col = "blue",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
segments(x0 = mean(machos), y0 = 0.8,
         x1 = mean(machos), y1 = 1.2,
         col = "red", lwd = 1)
points(mean(machos), 1, col = 3, pch = 19)


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
#####-------------cajas y bigotes-------------
boxplot(hembras,
        main = "Tamaño de los 
     langostinos",
        xlab = "Largo en cm",
        ylab = "Hembras",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)


segments(x0 = mean(hembras), y0 = 0.8,
         x1 = mean(hembras), y1 = 1.2,
         col = "red", lwd = 1)
points(mean(hembras), 1, col = 3, pch = 19)



#-------------------Ejercicio 2 -------------------

# x= numero de billetes falsos 
#p(x<=25) donde n=25, size=900 y la p=0.015
pbinom(25,900,0.015)
sum(dbinom(0:900,900,0.015))

#numero de billetes falsos que esten 20 y 30 
#p(20< x <30)
pbinom(30,900,0.015)-pbinom(20,900,0.015)

# que mas de 10 billetes sean falsos
#p(x>10)
1-pbinom(10,900,0.015)

plot(0:900,dbinom(0:900,900,0.015),xlab = " Numero de billetes falsos ", 
     ylab = "Probabilidad",main="distribucion binomial (n=900, p=0.015)", 
     pch=20, col="blue", bg="green", bty="l", tcl=0.9);

plot(0:40,dbinom(0:40,900,0.015),xlab = " Numero de billetes falsos ", 
     ylab = "Probabilidad",main="distribucion binomial (n=900, p=0.015)", 
     pch=20, col="blue", bg="green", bty="l", tcl=0.9);

sum(dbinom(0:30,900,0.015))
pbinom(25,900,0.015)

#-------------------------ejercicio 3 ----------------

# x= habitantes que superaran los 92 años de edad
#p(x>92) la varianza = 25 la desviacion estandar= 5 media =76  q=92
par(mfrow = c(1,2))
1-pnorm(92,76,5)
(1-pnorm(92,76,5))*100
x <- seq(60, 95, 0.1)
plot(x, dnorm(x, mean = 76, sd = 5), type = "l",
     ylim = c(0, 0.08), xlab = "Edad", ylab = "Frecuencia", 
     main=expression(paste("Distribución normal ",mu==76," ", sigma==5)), 
     lwd = 2, col = "red")
regionX=seq(92,95,0.01)            
xP <- c(92,regionX,95)  
yP <- c(0,dnorm(regionX,76,5),0)
polygon(xP,yP,col="orange1")

text(90, 0.02, "P(x>92)")
text(90, 0.01, "0.0687%")
abline(v = 76)


#plotDist("norm",xlim = x, mean = 76, sd = 5, groups = x > 92, type ="h")

# cuantos viviran menos de 55 y mas de 75 años de edad
#p(x<55) para lo de menos de 55
pnorm(55,76,5)*100

#p(x>75) para los de mas de 75 años
(1-pnorm(75,76,5))*100


x <- seq(50, 90, 0.1)
plot(x, dnorm(x, mean = 76, sd = 5), type = "l",
     ylim = c(0, 0.08), xlab = "Edad", ylab = "Frecuencia", 
     main=expression(paste("Distribución normal ",mu==76," ", sigma==5)), 
     lwd = 2, col = "red")
regionX1=seq(50,55,0.01)            
xP <- c(50,regionX1,55)  
yP <- c(0,dnorm(regionX1,76,5),0)
polygon(xP,yP,col="orange1")

regionX2=seq(75,90,0.01)            
xP <- c(75,regionX2,90)  
yP <- c(0,dnorm(regionX2,76,5),0)
polygon(xP,yP,col="orange1")

text(55, 0.02, "P(x<55)")
text(55, 0.01, "0.0013%")

text(80, 0.02, "P(x>75)")
text(80, 0.01, "57.926%")
abline(v = 76)




summary(hembras)
summary(machos)

