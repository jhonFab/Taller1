hembras<-c(183.2,184.1,183.0,204.3,176.5,179.0,188.3,186.8,202.2,182.5,190.0,
           178.1,193.2,180.4,184.3,189.2,189.1,203.1,166.8,196.3,193.3,187.3,
           185.8,189.3,195.5,202.4,210.8)

machos<-c(140.9,121.7,173.8,154.5,109.2,150.7,203.3,163.0,137.7,173.9,177.4,
          154.8,177.5,153.4,138.7,136.7,165.3,126.7,118.9,140.0,192.7,134.4,
          175.0,169.8,153.9,176.7,150.0)

#1.a) Haga un histograma con cinco clases y determine la distribución de los 
#datos para cada sexo. Explique acerca de la distribución del tamaño para cada género. 
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

hist(machos, breaks = nbarrasm, main = "Histograma de frecuencias", 
     ylab = "Frecuencia")
