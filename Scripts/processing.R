
## Processing


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


# usage

packages <- c("foreign","gdata","wnominate","MCMCpack","pscl","anonimate","oc", "dplyr", "tidyverse","xtable")
ipak(packages)

entrada <-read_csv("data/Input_data/votos_comision_Forma de Estado.csv")


comision <-dplyr::filter(entrada, (`8706` != (1 | 0)) | (`7733` != (1|0)) | (`7697` != (1 | 0)) | (`8686` !=(1 | 0)) |(`8697` != (1 | 0)))


xtable(as_tibble(print(comision$nombres)))

saveRDS(comision, file = "data/Final_data/comision_final.RDS")


# aqui trabajaremos con los la libreria wnominate
# (weighted nominal three steps estimation)
# supuestos:
# Recordemos, P(vote que y vote Yea en tema j) = P(Uijy-Uijn>0) 
# lo que es igual a P(eijn - eijy < uijy - uijn)
# Por lo tanto, es la probabilidad acumulada de la diferencia de
# los errores.

# Los datos para estimar w-nominate pueden venir en varios formatos
# aqui vemos el caso de comisiona base de datos en formato .csv
# fuente voteview.com

# esta base contiene votaciones de los representantes de naciones comisionidas

comision <- readRDS("data/Final_data/comision_final.RDS")

# preparamos la base de datos

nombre_perso <-comision[,2]
comision <- comision[,-c(1,2)] # en la parte de datos solo deben quedar los votos

# para correr wnominate se crea el objeto de clase rollcall
# con la opcion F1 pueden ver el detalle de los elementos del objeto
rc <- rollcall(comision,             
               yea=1, 
               nay=0,
               missing=NA, # todos los otros datos quedan como missing
               legis.names=nombre_perso,
               legis.data=NULL,
               desc="comision 31 to 33")

result <- wnominate(rc, dims=1, polarity=14)
summary(result) # el objeto results contiene la estimacion
# APRE: Aggregated Proportional reduction of error.
# GMP: Geometric mean probability
# miden la mejora en la estimacion al pasar de comisiona a dos dimensiones

names(result) # el objeto result contiene 7 objetos
head(result$legislators)
head(result$rollcalls)
result$dimensions
head(result$eigenvalues)
result$beta
result$weights
result$fits

WEIGHT=(result$weights[2])/(result$weights[1]) # peso relativo dado a la segcomisionda dimension
X1 <- result$legislators$coord1D   # primera dimension
X2 <- (result$legislators$coord2D)*WEIGHT # segcomisionda dimension
windows()

plot(X1,type="n",asp=1,
     xlab="1a dimension",
     ylab="2a dimension",
     xlim=c(-1.0,1.0),ylim=c(-1.0,1.0),font=2,cex=1.2)
mtext("Estimación Ideológica comisión 'Forma del Estado', en Convención Constitucional",side=3,line=1.50,cex=1.2,font=2)
points(X1[result$legislators$party == "Other"],X2[result$legislators$party == "Other"],pch=16,col="blue",font=2)
points(X1[result$legislators$party == "WP"],X2[result$legislators$party == "WP"],pch=16,col="red",font=2)

# el paquete trae por defecto la posibilidad de comision plot de los resultados
plot(result)

table(result$beta)


# tambien se puede separar para efectos de publicacion
plot.coords(result) # separa por grupos
plot.scree(result) # peso relativo de cada eigenvalue
plot.cutlines(result, lines=5) # pcomisiontos de corte de cada proyecto
plot.angles(result) # histograma de los pcomisiontos de corte por angulos


