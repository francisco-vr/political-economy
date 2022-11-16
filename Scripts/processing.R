
## Processing


ipak <- fcomisionction(pkg){
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

plot(X1,X2,type="n",asp=1,
     xlab="1a dimension",
     ylab="2a dimension",
     xlim=c(-1.0,1.0),ylim=c(-1.0,1.0),font=2,cex=1.2)
mtext("Estimación Ideológica comisión 'Forma del Estado', en Convención Constitucional",side=3,line=1.50,cex=1.2,font=2)
points(X1[result$legislators$party == "Other"],X2[result$legislators$party == "Other"],pch=16,col="blue",font=2)
points(X1[result$legislators$party == "WP"],X2[result$legislators$party == "WP"],pch=16,col="red",font=2)

# el paquete trae por defecto la posibilidad de comision plot de los resultados
plot(result)

hola <-ggplot(result$legislators)


# tambien se puede separar para efectos de publicacion
plot.coords(result) # separa por grupos
plot.scree(result) # peso relativo de cada eigenvalue
plot.cutlines(result, lines=5) # pcomisiontos de corte de cada proyecto
plot.angles(result) # histograma de los pcomisiontos de corte por angulos

# entonces ahora hagamos lo mismo con datos de Chile
senado <- read.csv(paste0(aqui,"/votos2014_2016_procesado.csv"),sep=";")
nombres <- senado[,1]
senado <- senado[,2:NCOL(senado)]
rc_senado <- rollcall(senado,             
               yea=c(1), # reduce los valores a dos grupos yea/nay
               nay=c(-1),
               missing=c(0), # todos los otros datos quedan como missing
               notInLegis=NULL, # vector de ausentes en que seccion
               legis.names=nombres,
               legis.data=NULL,
               desc="Senado 2014-15")
result_senado <- wnominate(rc_senado, dims=2, polarity=c(35,35))
summary(result_senado) # el objeto results contiene la estimacion
windows()
plot(result_senado)

WEIGHT=(result_senado$weights[2])/(result_senado$weights[1]) # peso relativo dado a la segcomisionda dimension
X1 <- result_senado$legislators$coord1D   # primera dimension
X2 <- (result_senado$legislators$coord2D)*WEIGHT # segcomisionda dimension


# Veamos el grafico ahora paso a paso

proyecto <- 27
voto <- as.integer(rc_senado$votes[,proyecto])
DL1 <- result_senado$rollcalls[proyecto,7]  # spread en dim 1
DL2 <- result_senado$rollcalls[proyecto,8]  # spread en dim 2
ZM1 <- result_senado$rollcalls[proyecto,9]  # pcomisionto medio en dim 1
ZM2 <- result_senado$rollcalls[proyecto,10] # pcomisionto medio en dim 2
YEA1 <- ZM1-DL1              # diff entre pcomisionto medio y desv en dim 1
YEA2W <- (ZM2-DL2)*WEIGHT    # diff entre pcomisionto medio y desv en dim 2
NAY1 <- ZM1+DL1              # idem para votos no en dim 1
NAY2W <- (ZM2+DL2)*WEIGHT    # idem para votos no en dim 2
A1 <- NAY1 - YEA1            # distancia yes-no  
A2 <- NAY2W - YEA2W
ALENGTH <- sqrt(A1*A1 + A2*A2) # radio
N1W <- A1/ALENGTH            # distancia (yes-no)/radio
N2W <- A2/ALENGTH
if (N1W < 0){  # nos aseguramos que sean distancias, por lo tanto, signo +
  N1W <- -N1W
  N2W <- -N2W
}
ws <- N1W*ZM1 + N2W*ZM2*WEIGHT  # pondera pesos
xws <- ws*N1W
yws <- ws*N2W


windows()
plot(X1, X2,
     xlab="1ra dimensión (izquierda-derecha)
     N=En contra, S=A favor",
     ylab="2da Dimensión",
     #col=partidos,
     pch=16,
     main="Senado - WNOMINATE - Proyecto de ley")
segments(xws+N2W, yws-N1W, xws-N2W, yws+N1W, col="black", lwd=2)

# Boostrapping en WNOMINATE
library(ellipse)
result_senado_boot <- wnominate(rc_senado, 
                                   ubeta=15, # valores por defecto
                                   uweights=0.5, # valores por defecto
                                   dims=2, 
                                   minvotes=10,
                                   lop=0.025, 
                                   trials=20, # boots
                                   polarity=c(1,5), 
                                   verbose=FALSE)
std1 <- result_senado_boot$legislators$se1D
std2 <- result_senado_boot$legislators$se2D * WEIGHT
corr12 <- result_senado_boot$legislators$corr.1

windows()
plot(X1, X2,
     xlab="1ra dimensiÃ³n (izquierda-derecha)
     Pcomisiontos: Ideal points; Elipses: Margen de error",
     ylab="2da DimensiÃ³n",
     #col=partidos,
     pch=16,
     main="Ejemplo - WNOMINATE
     Intervalos de confianza de pcomisiontos ideales - NM",
     type="n")

for (i in 1:nrow(result_senado_boot$legislators)){
#  if(!is.na(corr12[i]) & (coaliciones[i]=="red")){
  if(!is.na(corr12[i]) ){  
    lines(c(X1[i],X1[i]), c(X2[i]- 1.96*std2[i], X2[i] + 1.96*std2[i]),col="gray")
    lines(c(X1[i] - 1.96*std1[i], X1[i] + 1.96*std1[i]), c(X2[i], X2[i]),col="gray")
    if (abs(corr12[i]) > .30){
      lines(ellipse(x=corr12[i], scale=c(std1[i],std2[i]),centre=c (X1[i] ,X2[i])), col="gray")
    }
  }
}
#points(X1[coaliciones == "red"], X2[coaliciones == "red"], pch=16, col="red", font=2)
segments(xws+N2W, yws-N1W, xws-N2W, yws+N1W, lwd=2, col="black")
polarity <- X1*N1W + X2*N2W - ws


library(devtools)
devtools::install_github("jaytimm/wnomadds")
library(wnomadds)
