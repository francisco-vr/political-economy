
## Processing


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


# usage

packages <- c("foreign","gdata","wnominate","MCMCpack","pscl","anonimate","oc", "dplyr", "tidyverse","xtable",
              "modelsummary", "data.table")
ipak(packages)

entrada <-read_csv("data/Input_data/votos_comision_Forma de Estado.csv")


comision <-dplyr::filter(entrada, (`8706` != (1 | 0)) | (`7733` != (1|0)) | (`7697` != (1 | 0)) | (`8686` !=(1 | 0)) |(`8697` != (1 | 0)))


## Crear subcomision 1

subcom1 <-comision[c(1,3,4,5,7,10,11,14,16,17,19,20,23,25),]

subcom2 <-comision[c(2,6,8,9,12,13,15,18,21,22,24),]

## Crear subcomision 2



xtable(as_tibble(print(comision$nombres)))

saveRDS(comision, file = "data/Final_data/comision_final.RDS")
saveRDS(subcom1, file = "data/Final_data/subcom1.RDS")
saveRDS(subcom2, file = "data/Final_data/subcom2.RDS")

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

### ANÀLISIS SUBCOMISIÓN 1 ###

# esta base contiene votaciones de los representantes de naciones comisionidas

subcom1 <- readRDS("data/Final_data/subcom1.RDS")

# preparamos la base de datos

lista_subcom1 <-subcom1[,2]
subcom1 <- subcom1[,-c(1,2)] # en la parte de datos solo deben quedar los votos

# para correr wnominate se crea el objeto de clase rollcall
# con la opcion F1 pueden ver el detalle de los elementos del objeto
rc <- rollcall(subcom1,             
               yea=1, 
               nay=0,
               missing=NA, # todos los otros datos quedan como missing
               legis.names=lista_subcom1,
               legis.data=NULL,
               desc="")

result_subcom1 <- wnominate(rc, dims=1, polarity=8)
summary(result_subcom1) # el objeto results contiene la estimacion



tabla_subcom1 <-data.frame(lista_subcom1,result_subcom1$legislators)
tabla_subcom1 <-tabla_subcom1[,c(1,6,7,8)]
tabla_subcom1 <-tabla_subcom1[order(tabla_subcom1$coord1D), c(1,2,3,4)]
tabla_subcom1$rank <-rank(tabla_subcom1$coord1D)

saveRDS(tabla_subcom1, file = "Results/tabla_subcom1.rds")


xtable(tabla_subcom1, caption = "Tabla de estimación ideológica, Subcomisión 1")

# APRE: Aggregated Proportional reduction of error.
# GMP: Geometric mean probability
# miden la mejora en la estimacion al pasar de comisiona a dos dimensiones

plot(X1,type="n",asp=1,
     xlab="1a dimension",
     ylab="2a dimension",
     xlim=c(-1.0,1.0),ylim=c(-1.0,1.0),font=2,cex=1.2)
mtext("Estimación Ideológica comisión 'Forma del Estado', en Convención Constitucional",side=3,line=1.50,cex=1.2,font=2)
points(X1[result$legislators$party == "Other"],X2[result$legislators$party == "Other"],pch=16,col="blue",font=2)
points(X1[result$legislators$party == "WP"],X2[result$legislators$party == "WP"],pch=16,col="red",font=2)

# el paquete trae por defecto la posibilidad de comision plot de los resultados
plot(result)


Plot_Subcom1 <-ggplot(tabla_subcom1, aes(x = coord1D, y = rank)) +
  geom_point() +
  geom_line()+
  geom_text(label = tabla_subcom1$nombres, nudge_y = 0.3, check_overlap = T) +
  labs(y = "Ranking",
       x = "Coordenadas",
       title= "Estimación Ideológica Subcomisión 1, Formas del Estado",
       subtitle = "1 dimensión",
       caption = "Línea roja: Votante mediano de la izquierda. Línea azul: Votante mediano de la derecha")+
  geom_vline(xintercept = -0.7432000, colour = "red", linetype = "dashed")+
  geom_vline(xintercept = -0.8328199, colour = "blue", linetype = "dashed")+
  theme(axis.text = element_text(size = 15),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())

ggsave(Plot_Subcom1, filename = "Results/Plot_Subcom1.png",
       dpi = 400, width = 15, height = 9)



### ANALISIS SUBCOMISION 2 ###

subcom2 <- readRDS("data/Final_data/subcom2.RDS")

# preparamos la base de datos

lista_subcom2 <-subcom2[,2]
subcom2 <- subcom2[,-c(1,2)] # en la parte de datos solo deben quedar los votos

# para correr wnominate se crea el objeto de clase rollcall
# con la opcion F1 pueden ver el detalle de los elementos del objeto
rc2 <- rollcall(subcom2,             
               yea=1, 
               nay=0,
               missing=NA, # todos los otros datos quedan como missing
               legis.names=lista_subcom2,
               legis.data=NULL,
               desc="")

result_subcom2 <- wnominate(rc2, dims=1, polarity=6) #Se utiliza a alvaro Jofré cáceres como extremo por derecha para ordenar
summary(result_subcom2) # el objeto results contiene la estimacion



tabla_subcom2 <-data.frame(lista_subcom2,result_subcom2$legislators)
tabla_subcom2 <-tabla_subcom2[,c(1,6,7,8)]
tabla_subcom2 <-tabla_subcom2[order(tabla_subcom2$coord1D), c(1,2,3,4)]
tabla_subcom2$rank <-rank(tabla_subcom2$coord1D)

saveRDS(tabla_subcom2, file = "Results/tabla_subcom2.rds")


xtable(tabla_subcom2, caption = "Tabla de estimación ideológica de subcomisión 2, Forma del Estado")

# APRE: Aggregated Proportional reduction of error.
# GMP: Geometric mean probability
# miden la mejora en la estimacion al pasar de comisiona a dos dimensiones


# el paquete trae por defecto la posibilidad de comision plot de los resultados
plot(result_subcom2)


Plot_Subcom2 <-ggplot(tabla_subcom2, aes(x = coord1D, y = rank)) +
  geom_point() +
  geom_line()+
  geom_text(label = tabla_subcom2$nombres, nudge_y = 0.3, check_overlap = T) +
  labs(y = "Ranking",
       x = "Coordenadas",
       title= "Estimación Ideológica Subcomisión 2, Forma del Estado",
       subtitle = "1 dimensión",
       caption = "Línea naranja: votante mediano tanto para izquierda como derecha")+
  geom_vline(xintercept = -0.55809736, colour = "orange", linetype = "dashed")+
  theme(axis.text = element_text(size = 15),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.caption = element_text(size = 14),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())

ggsave(Plot_Subcom2, filename = "Results/Plot_Subcom2.png",
       dpi = 400, width = 15, height = 9)


### ANÀLISIS COMISION FORMA DEL ESTADO GENERAL ####


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
               desc="")

result <- wnominate(rc, dims=1, polarity=14) # Se utilida a Harry Jurguensen como extremo por derecha
summary(result) # el objeto results contiene la estimacion

comision_general <-data.frame(nombre_perso,result$legislators)
comision_general <-comision_general[,c(1,6,7,8)]
comision_general <-comision_general[order(comision_general$coord1D), c(1,2,3,4)]
comision_general$rank <-rank(comision_general$coord1D)

saveRDS(comision_general, file = "data/Final_data/comision_general.rds")


xtable(comision_general, caption = "Tabla de comisión Forma del Estado, ordenada por estimación ideológica")

# el paquete trae por defecto la posibilidad de comision plot de los resultados

plot(result)

# Ploteo con más detalles:

Plot_comision_general <-ggplot(comision_general, aes(x = coord1D, y = rank)) +
  geom_point() +
  geom_line()+
  geom_text(label = comision_general$nombres, nudge_y = 0.3, check_overlap = T) +
  labs(y = "Ranking",
       x = "Coordenadas",
       title= "Estimación Ideológica comision Forma del Estado",
       subtitle = "1 dimensión",
       caption = "Línea verde: votante mediano tanto para izquierda como derecha")+
  geom_vline(xintercept = -0.7040421, colour = "green", linetype = "dashed")+
  theme(axis.text = element_text(size = 15),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.caption = element_text(size = 14),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())

ggsave(Plot_comision_general, filename = "Results/Plot_comision_general.png",
       dpi = 400, width = 15, height = 9)


## ANÁLISIS DEL PLENO COMPLETO


pleno <-read_csv("data/Input_data/scrap_votos_plenari_final.csv")


# preparamos la base de datos
#
votos <-dplyr::select(pleno, "nombre", `1022`, `1023`,`1071`,`1070`,`1069`,`1068`,`1067`,`1066`,`1065`,`1132`:`1224`,
                      `1892`:`1908`, `1970`:`2044`) #Elegir columnas correctas para ver el match de votos con comisión
constituyente <-pleno[,2]
x <-pleno[,1]


pleno <- pleno[,-c(1,2)] # en la parte de datos solo deben quedar los votos

# para correr wnominate se crea el objeto de clase rollcall
# con la opcion F1 pueden ver el detalle de los elementos del objeto
rc_pleno <- rollcall(pleno,             
               yea=1, 
               nay=0,
               missing=NA, # todos los otros datos quedan como missing
               legis.names=constituyente,
               legis.data=NULL,
               desc="")

result <- wnominate(rc_pleno, dims=1, polarity=97) # Se utilida a Montealegre en base a la estimacion de Fabrega (2021)
summary(result) # el objeto results contiene la estimacion

plot(result)

pleno_general <-data.frame(constituyente,result$legislators)
pleno_general <-pleno_general[,c(1,6,7,8)]
pleno_general <-pleno_general[order(pleno_general$coord1D), c(1,2,3,4)]
pleno_general$rank <-rank(pleno_general$coord1D)

saveRDS(pleno_general, file = "data/Final_data/tabla_pleno.rds")



##AGREGAR COLUMNAS DE VOTACIÓN PARA VER PORCENTAJE DE ÉXITO

Encoding(pleno_general[[1]]) <-"UTF-8"


Plot_pleno <-ggplot(pleno_general, aes(x = coord1D, y = rank)) +
  geom_point()+ 
  geom_line()+
  geom_text(label = pleno_general$nombre, nudge_y = 0.3, check_overlap = T)+
  labs(y = "Ranking",
       x = "Coordenadas",
       title= "Estimación Ideológica del Pleno de la Convención Constitucional y votantes pivotales",
       subtitle = "1 dimensión",
       caption = "Línea verde: votante pivotal para la izquierda en el pleno de la convención. \n Línea roja: votante medio subcomisión 1 y comisión Forma del Estado en general. \n Linea azul: votante medio subcomisión 2")+
  geom_vline(xintercept = -0.281722039, colour = "green", linetype = "dashed")+
  geom_vline(xintercept = -0.660621226, colour = "red", linetype = "dashed")+
  geom_vline(xintercept = -0.332806796, colour = "blue", linetype = "dashed")+
  theme(axis.text = element_text(size = 10),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.caption = element_text(size = 14),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())

ggsave(Plot_pleno, filename = "Results/Plot_pleno.png",
       dpi = 400, width = 15, height = 24)


##PLOTEANDO ÉXITO


pleno_voto <-merge(x = pleno_general, y = votos, by = "nombre")
pleno_voto <-pleno_voto[order(pleno_general$coord1D), ]
pleno_voto$rank <-rank(pleno_voto$coord1D)

saveRDS(pleno_voto, file = "data/Final_data/pleno_votos.RDS")


Encoding(pleno_general[[1]]) <-"UTF-8"


loop.vector <-6:139

for (i in 6:ncol(pleno_voto)){
  
 p <-ggplot(pleno_voto, aes(x = coord1D, y = rank, color = as.character(pleno_voto[,i]))) +
   geom_point()+ 
   geom_line()+
   geom_text(label = pleno_voto$nombre, nudge_y = 0.3, check_overlap = T)+
   labs(y = "Ranking",
        x = "Coordenadas",
        title= "Votos en pleno de convención relativos a la comisión Formas del Estado",
        subtitle = paste("Voto", i-5),
        caption = "Línea verde: votante pivotal para la izquierda en el pleno de la convención. \n Línea roja: votante medio subcomisión 1 y comisión Forma del Estado en general. \n Linea azul: votante medio subcomisión 2")+
     scale_color_discrete(name = "Votación", labels = c("En contra", "A favor"))+
     geom_vline(xintercept = -0.33147427, colour = "green", linetype = "dashed")+
   geom_vline(xintercept = -0.67978555, colour = "red", linetype = "dashed")+
   geom_vline(xintercept = -0.41015172, colour = "blue", linetype = "dashed")+
   theme(axis.text = element_text(size = 10),
         axis.title= element_text(size=16,face="bold"),
         plot.title = element_text(size = 18, face = "bold"),
         plot.caption = element_text(size = 12),
         panel.grid.major = element_line(colour = "grey70", size = 0.2),
         panel.grid.minor = element_blank())
 
 nombres <-pleno_voto[,6:139]%>%
   print(colnames(nombres))
  
  fp <-file.path("Results/gif_plot/", paste0(print(pleno_voto,colnames(i),".png")))
  
  
  ggsave(p, filename = fp, device = png, dpi = 300, width = 18, height = 22)
  
}

saveRDS(pleno_voto, file = "data/Final_data/pleno_votos_comision.RDS")


Plot_pleno_voto <-ggplot(pleno_voto, aes(x = coord1D, y = rank, color = as.character(X15032022_06))) +
  geom_point()+ 
  geom_line()+
  geom_text(label = pleno_general$candidato, nudge_y = 0.3, check_overlap = T)+
  labs(y = "Ranking",
       x = "Coordenadas",
       title= "Votaciones a favor y en contra cruzada con estimación ideológica",
       subtitle = "1 dimensión",
       caption = "Línea verde: votante pivotal para la izquierda en el pleno de la convención. \n Línea roja: votante medio subcomisión 1 y comisión Forma del Estado en general. \n Linea azul: votante medio subcomisión 2")+
  geom_vline(xintercept = -0.33147427, colour = "green", linetype = "dashed")+
  geom_vline(xintercept = -0.67978555, colour = "red", linetype = "dashed")+
  geom_vline(xintercept = -0.41015172, colour = "blue", linetype = "dashed")+
  theme(axis.text = element_text(size = 10),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.caption = element_text(size = 14),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())

ggsave(Plot_pleno_voto, filename = "Results/Plot_pleno_vot.png",
       dpi = 400, width = 15, height = 20)



