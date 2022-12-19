### Creación de plots para ver taza de éxito de la comisión por sobre el pleno

pleno_voto <-readRDS("data/Final_data/pleno_votos.RDS")

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
    geom_vline(xintercept = -0.33147427, colour = "green", linetype = "longdash")+
    geom_vline(xintercept = -0.67978555, colour = "red", linetype = "longdash")+
    geom_vline(xintercept = -0.41015172, colour = "blue", linetype = "longdash")+
    theme(axis.text = element_text(size = 10),
          axis.title= element_text(size=16,face="bold"),
          plot.title = element_text(size = 18, face = "bold"),
          plot.caption = element_text(size = 12),
          panel.grid.major = element_line(colour = "grey70", size = 0.2),
          panel.grid.minor = element_blank())
  
  
  fp <-file.path("Results/gif_plot/", paste0(i-5,".png"))
  
  
  ggsave(p, filename = fp, device = png, dpi = 150, width = 15, height = 15)
  
}


imgs <- list.files("Results/gif_plot",full.names = TRUE)
img_list <- lapply(imgs, image_read)

img_joined <- image_join(img_list)

img_animated <- image_animate(img_joined, fps = 3)

saveRDS(pleno_voto, file = "data/Final_data/pleno_votos_comision.RDS")


voto_informe_2 <-ggplot(pleno_voto, aes(x = coord1D, y = rank, color = as.character(`2043`))) +
  geom_point()+ 
  geom_line()+
  geom_text(label = pleno_voto$nombre, nudge_y = 0.3, check_overlap = T)+
  labs(y = "Ranking",
       x = "Coordenadas",
       title= "Votos en pleno de convención relativos a la comisión Formas del Estado",
       caption = "Línea verde: votante pivotal para la izquierda en el pleno de la convención. \n Línea roja: votante medio subcomisión 1 y comisión Forma del Estado en general. \n Linea azul: votante medio subcomisión 2")+
  scale_color_discrete(name = "Votación", labels = c("En contra", "A favor"))+
  geom_vline(xintercept = -0.33147427, colour = "green", linetype = "dashed")+
  geom_vline(xintercept = -0.67978555, colour = "red", linetype = "dashed")+
  geom_vline(xintercept = -0.41015172, colour = "blue", linetype = "dashed")+
  theme(axis.text = element_text(size = 10),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.caption = element_text(size = 14),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())

ggsave(voto_informe_2, filename = "Results/voto_informe_2.png",
       dpi = 400, width = 15, height = 20)



