### Creación de plots para ver taza de éxito de la comisión por sobre el pleno

readRDS("data/Final_data/pleno_votos_comision.RDS")

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