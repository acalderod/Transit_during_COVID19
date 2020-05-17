#paquetes

library(tidyverse)
library(janitor)
library(ggthemes)

#Importación de archivo
#El archivo se encuentra en: https://datos.cdmx.gob.mx/explore/dataset/afluencia-preliminar-en-transporte-publico/information/

data <- read.csv("afluencia-preliminar-en-transporte-publico.csv")

# Limpieza de archivo

data$FECHA <- as.Date(data$FECHA, format = "%d/%m/%Y")

#Desarrollo del gráfico

plot <- data %>%
  filter(!is.na(AFLUENCIA.TOTAL) & ORGANISMO == "Metrobús") %>%
  ggplot(aes(FECHA, AFLUENCIA.TOTAL, color = LINEA.SERVICIO))+
  geom_line(size = 1)+
  labs(title = "Preliminar daily ridership during COVID-19 pandemic",
       subtitle = "Mexico City's BRT system (Metrobús)",
       x = "",
       y = "Daily users",
       caption = "Data retrieved from Secretaría de Movilidad (ADIP, 2020). @acalderod")+
  scale_x_date(breaks = c(seq(from = as.Date("2020-03-01"), 
                              to = as.Date("2020-05-14"), 
                              by = 7)),
               date_labels = "%b-%d")+
  scale_color_discrete(name = "Line")+
  scale_y_continuous(breaks = seq(0, 600000, 100000))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 17),
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_text(size = 16),
        axis.text.y = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 18),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.background = element_rect(fill = "ivory1",
                                         colour = "white"))+
  geom_vline(xintercept = as.Date("2020-03-23"), 
             size = 1,linetype = "dashed", color = "snow4")+
  geom_text(x= as.Date("2020-04-06") ,y = 350000,
            label = "Official social distancing strategies", 
            size = 7, 
            color = "snow4",
            hjust = 0.5,
            vjust = -1)+
  ggsave(filename = "Afluencia_metro.png",
         dpi = 200, width = 16.2, height = 9)
  
#Variable que almacena la gráfica

plot
