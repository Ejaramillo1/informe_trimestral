library(easypackages)
my_packages <- c("readr", "readxl", "tidyverse", "lubridate", "xts",  "tidyquant", "ggrepel", "reshape2", "ggseas")
libraries(my_packages)

db <- read.csv("BIE_BIE20180413143330.csv", encoding = "Latin-1")

bd <- read_excel("BIE_BIE20180416182227.xls", 
                 skip = 1)




# Situación general de la economía

### Tendencia del Indicador Global de la Actividad Económica

colnames(bd)


bd %>% 
  select(contains("Indicador global de la actividad económica Tendencia"), Periodo) %>%
  rename_at(vars(contains("Indicador global de la actividad económica Tendencia")), funs(stringr::str_replace_all(.,"(.+>)", "")))


bd %>%
  select(contains("Indicador global de la actividad económica Tendencia"), Periodo) %>%
  rename_all(funs(stringr::str_replace_all(.,"(.+>)", "")))
  rename("IGAE" = " Indicador global de la actividad económica Tendencia f3/ (Índice base 2013=100)") %>%
  separate(Periodo, into = c("year", "month"), sep = "/") %>%
  filter(!is.na(IGAE)) %>% 
  mutate(dia = 01,
         fecha = mdy(paste0(month, "-", dia, "-", year)),
         rocigae = ROC(IGAE)) %>%
  ggplot(aes(fecha, rocigae)) + 
  geom_line() +
  labs(title = "Indicador Global de la Actividad Económica",
       subtitle = "Tendencia de largo plazo IGAE",
       caption = "Fuente: Elaboración propia con datos de INEGI") +
  scale_x_date(date_breaks = "3 year", date_labels = "%y %b") +
  theme_tq()

dev.off()


ggsave("IGAE.jpg" ,height=7, width=10)



### Indicador Global de la actividad económica



db %>%
  select(contains("Indicador.global.de.la.actividad.económica"), Periodo) %>%
  rename("IGAE" = "Indicadores.económicos.de.coyuntura...Indicador.global.de.la.actividad.económica..base.2013...Series.originales...Índice.de.volumen.físico.Total.p2...f3...Índice.base.2013.100..Mensual") %>%
  select(Periodo, IGAE) %>%
  filter(!is.na(IGAE)) %>%
  mutate(Periodo = ymd(as_date(paste0(Periodo, sep = "/" ,"01")))) %>%
  filter(Periodo > "2015-01-01") %>%
  ggplot(aes(x = Periodo, y = IGAE, label = paste0(month(Periodo, label = TRUE), "-" ,year(Periodo)))) +
  geom_point() +
  geom_vline(aes(xintercept = as.numeric(Periodo[c(24)])), linetype = 4, colour = "black") +
  geom_line() +
  geom_ma(n = 3) +
  labs(title = "Indicador global de la actividad económica",
       subtitle = "Tendencia de largo plazo del IGAE",
       caption = "Fuente: Elaboración propia con datos del INEGI") + 
  theme_tq()


dev.off()


ggsave("IGAE2.jpg",height=7, width=10)




### Incremento porcentual respecto al periodo pasado IGAE



db %>%
  select(contains("Indicador.global.de.la.actividad.económica"), Periodo) %>%
  rename("IGAE" = "Indicadores.económicos.de.coyuntura...Indicador.global.de.la.actividad.económica..base.2013...Series.originales...Índice.de.volumen.físico.Total.p2...f3...Índice.base.2013.100..Mensual") %>%
  select(Periodo, IGAE) %>%
  filter(!is.na(IGAE)) %>%
  mutate(Periodo = ymd(as_date(paste0(Periodo, sep = "/" ,"01"))),
         prc_igae = c(NA, exp(diff(log(IGAE)))-1),
         color = if_else(prc_igae < 0, "negative", "positive")) %>%
  filter(Periodo > "2015-01-01") %>%
  ggplot(aes(x = Periodo, y = prc_igae*100)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = color)) +
  geom_text_repel(aes(label = round(prc_igae * 100, digits = 2)), size = 3) +
  theme_tq() + 
  scale_fill_manual(values = c(positive = "#6ebe4c" , negative = "#ca2128" )) + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b") +
  ylab(label = "Incremento porcentual") +
  theme(legend.position = "none") + 
  geom_vline(aes(xintercept = as.numeric(Periodo[c(24)])), linetype = 4, colour = "black") +
  labs(title = "Indicador global de la actividad económica",
       subtitle = "Incremento porcentual respecto al mes pasado",
       caption = "Fuente: Elaboración propia con datos de INEGI")

dev.off()


ggsave("IGAE3.jpg",height=7, width=10)




### Exportaciones


bd %>%
  select(contains("Exportaciones no petroleras"), Periodo) %>%
  rename_all(
    funs(
      str_replace_all(., "(.+>)", "") %>%
        str_trim(., side = "both")
    )
  ) %>%
  rename(., "export_orig" = "Exportaciones no petroleras Original r3 / f15/ (Millones de dólares)",
         "export_orig_calend" = "Exportaciones no petroleras Original corregida por efectos de calendario f16/ (Millones de dólares)",
         "export_dest" = "Exportaciones no petroleras Desestacionalizada f16/ (Millones de dólares)",
         "export_trend" = "Exportaciones no petroleras Tendencia f16/ (Millones de dólares)",
         "export_cicl" = "Exportaciones no petroleras Ciclo f16/ (Puntos)") %>%
  filter(!is.na(export_orig) & !Periodo %in% c("2018/02")) %>%
  separate(Periodo, into = c("year", "month"), sep = "/") %>%
  mutate(dia = 01,
         Periodo = mdy(paste0(month, "-", dia, "-", year))) %>%
  select(-year, -month, -dia, - export_cicl) %>%
  filter(Periodo > "2009-12-01") %>%
  gather(key = type, value = "value", -Periodo) %>%
  ggplot(aes(Periodo, value)) + 
  geom_line() + 
  facet_wrap(~type) + 
  labs(title = "Exportaciones en México",
       subtitle = "Exportaciones mexicanas en dólares periodo 2010-2018",
       caption = "Fuente: Elaboración propia con datos del INEGI") +
  theme_tq()

dev.off()


ggsave("exportaciones.jpg",height=7, width=10)



bd %>%
  select(contains("Exportaciones no petroleras"), Periodo) %>%
  rename_all(
    funs(
      str_replace_all(., "(.+>)","") %>%
        str_trim(., side = "both")
    )
  ) %>%
  rename("export_dest" = "Exportaciones no petroleras Desestacionalizada f16/ (Millones de dólares)") %>%
  filter(!is.na(export_dest) & !Periodo %in% c("2018/02")) %>%
  separate(Periodo, into = c("year", "month"), sep = "/") %>%
  mutate(dia = 01,
         Periodo = mdy(paste0(month, "_", dia, "_", year)),
         rocexp = ROC(export_dest),
         color = if_else(rocexp < 0, "negative", "positive")) %>%
  filter(Periodo > "2009-12-01") %>%
  select(Periodo, export_dest, rocexp, color) %>%
  ggplot(aes(Periodo, rocexp, fill = color)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Exportaciones en México",
       subtitle = "Exportaciones mexicanas en Millones de dólares para el periodo 2010-2018",
       caption = "Fuente: Elaboración propia con datos del INEGI") + 
  theme_tq() +
  scale_fill_manual(values = c(positive = "#6ebe4c" , negative = "#ca2128" )) + 
  scale_x_date(date_breaks = "9 month", date_labels = "%b %y") +
  theme(legend.position = "none")

dev.off()


ggsave("Exportaciones2.jpg",height=7, width=10)




### Importaciones

bd %>%
  select(contains("Importaciones no petroleras"), Periodo) %>%
  rename_all(
    funs(
      str_replace_all(., "(.+>)","") %>%
        str_trim(., side = "both")
    )
  ) %>%
  rename("import_orig" = "Importaciones no petroleras Original r3 / f15/ (Millones de dólares)",
         "import_orig_calend" = "Importaciones no petroleras Original corregida por efectos de calendario f16/ (Millones de dólares)",
         "import_dest" = "Importaciones no petroleras Desestacionalizada f16/ (Millones de dólares)",
         "import_trend" = "Importaciones no petroleras Tendencia f16/ (Millones de dólares)",
         "import_cicl" = "Importaciones no petroleras Ciclo f16/ (Puntos)") %>%
  filter(!is.na(import_orig) & !Periodo %in% c("2018/02")) %>%
  separate(Periodo, into = c("year", "month"), sep = "/") %>%
  mutate(dia = 01,
         Periodo = mdy(paste0(month, "_", dia, "_", year))) %>%
  select(-year,-month, -dia, -import_cicl) %>%
  filter(Periodo > "2009-12-01") %>%
  gather(key = "type", value = "value", -Periodo) %>%
  ggplot(aes(Periodo, value)) + 
  geom_line() +
  facet_wrap(~type) + 
  labs(title = "Importaciones en México",
       subtitle = "Importaciones mexicanas en Millones de dólares periodo 2010-2018",
       caption = "Fuente: Elaboración propia con datos del INEGI") + 
  theme_tq()


dev.off()


ggsave("Importaciones.jpg",height=7, width=10)






bd %>%
  select(contains("Importaciones no petroleras"), Periodo) %>%
  rename_all(
    funs(
      str_replace_all(., "(.+>)","") %>%
        str_trim(., side = "both")
    )
  ) %>%
  rename("import_dest" = "Importaciones no petroleras Desestacionalizada f16/ (Millones de dólares)") %>%
  filter(!is.na(import_dest) & !Periodo %in% c("2018/02")) %>%
  separate(Periodo, into = c("year", "month"), sep = "/") %>%
  mutate(dia = 01,
         Periodo = mdy(paste0(month, "_", dia, "_", year)),
         rocimp = ROC(import_dest),
         color = if_else(rocimp < 0, "negative", "positive")) %>%
  filter(Periodo > "2009-12-01") %>%
  select(Periodo, import_dest, rocimp, color) %>%
  ggplot(aes(Periodo, rocimp, fill = color)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Importaciones en México",
       subtitle = "Importaciones mexicanas en Millones de dólares para el periodo 2010-2018",
       caption = "Fuente: Elaboración propia con datos del INEGI") + 
  theme_tq() +
  scale_fill_manual(values = c(positive = "#6ebe4c" , negative = "#ca2128" )) + 
  scale_x_date(date_breaks = "9 month", date_labels = "%b %y") +
  theme(legend.position = "none")



dev.off()


ggsave("Importaciones2.jpg",height=7, width=10)





### Empleo


bd %>%
  select(contains("empleo"), Periodo) %>%
  rename_all(
    funs(
      str_replace_all(., "(.+>)", "") %>%
        str_trim(., side = "both")
    )
  ) %>%
  rename("empl_manuf" = "Tendencia del empleo en las manufacturas Original corregida por efectos de calendario f10/ (Porcentaje del balance)") %>%
  filter(!is.na(empl_manuf) & !Periodo %in% c("2018/2", "2018/3")) %>%
  separate(Periodo, into = c("year", "month"), sep = "/") %>%
  mutate(dia = 01,
         Periodo = mdy(paste0(month, "-", dia, "-", year)),
         color = if_else(empl_manuf > 0, "positive", "negative")) %>%
  select(empl_manuf, Periodo, color) %>%
  filter(Periodo > "2009-12-01") %>%
  ggplot(aes(Periodo, empl_manuf, fill = color)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Tendencia del empleo en México",
       subtitle = "Tendencia del empleo en la industria manufacturera en México periodo 2010-2018",
       caption = "Fuente: Elaboración propia con datos del INEGI") + 
  theme_tq() +
  scale_fill_manual(values = c(positive = "#6ebe4c" , negative = "#ca2128" )) + 
  scale_x_date(date_breaks = "9 month", date_labels = "%b %y") +
  theme(legend.position = "none")

dev.off()


ggsave("Empleo.jpg",height=7, width=10)




# Situación del mercado inmobiliario por el lado de la oferta
### Edificación privada de edificios industriales comerciales y de servicios


edif <- read.csv("BIE_BIE20180417124546.csv", encoding = "Latin-1")


edif %>%
  rename(., "edif_ind" = "Edificios.industriales..comerciales.y.de.servicios.") %>%
  filter(!is.na(edif_ind) & !Periodo %in% c("2018/02")) %>%
  separate(Periodo, into = c("year", "month"), sep = "/") %>%
  mutate(dia = 01,
         Periodo = mdy(paste0(month, "-", dia, "-", year))) %>%
  select(-year, -month, -dia) %>%
  filter(Periodo > "2009-12-01") %>%
  ggplot(aes(Periodo, edif_ind)) + 
  geom_line() +
  labs(title = "Edificación",
       subtitle = "Edificación de inmuebles industriales comerciales y de servicios en México",
       caption = "Fuente: Elaboración propia con datos del INEGI") +
  theme_tq()



dev.off()


ggsave("edificacion.jpg",height=7, width=10)




### Tasa de crecimiento de la edificación de edificios industriales, comerciales y de servicios


edif %>%
  rename(., "edif_ind" = "Edificios.industriales..comerciales.y.de.servicios.") %>%
  filter(!is.na(edif_ind) & !Periodo %in% c("2018/02")) %>%
  separate(Periodo, into = c("year", "month"), sep = "/") %>%
  mutate(dia = 01,
         Periodo = mdy(paste0(month, "-", dia, "-", year)),
         rocedif = ROC(edif_ind),
         color = if_else(rocedif > 0, "positive", "negative")) %>%
  select(-year, -month, -dia) %>%
  filter(Periodo > "2009-12-01") %>%
  ggplot(aes(Periodo, rocedif, fill = color)) + 
  geom_bar(stat = "identity") +
  labs(title = "Edificación",
       subtitle = "Edificación de inmuebles industriales, comerciales y de servicios",
       caption = "Fuente: Elaboración propia con datos del INEGI") +
  theme_tq() + 
  scale_fill_manual(values = c(positive = "#6ebe4c" , negative = "#ca2128" )) + 
  scale_x_date(date_breaks = "9 month", date_labels = "%b%y") +
  ylab(label = "Incremento porcentual") + 
  theme(legend.position = "null")

dev.off()


ggsave("edificacion2.jpg",height=7, width=10)




# Situación del mercado inmobiliario para uso industrial por el lado de la demanda
### Servicios de arrendamiento de bienes inmuebles




servic <- read.csv("BIE_BIE20180417143703.csv", encoding = "Latin-1")


servic %>%
  separate(Periodo, into = c("year", "month"), sep = "/") %>%
  mutate(dia = 01,
         Periodo = mdy(paste0(month, "-", dia, "-", year)),
         color = if_else(inmob_br > 0, "positive", "negative")) %>%
  select(-year, -month, -dia) %>%
  filter(Periodo > "2009-12-01") %>% 
  ggplot(aes(Periodo, inmob_br, fill = color)) + 
  geom_bar(stat = "identity") +
  labs(title = "Servicios de arrendamiento",
       subtitle = "Servicios de arrendamiento de bienes inmuebles",
       caption = "Fuente: Elaboración propia con datos del INEGI") +
  theme_tq() +
  scale_fill_manual(values = c(positive = "#6ebe4c" , negative = "#ca2128" )) + 
  scale_x_date(date_breaks = "9 month", date_labels = "%b %y") +
  theme(legend.position = "none")


dev.off()


ggsave("serv_arrend.jpg",height=7, width=10)






### Número de empresas registradas en IMMEX


db %>%
  select(contains("Número.de.establecimientos.activos.según.entidades.federativas.y.municipios.Total.nacional"), Periodo) %>%
  rename("immex_no" = "Manufacturas...Industria.manufacturera..maquiladora.y.de.servicios.de.exportación..IMMEX....Por.entidad.federativa...Establecimientos.manufactureros...Número.de.establecimientos.activos.según.entidades.federativas.y.municipios.Total.nacional.b...p9...f6...Número.de.establecimientos..Mensual") %>%
  filter(!is.na(immex_no)) %>%
  separate(Periodo,into = c("anio", "mes")) %>%
  mutate(day = 01,
         periodo = ymd(as_date(paste(anio, mes, day, sep = "/")))) %>%
  filter(periodo > "2009-12-01") %>%
  group_by(year(periodo)) %>%
  summarise(m_immex = mean(immex_no, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(aes(x = `year(periodo)`, y = m_immex)) +
  geom_point(aes(x = `year(periodo)`, y = m_immex)) +
  geom_label_repel(aes(x = `year(periodo)`, y = m_immex, label = round(m_immex))) + 
  theme_minimal() + 
  ggtitle("Empresas registradas en IMMEX") +
  xlab("Año") +
  ylab("Número de empresas registradas") + 
  scale_x_continuous(breaks = c(2007,2008,2009,2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))


dev.off()


ggsave("immex.jpg",height=7, width=10)


