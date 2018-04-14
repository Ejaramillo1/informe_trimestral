library(easypackages)
my_packages <- c("readr", "tidyverse", "lubridate", "xts",  "tidyquant", "ggrepel", "reshape2")
libraries(my_packages)

db <- read.csv("BIE_BIE20180413143330.csv", encoding = "Latin-1")

data <- db %>%
  select(contains("Número.de.establecimientos.activos.según.entidades.federativas.y.municipios.Total.nacional"), Periodo) %>%
  rename("immex_no" = "Manufacturas...Industria.manufacturera..maquiladora.y.de.servicios.de.exportación..IMMEX....Por.entidad.federativa...Establecimientos.manufactureros...Número.de.establecimientos.activos.según.entidades.federativas.y.municipios.Total.nacional.b...p9...f6...Número.de.establecimientos..Mensual") %>%
  filter(!is.na(immex_no)) %>%
  separate(Periodo,into = c("anio", "mes")) %>%
  mutate(day = 01,
    periodo = ymd(as_date(paste(anio, mes, day, sep = "/")))) %>%
  mutate(growth = c(NA, exp(diff(log(immex_no)))-1))



data %>%
  filter(anio > 2014) %>%
  ggplot() +
  geom_histogram(aes(growth, fill = anio))


data %>%
  filter(anio > 2014) %>%
  ggplot() +
  geom_histogram(aes())
  scale_fill_manual(values = c("#6ebe4c", 
                               "#2e9fd9", 
                               "#a74e9d", 
                               "#22602c", 
                               "#b7451d", 
                               "#195772",
                               "#ca2128",
                               "#5e1e60",
                               "#771021",
                               "#b7451d",
                               "#22602c",
                               "#195772"))


data %>%
  mutate(month = month(periodo, label = TRUE),
         year = year(periodo)) %>%
  filter(year > 2014) %>%
  spread(year, growth, month) %>%
  ggplot(aes(x = periodo, y = growth)) + 
  geom_col(aes(fill = factor(year)))  
data %>%
  ggplot(aes(x = periodo, y = immex_no)) + 
  geom_line() +
  geom_smooth()


data %>%
  ggplot(aes(x = month, y = immex_no, group = year)) + 
  geom_area(aes(colour = year))
