ruta_archivo <- "C:/Users/finnegans/Downloads/sistema-unico-de-atencion-ciudadana-2021.csv"
library(readr)
install.packages("dplyr")
library(dplyr)
atencion_ciudadano <- read_delim(
  ruta_archivo,
  delim = ";",  # Ajustar el delimitador según el archivo
  locale = locale(encoding = "UTF-8"),
  col_names = TRUE,  # Asegurarse de que el archivo tenga nombres de columnas
  trim_ws = TRUE  # Eliminar espacios en blanco adicionales alrededor de los datos
)
str(atencion_ciudadano)
names(atencion_ciudadano)
head(atencion_ciudadano)
#separo periodo por mes y año para facilitar filtrado
atencion_ciudadano <- mutate(atencion_ciudadano, AÑO=substr(periodo, 1, 4), MES=substr(periodo,5,6) )
#datos de 2021
datos_2021 <- subset(atencion_ciudadano, AÑO == 2021)
#contactos_abiertos
contactos_abiertos <- datos_2021 %>%
  filter(estado_del_contacto == "Abierto")
#contactos por genero
contactos_por_genero <- atencion_ciudadano %>%
  filter(genero != "" & !is.na(genero)) %>%
  group_by(genero) %>%
  summarise(cantidad_contactos = n())
#cantidad de datos por tipo de prestación y categoria
cantidad_contactos_por_tipo_categoria <- atencion_ciudadano %>%
  filter(tipo_prestacion != "" & !is.na(tipo_prestacion) & categoria != "" & !is.na(categoria)) %>%
  summarise(cantidad_contactos= n())

