ruta_archivo <- "C:/Users/finnegans/Downloads/sistema-unico-de-atencion-ciudadana-2021.csv"
library(readr)
install.packages("dplyr")
library(dplyr)
library(stringr)
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

remove_accents <- function(text) {
  iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT")
}

atencion_ciudadano <- atencion_ciudadano %>%
  mutate(
    tipo_prestacion = trimws(toupper(remove_accents(as.character(tipo_prestacion)))),
    categoria = trimws(toupper(remove_accents(as.character(categoria)))),
    subcategoria = trimws(toupper(remove_accents(as.character(subcategoria))))
  )

#Cantidad de datos cuyo tipo_prestación sea "solicitud" categoria = "alumbrado" y subcategoria
#"reparación de luminaria
tipo_prestación_solicitud <- atencion_ciudadano %>%
  filter(tipo_prestacion == "SOLICITUD")%>%
  summarise(cantidad_contactos = n())
categoria <- atencion_ciudadano %>%
  filter(categoria == "ALUMBRADO") %>%
  summarise(cantidad_contanctos = n())
atencion_ciudadano <- atencion_ciudadano %>%
  mutate(
    subcategoria = trimws(subcategoria)
  )

sub_categoria <- atencion_ciudadano %>%
filter( grepl("REPARACIÓN DE LUMINARIA", subcategoria, ignore.case = TRUE)) %>%
select(subcategoria) %>%
summarise(cantidad_contactos = n())
#Por domicilio_barrio en el mes de junio de 2021 mes con mayor cant de contactos cuyo
#tipo_prestacion sea "denuncia"
contactos_junio_denuncia <- atencion_ciudadano %>%
  filter(
     MES == "06", # Filtrar por el mes de junio (cambiar según el número de mes correspondiente)
    tipo_prestacion == "DENUNCIA"
  )

# Obtener el barrio con la mayor cantidad de contactos de denuncia en junio de 2021
barrio_con_mas_denuncias <- contactos_junio_denuncia %>%
  group_by(domicilio_barrio) %>%
  summarise(cantidad_contactos = n()) %>%
  arrange(desc(cantidad_contactos)) %>%
  slice(1) 
#Valor de media, mediana, varianza y desviacion estandar correspondiente a la cantidad de contactos por mes
head(atencion_ciudadano)
estadistica_por_mes <- atencion_ciudadano %>%
  group_by(MES) %>%
  summarise(
    media = mean(n(), na.rm = TRUE),
    mediana = median(n(), na.rm = TRUE),
    varianza = var(n(), na.rm = TRUE),
    desviacion_estandar = sd(n(), na.rm = TRUE)
  )
