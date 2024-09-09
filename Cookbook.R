library(readxl)
library(janitor)
library(magrittr) # For pipelines
library(lubridate)
library(dplyr)
library(ggplot2)
library(stringr)

Datos_356_2016_23 <- read_excel("Datos_356_2016_23.xlsx")
df <- Datos_356_2016_23
# rm(Datos_356_2016_23)

df %<>% clean_names
glimpse(df)


df$consecutive <- as.numeric(df$consecutive)
df$nacionalidad <- as.factor(df$nacionalidad)
df$semana <- as.numeric(df$semana)
df$cod_eve <- as.numeric(df$cod_eve)
df$ano <- as.numeric(df$ano)
df$sexo <- as.factor(df$sexo)

summary(df)

## Codigo del evento (cod_eve) y nombre_evento ----

table(df$cod_eve)

table(df$nombre_evento)

# En esta caso solo se trata con el intento de suicidio
# por el cual la variable en cuestion no aportado información adicional.
# Se procede a eliminar esta variable de la base de datos.

df <- df %>% select(-"cod_eve", -"nombre_evento")


## nom_est_f_caso, confirmados, va_sispro, Estado_final_de_caso

table(df$nom_est_f_caso)
table(df$confirmados)
table(df$va_sispro)
table(df$estado_final_de_caso)

# En esta caso todos los casos fueron confirmados por clinicas
# Esta variable no ofrece mayor información, se procede a eliminarla

df <- df %>% select(-"nom_est_f_caso",-"confirmados",-"va_sispro",-"estado_final_de_caso")

## fecha de notificación ----

df$fec_not <- ymd(df$fec_not)

barplot(table(df$fec_not))

## semana (epidemiologica) ----

summary(df$semana)

# No se encuentran datos faltantes.

barplot(table(df$semana))

# El grafico de barras indica que las observerciones varian
# en función a la semana epidemiologica.

## Año ----

summary(df$ano) # No se encuentran datos faltantes.
barplot(table(df$ano))

# el grafico de barras indica que el número de suicidios ha aumentado
# de los años.

## Edad ----

summary(df$edad)

hist(df$edad)
boxplot(df$edad)

goft::gamma_test(df$edad)


## uni_med ----

table(df$uni_med)

# esta variable esta solo contiene unos por lo cual no 
# aporta mayor información. Se procede a excluirla del dataset

df <- df %>% select(-"uni_med")
 
## Sexo -----

table(df$sexo)

df$sexo  <- str_replace(df$sexo, "F", "Femenino")
df$sexo  <- str_replace(df$sexo, "M", "Masculino")

Tabla_sex <- df %>% 
  group_by(sexo) %>%
  count() %>% 
  ungroup() %>% 
  mutate(Porcentaje = `n` / sum(`n`)) %>% 
  arrange(Porcentaje) %>%
  mutate(etiquetas = scales::percent(Porcentaje))


ggplot(Tabla_sex , aes(x = "", y = Porcentaje, fill = sexo)) +
  geom_col(color = "black") +
  geom_label(aes(label = etiquetas),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Sexo")) + scale_color_gradient() +
  coord_polar(theta = "y") + ggtitle ("")

df$sexo <- as.factor(df$sexo)

## Ocupación -----

str(df$ocupacion)

df_2 <- data.frame(ocupacion = df$ocupacion)


df_2 <- df_2 %>%
  mutate(
    ocupacion_3digitos = case_when(
      # Comprobar si los primeros tres dígitos son "999"
      str_sub(ocupacion, 1, 3) == "999" ~ "999",  # Asignar "999" si los primeros tres dígitos son "999"
      TRUE ~ {
        # Asegurar que el código tenga al menos 3 dígitos
        codigo_padded <- str_pad(ocupacion, width = 3, pad = "0")
        primer_digito <- substr(codigo_padded, 1, 1)
        paste0(primer_digito, "00")  # Agregar dos ceros
      }
    )
  )

ocupacion_3digitos <- df_2$ocupacion_3digitos

rm(df_2)

# Dado que las ocupaciones se encuentran codificadas según
# Clasificación Única de Ocupaciones para Colombia (CUOC)
# se utlizara un diccionario para recodificar la variable
# El documento base para crear el diccionario fue tomado de:
# https://www.dane.gov.co/index.php/sistema-estadistico-nacional-sen/normas-y-estandares/nomenclaturas-y-clasificaciones/clasificaciones/clasificacion-internacional-uniforme-de-ocupaciones-ciuo

# note que solo se utiliza tres digitos para clasificar

DANE_ocupacion <- read_excel("Diccionario_ocupación_DANE.xlsx")

str(DANE_ocupacion)

DANE_ocupacion$Ocupación <- tolower(DANE_ocupacion$Ocupación)


DANE_ocupacion$Codigo_DANE <- round(DANE_ocupacion$Codigo_DANE,3)

DANE_ocupacion <- rbind(DANE_ocupacion,c(999, "Ocupación no registrada en CUOC"))

# Crear el diccionario solo con los códigos de cuatro dígitos
diccionario_DANE_ocupacion <- setNames(as.list(DANE_ocupacion$Ocupación), c("000","100","200","300","400","500","600","700","800","900","999"))

str(diccionario_DANE_ocupacion)
str(ocupacion_3digitos)

df$ocupacion_recoded <- recode(ocupacion_3digitos, !!!diccionario_DANE_ocupacion)

df$ocupacion_recoded  <- as.factor(ocupacion_recoded)

table(ocupacion_recoded)

which(colnames(df) == "ocupacion")

df <- df %>%
  select(all_of(c(names(df)[1:15], "ocupacion_recoded", names(df)[16:20])))

levels(df$ocupacion_recoded)

barplot(sort(table(df$ocupacion_recoded),decreasing = TRUE), las = 2)

## gru_pob & nom_grupo ----

table(df$nacionalidad)


## Variables dicotomicas

# En lo que respecta a als varibales dicotomicas el DANE suele codificar 
# Si como 1 y NO como 2. Si bien es importante confirmar esto en la documentación
# por el momento realizaremos estos cambios (Recodificación).

recod_di <- function(vector){
  vecto <- ifelse(vector == 2,0,1)
  return(vecto)
}

table(recod_di(df$gp_discapa))

list_dico <- c("GP_DISCAPA","GP_DESPLAZ","GP_MIGRANT","GP_CARCELA","GP_GESTAN","GP_INDIGEN","GP_POBICFB","GP_MAD_COM",
               "GP_DESMOVI","GP_PSIQUIA","GP_VIC_VIO","GP_OTROS")

list_dico <- tolower(list_dico)

df[list_dico] <- data.frame(lapply(df[list_dico], recod_di))
df[list_dico] <- data.frame(lapply(df[list_dico], as.factor))

summary(df)

# Municipio_ocurrencia

table(as.factor(df$municipio_ocurrencia))

which(is.na(df$municipio_ocurrencia)) # Hay datos faltantes


# Minicipio de notificación

table(as.factor(df$municipio_notificacion))

which(is.na(df$municipio_notificacion)) # Hay datos faltantes


son_iguales <- trimws(tolower(df$municipio_notificacion)) == trimws(tolower(df$municipio_ocurrencia))

table(son_iguales)

df3 <- df[which(son_iguales == FALSE),]

# tenemos que ambos vectores no son iguales. Por lo tanto, procederemos a llevar las cadenas
# a minuscula, preservando ambas variables.


df$municipio_notificacion <- tolower(df$municipio_notificacion)
df$municipio_ocurrencia <- tolower(df$municipio_ocurrencia)


son_iguales <- trimws(tolower(df$municipio_residencia)) == trimws(tolower(df$municipio_ocurrencia))

table(son_iguales)

df3 <- df[which(son_iguales == FALSE),]


df$municipio_residencia <- gsub(".*MUNICIPIO DESCONOCIDO.*", "desconocido", df$municipio_residencia)
df$municipio_residencia <- gsub("* PROCEDENCIA DESCONOCIDA*", "desconocido", df$municipio_residencia)
df$municipio_residencia <- gsub("*desconocido", "desconocido", df$municipio_residencia)
df$municipio_residencia <- gsub("* EXTERIOR. PAÍS DESCONOCIDO", "desconocido", df$municipio_residencia)


# Podemos utilizar los datos del municipio de residencia, para solicionar algunas datos
# faltantes del pais de residencia.

df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_ESTADOS UNIDOS","Estados Unidos",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_VENEZUELA","Venezuela",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_ESPAÑA","España",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_ECUADOR","Ecuador",df$pais_residencia)

df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_CHILE","Chile",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_REPÚBLICA CENTROAFRICANA","Republica Centroafricana",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_REPUBLICA DEMOCRÁTICA DEL CONGO","Republica democratica del congo",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_ISRAEL","Israel",df$pais_residencia)

df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_ESLOVENIA","Eslovenia",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_COMORAS","Comoras",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_AFGANISTÁN","Afganistan",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_BRASIL","Brasil",df$pais_residencia)

df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_GRANADA","Granada",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_TAIWÁN","Taiwan",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_COMORAS","Comoras",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_LIBANO","Libano",df$pais_residencia)

df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_REPÚBLICA ÁRABE SIRIA","Republica Arabe Siria",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_REPUBLICA POPULAR DEL CONGO","Republica del Congo",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_MARTINICA","Martinica",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "MONTSERRAT","Monserrat",df$pais_residencia)


df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_CANADÁ","Canada",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_ARGENTINA","Argentina",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_CUBA","Cuba",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_MAURICIO","Mauricio",df$pais_residencia)

df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_NAURU","Nauru",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_CHINA","China",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_INDIA","India",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_ISLAS FEROE","Islas Feroe",df$pais_residencia)

df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_JORDANIA","Jordania",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "ANDORRA","Andorra",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "BULGARIA","Bulgaria",df$pais_residencia)
df$pais_residencia <-  ifelse(df$municipio_residencia == "EXTERIOR_PERU","Peru",df$pais_residencia)


## Pais de residencia

df3 <- df[which(is.na(df$pais_residencia)),]

levels(as.factor(df3$departamento_residencia))

# El analisis de los departamentos de residencia indica que todos los faltantes en
# pais de residencia estan asociados a algun departamento del pais. 
# Procedemos sustituir los faltantes con colombia.

df$pais_residencia <-  ifelse(is.na(df$pais_residencia),"COLOMBIA",df$pais_residencia)

table(df$pais_residencia)

which(is.na(df$pais_residencia))

# Dado que se ha lidiado con los faltantes, procederemos a segurar que la 
# varibale se codifique en minusculas

df$pais_residencia <- tolower(df$pais_residencia)

## FEC_DEF, CER_DEF, CBMTE


# Un examen de la base de datos revela que estas varibales estan vacias, por lo que
# se procedera a eliminarlas


df <- df %>% select(-"cbmte",-"cer_def",-"fec_def")

## con fin

table(df$con_fin)

# esta varibale solo presenta 4 casos en ceros, y el resto en uno. Dado a que
# no contiene mayor información, se procede a eliminarla de la base de datos.

df <- df %>% select(-"con_fin")

## tip_cas

table(df$tip_cas)
df <- df %>% select(-"tip_cas")



## Tip_ss

## Esta varibale podria representar el tipo de atención. 

table(df$tip_ss)

which(is.na(df$tip_ss))

df[df$tip_ss == "S",]$tip_ss  <- "Subsidiado"
df[df$tip_ss == "C",]$tip_ss  <- "Contributivo"
df[df$tip_ss == "P",]$tip_ss  <- "Excepción"
df[df$tip_ss == "N",]$tip_ss  <- "No asegurado"
df[df$tip_ss == "I",]$tip_ss  <- "Inderminado/Pendiente"
df[df$tip_ss == "E",]$tip_ss  <- "Inderminado/Pendiente"


# Tal que no se encuentran datos faltantes y las categoreias parecen estar
# corretas. Se concluye su analisis hasta tener más informacion de la base de
# datos.

## gruop_pob

length(which(is.na(df$gru_pob)))
df <- df %>% select(-"gru_pob")

## cod_ase (TERMINAR)

#corresponden a Entidades Administradoras de Planes de Salud (EAPB) 
#en Colombia. Estas entidades son responsables de la administración y 
#prestación de servicios de salud a sus afiliados, gestionando tanto el 
#régimen contributivo 
#como el subsidiado en el sistema de seguridad social en salud.

length(which(is.na(df$cod_ase)))


## area

table(df$area)

# En bases de datos como las del SIVIGILA, la variable area codificada:
# 1: Urbano 
# 2: Rural 
# 3: Cabecera municipal 

df[df$area == 1,]$area <- "Urbano"
df[df$area == 2,]$area <- "Rural"
df[df$area == 3,]$area <- "Cabecera municipal"

## estrato

table(as.factor(df$estrato))


## EPS data


# Algunas varibales relacionadas con los establecimientos 
# De los cuales se reporta el suceso no son relevantes para el
# presente trabajo. Por lo cual procedemos a excluirlas del dataset

df <- df %>% select(-"fm_fuerza",-"fm_grado",-"fm_unidad",-"nom_upgd")


# Cod municipio, pais, depart

# Esta información ya se encuentra en las variables tratadas anteriormente
# Por lo cual procedemos a eliminarla

df2 <- df %>% select(-"cod_dpto_n",-"cod_dpto_o",-"cod_dpto_r",
                    -"cod_mun_n",-"cod_mun_o",-"cod_mun_r",
                    -"cod_pais_o",-"cod_pais_r",-"cod_sub",-"cod_ase")

# sem_ges

# En este caso particular no es de nuestro interes las semanas de gestación de las
# embarazadas

df2 <- df2 %>% select(-"sem_ges")

# Otras fechas 

# Hay otras fechas que no son de nuestro interes en el df,
# se procede a excluirlas

df2 <- df2 %>% select(-"fecha_nto",-"fec_arc_xl")

# Tambien se elimana varibales relacionadas con consecutivos

df2 <- df2 %>% select(-"consecutive",-"cod_pre",-"consecutive_origen")

# se cambia el nombre de la variables referidad a la hospitalización
# y se recodifica

df2 <- df2 %>%
  rename("Admission_H" = "pac_hos")

df2$Admission_H <- ifelse(df2$Admission_H == 1, 1,0)

## Pertenencia Etnica

df2[df2$per_etn == 5,]$per_etn  <- "Afro"
df2[df2$per_etn == 1,]$per_etn  <- "Indigena"
df2[df2$per_etn == 4,]$per_etn  <- "Palenquero"
df2[df2$per_etn == 3,]$per_etn  <- "Raizal"
df2[df2$per_etn == 2,]$per_etn  <- "Rom/gitano"
df2[df2$per_etn == 6,]$per_etn  <- "Otro"

df2 <- df2 %>%
  rename("Etnia" = "per_etn")

df2 <- df2 %>% select(-"nom_grupo")


# Pais y municipio

# En este ejercicio nos intersea el departamento y municipo de ocurrencia, por lo cual
# los datos como el departamento de residencia seran excluidos del la base de datos


df2 <- df2 %>% select(-"municipio_ocurrencia",-"departamento_residencia",-"municipio_residencia",
                      -"departamento_notificacion",-"municipio_notificacion")

# Fechas (no info)

# En el momento no se tiene datos acerca de las variables de 

df2 <- df2 %>% select(-"ini_sin",-"fec_con")

# fecha_ajus

df2 <- df2 %>% select(-"fec_aju",-"fuente")

# Nacionalidad

# Las nacionalidades no reportadas se indentifican como tal

df2 <- df2 %>% select(-"nacionalidad")

df2$nombre_nacionalidad <- tolower(df2$nombre_nacionalidad)

df2 <- df2 %>%
  rename("Nacionalidad" = "nombre_nacionalidad")

table(df2$Nacionalidad)

df2 <- df2 %>% select(-"ajuste")


df2$pais_ocurrencia <- tolower(df2$pais_ocurrencia)
df2$departamento_ocurrencia <- tolower(df2$departamento_ocurrencia)

df2 <- df2 %>% select(-"pais_residencia")

table(df2$departamento_ocurrencia)
table(df2$pais_ocurrencia)

# Estrato

df2$estrato <- ifelse(df2$estrato == 100, NA, df2$estrato)

# Nota

# Solo se trabajaran con los registros donde el pais de ocurrencia sea colombia

df2 <- df2[df2$pais_ocurrencia == "colombia",]

table(df2$pais_ocurrencia)

df2 <- df2 %>% select(-"pais_ocurrencia")

## cambiemos el nombre de las varibales

colnames(df2) <- c("date", "week", "year", "age", "nationality", "sex", "area", 
                   "occupation", "ss_type", "ethnicity", "ses", "disability", 
                   "displaced", "migrant", "incarcerated", "pregnant", "indigenous", 
                   "low_income", "single_mother", "demobilized", "psychiatric", 
                   "violence_victim", "others", "admission_H", "date_H", "department")

## Antes de exportar la base de datos limpia, aseguremos que nos hay caracteres especiales
## espacios que puedan ser problemas en un futuro:

clean_values <- function(x) {
  # Convertir a caracteres si es necesario
  x <- as.character(x)
  x <- str_replace_all(x, "ñ", "n")
  # Eliminar caracteres especiales y tildes
  x <- str_replace_all(x, "[^[:alnum:][:space:]]", "")  # Eliminar caracteres no alfanuméricos
  x <- str_remove_all(x, "[[:punct:]]")  # Eliminar signos de puntuación
  x <- str_to_lower(x)  # Convertir a minúsculas
  
  return(x)
}

# Aplicar la función a todo el dataframe
df2 <- df2 %>%
  mutate(across(everything(), clean_values))

df2 <- df2 %>%
  clean_names()

summary(df2)

selected_columns <- c("nationality","sex","area","ss_type","ethnicity",
                      "department")
df2[selected_columns] <- lapply(df2[selected_columns], as.factor)
df2$date <- lubridate::ymd(df2$date)
df2$date_h <- lubridate::ymd(df2$date_h)

selected_columns <- c("week","year","age","occupation","ses",
                      "disability","displaced","migrant","incarcerated",
                      "pregnant","indigenous","low_income","single_mother",
                      "demobilized","psychiatric","violence_victim","others",
                      "admission_h")
df2[selected_columns] <- lapply(df2[selected_columns], as.numeric)

colnames(df2)
write.csv(df2, "Datos_356_clean.csv", row.names = TRUE, na = "")

