#instalar los paquetes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

#cargar las librerias
library(tidyverse)
library(ggplot2)
library(readxl)
library(scales)

# cargar bases de datos
# Le damos un nombre al url
url <- "https://github.com/joyceliss/LIDE_ESPOL_Joyce_Sarmiento/raw/main/Base%20de%20datos.zip"
# Creamos un directorio temporal
td <- tempdir()
# Creamos una carpeta temporal
tf <- tempfile(tmpdir=td, fileext = ".zip")
# Descargamos los discap en la carpeta temporal
download.file(url,tf)
# Obtenemos el nombre del archivo dentro del archivo zip, lo descomprimimos (unzip), obtenemos el nombre del 
# parche, y finalmente lo cargamos
WVs.f.name <- unzip(tf, list=TRUE)[1,1]
WV6.f.name <- unzip(tf, list=TRUE)[2,1]

unzip(tf, files=WVs.f.name, exdir=td, overwrite=TRUE)
unzip(tf, files=WV6.f.name, exdir=td, overwrite=TRUE)

WVs.f.path <- file.path(td, WVs.f.name)
WV6.f.path <- file.path(td, WV6.f.name)

data2018 <- read_excel(WVs.f.path)
data2013 <- read_excel(WV6.f.path)


#Data.frame con los datos que nos interesa
sexo <- data2018$`Q260: Sex`
edad <- data2018$`Q262: Age`
edad_intervalos <- data2018$`X003R: Age recoded (6 intervals)`
nivel_educativo <- data2018$`Q275R: Highest educational level: Respondent (recoded into 3 groups)`
religion <- data2018$`Q289: Religious denominations - major groups`
aborto2018 <- data2018$`Q184: Justifiable: Abortion`
aborto2013 <- data2013$`V204: Justifiable: Abortion`

data_aborto2018 <- data.frame(sexo, edad, edad_intervalos, nivel_educativo, religion, aborto2018)
data_aborto2013 <- data.frame(aborto2013)

#Filtro de las bases sin numeros negativos
data_aborto2018 <- data_aborto2018 %>%
  filter(aborto2018 >= 1, nivel_educativo >= 0, religion >= 0)

#Creación de variable dicotomica sobre el pensamiento del aborto 2018
R = length(data_aborto2018$aborto2018)
acumulador2018 <- rep(0,R)
for (i in 1:R) {
  if ((data_aborto2018$aborto2018[i] <= 6) & (data_aborto2018$aborto2018[i] >=1)) {acumulador2018[i] <- 'No justifica'}
  else if (data_aborto2018$aborto2018[i] >= 7 ) {acumulador2018[i] <- 'Justifica'}
  else if (data_aborto2018$aborto2018[i] < 1) {acumulador2018[i] <- 'No Aplica'} 
}
data_aborto2018$abortoDummy2018 <- unlist(acumulador2018)

#Creación de variable dicotomica sobre el pensamiento del aborto 2013
R = length(aborto2013)
acumulador2013 <- rep(0,R)
for (i in 1:R) {
  if ((aborto2013[i] <= 6) & (aborto2013[i] >=1)) {acumulador2013[i] <- 'No justifica'}
  else if (aborto2013[i] >= 7 ) {acumulador2013[i] <- 'Justifica'}
  else if (aborto2013[i] < 1) {acumulador2013[i] <- 'No Aplica'} 
}
data_aborto2013$abortoDummy2013 <- unlist(acumulador2013)


#Comparación entre pensamiento del aborto del 2013 y 2018
ggplot(data_aborto2013, aes(x = abortoDummy2013)) + geom_bar() + labs(
  title = "Justifica o No justifica el aborto en el año 2013",
  x = "Respuesta",
  y = "Cantidad" 
  )

ggplot(data_aborto2018, aes(x = abortoDummy2018)) + geom_bar() + labs(
  title = "Justifica o No justifica el aborto en el año 2018",
  x = "Respuesta",
  y = "Cantidad" 
  )


#Visualizacion de datos del 2018
#Comparación Hombre vs Mujer
sexo_labs <- c("Hombre", "Mujer")
names(sexo_labs) <- c("1", "2")

ggplot(data_aborto2018, aes(x = abortoDummy2018, y = (..count..)/sum(..count..), fill = abortoDummy2018)) + geom_bar() + facet_wrap(~sexo, labeller = labeller(sexo = sexo_labs), scales = "free") + labs(
  title = "Justifica o No Justifica el Aborto",
  x = "Respuesta",
  y = NULL 
) + guides(fill=FALSE) + geom_text(stat='count',aes(label = paste(round((..count..)/sum(..count..)*100), "%")), vjust=-0.5, size=2.5) + 
  theme(
    rect = element_blank(),
  )


#Filtro entre hombres y mujeres
data_aborto2018_Man <- data_aborto2018 %>%
  filter(sexo == 1)
data_aborto2018_Woman <- data_aborto2018 %>%
  filter(sexo == 2)

#Comparación Nivel educativo dividio por hombre y mujer
nivel_educativo_labs <- c("Inferior", "Medio", "Superior")
names(nivel_educativo_labs) <- c("1", "2", "3")

ggplot(data_aborto2018_Man, aes(x = abortoDummy2018, y = (..count..)/sum(..count..), fill = abortoDummy2018)) + geom_bar() + facet_wrap(~sexo+nivel_educativo, labeller = labeller(sexo = sexo_labs, nivel_educativo = nivel_educativo_labs)) + labs(
  title = "Justifica o No Justifica el Aborto",
  x = "Respuesta",
  y = NULL 
) + guides(fill=FALSE) + geom_text(stat='count',aes(label = paste(round((..count..)/sum(..count..)*100), "%")), vjust=-0.5, size=2.5) + 
  theme(
    rect = element_blank()
  )


nivel_educativo_labs <- c("Inferior", "Medio", "Superior")
names(nivel_educativo_labs) <- c("1", "2", "3")

ggplot(data_aborto2018_Woman, aes(x = abortoDummy2018, y = (..count..)/sum(..count..), fill = abortoDummy2018)) + geom_bar() + facet_wrap(~sexo+nivel_educativo, labeller = labeller(sexo = sexo_labs, nivel_educativo = nivel_educativo_labs)) + labs(
  title = "Justifica o No Justifica el Aborto",
  x = "Respuesta",
  y = NULL 
) + guides(fill=FALSE) + geom_text(stat='count',aes(label = paste(round((..count..)/sum(..count..)*100), "%")), vjust=-0.5, size=2.5) + 
  theme(
    rect = element_blank())

#Comparación edades dividio por hombre y mujer
edad_intervalos_labs <- c("16 a 24 años", "25 a 34 años", "35 a 44 años", "45 a 54 años", "55 a 64 años", "mayor a 65 años")
names(edad_intervalos_labs) <- c("1", "2", "3", "4", "5", "6")

ggplot(data_aborto2018_Man, aes(x = abortoDummy2018, y = (..count..)/sum(..count..), fill = abortoDummy2018)) + geom_bar() + facet_wrap(~sexo+edad_intervalos, labeller = labeller(sexo = sexo_labs, edad_intervalos = edad_intervalos_labs))+ labs(
  title = "Justifica o No Justifica el Aborto",
  x = "Respuesta",
  y = NULL 
) + guides(fill=FALSE) + geom_text(stat='count',aes(label = paste(round((..count..)/sum(..count..)*100), "%")), vjust=-0.5, size=2.5) + 
  theme(
    rect = element_blank())


edad_intervalos_labs <- c("16 a 24 años", "25 a 34 años", "35 a 44 años", "45 a 54 años", "55 a 64 años", "mayor a 65 años")
names(edad_intervalos_labs) <- c("1", "2", "3", "4", "5", "6")

ggplot(data_aborto2018_Woman, aes(x = abortoDummy2018, y = (..count..)/sum(..count..), fill = abortoDummy2018)) + geom_bar() + facet_wrap(~sexo+edad_intervalos, labeller = labeller(sexo = sexo_labs, edad_intervalos = edad_intervalos_labs))+ labs(
  title = "Justifica o No Justifica el Aborto",
  x = "Respuesta",
  y = NULL 
) + guides(fill=FALSE) + geom_text(stat='count',aes(label = paste(round((..count..)/sum(..count..)*100), "%")), vjust=-0.5, size=2.5) + 
  theme(
    rect = element_blank())



#Comparación religión dividio por hombre y mujer
religion_labs <- c("No creyente", "Catolico", "Protestante", "Ortodoxo", "Otro Cristiano", "Otro")
names(religion_labs) <- c("0", "1", "2", "3", "8", "9")

ggplot(data_aborto2018_Man, aes(x = abortoDummy2018, y = (..count..)/sum(..count..), fill = abortoDummy2018)) + geom_bar() + facet_wrap(~sexo+religion, labeller = labeller(sexo = sexo_labs, religion = religion_labs))+ labs(
  title = "Justifica o No Justifica el Aborto",
  x = "Respuesta",
  y = NULL 
) + guides(fill=FALSE) + geom_text(stat='count',aes(label = paste(round((..count..)/sum(..count..)*100), "%")), vjust=-0.5, size=2.5) + 
  theme(
    rect = element_blank())


religion_labs <- c("No creyente", "Catolico", "Protestante", "Ortodoxo", "Otro Cristiano", "Otro")
names(religion_labs) <- c("0", "1", "2", "3", "8", "9")

ggplot(data_aborto2018_Woman, aes(x = abortoDummy2018, y = (..count..)/sum(..count..), fill = abortoDummy2018)) + geom_bar() + facet_wrap(~sexo+religion, labeller = labeller(sexo = sexo_labs, religion = religion_labs))+ labs(
  title = "Justifica o No Justifica el Aborto",
  x = "Respuesta",
  y = NULL 
) + guides(fill=FALSE) + geom_text(stat='count',aes(label = paste(round((..count..)/sum(..count..)*100), "%")), vjust=-0.5, size=2.5) + 
  theme(
    rect = element_blank())


