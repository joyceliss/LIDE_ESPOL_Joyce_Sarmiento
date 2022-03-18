if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(ggplot2)

# cargar bases de datos
library(readxl)
data2018 <- read_excel("C:/Users/Holger/Desktop/Joyce/Proyectos R/F00010936-WVS_Wave_7_Ecuador_Excel_v2.0.xlsx")

data2013 <- read_excel("C:/Users/Holger/Desktop/Joyce/Proyectos R/F00007586-WV6_Data_Ecuador_Excel_v20201117.xlsx")




sexo <- data2018$`Q260: Sex`
edad <- data2018$`Q262: Age`
edad_intervalos <- data2018$`X003R: Age recoded (6 intervals)`
nivel_educativo <- data2018$`Q275R: Highest educational level: Respondent (recoded into 3 groups)`
religion <- data2018$`Q289: Religious denominations - major groups`
aborto2018 <- data2018$`Q184: Justifiable: Abortion`
aborto2013 <- data2013$`V204: Justifiable: Abortion`

data_aborto2018 <- data.frame(sexo, edad, edad_intervalos, nivel_educativo, religion, aborto2018)
data_aborto2013 <- data.frame(aborto2013)
str(data_aborto2013)

data_aborto2013$aborto2013Dico <- if(data_aborto2013$aborto2013<=6) {0} else {1}
