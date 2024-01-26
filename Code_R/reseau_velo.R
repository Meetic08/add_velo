############################# PROJET ADD PISTE CYCLABE -- Léo GABET ###################################

############################### Charger les différentes librairies ######################################

library(installr)
library(lmtest)
library(tidyverse)
library(data.table)
library(magrittr)
library(DAAG)
library(plotly)
library(tseries)
library(olsrr) 
library(car)
library(readr)
library(readxl)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(openxlsx)
library(Factoshiny)
library(tidyr)

############# On importe les données #################
don <- read_delim("C:/Users/33662/desktop/add_velo/data/reseau-cyclable.csv")
# On visualise nos données : 
View(don)
summary(don)
colnames(don)


##### Séparer geo_point_2d en une colonne latitude et une colonne longitude #####
don <- separate(don, geo_point_2d, into = c("latitudes", "longitudes"), sep = ", ")

table(don$an)
plot(don$an)
sum(is.na(don$an))
summary(don$an)

############### Arrondissement longueur troncon" 
don <- don %>%
  mutate(`Longueur du troncon en m` = round(`Longueur du troncon en m`, 0))
summary(don$`Longueur du troncon en m`)

don <- don %>%
  mutate(latitudes = as.numeric(latitudes),
         longitudes = as.numeric(longitudes))

# Exclure les lignes avec latitudes et longitudes égales à 0
don_filtre <- don %>%
  filter(latitudes != 0 | longitudes != 0)

summary(don$latitudes)
summary(don$longitudes)

##################### Export vers fichier txt ##########################
ma_data_table <- as.data.table(don)
library(writexl)
write.csv(ma_data_table, "C:/Users/33662/Desktop/add_velo/data/data_cycle-piste.csv")


############################# Verif export ################################
library(readr)
df <- read.table("C:/Users/33662/Desktop/add_velo/data/data_cycle-piste.csv", sep=",", header = TRUE)
View(df)

