############################# PROJET ADD -- Léo GABET ###################################

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

############# On importe les données par un document xlsx avec comme séparateur les colones #################

don <- read_csv("C:/Users/33662/desktop/add_velo/data/accidentsVelo.csv")
# On visualise nos données : 
View(don)

summary(don)



Factoshiny(don)


########### Seance du 6 & 20 octobre GROUPE 7 ##########################
baseAFC = as.data.frame(cbind(don$Statut_exploit,don$Diplome))
baseAFC

# On enlève les valeurs NA 
baseAFC=na.omit(baseAFC)

# tableau de contingence
contingence = table(baseAFC)
contingence

# profils lignes et colonnes
ligne = prop.table(contingence, margin = 1)
ligne
colonne = prop.table(contingence, margin = 2)
colonne

# masses pour les lignes et les colonnes
nb_ind = sum(contingence)
masse_ligne = rowSums(contingence)/nb_ind
masse_col = colSums(contingence)/nb_ind

# excel contingence
tab_cont <- "tableau_contingence.xlsx"
write.xlsx(contingence, tab_cont)
view(contingence)

# excel ligne
tab_ligne <- "tableau_ligne.xlsx"
write.xlsx(ligne, tab_ligne)
view(ligne)

# excel colonne
tab_colonne <- "tableau_colonne.xlsx"
write.xlsx(colonne, tab_colonne)
view(colonne)

# Affichage graphe AFC
resultats_afc <- CA(contingence)

# Interprétation :
# Pour les colonnes :
# Aucun diplôme, Niveau Bac +1 et Niveau Bac +3 contribuent bcp à la dimension 1
# Aucun diplôme, Niveau Bac +1 et Niveau Bac +3 contribuent bcp à la dimension 2
# Aucun diplôme et CAP/BEP très bien expliqués par la 1ère dimension et
# BAC +1, +2, +3 et +4 bien expliqués par les deux premières dimensions réunies.

# Pour les lignes :
# retraité contribue énormément à la dimension 1
# Associé co-gérant, En cours de reprise, Fils/fille employé et salarié hors famille contribuent bcp à la dimension 1
# Associé co-gérant contribue énormément à la dimension 2.
# Retraité, En cours de reprise, Fils/fille employé et salarié hors famille contribuent bcp à la dimension 2
# Retraité très bien expliqué par la 1ère dimension
# Chef d'exploitation bien expliqué par la dimension 2
# Associé co-gérant bien expliqué par les deux premières dimensions réunies.

# Effectuer le test du Chi carré
chi_squared_test <- chisq.test(contingence)
chi_squared_test


# Visualiser les contributions des variables aux axes
fviz_contrib(resultats_afc, choice = "row")

# Visualiser les cosinus carrés des variables aux axes
fviz_cos2(resultats_afc, choice = "col")


# profils lignes et colonnes
ligne = prop.table(contingence, margin = 1)
ligne
colonne = prop.table(contingence, margin = 2)
colonne

barplot(ligne, beside = TRUE,
        space = c(0,1))

resultats_afc$row
resultats_afc$col






############################################################