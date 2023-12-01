############################# PROJET ADD nettoyage -- Léo GABET ###################################

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

############# On importe les données #################

don <- read_delim("C:/Users/33662/desktop/add_velo/data/accidentsVelo.csv", delim=";")
# On visualise nos données : 
View(don)

# Fonction pour changer les valeurs quanti en quali sur le genre 
Genre <- function(valeur) {
  if (valeur == 1) {
    return("Homme")
  } else if (valeur == 2) {
    return("Femme")
  } else {
    return("Valeur inconnue")
  }
}

# Appliquer la fonction à la colonne
genre_quali <- sapply(don$sexe, Genre)
table (genre_quali)
don$sexe = genre_quali
names(don)[names(don) == "sexe"] <- "Genre"


type_intersection <- function(valeur) {
  case_when(
    valeur == 0 ~ "Non renseigné",
    valeur == 1 ~ "Hors intersection",
    valeur == 2 ~ "Intersection en X",
    valeur == 3 ~ "Intersection en T",
    valeur == 4 ~ "Intersection en Y",
    valeur == 5 ~ "Intersection à plus de 4 branches",
    valeur == 6 ~ "Giratoire",
    valeur == 7 ~ "Place",
    valeur == 8 ~ "Passage à niveau",
    valeur == 9 ~ "Autre intersection",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$int)
int_quali <- sapply(don$int, type_intersection)
table(int_quali)
don$int = int_quali
names(don)[names(don) == "int"] <- "intersection"



type_collision <- function(valeur) {
  case_when(
    valeur == 1 ~ "Deux véhicules - frontale",
    valeur == 2 ~ "Deux véhicules - par l'arrière",
    valeur == 3 ~ "Deux véhicules - par le coté",
    valeur == 4 ~ "Trois véhicules et plus - en chaine",
    valeur == 5 ~ "Trois véhicules et plus - colisions multiples",
    valeur == 6 ~ "Autre collision",
    valeur == 7 ~ "Sans collision",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$col)
col_quali <- sapply(don$col, type_collision)
table(col_quali)
don$col = col_quali
names(don)[names(don) == "col"] <- "Collision"




Luminisote <- function(valeur) {
  case_when(
    valeur == 1 ~ "Plein jour",
    valeur == 2 ~ "Crépuscule ou aube",
    valeur == 3 ~ "Nuit sans éclairage public",
    valeur == 4 ~ "Nuit sans éclairage public non allumé",
    valeur == 5 ~ "Nuit sans éclairage public allumé",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$lum)
lum_quali <- sapply(don$lum, Luminisote)
table(lum_quali)
don$lum = lum_quali
names(don)[names(don) == "lum"] <- "Luminosité"


Cond_atmosph <- function(valeur) {
  case_when(
    valeur == 1 ~ "Normale",
    valeur == 2 ~ "Pluie légère",
    valeur == 3 ~ "Pluie forte",
    valeur == 4 ~ "Neige - grêle",
    valeur == 5 ~ "Brouillard - fumée",
    valeur == 6 ~ "Vent fort - tempête",
    valeur == 7 ~ "Temps éblouissant",
    valeur == 8 ~ "Temps couvert",
    valeur == 9 ~ "Autre",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$atm)
atm_quali <- sapply(don$atm, Cond_atmosph)
table(atm_quali)
don$atm = atm_quali
names(don)[names(don) == "atm"] <- "Meteo"

Cat_route <- function(valeur) {
  case_when(
    valeur == 1 ~ "Autoroute",
    valeur == 2 ~ "Route nationale",
    valeur == 3 ~ "Route départementale ",
    valeur == 4 ~ "Voie communale ",
    valeur == 5 ~ "Hors réseau public",
    valeur == 6 ~ "Parc de stationnement ouvert à la circulation publique",
    valeur == 7 ~ "Routes de métropole urbaine",
    valeur == 8 ~ "Autre",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$catr)
cat_quali <- sapply(don$catr, Cat_route)
table(cat_quali)
don$catr = cat_quali
names(don)[names(don) == "catr"] <- "Route"

Circu <- function(valeur) {
  case_when(
    valeur == 0 ~ "Non renseigné",
    valeur == 1 ~ "A sens unique",
    valeur == 2 ~ "Bidirectionnelle",
    valeur == 3 ~ "A chaussées séparées",
    valeur == 4 ~ "Avec voies d'affectation variable",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$circ)
circu_quali <- sapply(don$circ, Circu)
table(circu_quali)
don$circ = circu_quali
names(don)[names(don) == "circ"] <- "Circulation"

Profi <- function(valeur) {
  case_when(
    valeur == -1 ~ "Non renseigné",
    valeur == 1 ~ "Plat",
    valeur == 2 ~ "Pente",
    valeur == 3 ~ "Sommet de côte",
    valeur == 4 ~ "Bas de côte",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$prof)
profile_quali <- sapply(don$prof, Profi)
table(profile_quali)
don$prof = profile_quali
names(don)[names(don) == "prof"] <- "Profil route"

Plan <- function(valeur) {
  case_when(
    valeur == -1 ~ "Non renseigné",
    valeur == 1 ~ "Partie rectiligne",
    valeur == 2 ~ "En courbe à gauche",
    valeur == 3 ~ "En courbe à droite",
    valeur == 4 ~ "En S",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$plan)
plan_quali <- sapply(don$plan, Plan)
table(plan_quali)
don$plan = plan_quali
names(don)[names(don) == "plan"] <- "Plan route"



Surface <- function(valeur) {
  case_when(
    valeur == -1 ~ "Non renseigné",
    valeur == 1 ~ "Normale",
    valeur == 2 ~ "Mouillée",
    valeur == 3 ~ "Flaques",
    valeur == 4 ~ "Inondée",
    valeur == 5 ~ "Enneigée",
    valeur == 6 ~ "Boue",
    valeur == 7 ~ "Verglacée",
    valeur == 8 ~ "Corps gras - huile",
    valeur == 9 ~ "Autre",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$surf)
surface_quali <- sapply(don$surf, Surface)
table(surface_quali)
don$surf = surface_quali
names(don)[names(don) == "surf"] <- "Surface"



Infrac <- function(valeur) {
  case_when(
    valeur == -1 ~ "Non renseigné",
    valeur == 1 ~ "Aucun",
    valeur == 2 ~ "Souterrain – tunnel",
    valeur == 3 ~ "Pont – autopont ",
    valeur == 4 ~ "Bretelle d’échangeur ou de raccordement Inondée",
    valeur == 5 ~ "Voie ferrée",
    valeur == 6 ~ "Carrefour aménagé",
    valeur == 7 ~ "Zone piétonne",
    valeur == 8 ~ "Zone de péage",
    valeur == 9 ~ "Chantier",
    valeur == 10 ~ "Autre",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$infra)
infra_quali <- sapply(don$infra, Infrac)
table(infra_quali)
don$infra = infra_quali
names(don)[names(don) == "infra"] <- "Lieu"


Situation <- function(valeur) {
  case_when(
    valeur == -1 ~ "Non renseigné",
    valeur == 1 ~ "Aucun",
    valeur == 2 ~ "Sur chaussée",
    valeur == 3 ~ "Sur bande d'arrêt d'urgence",
    valeur == 4 ~ "Sur accotement",
    valeur == 5 ~ "Sur trottoir",
    valeur == 6 ~ "Sur piste cyclable",
    valeur == 7 ~ "Sur autre voie spéciale",
    valeur == 8 ~ "Zone de péage",
    valeur == 9 ~ "Autres",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$situ)
situ_quali <- sapply(don$situ, Situation)
table(situ_quali)
don$situ = situ_quali
names(don)[names(don) == "situ"] <- "Situation"



Grave <- function(valeur) {
  case_when(
    valeur == 1 ~ "Indemne",
    valeur == 2 ~ "Tué",
    valeur == 3 ~ "Blessé hospitalisé",
    valeur == 4 ~ "Blessé léger",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$grav)
grav_quali <- sapply(don$grav, Grave)
table(grav_quali)
don$grav = grav_quali
names(don)[names(don) == "grav"] <- "Conséquence"



Motif <- function(valeur) {
  case_when(
    valeur == -1 ~ "Non renseigné",
    valeur == 0 ~ "Non renseigné",
    valeur == 1 ~ "Domicile – travail ",
    valeur == 2 ~ "Domicile – école",
    valeur == 3 ~ "Courses - achats",
    valeur == 4 ~ "Utilisation professionnelle",
    valeur == 5 ~ "Promenade - loisirs",
    valeur == 6 ~ "Autre",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$trajet)
motif_quali <- sapply(don$trajet, Motif)
table(motif_quali)
don$trajet = motif_quali
names(don)[names(don) == "trajet"] <- "Contexte"



secu <- function(valeur) {
  case_when(
    valeur == 0 ~ "Non renseigné",
    valeur == 1 ~ "Oui",
    valeur == 2 ~ "Non",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$secuexist)
secu_quali <- sapply(don$secuexist, secu)
table(secu_quali)
don$secuexist = secu_quali
names(don)[names(don) == "secuexist"] <- "Securité équip"





equi <- function(valeur) {
  case_when(
    valeur == 0 ~ "Aucun équipement",
    valeur == 1 ~ "Ceinture",
    valeur == 2 ~ "Casque",
    valeur == 3 ~ "Dispositif enfants",
    valeur == 4 ~ "Gilet réfléchissant",
    valeur == 5 ~ "Airbag",
    valeur == 6 ~ "Gants",
    valeur == 7 ~ "Non déterminable",
    valeur == 8 ~ "Autre",
    valeur == 9 ~ "NA",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$equipement)
equi_quali <- sapply(don$equipement, equi)
table(equi_quali)
don$equipement = equi_quali
names(don)[names(don) == "equipement"] <- "Equipement sécu"


obstacle <- function(valeur) {
  case_when(
    valeur == -1 ~ "Non renseigné",
    valeur == 0 ~ "Aucun",
    valeur == 1 ~ "Piéton",
    valeur == 2 ~ "Véhicule",
    valeur == 3 ~ "Véhicule sur rail",
    valeur == 4 ~ "Animal domestique",
    valeur == 5 ~ "Animal sauvage",
    valeur == 6 ~ "Animal domestique",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$obsm)
obsm_quali <- sapply(don$obsm, obstacle)
table(obsm_quali)
don$obsm = obsm_quali
names(don)[names(don) == "obsm"] <- "Obstacle mobile"

choc <- function(valeur) {
  case_when(
    valeur == -1 ~ "Non renseigné",
    valeur == 0 ~ "Aucun",
    valeur == 1 ~ "Avant",
    valeur == 2 ~ "Avant droit",
    valeur == 3 ~ "Avant gauche",
    valeur == 4 ~ "Arrière",
    valeur == 5 ~ "Arrière droit",
    valeur == 6 ~ "Arrière gauche",
    valeur == 7 ~ "Coté droit",
    valeur == 8 ~ "Coté gauche",
    valeur == 9 ~ "Cos multiples (tonneaux)",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$choc)
choc_quali <- sapply(don$choc, choc)
table(choc_quali)
don$choc = choc_quali
names(don)[names(don) == "choc"] <- "Choc subi"


agg <- function(valeur) {
  case_when(
    valeur == 1 ~ "Hors agglomération",
    valeur == 2 ~ "En agglomération",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$agg)
agg_quali <- sapply(don$agg, agg)
table(agg_quali)
don$agg = agg_quali
names(don)[names(don) == "agg"] <- "Agglomération"

# Supprimer les colonnes spécifiées du data.frame "don"
# Supprimer les colonnes spécifiées du data.frame "don"
don <- subset(don, select = -c(Num_Acc, date, com, lat, long, nbv, lartpc, larrout, obs, manv, vehiculeid, typevehicules, manoeuvehicules, numVehicules))



don <- don[don$Genre != "Valeur inconnue", ]
table(don$Genre)

don_clean = don
view(don_clean)


library(data.table)

ma_data_table <- as.data.table(don_clean)
library(writexl)
write.csv(ma_data_table, "C:/Users/33662/Desktop/add_velo/data/data_clean.txt")




library(readr)
data_clean <- read.table("C:/Users/33662/Desktop/add_velo/data/data_clean.txt", sep=",", header = TRUE)
View(data_clean)






Factoshiny(ma_data_table)


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