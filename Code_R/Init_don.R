############################# PROJET ADD nettoyage -- Leo GABET ###################################

############################### Charger les differentes librairies ######################################

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

############# On importe les donnees #################

don <- read_delim("C:/Users/33662/desktop/add_velo/data/accidentsVelo.csv", delim=";")
# On visualise nos donnees : 
View(don)

don <- don %>%
  filter(age <= 100)

# Convertir le format "00:00" en heures numériques
don <- don %>%
  mutate(heure_numerique = as.numeric(substr(hrmn, 1, 2)) * 100 + as.numeric(substr(hrmn, 4, 5)))

# Définir les intervalles pour les tranches horaires
intervalles <- c(0, 600, 1200, 1800, 2400)
labels <- c("Nuit", "Matin", "Apres-midi", "Soir")

# Ajouter une colonne 'tranche_horaire' basée sur les intervalles définis
don <- don %>%
  mutate(tranche_horaire = cut(heure_numerique, breaks = intervalles, labels = labels, include.lowest = TRUE))

# Convertir 'tranche_horaire' en facteur pour obtenir les catégories
don$tranche_horaire <- factor(don$tranche_horaire, levels = labels)

# Remplacer la colonne 'hrmn' par 'tranche_horaire'
don$hrmn <- don$tranche_horaire

# Supprimer la colonne temporaire 'tranche_horaire'
don <- don %>% select(-tranche_horaire, -heure_numerique)

names(don)[names(don) == "hrmn"] <- "Tranche_horaire"


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

# Appliquer la fonction a la colonne
genre_quali <- sapply(don$sexe, Genre)
table (genre_quali)
don$sexe = genre_quali
names(don)[names(don) == "sexe"] <- "Genre"


type_intersection <- function(valeur) {
  case_when(
    valeur == 0 ~ "Non renseigne",
    valeur == 1 ~ "Hors intersection",
    valeur == 2 ~ "Intersection en X",
    valeur == 3 ~ "Intersection en T",
    valeur == 4 ~ "Intersection en Y",
    valeur == 5 ~ "Intersection a plus de 4 branches",
    valeur == 6 ~ "Giratoire",
    valeur == 7 ~ "Place",
    valeur == 8 ~ "Passage a niveau",
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
    valeur == 1 ~ "Deux vehicules - frontale",
    valeur == 2 ~ "Deux vehicules - par l'arriere",
    valeur == 3 ~ "Deux vehicules - par le cote",
    valeur == 4 ~ "Trois vehicules et plus - en chaine",
    valeur == 5 ~ "Trois vehicules et plus - colisions multiples",
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
    valeur == 2 ~ "Crepuscule ou aube",
    valeur == 3 ~ "Nuit sans eclairage public",
    valeur == 4 ~ "Nuit sans eclairage public non allume",
    valeur == 5 ~ "Nuit sans eclairage public allume",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$lum)
lum_quali <- sapply(don$lum, Luminisote)
table(lum_quali)
don$lum = lum_quali
names(don)[names(don) == "lum"] <- "Luminosite"


Cond_atmosph <- function(valeur) {
  case_when(
    valeur == 1 ~ "Normale",
    valeur == 2 ~ "Pluie legere",
    valeur == 3 ~ "Pluie forte",
    valeur == 4 ~ "Neige - grele",
    valeur == 5 ~ "Brouillard - fumee",
    valeur == 6 ~ "Vent fort - tempete",
    valeur == 7 ~ "Temps eblouissant",
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
    valeur == 3 ~ "Route departementale ",
    valeur == 4 ~ "Voie communale ",
    valeur == 5 ~ "Hors reseau public",
    valeur == 6 ~ "Parc de stationnement ouvert a la circulation publique",
    valeur == 7 ~ "Routes de metropole urbaine",
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
    valeur == 0 ~ "Non renseigne",
    valeur == 1 ~ "A sens unique",
    valeur == 2 ~ "Bidirectionnelle",
    valeur == 3 ~ "A chaussees separees",
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
    valeur == -1 ~ "Non renseigne",
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
    valeur == -1 ~ "Non renseigne",
    valeur == 1 ~ "Partie rectiligne",
    valeur == 2 ~ "En courbe a gauche",
    valeur == 3 ~ "En courbe a droite",
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
    valeur == -1 ~ "Non renseigne",
    valeur == 1 ~ "Normale",
    valeur == 2 ~ "Mouillee",
    valeur == 3 ~ "Flaques",
    valeur == 4 ~ "Inondee",
    valeur == 5 ~ "Enneigee",
    valeur == 6 ~ "Boue",
    valeur == 7 ~ "Verglacee",
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
    valeur == -1 ~ "Non renseigne",
    valeur == 1 ~ "Aucun",
    valeur == 2 ~ "Souterrain – tunnel",
    valeur == 3 ~ "Pont – autopont ",
    valeur == 4 ~ "Bretelle d’echangeur ou de raccordement Inondee",
    valeur == 5 ~ "Voie ferree",
    valeur == 6 ~ "Carrefour amenage",
    valeur == 7 ~ "Zone pietonne",
    valeur == 8 ~ "Zone de peage",
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
    valeur == -1 ~ "Non renseigne",
    valeur == 1 ~ "Aucun",
    valeur == 2 ~ "Sur chaussee",
    valeur == 3 ~ "Sur bande d'arret d'urgence",
    valeur == 4 ~ "Sur accotement",
    valeur == 5 ~ "Sur trottoir",
    valeur == 6 ~ "Sur piste cyclable",
    valeur == 7 ~ "Sur autre voie speciale",
    valeur == 8 ~ "Zone de peage",
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
    valeur == 2 ~ "Tue",
    valeur == 3 ~ "Blesse hospitalise",
    valeur == 4 ~ "Blesse leger",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$grav)
grav_quali <- sapply(don$grav, Grave)
table(grav_quali)
don$grav = grav_quali
names(don)[names(don) == "grav"] <- "Consequence"



Motif <- function(valeur) {
  case_when(
    valeur == -1 ~ "Non renseigne",
    valeur == 0 ~ "Non renseigne",
    valeur == 1 ~ "Domicile – travail ",
    valeur == 2 ~ "Domicile – ecole",
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
    valeur == 0 ~ "Non renseigne",
    valeur == 1 ~ "Oui",
    valeur == 2 ~ "Non",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$secuexist)
secu_quali <- sapply(don$secuexist, secu)
table(secu_quali)
don$secuexist = secu_quali
names(don)[names(don) == "secuexist"] <- "Securite equip"





equi <- function(valeur) {
  case_when(
    valeur == 0 ~ "Aucun equipement",
    valeur == 1 ~ "Ceinture",
    valeur == 2 ~ "Casque",
    valeur == 3 ~ "Dispositif enfants",
    valeur == 4 ~ "Gilet reflechissant",
    valeur == 5 ~ "Airbag",
    valeur == 6 ~ "Gants",
    valeur == 7 ~ "Non determinable",
    valeur == 8 ~ "Autre",
    valeur == 9 ~ "NA",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$equipement)
equi_quali <- sapply(don$equipement, equi)
table(equi_quali)
don$equipement = equi_quali
names(don)[names(don) == "equipement"] <- "Equipement secu"


obstacle <- function(valeur) {
  case_when(
    valeur == -1 ~ "Non renseigne",
    valeur == 0 ~ "Aucun",
    valeur == 1 ~ "Pieton",
    valeur == 2 ~ "Vehicule",
    valeur == 3 ~ "Vehicule sur rail",
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
    valeur == -1 ~ "Non renseigne",
    valeur == 0 ~ "Aucun",
    valeur == 1 ~ "Avant",
    valeur == 2 ~ "Avant droit",
    valeur == 3 ~ "Avant gauche",
    valeur == 4 ~ "Arriere",
    valeur == 5 ~ "Arriere droit",
    valeur == 6 ~ "Arriere gauche",
    valeur == 7 ~ "Cote droit",
    valeur == 8 ~ "Cote gauche",
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
    valeur == 1 ~ "Hors agglomeration",
    valeur == 2 ~ "En agglomeration",
    TRUE ~ "Valeur inconnue"
  )
}
table(don$agg)
agg_quali <- sapply(don$agg, agg)
table(agg_quali)
don$agg = agg_quali
names(don)[names(don) == "agg"] <- "Agglomeration"


# Supprimer les colonnes specifiees du data.frame "don"
don <- subset(don, select = -c(Num_Acc, date, com, nbv, lartpc, larrout, obs, manv, vehiculeid, typevehicules, manoeuvehicules, numVehicules))


don <- don[don$Genre != "Valeur inconnue", ]
table(don$Genre)


don_clean = don
view(don_clean)


##################### Export vers fichier txt ##########################
ma_data_table <- as.data.table(don_clean)
library(writexl)
write.csv(ma_data_table, "C:/Users/33662/Desktop/add_velo/data/data_clean.txt")


############################# Verif export ################################
library(readr)
data_clean <- read.table("C:/Users/33662/Desktop/add_velo/data/data_clean.txt", sep=",", header = TRUE)
View(data_clean)

colnames(data_clean)

don_clean <- data_clean %>%
  mutate(lat = as.numeric(lat),
         long = as.numeric(long))


summary(data_clean$lat)
summary(data_clean$long)



############################################################