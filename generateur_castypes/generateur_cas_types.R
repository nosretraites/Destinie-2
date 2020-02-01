# Générateur de cas-types

# Données d'entrées :
# - année de naissance
# - choix du cas-type
# - âge de début de carrière

# Sortie :
# une table ech, une table fam et une table emp
######################################
library(tibble)
library(dplyr)
library(xlsx)

####### CHARGEMENT DONNEES ############

macro <- read.xlsx("./generateur_castypes/macro_projetee.xls", sheetIndex = 1, colClasses = "numeric")

remun_relative <- read.xlsx("./generateur_castypes/salaire_age.xls", sheetIndex = 1) %>% 
  rename(rowname = Age) %>% 
  column_to_rownames() %>% 
  as.matrix()

age_debut_carriere <- read.xlsx("./generateur_castypes/salaire_age.xls", sheetIndex = 2) %>% 
  rename(rowname = Naissance) %>% 
  column_to_rownames() %>% 
  as.matrix()

taux_prime <- list(
  const = read.xlsx("./generateur_castypes/salaire_age.xls", sheetIndex = 3) %>% 
    rename(rowname = Age) %>% 
    column_to_rownames() %>% 
    as.matrix(),
  hausse = read.xlsx("./generateur_castypes/salaire_age.xls", sheetIndex = 4) %>% 
    rename(rowname = Age) %>% 
    column_to_rownames() %>% 
    as.matrix()
)

#####################################

###############################
# UTILITAIRES DE CONSTRUCTION DE
# DONNEES D'ENTREE
################################

#### Définition cas-type générique ####
setClass("cas.type", slots=list(
  type = "character",
  ech = "data.frame",
  fam = "data.frame",
  emp = "data.frame",
  age_debut = "numeric",
  prorata = "numeric"
))

#### Initialisation d'un cas-type ####
ct.init <- function(anaiss, id = 1, cas, typeFP = "NONFP", age_debut = NULL) {
  
  typeFP_int = switch(typeFP, FPE = 0, FPTH = 1, NONFP = 2, "ERROR")
  if (typeFP_int == "ERROR") {stop("typeFP doit être FPE, FPTH ou NONFP")}
  
  ech = data.frame(
    Id = id, sexe = 1, findet = 0, typeFP = typeFP_int,
    anaiss = anaiss, neFrance = 1, k = 0, taux_prim = 0,
    moisnaiss = 1, emigrant = 0, tresdip = 0,
    peudip = 0, dipl= 0
  ) %>% 
    mutate_at(vars(-k, -taux_prim), as.integer) %>%
    mutate_at(vars(k, taux_prim), as.numeric)
  
  emp = data.frame(
    Id = id,
    age = 0:79,
    statut = 0, 
    salaire = 0
  ) %>% 
    mutate_at(vars(-salaire), as.integer) %>% 
    mutate_at(vars(salaire), as.numeric)
  
  fam = data.frame(
    pere = 0, mere = 0, enf1 = 0, enf2 = 0,
    enf3 = 0, enf4 = 0, enf5 = 0, enf6 = 0,
    matri = 1, Id = id, annee = 2019, conjoint = 0
  ) %>% 
    mutate_all(as.integer)
  
  if (is.null(age_debut)) {
    age_debut <- age_debut_carriere[as.character(anaiss), cas]
  }
  if (age_debut == floor(age_debut)) {
    prorata <- 1
  } else {
    prorata <- age_debut - floor(age_debut)
  }
  
  return(new("cas.type", ech = ech, emp = emp, fam = fam, age_debut = age_debut, prorata = prorata))
}

# passe du salaire relatif au salaire en nominal
calcul_salaire <- function(castype, macro, cas) {
  # remplissage de la colonne salaire
  castype@emp$salaire[18:68] <- remun_relative[, cas] * ifelse(castype@emp$age[18:68] >= floor(castype@age_debut), 1, 0)
  
  # proratisation du premier salaire
  castype@emp$salaire[floor(castype@age_debut)+1] <- castype@emp$salaire[floor(castype@age_debut)+1]*castype@prorata
  
  # passage du salaire relatif au salaire en nominal
  castype@emp$annee <- castype@emp$age + castype@ech$anaiss
  castype@emp <- left_join(castype@emp, macro[, c("annee", "SMPT")], by="annee")
  castype@emp$salaire <- castype@emp$salaire*castype@emp$SMPT
  
  castype@emp$annee <- NULL
  castype@emp$SMPT <- NULL
  
  return(castype)
}

#### save_xls : enregistrer un/des cas-types en .xls ####
save_xls <- function(castype, filename = "./generateur_castypes/cas_type.xlsx") {
  write.xlsx2(castype@ech, file = filename, sheetName = "ech", row.names = FALSE)
  write.xlsx2(castype@emp, file = filename, sheetName = "emp", append=TRUE, row.names = FALSE)
  write.xlsx2(castype@fam, file = filename, sheetName = "fam", append=TRUE, row.names = FALSE)
}

#### join_castypes : concaténer deux cas-type ####
join_castypes <- function(x, y) {
  return(new("cas.type", 
             ech = rbind(x@ech, y@ech),
             emp = rbind(x@emp, y@emp),
             fam = rbind(x@fam, y@fam)
  ))
}

##############################
# IMPLEMENTATION DES DIFFERENTS
# CAS-TYPES DU COR
##############################

#### cas-type salarié (COR 1) ####
cor1 <- function(anaiss, id = 1, age_debut = NULL) {
  message("Attention, la fonction COR 1 ne gère pas le passage au statut cadre")
  # initialisation du cas-type
  salarie <- ct.init(anaiss, id = id, cas = "Cas.1", age_debut = age_debut)
  
  # remplissage de la colonne statut (1 = salarié du privé)
  salarie@emp$statut <- ifelse(salarie@emp$age >= floor(salarie@age_debut), 1, 63)
  
  # calcul du salaire
  salarie <- calcul_salaire(salarie, macro, cas = "Cas.1")
  
  return(salarie)
}

#### cas-type salarié (COR2) ####
cor2 <- function(anaiss, id = 1, age_debut = NULL) {
  # initialisation du cas-type
  salarie <- ct.init(anaiss, id = id, cas = "Cas.2", age_debut = age_debut)
  
  # remplissage de la colonne statut (1 = salarié du privé)
  salarie@emp$statut <- as.integer(ifelse(salarie@emp$age >= floor(salarie@age_debut), 1, 63))
  
  # calcul du salaire
  salarie <- calcul_salaire(salarie, macro, cas = "Cas.2")
  
  return(salarie)
}

#### cas-type salarié (COR5) ####
cor5 <- function(anaiss, id = 1, age_dersal, hypothese = "const", age_debut = NULL) {
  # initialisation du cas-type
  salarie <- ct.init(anaiss, id = id, cas = "Cas.5", age_debut = age_debut)
  
  # remplissage de la colonne statut (321 = FPE sédentaire)
  salarie@emp$statut <- as.integer(ifelse(salarie@emp$age >= floor(salarie@age_debut), 321, 63))
  
  # calcul du salaire
  salarie <- calcul_salaire(salarie, macro, cas = "Cas.5")
  
  # taux de prime du dernier salaire (2 hypothèses: const ou hausse)
  salarie@ech$taux_prim <- taux_prime[[hypothese]][as.character(age_dersal), paste0("Génération.", anaiss)]
  
  return(salarie)
}