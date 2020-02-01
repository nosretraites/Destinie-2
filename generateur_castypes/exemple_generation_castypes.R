#### CHARGEMENT DES FONCTIONS #########
source("./generateur_castypes/generateur_cas_types.R")

########## LANCEMENT DU PROGRAMME ########

# cette commande crée un objet R avec la trajectoire de carrière cor2
# et enregistre un fichier excel utilisable dans destinie.reformedesretraites.fr
s <- cor2(1960)
save_xls(s)

# cette commande crée 41 cas-types COR 2 nés de 1950 à 1990
s <- cor2(1950, id = 1)
for (i in 1951:1990) {
  s <- join_castypes(s, cor2(i, id = i-1949))
}
save_xls(s, "./generateur_castypes/cor2_1950_1990.xlsx")

# cette commande crée 4 cas-types COR 2 des génératons 75, 80, 90, 2003 avec début de carrière à 22 ans
cor2(1975, age_debut = 22) %>% 
  join_castypes(cor2(1980, id = 2, age_debut = 22)) %>% 
  join_castypes(cor2(1990, age_debut = 22)) %>% 
  join_castypes(cor2(2003, age_debut = 22)) %>% 
  save_xls("./generateur_castypes/cas_type_cor2_gen75_80_90_03.xlsx")
