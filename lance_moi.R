
# Script principal

source("./de_la_data.R")



# # # 1) Caprices du Destin (Chaos niveau 14)

# # Paramètres
nb_jet <- 1

# # Exec
sample(etatNeg, nb_jet, T)



# # # 2) Random (Chaos niveau 34)

# # Paramètres
lvlMax <- 70 # 50, 60 ou 70
nombreDeSorts <- 4

# # Exec
sortAuHasard(lvlMax, nombreDeSorts)



# # # 3) Random (Chaos niveau 34)

# # Paramètres
niveau <- 4
nombreDeCrea <- 1

# # Exec
for (i in 1:nombreDeCrea) {
  print(paste("Creature", i))
  print(paste(sample(part1, 1),sample(part2, 1),sample(part3, 1)))
  
  maClasse <- sample(classes, 1)
  print(paste(maClasse, "niveau", niveau))
}



# # # 4) Ogham loot

# # Paramètres : nombre d'Oghams
nb <- 5
avecDetail <- F

# # Exec
monLoot <- lootOg(nb)
monLoot <- monLoot[order(monLoot)]

a <- table(monLoot)
names(a) <- NomsOg[as.integer(names(a))]

if (avecDetail) {
  print(ifelse(nb >1, "Oghams obtenus :", "Ogham obtenu :"))
  print(a)
  #print(descArme[unique(monLoot)])
} else {
  print(ifelse(nb >1, "Oghams obtenus :", "Ogham obtenu :"))
  print(a)
}


