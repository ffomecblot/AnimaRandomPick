
# Données à charger dans le script principal


# # 1 # #

etatNeg <- c('Peur : -60 TlA autres que fuir, sauf si passe un test 80 en Impassibilite',
             'Terreur : ne peut faire que fuir, sauf si passe un test 140 en Impassibilite',
             'Douleur : -40 TlA sauf si passe un test 80 de Resistance a la douleur',
             'Douleur extreme : -80 TlA sauf si passe un test 140 de Resistance a la douleur',
             'Faiblesse physique : -4 en Force, Dexterite, Agilite, Consitution',
             'Faiblesse mentale : -4 en Intelligence, Pouvoir, Volonte, Perception',
             'Paralysie partielle : -80 aux jets de combat, -30 aux autres actions et a l initiative',
             'Paralysie complete : -200 TlA et initiative',
             'Colere : attaque les cibles les plus proches le plus fort possible, sauf si passe un
             test 120 en Impassibilite',
             'Cecite : aveugle et -80 TlA ',
             'Surdite : ne peut plus entendre',
             'Mutisme : ne peut plus parler',
             'Fascination : aucune action active ni se deplacer, sauf si passe un test 120 en Impassibilite',
             'Degats egaux a la marge d echec',
             'Degats egaux au double de la marge d echec',
             'Inconscience',
             'Hallucinations, sauf si passe un test 140 en Impassibilite',
             'Folie, sauf si passe un test 140 en Impassibilite',
             'Petit coup de barre : malus de -20 TlA',
             'Extenuation : perd 5 points de fatigue (ne reviennent pas a la fin du sort)',
             'Nausse et mal de crane cinglant : malus de -50 TlA')




# # # 2 # # #

# # Fonctions
maVoie <- function (lv, selec = 1) {
  # def
  nomsPri <- c("Lumiere","Obscurite","Creation","Destruction","Necro","Eau","Feu","Terre",
               "Air","Essence","Illusion")
  nomsSec <- c("Chaos","Connaissance","Guerre","Literae",
               "Mort","Musique","Noblesse","Paix","Peche","Reve",
               "Sang","Secret","Temps","Vide")
  
  if (lv %% 10 == 4) {
    # tirer une secondaire
    voie <- sample(nomsSec,1)
  } else {
    # choisir la primaire
    voie <- nomsPri[selec]
  }
  return(voie)
}

sortAuHasard <- function(lvlMax = 40, nb = 1) {
  # def
  nomsRang <- c("basique", "intermediaire", "avance", "arcane")
  
  for (i in 1:nb) {
    rang <- sample(nomsRang, 1)
    lv <- sample(1:(lvlMax/2), 1) *2
    
    minMaj <- sample(1:11, 1)
    if (minMaj < 6) {
      # Voie majeure
      voie <- maVoie(lv, minMaj)
      
    } else {
      # Voie mineure ; relancer pour ne pas tomber sur un 8
      while (lv %% 10 == 8) {
        lv <- sample(1:(lvlMax/2), 1) *2
      }
      
      voie <- maVoie(lv, minMaj)
    }
    print(paste0("Sort de niveau ", lv, ", de la voie ", voie, ", au rang ", rang, "."))
  }
}



# # # 3 # # #

# Nom de la creature
part1 <- c('Gloom', 'Gray', 'Dire', 'Black', 'Shadow', 'Haze', 'Wind', 'Storm', 'Warp', 'Night',
           'Moon', 'Star', 'Pit', 'Flame', 'Ice', 'Seethe', 'Sharp', 'Ash', 'Blade', 'Steel', 'Stone',
           'Rust', 'Mold', 'Blight', 'Plague', 'Rot', 'Ooze', 'Puke', 'Snot', 'Bile', 'Blood', 'Pulse',
           'Gut', 'Gore', 'Flesh', 'Bone', 'Spine', 'Mind', 'Spirit', 'Soul', 'Wrath', 'Grief', 'Foul',
           'Vile', 'Sin', 'Chaos', 'Dread', 'Doom', 'Bane', 'Death', 'Viper', 'Dragon', 'Devil')
part2 <- c('Touch', 'Spell', 'Feast', 'Wound', 'Grin', 'Maim', 'Hack', 'Bite', 'Rend', 'Burn', 'Ripper',
           'Kill', 'Call', 'Vex', 'Jade', 'Web', 'Shield', 'Killer', 'Razor', 'Drinker', 'Shifter',
           'Crawler', 'Dancer', 'Bender', 'Weaver', 'Eater', 'Widow', 'Maggot', 'Spawn', 'Wight',
           'Grumble', 'Growler', 'Snarl', 'Wolf', 'Crow', 'Raven', 'Hawk', 'Cloud', 'Bang', 'Head',
           'Skull', 'Brow', 'Eye', 'Maw', 'Tongue', 'Fang', 'Horn', 'Thorn', 'Claw', 'Fist', 'Heart',
           'Shank', 'Skin', 'Wing', 'Pox', 'Fester', 'Blister', 'Pus', 'Slime', 'Drool', 'Froth',
           'Sludge', 'Venom', 'Poison', 'Break', 'Shard', 'Flame', 'Maul', 'Thirst', 'Lust')
part3 <- c('the Hammer', 'the Axe', 'the Sharp', 'the Jagged', 'the Flayer', 'the Slasher', 'the Impaler',
           'the Hunter', 'the Slayer', 'the Mauler', 'the Destroyer', 'the Quick', 'the Witch', 'the Mad',
           'the Wraith', 'the Shade', 'the Dead', 'the Unholy', 'the Howler', 'the Grim', 'the Dark',
           'the Tainted', 'the Unclean', 'the Hungry', 'the Cold')

# Classe de la creature ; 1 = guerrier, 2 = magicien, 3 = d
classes <- c("Guerrier")




# # # 4 # # #

# Noms
NomsOg <- c("El","Eld","Tir","Nef","Eth","Ith","Tal","Ral","Ort","Thul","Amn","Sol","Shael","Dol","Hel","Io",
          "Lum","Ko","Fal","Lem","Pul","Um","Mal","Ist","Gul","Vex","Ohm","Lo","Sur","Ber","Jah","Cham","Zod")

# Proba
lootOg <- function(n=1) {
  u <- integer(n)
  for (i in 1:n) {
    u[i] <- min(floor(runif(3,0,length(NomsOg)))+1)
  }
  return(u)
}



