#####################################
### Script interpolation spatiale ###
#####################################

# Université de Bordeaux
# Annee universitaire 2020 - 2021
# Abou Kouta

setwd ("/home/abou/Documents/Master2/statistique/TDM-20200917/TDM_Interpolation/Interpolation_data_script") # indiquer le chemin vers le repertoire courant


##-------------------------------##
## OBJECTIFS DE LA SEANCE DE TDM ##
##-------------------------------##

## Realiser la modelisation spatiale de la richesse specifique (interpolation spatiale) des communautes vegetales
## sur l'etendue de la RNN des pres sales d'Ares et Lege Cap-Ferret a partir de donnees collectees ponctuellement

#-------------------------------------------------------------#
# Installation et chargement des paquets que l'on va utiliser #
#-------------------------------------------------------------#

install.packages (c ("sp", "gstat"))
## Attention, si l'installation se fait à partir de fichiers locaux, 
## le paquet gstat necessite : zoo, lattice, sp, spacetime, xts
library (sp)
library (gstat)


#-------------------------#
# Observation des donnees #
#-------------------------#

read.csv ("Donnees_Ares_simplifiees_LA93_v2.csv", header = T) -> donnees.df
donnees <- donnees.df
donnees
# Observer le contenu du jeu de donnees
summary (donnees.df)
names (donnees.df)
head (donnees.df)

class (donnees)
class (donnees.df)
xy <- donnees [, c(2:3)]
xy
	### /?\ A quelle classe d'objet appartiennent les objets 'meuse' et 'meuse.df' ?

names (donnees)
coordinates (donnees) = ~X_L93+Y_L93
class (donnees)
summary (donnees)

	### /?\ A quoi sert la fonction 'coordinates' ?
	### /?\ A quelle classe d'objet appartient l'objet 'donnees' ainsi cree ?
  ### /?\ Pourquoi, a votre avis, a-t-on enregistre le jeu de donnees sous deux noms differents ?


# Observer la structure de la variable 'richesse specifique'
hist (donnees.df$SpRichness)


# Representation cartographique de la richesse specifique

bubble (donnees, "SpRichness", col = c("#00ff0088", "#00ff0088"), main = "Species richness")
spplot (donnees, "SpRichness", main = "Species richness")
plot (donnees)


#---------------------------------------#
# Creation de la grille d'interpolation #
#---------------------------------------#

	### /?\ Pourquoi cette etape est-elle necessaire ?

## Creation du polygone representant le contour de la zone d'etude
read.csv ("Zone_noeud_LA93.csv", h=T) -> zone
head(zone)
xy.zone <- zone [, c(2:3)]
xy.zone
Zone.polygon <- Polygon (as.matrix (xy.zone), hole = TRUE)
Zone.polygon
Zone.polygons <- Polygons (list (Zone.polygon), ID = "Polyg")
Zone.polygons
Zone.spatial.polygons <- SpatialPolygons (list (Zone.polygons))
plot (Zone.spatial.polygons, border ="blue", axes = T)

Zone.grid <- spsample (Zone.spatial.polygons, cellsize = c(20,20), type = "regular", offset = c(0, 0))

	### /?\ A quoi sert la fonction spsample ? Quels en sont les arguments ?

class (Zone.grid)
plot (Zone.grid)
gridded (Zone.grid)
gridded (Zone.grid) <- TRUE
class (Zone.grid)
plot (Zone.grid)

  ### A quoi servent les lignes de commande ci-dessus ?

#------------------------#
# Interpolation spatiale #
#------------------------#

# Cette section requiert le paquet "gstat"

# 1) Interpolation par surface de tendance ("trend surface analysis" ou "TSA")
#-----

# TSA de premier ordre
	
   ### /?\ Que signifie l'ordre de la TSA ?

SpRichness.tsa.1 <- krige (SpRichness ~ 1, donnees, Zone.grid, degree = 1)
spplot (SpRichness.tsa.1 ["var1.pred"], main = "Species richness - 1st order TSA interpolation")

# TSA de second ordre
SpRichness.tsa.2 <- krige (SpRichness ~ 1, donnees, Zone.grid, degree = 2)
spplot (SpRichness.tsa.2 ["var1.pred"], main = "Species richness - 2nd order TSA interpolation")

# TSA de troisieme ordre
SpRichness.tsa.3 <- krige (SpRichness ~ 1, donnees, Zone.grid, degree = 3)
spplot (SpRichness.tsa.3 ["var1.pred"], main = "Species richness - 3rd order TSA interpolation")



# 2)Interpolation par l'inverse de la distance ("inverse distance weighted interpolation" ou "idw")
#-----

SpRichness.idw = idw (SpRichness~1, donnees, Zone.grid, idp = 2)
summary (SpRichness.idw)
spplot (SpRichness.idw ["var1.pred"], main = "Species richness - inverse distance weighted interpolation")

	### /?\ Quels sont les differents arguments de la fonction idw ?


# 3) Interpolation par krigeage
#-----

  ### /?\ Quelles sont les principales étapes du krigeage ?

# 3.1) Variogramme empirique et variogramme ajuste

hscat(SpRichness~1, donnees, (0:9) * 50)

	### /?\ A quoi sert la fonction hscat ?
	### /?\ Interpretez les resultats.

SpRichness.var.cloud <- variogram (SpRichness ~ 1, donnees, cloud = TRUE)
plot (SpRichness.var.cloud)

	### /?\ Que represente SpRichness.var.cloud ?

SpRichness.var.emp <- variogram (SpRichness ~ 1, donnees)
plot (SpRichness.var.emp)
summary (SpRichness.var.emp)
SpRichness.var.emp

	### /?\ Que represente SpRichness.var.emp ?


	### /?\ Quels sont les differents parametres qui permettent de decrire un variogramme ?
	### /?\ Dans le cas present, quelle valeur donneriez-vous a ces parametres ?


show.vgms()
vgm()

  ### /?\ Quel modele pourrait s'ajuster le mieux au variogramme ?


	### /?\ Completez la fonction ci-dessous en remplacant les termes 'argument1' - 'argument4' par les valeurs ou termes requis.
SpRichness.fit.1 <- fit.variogram (SpRichness.var.emp, model = vgm (argument1, argument2, argument3, argument4))
plot (SpRichness.var.emp, SpRichness.fit.1)

	### /?\ Est-ce que l'ajustement est correct ?

	### /?\ Essayez d'autres valeurs de parametres ou d'autres modeles de variogrammes
SpRichness.fit.2 <- # A completer
SpRichness.fit.3 <- # A completer
SpRichness.fit.4 <- # A completer

# 3.2) Krigeage

SpRichness.kriged.1 <- krige (SpRichness ~ 1, donnees, Zone.grid, model = SpRichness.fit.1)
summary (SpRichness.kriged.1)
spplot (SpRichness.kriged.1 ["var1.pred"])
spplot (SpRichness.kriged.1 ["var1.var"])

	### /?\ Que representent ces deux cartes ?

