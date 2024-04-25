library("ggplot2")
library("FactoMineR" )
library("factoextra")
library("corrplot")

# Chargement des donn??es interne de R
data(poison)

# Visualiser la dimension et les 7 premi??res variables pour 10 premiers individus
dim(poison)
head(poison[, 1:7], 10)
head(poison, n=10)

# D??limitation des donn??ees
mesactifs = poison[1:55, 5:15]

# Quelques stat descs
summary(mesactifs)
actif_table = table(mesactifs)

# Test de khi 2
chisq.test(mesactifs)

#ACM
monacm = MCA(mesactifs)
monacm
monacm = MCA(poison, ind = 53:55, quanti.sup = 1:2, quali.sup = 3:4)
monacm

#Valeurs propre
msvp = get_eigenvalue(monacm)
msvp
fviz_screeplot(monacm, addlabels = TRUE, ylim = c(0,45))

# Graphe des variables
fviz_mca_var(monacm, repel = TRUE, ggtheme = theme_minimal())

# Graphe des variables avec couleur selon les cos2
fviz_mca_var(monacm, col.var = "cos2", gradient.cols = c('#00AFBB', '#E7B800', '#FC4E07'), repel = TRUE, ggtheme = theme_minimal())

fviz_mca_var(monacm, alpha.var = "cos2", repel = TRUE, ggtheme = theme_minimal())

var = get_mca_var(monacm)
var
head(var$coord)
head(var$cos2)
head(var$contrib)

corrplot(var$cos2, is.corr=FALSE)

## Contribution des modalit??s ?? chaque axe (pour les 15 plus grandes contributions ?? 
#l???axe 1 par exemple, ou aux axes 1 et 2 ???) 
fviz_contrib (monacm, choice = "var", axes = 1, top = 15) 
fviz_contrib (monacm, choice = "var", axes = 1:2, top = 15) 

## Graph des variables avec couleur selon les contributions 
fviz_mca_var(monacm, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", 
                                                            "#FC4E07"), repel = TRUE, ggtheme = theme_minimal()) 
fviz_mca_var (monacm, alpha.var = "contrib", repel = TRUE, ggtheme = theme_minimal ()) 
## R??sultats pour les individus 
ind <- get_mca_ind (monacm) 
ind 
head(ind$coord) 
head(ind$cos2) 
head(ind$contrib)

## Possible de construire les m??mes graphs que dans le cas des variables. Remplacer le 
#?? .var ?? par ?? .ind ?? dans les commandes concern??es. 
#Cas particulier du graph des individus avec une variable projet??e ?? l???int??rieur. 
fviz_mca_ind (monacm, label = "none", # masquer le texte des individus 
              habillage = "Vomiting", # colorer par groupes 
              palette = c ("#00AFBB", "#E7B800"), addEllipses = TRUE, ellipse.type = "confidence", 
              ggtheme = theme_minimal ()) 
## Les suppl??mentaires : j???en fais quoi ? 
monacm$quali.sup 
monacm$quanti 
monacm$ind.sup 