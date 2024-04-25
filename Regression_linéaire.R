# Pratique regresssion lin??aire - Prix des appartements en fonction de la surface

library(ggplot2)

prix = c(130,280,268,500,320,250,378,250,350,300,155,245,200,325,85,78,375,200,270,85)
surface = c(28,50,55,110,60,48,90,35,86,65,32,52,40,70,28,30,105,52,80,20)

apart = data.frame(prix,surface) # Cr??ation du dataframe apart
apart

# Affichage de la droite de regression
ggplot(apart, aes(x=surface, y=prix)) + 
          geom_point() +
          geom_smooth(method = lm, se=FALSE)

# Obtenir les coefficients de la regression
coef(lm(prix~surface))
 
# Obtenir les param??tres du mod??le
model = lm(prix~surface)
names(model)
summary(model)

# Tableau des r??sidus : Cr??ation d'un tableau pour les valeurs r??elles, les valeurs pr??dictes et les r??sidus
round((data.frame(prix, val.predicted = fitted(model), 
                  residus = resid(model))), 2)

# Calcul de la MSE (Racine du quotient de la somme des carr??s des r??sidus par n-2)
n=20
sqrt(sum(resid(model)^2)/(n-2))

# Repr??sentation du graphique des r??sidus
plot(fitted(model), resid(model), xlab = "Valeurs predictes", ylab = "Residus")

# Intervalles de confiance
confint(model)

# Analyse de la variance
anova(model)

model$fitted.values