tab1=read.table("Data_climate.txt", header = TRUE)
tab1 #tableau de données des stations
summary(tab1)
names(tab1)
dim(tab1)

tab2=read.table("Data_fish.txt", header = TRUE)
tab2 #tableau présence/absence des espèces
summary(tab2)
names(tab2)
dim(tab2)

tab3=read.table("Data_traits.txt", header = TRUE)
tab3 #tableau de données des caractéristiques des espèces
summary(tab3)
names(tab3)
dim(tab3)

### Partie 1 ###

#Question 1 : la richesse spé dépend de la température (GLM)

#ajout de la richesse spécifique au tableau de données des stations
options(max.print=9999)
richess=apply(tab2[,c(4:626)],1,sum)
richess
tab1bis=cbind(tab1,richess)

#GLM
hist(tab1bis$richess)
plot(tab1bis$richess~tab1bis$Mean)
model1=glm(tab1bis$richess~tab1bis$Mean,family=poisson)
summary(model1)
#H0 = pas d'effet de la température sur la richesse spécifique
#H1 = effet de la température sur la richesse spécifique
anova(model1,test="Chisq")
#pvalue<0.05 rejet H0. Il y a un effet de la température sur la richesse spécifique
#R2
120.02/129839
# très faible 
plot(richess~Mean, data=tab1bis)
points(tab1bis$Mean, predict(model1, type="response"), col="red", pch=16)

# paramètre de dispersion
129719/8152
1-pchisq(129719,8152) # -> 0 donc modele mal adapté
#H0 = pas d'effet de la température sur la richesse spécifique
#H1 = effet de la température sur la richesse spécifique
model2=glm(tab1bis$richess~tab1bis$Mean,family=quasipoisson)
# choix de la loi de quasi poisson pour prendre en compte la surdispersion 
summary(model2) # prend encompte la surdispersion très élvée (15,49003)
anova(model2,test="F")
#pvalue<0.05 (moins significatif que avant) rejet H0. Il y a un effet de la température sur la richesse spécifique
#quand température diminue richesse spé augmente 

#Déviance (scee)=120.02
#Déviance résiduelle (scer)=129719
#Déviance totale (scet)=129839

#R² = Déviance / Déviance totale
120.02/129839
#pourcentage expliqué très faible 
#limite des stats sur de très grands jeu de données

#Question 2:la richesse spéficique dépend longitude, latitude et températion corrélées (GLM avec variables explicatives multiples)

plot(tab1$Longitude~tab1$Latitude)
plot(tab1$Latitude~tab1$Longitude)
plot(tab1$Latitude, tab1$Mean) # corrélation visible
plot(tab1$Longitude, tab1$Mean) 

#vérification de la corrélation entre la température moyenne et la latitude
par(mfrow=c(1,2))
hist(tab1$Mean)
hist(tab1$Latitude)
#comme échantillon plus de 5000 considéré comme normalité
#test paramétrique : pearson = paramétrique, spearman = non paramétrique 
par(mfrow=c(1,1))
plot(tab1$Latitude, tab1$Mean)
#H0 pas de corrélation, H1 corrélation
cor.test(tab1$Latitude, tab1$Mean, method="pearson")
# rejet de HO et acceptation H1 corrélation entre les deux
# coefficient de corrélation négatif donc quand la latitude augmente la température diminue

#GLM avec variables explicatives multiples
plot(richess~Mean, data=tab1bis)
plot(richess~Latitude, data=tab1bis)
plot(richess~Longitude, data=tab1bis)

#GLM avec les 3 variables
modelmulti=glm(richess~Latitude+Longitude+Mean, data=tab1bis, family=quasipoisson)
summary(modelmulti)
#H0 pas de corrélation, H1 corrélation
anova(modelmulti,test="F")
# rejet de H0 donc corrélation 

#pseudo R² = Déviance / Déviance totale
60.1/129839 #Latitude
12570.1/129839 #Longitude
1367.5/129839 # tepérature

modelmulti1=glm(richess~Longitude+Latitude, data=tab1bis, family=quasipoisson)
summary(modelmulti1)
#H0 pas de corrélation, H1 corrélation
anova(modelmulti1,test="F")
# rejet de H0 donc corrélation

#pseudo R² = Déviance / Déviance totale
11926.9/129839 #Longitude
703.3/129839 #latitude

modelmulti2=glm(richess~Latitude+Mean, data=tab1bis, family=quasipoisson)
summary(modelmulti2)
#H0 pas de corrélation, H1 corrélation
anova(modelmulti2,test="F")
# rejet de H0 donc corrélation

#pseudo R² = Déviance / Déviance totale
60.085/129839 #Latitude
83.715/129839 #température moyenne

modelmulti3=glm(richess~Longitude+Mean, data=tab1bis, family=quasipoisson)
summary(modelmulti3)
#H0 pas de corrélation, H1 corrélation
anova(modelmulti3,test="F")
# rejet de H0 donc corrélation

#pseudo R² = Déviance / Déviance totale
11926.9/129839 #longitude
1601.6/129839 #latitude

#regarder la distribution horizontale des espèces
summary(tab3)
rich_spe=colSums(tab3[c(9:15)], na.rm = TRUE)
rich_spe #possible tableau sur excel

#Question 3 : corrélatione entre la profondeur max et min ?

#Vérifier que corrélation entre profondeur min et profondeur max
par(mfrow=c(1,2))
hist(tab3$Min_depth)
hist(tab3$Max_depth)
# H0 : normalité ; H1 : pas de normalité
shapiro.test(tab3$Min_depth) #  rejet H0 les données ne sont pas distribuées normalement 
shapiro.test(tab3$Max_depth) # idem
# faire un test non paramétrique : pearson = paramétrique, spearman = non paramétrique 
par(mfrow=c(1,1))
plot(tab3$Max_depth, tab3$Min_depth)
#H0 pas de corrélation, H1 corrélation
cor.test(tab3$Max_depth,tab3$Min_depth, method="spearman")
# rejet de HO et acceptation H1 corrélation entre les deux 

# Question 4 : la profondeur moyenne des espèces dépend de leur répartition verticale

apply(tab3[,c(16,17)],1,mean)
tab3$Mean_depth<-(tab3$Max_depth+tab3$Min_depth)/2 # ajout de la profondeur moyenne dans le tableau de données
names(tab3)

boxplot(Mean_depth~Vertical_Distribution, data = tab3)
model1 = lm(Mean_depth~Vertical_Distribution, data = tab3)
shapiro.test(residuals(model1)) #les résidus ne sont pas distribués en suivant une loi normale

# transformation log
model2= lm(log(Mean_depth)~Vertical_Distribution, data = tab3)
shapiro.test(residuals(model2)) #les résidus ne sont pas distribués en suivant une loi normale
# transformation sqrt
model3= lm(sqrt(Mean_depth)~Vertical_Distribution, data = tab3)
shapiro.test(residuals(model3)) #les résidus ne sont pas distribués en suivant une loi normale

# test non paramétrique 
#H0: toutes les moyennes sont égales ; H1 : au moins une moyennes est différente
kruskal.test(Mean_depth ~ Vertical_Distribution, data = tab3)
#rejet H0 au moins une moyenne différentes -> test post hoc
pairwise.wilcox.test(tab3$Mean_depth, tab3$Vertical_Distribution)
# différence signifiative entre pelagic/benthique et demersal/benthique

### Partie 2 ###

tab_out_herbi=tab3[-c(20,239,255,469,472,503,512,516),]
#on enlève les herbivores car ils sont sousreprésentés

#Question 1  : La taille est-elle influencée par le niveau trophique et la diet (Ancova)

plot(Max_length ~ Trophic_level, col=ifelse(Diet=="Carnivorous","grey",ifelse(Diet=="Piscivorous", "blue", "red")), data=tab_out_herbi)

modelancova=lm(Max_length~Trophic_level+Diet+Diet:Trophic_level, data=tab_out_herbi)
par(mfrow = c (1,2))
plot(modelancova, which = 1:2)
hist(residuals(modelancova))
library(lmtest)
shapiro.test(residuals(modelancova)) # rejet de H0 les résidus ne sont pas distribués normalement
dwtest(modelancova);bptest(modelancova)

#Transformation log
modelancova2=lm(log(Max_length)~Trophic_level+Diet+Diet:Trophic_level, data=tab_out_herbi)
par(mfrow = c (1,2))
plot(modelancova2, which = 1:2)
library(lmtest)
shapiro.test(residuals(modelancova2)) # rejet de H0 les résidus ne sont pas distribués normalement
dwtest(modelancova2);bptest(modelancova2)
hist(residuals(modelancova2)) 
# les résidus semblent distribués normalement or grâce au théorème central limite : vu qu’on a plus de 30 échantillons,
#la distribution est considérée comme normale donc on fait une ancova sur la transformation log car la non normalité des résidus n’influence pas la puissancec du test paramètrique 
library(car)
Anova(modelancova2)
summary(modelancova2)
#pvalue de l'interaction < 0.05 donc effet significatif de du niveau trophique en fonction de la diet

#représentation graphique des droites de regression
par(mfrow = c (1,2))
plot(Max_length ~ Trophic_level, col=ifelse(Diet=="Carnivorous","grey",ifelse(Diet=="Piscivorous", "blue", "red")), data=tab_out_herbi)
rev(sort(tab_out_herbi$Max_length)) #pour visualiser la plus grande et la plus petite valeur pour ajuster le graph
plot(Max_length ~ Trophic_level, col=ifelse(Diet=="Carnivorous","grey",ifelse(Diet=="Piscivorous", "blue", "red")), ylim=c(2,10), data=tab_out_herbi)
#reduit juste pour visualiser les droites
abline(a=-2.5175, b=1.6902, col="grey",lwd=2)
abline(a=-2.5175+4.7279, b=1.6902-1.5131, col="blue",lwd=2)
abline(a=-2.5175+3.0406, b=1.6902-0.7908, col="red",lwd=2)
legend("topleft", legend = c("Carnivores", "Piscivores", "Omnivores"), pch = 1, cex = 1, col = c("grey","blue","red"))

#Question 2 : Le niveau trophique a-t-il une influence sur le comportement (général)?

# Anova 1
boxplot(Trophic_level~Behavior, data = tab_out_herbi)
model = lm (Trophic_level~Behavior, data= tab_out_herbi)
par(mfrow = c (1,2))
plot(model, which = 1:2)
shapiro.test(residuals(model)) ; bptest (model) ; dwtest (model)    
# non rejet H0 donc les variances ne sont pas normalement distribuées
# transformation log
modelbis= lm(log(Trophic_level)~Behavior, data = tab_out_herbi)
shapiro.test(residuals(modelbis)) #les résidus ne sont pas distribués en suivant une loi normale
# transformation sqrt
model3= lm(sqrt(Trophic_level)~Behavior, data = tab_out_herbi)
shapiro.test(residuals(model3)) #les résidus ne sont pas distribués en suivant une loi normale

# test non paramétrique 
#H0: toutes les moyennes sont égales ; H1 : au moins une moyennes est différente
kruskal.test(Trophic_level~Behavior, data = tab_out_herbi)
plot(Trophic_level~Behavior, data = tab_out_herbi)
# Les comportements ne sont pas toujours les mêmes malgré les niveaux trophiques

#Question 3 : La diet influence la distribution verticale 

#Chi2 d'indépendance : 
table(tab_out_herbi$Diet,tab_out_herbi$Vertical_Distribution) -> tableaucontingence
tableaucontingence
chisq.test(tableaucontingence)$expected # règle de cochran ok
chisq.test(tableaucontingence)
## non rejet H0, les 2 variables sont donc indépendantes la distribution verticale n'est influencé par la diet.

