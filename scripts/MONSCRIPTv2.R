#
install.packages('corrplot')
library('corrplot')

# Importation des donnees sur R
crypto<-read.csv("crypto-markets.csv", header=TRUE)

# Selection des colonnes importantes
myvars <- c("slug","date","ranknow","high","low","market")

# Laisser que les colonnes selectionnees 
crypto <- crypto[myvars]

# Transformation de la colonne date au type Date, Ajout de la colonne annee
crypto$date = as.Date(crypto$date)
crypto$annee = format(crypto$date,  "%Y")


# -----------------------
#       QUESTION 1
# -----------------------
# 1.1. Recuperation des donnees :
bitcoin2016_2017<-crypto[which(crypto$slug=="bitcoin" & crypto$annee>=2016 & crypto$annee<=2017),]
bitcoin2018<-crypto[which(crypto$slug=="bitcoin" & crypto$annee==2018),]
bitcoin2013_2017<-crypto[which(crypto$slug=="bitcoin" & crypto$annee>=2013 & crypto$annee<=2017),]

# 1.2. Pour 2016 -> 2017
# 1.2.1 Les Moyennes
moyenneHighBitcoin2016_2017 <- mean(bitcoin2016_2017$high)          #2357.0528
moyenneLowBitcoin2016_2017 <- mean(bitcoin2016_2017$low)            #2183.8568
moyenneRanknowBitcoin2016_2017 <- mean(bitcoin2016_2017$ranknow)    #1
moyenneMarketBitcoin2016_2017 <- mean(bitcoin2016_2017$market)      #37654288445
# 1.2.2 Les variance
varHighBitcoin2016_2017<-var(bitcoin2016_2017$high)                 #12220075
varLowBitcoin2016_2017<-var(bitcoin2016_2017$low)                   #9754478
varRanknowBitcoin2016_2017<-var(bitcoin2016_2017$ranknow)           #0
varMarketBitcoin2016_2017<-var(bitcoin2016_2017$market)             #3.139999e+21

# 1.3. Pour 2018
# 1.3.1 Les Moyennes
moyenneHighBitcoin2018 <- mean(bitcoin2018$high)                    #8165.902
moyenneLowBitcoin2018 <- mean(bitcoin2018$low)                      #7707.94
moyenneRanknowBitcoin2018 <- mean(bitcoin2018$ranknow)              #1
moyenneMarketBitcoin2018 <- mean(bitcoin2018$market)                #135448190285

# 1.3.2 Les variance
varHighBitcoin2018<-var(bitcoin2018$high)                           #5643622
varLowBitcoin2018<-var(bitcoin2018$low)                             #4314394
varRanknowBitcoin2018<-var(bitcoin2018$ranknow)                     #0
varMarketBitoin2018<-var(bitcoin2018$market)                        #1.362884e+21

# ----------------------------------------------------------------------------------------------------------
# Pour la periode 2016/2017 on trouve une variance tres grande pour le cours minimal, maximal et le marche  |
# Cela signifie que le bitcoin a connu des changemenet enorme dans cette periode.                           |
# En 2018, la moyenne a connu une grande augmentation, le marche alors a connu une evolution cette annee.   |
# La variance (maximum, minimum et marche) est plus petite que la periode d'avant, alors cette annee etait  |
# plus stable pour le bitcoin que les annee d'avant. Le Bitcoin a resste sur le sommet du marche pendant    |
# les annee 2016, 2017 et 2018 vu que sa variance egale a 0                                                 |
# On voit egalement que la difference entre la moyenne du maximum et du minmum et plus grande en 2018 que   |
# entre 2016 et 2018, ce que peut dire : Augmentation du volume de transactions.                            |
# La variance du maximum est plus grande que du minimum, alors la valeur minimum du bitcoin n'a pas connu   |
# des grandes chutes, au contraire du valeur maximum qui a connu des grandes hausses surtout entre 2016 et  |
# 2017                                                                                                      | 
# ----------------------------------------------------------------------------------------------------------


# 1.4 On prend la periode 2013 jusqu'a 2017
# 1.2. Pour 2013 -> 2017
# 1.2.1 Les Moyennes
moyenneHighBitcoin2013_2017 <- mean(bitcoin2013_2017$high)          #1222.081
moyenneLowBitcoin2013_2017 <- mean(bitcoin2013_2017$low)            #1135.73
moyenneRanknowBitcoin2013_2017 <- mean(bitcoin2013_2017$ranknow)    #1
moyenneMarketBitcoin2013_2017 <- mean(bitcoin2013_2017$market)      #18836468734
# 1.2.2 Les variance
varHighBitcoin2013_2017<-var(bitcoin2013_2017$high)                 #6213753
varLowBitcoin2013_2017<-var(bitcoin2013_2017$low)                   #5013770
varRanknowBitcoin2013_2017<-var(bitcoin2013_2017$ranknow)           #0
varMarketBitcoin2013_2017<-var(bitcoin2013_2017$market)             #1.610756e+21

# ----------------------------------------------------------------------------------------------------------
# on voit que si on remonte dans le temps, la valeur des moyennes et des variances devient plus petits,    |
# on peut constater que la valeur du bitcoin a Considérablement changée avec le temps. et surtout pendant  |
# l'année 2018.                                                                                            |
# On peut construire des graphes pour confirmer ca :                                                       |
# ----------------------------------------------------------------------------------------------------------

#QUESTION 2

#On fait notre test sur plusieus devises qui ont des classements different
#Ripple - 2
ripple2018 <- crypto[which(crypto$ranknow == 'ripple' & crypto$annee==2018),]
#dogecoin 24
dogecoin2018 <- crypto[which(crypto$slug == 'dogecoin' & crypto$annee==2018),]
#factom - 50
factom2018 <- crypto[which(crypto$slug == 'factom' & crypto$annee==2018),]
#gxchain - 76
gxchain2018 <- crypto[which(crypto$slug == 'gxchain' & crypto$annee==2018),]

# Les variables pour tester la correlation :
corvars <- c("high","low","market","date")

#RIPPLE
# On test la correlation entre le Bitcoin et Ripple
ripple2018 <- ripple2018[corvars]
colnames(ripple2018) <- c("highRipple", "lowRipple", "marketRipple", "dateRipple")
ripple2018 <- merge(bitcoin2018[corvars], ripple2018, by.y = "dateRipple", by.x = "date")
ripple2018$date = NULL

corRipple2018 <- cor(ripple2018)

#DEGOCOIN
# On test la correlation entre le Bitcoin et degocoin
dogecoin2018 <- dogecoin2018[corvars]
colnames(dogecoin2018) <- c("highDogecoin", "lowDogecoin", "marketDogecoin", "dateDogecoin")
dogecoin2018 <- merge(bitcoin2018[corvars], dogecoin2018, by.y = "dateDogecoin", by.x = "date")
dogecoin2018$date = NULL

corDogecoin2018 <- cor(dogecoin2018)


#GXCHAIN
# On test la correlation entre le Bitcoin et Gxchain
gxchain2018 <- gxchain2018[corvars]
colnames(gxchain2018) <- c("highGxchain", "lowGxchain", "marketGxchain", "dateGxchain")
gxchain2018 <- merge(bitcoin2018[corvars], gxchain2018, by.y = "dateGxchain", by.x = "date")
gxchain2018$date = NULL

corGxchain2018 <- cor(gxchain2018)

