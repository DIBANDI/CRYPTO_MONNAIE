#importation des donnees sur R

crypto<-read.csv(file.choose(),sep=";",dec=",",header=TRUE)
crypto
#-----------------------------------------------------------
#crypto<-read.csv("crypto-markets.csv", header=TRUE)
#le sep pour moi est ',' et non pas ';' donc le read csv
#vas lire les colonnes directement. pas besoin de specifier
#le sep (**Comma**-separated values)
#-----------------------------------------------------------

# 1 transformation du format de la date en format anglais si le format est JJ/MM/AAAA

crypto$date_transformee=apply(crypto["date"],1,function(x) 
  if(nchar(unlist(strsplit(as.character(x), "/"))[3])==4)
    {
      as.Date(x,"%d/%m/%Y")
    }
  else
    {
      as.Date(x,"%d/%m/%y")
      }
  )
crypto$transf= as.Date(crypto$date_transformee, origin="1970-01-01")

#extraction de l'ann?e
crypto$annee=format(crypto$transf,  "%Y")

#extraction du mois
crypto$mois=months(crypto$transf)

#extraction du jour
crypto$jour=weekdays(crypto$transf)

# SINON ON EXTRAIT DIRECTEMENT LES JOURS MOIS ET ANNEES SUR LA COLONNE DATE
crypto$transf= as.Date(crypto$date, origin="1970-01-01")

#extraction de l'ann?e
crypto$annee=format(crypto$transf,  "%Y")
crypto$annee

#extraction du mois
crypto$mois=months(crypto$transf)

#extraction du jour
crypto$jour=weekdays(crypto$transf)

#-----------------------------------------------------------------------
#crypto$date = as.Date(crypto$date)
#crypto$annee = format(crypto$date,  "%Y")
#pas besoin de transformer la date ou et d'extraire le mois, et le jour
#-----------------------------------------------------------------------         

##########################################################################
#                                                                        #                                          
#      CALCUL DES ELEMENT:LA VARIANCE ET LA MOYENNE SUR LE BITCOIN POUR  #
#      LES L'INTERVALLES DES ANNEES  2016 ET 2017                        #
#                                                                        #
##########################################################################

#EXTRACTION DES DONNEES DE 2016 et 2017

crypto2016_2017<-crypto[which(crypto$annee>=2016 & crypto$annee<=2017),]
crypto2016_2017
#EXTRACTION DES BITCOIN DE 2016 et 2017                              
bitcoin2016_2017<-crypto2016_2017[which(crypto2016_2017$name=="Bitcoin"),]
bitcoin2016_2017
                              
#--------------------------------------------------------------------------------------------------------------------
#bitcoin2016_2017<-crypto[which(crypto$name=="Bitcoin" & crypto$annee>=2016 & crypto$annee<=2017),]
#On peut passer directement sans creer une autre tableau
#--------------------------------------------------------------------------------------------------------------------                              
                              
#LA MOYENNE DE COURS MINIMAL ET MAXIMALE DE BITCOIN DE 2016 et 2017 CORRESPOND A:

#nombre de ligne pour bitcoin 2016 et 2017
nbrelignebitcoin2016_2017<-length(bitcoin2016_2017$name)
nbrelignebitcoin2016_2017

#cours moyenne maximale
moyennemaxbitcoin2016_2017<-sum(bitcoin2016_2017$high)/sum(nbrelignebitcoin2016_2017)
moyennemaxbitcoin2016_2017
#------------------------------------------------------------------------------------
#moyennemaxbitcoin2016_2017 <- mean(bitcoin2016_2017$high)
#------------------------------------------------------------------------------------ 
                              
#cours moyenne minimal
moyenneminbitcoin2016_2017<-sum(bitcoin2016_2017$low)/sum(nbrelignebitcoin2016_2017)
moyenneminbitcoin2016_2017
#------------------------------------------------------------------------------------
#moyenneminbitcoin2016_2017 <- mean(bitcoin2016_2017$low)
#------------------------------------------------------------------------------------ 
                              
#MOYENNE DES TRANSACTION
moyennevalbitcoin2016_2017<-sum(bitcoin2016_2017$close)/sum(nbrelignebitcoin2016_2017)
moyennevalbitcoin2016_2017
#------------------------------------------------------------------------------------
#moyennevalbitcoin2016_2017 <- mean(bitcoin2016_2017$close)
#------------------------------------------------------------------------------------

#PROPORTION DU BITCOIN PAR RAPPORT AUX AUTRES DEVISES
moyenneprobitcoin2016_2017<-sum(bitcoin2016_2017$close_ratio)/sum(nbrelignebitcoin2016_2017)
moyenneprobitcoin2016_2017
#conclusion en faisant la moyenne sur le rang pour l'année 2016 et 2017, on trouve 1,
#cela revient à dire que par rapport aux autres devises le bitcoin occupe la premiere place en 2016

#CALCULONS LES VARIANCES DE CHAQUE DONNEES DONT LA MOYENNE A ETE CALCULE

#cours moyenne maximale
varmaxbitcoin2016_2017<-var(bitcoin2016_2017$high)
varmaxbitcoin2016_2017

#cours moyenne minimal
varminbitcoin2016_2017<-var(bitcoin2016_2017$low)
varminbitcoin2016_2017

#MOYENNE DES TRANSACTION
varvalbitcoin2016_2017<-var(bitcoin2016_2017$close)
varvalbitcoin2016_2017

#PROPORTION DU BITCOIN PAR RAPPORT AUX AUTRES DEVISES
varprobitcoin2016_2017<-var(bitcoin2016_2017$close_ratio)
varprobitcoin2016_2017





##########################################################################
#                                                                        #                                          
#      CALCUL DES ELEMENT:LA VARIANCE ET LA MOYENNE SUR LE BITCOIN POUR  #
#      LES L'INTERVALLE DES ANNEES  2018                                 #
#                                                                        #
##########################################################################





#EXTRACTION DES DONNEES DE CETTE ANNEE 2018

crypto2018<-crypto[which(crypto$annee==2018),]
crypto2018
#EXTRACTION DEs BITCOIN DE CETTE ANNEE 2018

bitcoin2018<-crypto2018[which(crypto2018$name=="Bitcoin"),]
bitcoin2018
#LA MOYENNE DE COURS MINIMAL ET MAXIMALE DE BITCOIN DE 2018 CORRESPOND A:

#nombre de ligne pour bitcoin 2018
nbrelignebitcoin2018<-length(bitcoin2018$name)
nbrelignebitcoin2018

#cours moyenne maximale
moyennemaxbitcoin2018<-sum(bitcoin2018$high)/sum(nbrelignebitcoin2018)
moyennemaxbitcoin2018

#cours moyenne minimal
moyenneminbitcoin2018<-sum(bitcoin2018$low)/sum(nbrelignebitcoin2018)
moyenneminbitcoin2018

#MOYENNE DES TRANSACTION
moyennevalbitcoin2018<-sum(bitcoin2018$close)/sum(nbrelignebitcoin2018)
moyennevalbitcoin2018

#PROPORTION DU BITCOIN PAR RAPPORT AUX AUTRES DEVISES
moyenneprobitcoin2018<-sum(bitcoin2018$close_ratio)/sum(nbrelignebitcoin2018)
moyenneprobitcoin2018


#CALCULONS LES VARIANCES DE CHAQUE DONNEES DONT LA MOYENNE A ETE CALCULE

#cours moyenne maximale
varmaxbitcoin2018<-var(bitcoin2018$high)
varmaxbitcoin2018

#cours moyenne minimal
varminbitcoin2018<-var(bitcoin2018$low)
varminbitcoin2018

#MOYENNE DES TRANSACTION
varvalbitcoin2018<-var(bitcoin2018$close)
varvalbitcoin2018

#PROPORTION DU BITCOIN PAR RAPPORT AUX AUTRES DEVISES
varprobitcoin2018<-var(bitcoin2018$close_ratio)
varprobitcoin2018




##########################################################################
#                                                                        #                                          
#      CALCUL DES ELEMENT:LA VARIANCE ET LA MOYENNE SUR LE BITCOIN POUR  #
#      LES L'INTERVALLES DES ANNEES  2016 ET 2018                        #
#                                                                        #
##########################################################################

#EXTRACTION DES DONNEES DE 2016 et 2018

crypto2016_2018<-crypto[which(crypto$annee>=2016 & crypto$annee<=2018),]
crypto2016_2018
#EXTRACTION DEs BITCOIN DE 2016 et 2018

bitcoin2016_2018<-crypto2016_2018[which(crypto2016_2018$name=="Bitcoin"),]
bitcoin2016_2018
#LA MOYENNE DE COURS MINIMAL ET MAXIMALE DE BITCOIN DE 2016 et 2018 CORRESPOND A:

#nombre de ligne pour bitcoin 2016 et 2017
nbrelignebitcoin2016_2018<-length(bitcoin2016_2018$name)
nbrelignebitcoin2016_2018

#cours moyenne maximale
moyennemaxbitcoin2016_2018<-sum(bitcoin2016_2018$high)/sum(nbrelignebitcoin2016_2018)
moyennemaxbitcoin2016_2018

#cours moyenne minimal
moyenneminbitcoin2016_2018<-sum(bitcoin2016_2018$low)/sum(nbrelignebitcoin2016_2018)
moyenneminbitcoin2016_2018

#MOYENNE DES TRANSACTION
moyennevalbitcoin2016_2018<-sum(bitcoin2016_2018$close)/sum(nbrelignebitcoin2016_2018)
moyennevalbitcoin2016_2018

#PROPORTION DU BITCOIN PAR RAPPORT AUX AUTRES DEVISES
moyenneprobitcoin2016_2018<-sum(bitcoin2016_2018$close_ratio)/sum(nbrelignebitcoin2016_2018)
moyenneprobitcoin2016_2018

#CALCULONS LES VARIANCES DE CHAQUE DONNEES DONT LA MOYENNE A ETE CALCULE

#cours moyenne maximale
varmaxbitcoin2016_2018<-var(bitcoin2016_2018$high)
varmaxbitcoin2016_2018

#cours moyenne minimal
varminbitcoin2016_2018<-var(bitcoin2016_2018$low)
varminbitcoin2016_2018

#MOYENNE DES TRANSACTION
varvalbitcoin2016_2018<-var(bitcoin2016_2018$close)
varvalbitcoin2016_2018

#PROPORTION DU BITCOIN PAR RAPPORT AUX AUTRES DEVISES
varprobitcoin2016_2018<-var(bitcoin2016_2018$close_ratio)
varprobitcoin2016_2018


#REPONSE AUX QUESTION 

#POUR LES CALCULS DES MOYENNES ET VARIANCE POUR CHAQUE VARIABLE POUR 
#LA PERIODE 2016-2017 NOUS AVAONS :

#MOYENNE COURS MINIMAL DU BITCOIN (myenneminbitcoin2016_2017) = 2183.857
#SA VARIANCE(varminbitcoin2016_2017) EST =9754478

#MOYENNE COURS MAXIMAL DU BITCOIN (myennemaxbitcoin2016_2017) = 2357.053
#SA VARIANCE(varmaxbitcoin2016_2017) EST = 12220075

#MOYENNE valeur des transaction (myennevalbitcoin2016_2017) = 2284.912
#SA VARIANCE(varvalbitcoin2016_2017) EST = 11159589

#MOYENNE proportions des transaction (myenneprobitcoin2016_2017) = 0.5846922
#SA VARIANCE(varprobitcoin2016_2017) EST = 0.08673171


#################################################################################^

#POUR LES CALCULS DES MOYENNES ET VARIANCE POUR CHAQUE VARIABLE POUR 
#LA PERIODE 2016-2018 NOUS AVAONS :

#MOYENNE COURS MINIMAL DU BITCOIN (myenneminbitcoin2016_2018) = 3912.729
#SA VARIANCE(varminbitcoin2016_2018) EST = 14613843

#MOYENNE COURS MAXIMAL DU BITCOIN (myennemaxbitcoin2016_2018) = 4175.048
#SA VARIANCE(varmaxbitcoin2016_2018) EST = 17416777

#MOYENNE valeur des transaction (myennevalbitcoin2016_2018) = 4055.357
#SA VARIANCE(varvalbitcoin2016_2018) EST = 16125286

#MOYENNE proportions des transaction (myenneprobitcoin2016_2018) = 0.5636118
#SA VARIANCE(varprobitcoin2016_2018) EST = 0.09019315

#################################################################################^

#POUR LES CALCULS DES MOYENNES ET VARIANCE POUR CHAQUE VARIABLE POUR 
#L'ANNEE 2018 NOUS AVAONS :

#MOYENNE COURS MINIMAL DU BITCOIN (myenneminbitcoin2018) = 7707.94
#SA VARIANCE(varminbitcoin2018) EST = 4314394

#MOYENNE COURS MAXIMAL DU BITCOIN (myennemaxbitcoin2018) = 8165.902
#SA VARIANCE(varmaxbitcoin2018) EST = 5643622

#MOYENNE valeur des transaction (myennevalbitcoin2018) = 7941.831
#SA VARIANCE(varvalbitcoin2018) EST = 5040718

#MOYENNE proportions des transaction (myenneprobitcoin2018) = 0.5173363
#SA VARIANCE(varprobitcoin2018) EST = 0.09494948

#PAR RAPPORT A LA PERIODE 2016_2018 LES DONNEES CALCULEES POUR L'ANNEE 
# 2018 ONT CONSIDERABLEMENT CHANGE, ON CONSTATE QUE LES MOYENNES et LES VARIANCES
#DE CHAQUE VARIABLE POUR L'ANNEE 2018 SONT PLUS GRANDE QUE CELLE DE 
# LA PERIODE 2016-2018.SAUF LA VARIABLE PROPORTION DES TRANSACTION DONT 
#LA MOYENNE ET LA VARIANCE SONT PLUS GRANDE EN 2016-2018 PAR RAPPORT A 2018

#NOOUS ALLONS ELARGIR LA FENETRE DE TEMPS DE 2013-2018 ENSUITE CALCULTER 
#LES MOYENNES DES VARIABLES ET CALCULLER LES MOYENNES DE CHAQUE VARIABLE ET TIRER
#UNE CONCLUSION.


##########################################################################
#                                                                        #                                          
#      CALCUL DES ELEMENT:LA VARIANCE ET LA MOYENNE SUR LE BITCOIN POUR  #
#      LES L'INTERVALLES DES ANNEES  2013 ET 2018                        #
#                                                                        #
##########################################################################

#EXTRACTION DES DONNEES DE 2013 et 2018

crypto2013_2018<-crypto[which(crypto$annee>=2013 & crypto$annee<=2018),]
crypto2013_2018
#EXTRACTION DEs BITCOIN DE 2013 et 2018

bitcoin2013_2018<-crypto2013_2018[which(crypto2013_2018$name=="Bitcoin"),]
bitcoin2013_2018
#LA MOYENNE DE COURS MINIMAL ET MAXIMALE DE BITCOIN DE 2013 et 2018 CORRESPOND A:

#nombre de ligne pour bitcoin 2013 et 2018
nbrelignebitcoin2013_2018<-length(bitcoin2013_2018$name)
nbrelignebitcoin2013_2018

#cours moyenne maximale
moyennemaxbitcoin2013_2018<-sum(bitcoin2013_2018$high)/sum(nbrelignebitcoin2013_2018)
moyennemaxbitcoin2013_2018
#moyennemaxbitcoin2013_2018= 2354.448

#cours moyenne minimal
moyenneminbitcoin2013_2018<-sum(bitcoin2013_2018$low)/sum(nbrelignebitcoin2013_2018)
moyenneminbitcoin2013_2018
#moyenneminbitcoin2013_2018= 2207.496

#MOYENNE DES TRANSACTION
moyennevalbitcoin2013_2018<-sum(bitcoin2013_2018$close)/sum(nbrelignebitcoin2013_2018)
moyennevalbitcoin2013_2018
#moyennevalbitcoin2013_2018=2207.496

#PROPORTION DU BITCOIN PAR RAPPORT AUX AUTRES DEVISES
moyenneprobitcoin2013_2018<-sum(bitcoin2013_2018$close_ratio)/sum(nbrelignebitcoin2013_2018)
moyenneprobitcoin2013_2018
#moyenneprobitcoin2013_2018=0.5410969

#CONCLUSION : APRES AVOIR CALCULE LES MOYENNE POUR LA PERIODE ELARGIE 2013-2018, ON CONSTATE
#QUE LES MOYENNES DE TOUTES LES VARIABLES ON DIMINUE,DONC PLUS ON ELARGIE LA PERIODE PLUS LA MOYENNE 
#DIMINUE

#TEST DE CORRELATION ENTRE LE BITCOIN ET LES AUTRES DEVISES
#EXTRACTION DEs AUTRES DEVISES  ANNEE 2018

autres2018<-crypto2018[which(crypto2018$name !="Bitcoin"),]
autres2018
table(bitcoin2018,autres2018)
#je cree deux vercteur A et B, A pour autres2018 et B pour bitcoin2018
# et je choisi une colonne: le maximum , high

#cours moyenne maximale
moyennemaxautres2018<-mean(autres2018$high)
moyennemaxautres2018

#cours moyenne minimal
moyenneminautres2018<-mean(autres2018$low)
moyenneminautres2018

#MOYENNE DES TRANSACTION
moyennevalautres2018<-mean(autres2018$close)
moyennevalautres2018

#le vecteur a 
a<-c(moyennemaxautres2018,moyennevalautres2018,moyenneminautres2018)
#le vecteur b
b<-c(moyennemaxbitcoin2018,moyennevalbitcoin2018,moyenneminbitcoin2018)
#il s'agit de verifier ici si il ya correlation entre a et b 
#on sait qu'il ya correlation forte, lorsque r=[-1,-0.5[U]+0.5, 1]
#et il ya correlation faible si r=[-0.5,+0.5]
r=cor(a,b)

#notre =0.9958561 qui appartient a ]+0.5, 1], donc il ya forte correlation
#entre le bitcoin et les autres devises 
#avec la methode de pearson on aura
cor.test(a,b, method = "pearson")

#le T de student t=10.95, le nombre de degre de liberte df=1 le risque de rejet sur
#l'hypothese(le p-value=0.05798) il est plus grand que 5% alors on rejete l'hypothese de 
#l'existance d'une correlation nulle
#~
plot(a,b, main = "Dispersion des points\ bitcoin et les autres devises",xlab="Autres devises",ylab = "Bitcoin")

droitederegression<-lm(b~a)
#je trace la droite avec la fonction abline
abline(droitederegression, col=2, lwd=3)

#derniere question determination de la distribution, nous avons un seul echantillon alors 
#nous allons terster la lois de student pour un echantillon. et conclure
#b=bitcoin alors on travaille avec le vecteur des moyenne
#b<-c(moyennemaxbitcoin2018,moyennevalbitcoin2018,moyenneminbitcoin2018),notre mu=100 et notre
#intervalle de confiance est a 95%, on fait un test bilateral

x<-t.test(b,mu=100,conf.level = 0.95,alternative = "two.sided")

#la p-value trouvee est plus petit que 0.001 alors il ya difference significative entre 
#les moyennes avec une probabilite de 99 pour mille
