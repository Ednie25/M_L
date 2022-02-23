
rm(list=ls())

library(data.table)
library(readr)
library(tidyverse) 
library(dplyr)
library(Hmisc)
library(skimr)
library(caret)
library(MASS)
library(car)
library(ROCR)


# Base portant sur le credit scoring :
# https://www.kaggle.com/kashnitsky/mlcourse?select=credit_scoring_sample.csv


dt<-data.table(read_csv("./credit_scoring.csv"))
attach(dt)
# 45 063 observations and 8 variables


## /////////////  TRAITEMENT DES DONNEES

# Renommons diff�rentes variables

data <- 
dt %>%
rename("Client_Qualite" = "SeriousDlqin2yrs",
         "retard_30_59_jours" = "NumberOfTime30-59DaysPastDueNotWorse", 
         "retard_60_89_jours"="NumberOfTime60-89DaysPastDueNotWorse",
         "Nbre_de_personnes_a_charge"="NumberOfDependents",
         "Revenu_mensuel" = "MonthlyIncome",
         "Ratio_dendettement" = "DebtRatio",
         "retard_90_plus_jours" = "NumberOfTimes90DaysLate")
data
# 45 063 observations and 8 variables


#Nous pouvons constater grace � la fonction describe la pr�sence de valeurs extremes et mannquantes dans certaines variables

data_1 <- na.omit(data) # supprimer les valeurs manquantes
# 36 420 observations and 8 variables


# Supprimons les valeurs extr�mes pour les retards concern�s

# retard_30_59_jours

data1 <- subset(data_1, data_1$retard_30_59_jours != 96 & data_1$retard_30_59_jours != 98)
summary(data1)  
# 36 322 observations et 8  variables
                  
# retard_60_89_jours

data2 <- subset(data1, data1$retard_60_89_jours != 96 & data1$retard_60_89_jours != 98)
summary(data2) 
# 36 322 observations et 8  variables

# D�terminons la corr�lation des variables explicatives 

cor(data2, method = c("pearson", "kendall", "spearman")) # apr�s suppression des valeurs manquantes et outliers
cor(data2)


#//////////////////////////////////////////////////////////////////////////////////#
#                        I -   REGRESSION LOGISTIQUE                              #
#/////////////////////////////////////////////////////////////////////////////////#


#######                     Scission de l'�chantillon                     ########

# Division des donn�es en �chantillon training et test

set.seed(123)
training.samples <- 
  data2$Client_Qualite %>%
  createDataPartition(p = 0.7, list = FALSE)

train <- data2[training.samples, ] # 25 426 observations and 8 variables
test <- data2[-training.samples, ] # 10 896 observations and 8 variables


library(leaps)
library(MASS)

# Mod�le de r�gression logistique 

m <- glm(Client_Qualite ~.,data=train,family=binomial())
summary(m) 


# V�rifions si le mod�le que nous avons est le meilleur mod�le selon le crit�re d'AIC 

result_aic <- stepAIC(m,scope=list(lower=~1,upper=~.), data=train,direction ="both") 
summary(result_aic)

# Nous remarquons que nous avons le m�me mod�le que le pr�c�dent, donc le mod�le pr�c�dent est bien le bon.
# C'est donc le mod�le qui a la valeur d'AIC la plus faible. 


## odds ratios ou rapport des cotes
exp(coef(m)) 

# Interpr�tation 
# pour un Y = 1 pour mauvais et 0 bon, un OR=1,90 pour la variable explicative retard_30_59_jours,
# signifiera que le rapport des mauvais aux bons est 1,90 fois plus important apr�s une variation d'une unit�
# de la variable explicative.


## V�rifions l'hypoth�se de multicolin�arit�                 

# Calculons les facteurs d'inflation de la variance (FIV) pour chaque variable pr�dictive du mod�le
car::vif(m)

# En calculant les FIV, nous v�rifions si la multicolin�art� est un probl�me ou pas. Si les FIV sont sup�rieur � 5,
# il y a en effet de la multicolin�arit� chez nos variables explicatives. Mais dans le cas de nos variables, nous
# remarquons qu'en g�n�ral ils ne d�passent pas 1.



####     Qualit� d'ajustement du mod�le             ###########################


# Tests statistiques sur les r�gresseurs


# Wald test 

library(survey)
regTermTest(m, "retard_30_59_jours")
regTermTest(m, "age")
regTermTest(m, "retard_60_89_jours")
regTermTest(m, "Nbre_de_personnes_a_charge")
regTermTest(m, "Revenu_mensuel")
regTermTest(m, "Ratio_dendettement")
regTermTest(m, "Ratio_dendettement")
regTermTest(m, "retard_90_plus_jours")



# Variable importance 

varImp(m)


################ VALIDATION DES VALEURS PREDITES #################################################

#####################  D�termination de la matrice de confusion ########################################

library(caret)
library(InformationValue)
library(ISLR)

# Utiliser un mod�le pour pr�dire la probabilit� de d�faut
predicted <- predict(m, test, type="response")
predicted

# Trouver la probabilit� de coupure optimale � utiliser pour maximiser la pr�cision
optimal <- optimalCutoff(test$Client_Qualite, predicted)[1]
optimal
#0.3799999, c'est le niveau le plus bas pouvant �tre accept� comme score

# Cr�er la matrice de confusion
confusionMatrix(test$Client_Qualite, predicted)



################################# ROC ################################################

# Courbe de roc

library(pROC)
ROC <- roc(Client_Qualite ~ predicted, data =test)
plot(ROC) 
ROC
# Interpr�tation : la courbe est tr�s �loign�e de la distribution.


# Calculer l'aire en dessous de la courbe de roc

test_roc = roc(test$Client_Qualite ~ predicted, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc) # le AUC (area under the curve) est de 0.8027623, donc la pr�cision de la pr�vision est de 82%



########   M�triques d'�valuation en rapport avec la matrice de confusion

#Nous pouvons �galement calculer les mesures suivantes � l'aide de la matrice de confusion:
  
#Sensitivity : Le � taux de vrais positifs � - le pourcentage d'individus que le mod�le a correctement pr�dit ferait d�faut.
#Specificity : Le � taux de vrais n�gatifs � - le pourcentage d'individus correctement pr�dits par le mod�le ne ferait pas d�faut.
# le taux total d'erreur de classification  : pourcentage du total des classifications incorrectes effectu�es par le mod�le.

# # De toutes les classes, combien nous avons pr�dit correctement, qui doit �tre aussi pr�cis que possible. 

#Calculons la sensibilit�

sensitivity(test$Client_Qualite, predicted)
#  0.3453799
# la proportion des vrais positifs correctement identifi�s.

#Calculons la sp�cificit�
specificity(test$Client_Qualite, predicted)
# 0.9680889
# la proportion des vrais n�gatifs correctement identifi�s.

#calculer le taux total d'erreur de classification
misClassError(test$Client_Qualite, predicted, threshold=optimal)
# 0.1683

# Le taux total d'erreur de classification est de 16,83% pour ce mod�le, c'est le taux d'erreur du modele.
# En g�n�ral, plus ce taux est bas, mieux le mod�le est capable de pr�dire les r�sultats,
# L'accurracy est �gal � 83,17 puisqu'il est obtenu en faisant 1-0.1683.


# Donc ce mod�le s'av�re �tre tr�s bon pour pr�dire si un individu fera d�faut ou non. 



# Analyse de la performance du mod�le suivant diff�rentes mesures 

# calcul d'une courbe ROC simple (x-axis: fpr, y-axis: tpr)
library(ROCR)

test$score <- predict(m, type = 'response', test)
pred <- prediction(test$score, test$Client_Qualite)
perf <- performance(pred, "tpr", "fpr") # "tpr", True positive rate; "fpr", False positive rate.
plot(perf)
# performance, function to create performance objects



# Courbe precision/recall (x-axis: recall, y-axis: precision)
perf1 <- performance(pred, "prec", "rec") #"prec", precision, Same as Positive predictive value.;"rec", Recall. Same as tpr.
perf1
plot(perf1)


# Courbe sensitivity/specificity  (x-axis: specificity, y-axis: sensitivity)
perf2 <- performance(pred, "sens", "spec") # "sens", Sensitivity. Same as tpr; "spec", Specificity. Same as tnr, true negative rate
perf2
plot(perf2)



#####                     II -   ARBRES DE DECISION            ######



#Step 1 : Cr�er deux �chantillons train/ensemble 
#Step 2 : Construire le mod�le
#Step 3 : Faire une pr�diction
#Step 4 : Mesurer la performance
#Step 5 : R�gler les hyper-param�tres


#Step 1 : Cr�er deux �chantillons train/ensemble 

set.seed(678)

create_train_test <- function(data, size = 0.7, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}


data_train <- create_train_test(data2, 0.7, train = TRUE)
data_test <- create_train_test(data2, 0.7, train = FALSE)
dim(data_train) # 25 425 observations et 8 variables 
dim(data_test) # 10897 observations et 8 variables 


# Step 2 : Construire le mod�le

library(rpart)
library(rpart.plot)

fit <- rpart(Client_Qualite ~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)


# Step 3 : Faire une pr�diction

predict_unseen <-predict(fit, data_test, type = 'class')


# Matrice de confusion
table_mat <- table(data_test$Client_Qualite, predict_unseen)
table_mat

#   predict_unseen
#       0    1
# 0  7770  388
# 1 1631 1108


#Step 4 : Mesurer la performance

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

# "Accuracy for test 0.814719647609434"


#Step 5 : R�gler les hyper-param�tres

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$Client_Qualite, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}



control2 <- rpart.control(minsplit = 200,
                         minbucket = round(1000/ 300),
                         maxdepth = 3,
                         cp = 0)
tune_fit2 <- rpart(Client_Qualite~., data = data_train, method = 'class', control = control2)
accuracy_tune(tune_fit2)















