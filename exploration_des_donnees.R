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

datastat <- data

histo <- hist(datastat$Client_Qualite,breaks=15,col="#F5D0A9",xlab="Client Qualit�",ylab="Fr�quences",
              main="Distribution de la variable Client qualit�",tck=0.01, freq=TRUE)


#Tableau des fr�aquences de la variable d'interet: 

tbl <- table(datastat$Client_Qualite)
tbl

tbl1 <- prop.table(table(datastat$Client_Qualite))
tbl1


#Distribution de l'age : 

histo <- hist(datastat$age,col="#F5D0A9",xlab="Client Qualit�",ylab="Fr�quences",
              main="Distribution de la variable Client qualit�",tck=0.01, freq=TRUE)

datastat <- setDT(datastat)[age<40, agegroup := "21-40"]
datastat[age>40 & age <65, agegroup := "40-65"]
datastat[age>65, agegroup := "age>65"]

#
tableau<-table(datastat$agegroup,datastat$Client_Qualite)
tableau

##
boxplot(datastat$age ~ datastat$Client_Qualite,
        col = "purple", border = "black",
        main = "Situation en fonction de l'�ge",
        xlab =  "Clien_Qualit�",
        ylab = "�ges[ann�es]")



datastat <- setDT(data)[Revenu_mensuel<7000, revenug := "<7000"]
datastat[Revenu_mensuel>7000 & age <50000, revenug := "7k-50k"]
datastat[Revenu_mensuel>50000, revenug := "rm>50k"]

tableau<-table(datastat$revenug,datastat$Client_Qualite)
tableau



#### Statistiques descriptives

skim_to_list(data) # liste des variables et des stat des
summary(data) # la fonction summary() permet de visualiser des statistiques de base pour chaque variable quantitative : Minimum, maximum, moyenne, m�diane, 1er quartile, 3�me quartile. 
describe(data) # la fonction describe() du package Hmisc permet d'obtenir pour chaque variable quantitative : le nombre d'observations, le nombre de valeurs manquantes, la moyenne, les quantiles � 5, 10,25,50,75,90, et 95%, les 5 plus basses valeurs, et 5 plus hautes



# Pr�sence de valeurs extremes et manquantes dans certaines variables

data_1 <- na.omit(data) # supprimer les valeurs manquantes
# 36 420 observations and 8 variables


# Suppression des valeurs extr�mes pour les retards concern�s

# retard_30_59_jours

data1 <- subset(data_1, data_1$retard_30_59_jours != 96 & data_1$retard_30_59_jours != 98)
summary(data1)  
# 36 322 observations et 8  variables

# retard_60_89_jours

data2 <- subset(data1, data1$retard_60_89_jours != 96 & data1$retard_60_89_jours != 98)
summary(data2) 
# 36 322 observations et 8  variables
 





