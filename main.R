##        LIBRARIES          ##
## ------------------------- ##

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("patchwork")
install.packages("hrbrthemes")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

## ------------------------- ##

## ----- DONNEES CSV ------ ##
vacsi_tot_reg_2021_03_01_20h15 <- read_csv("L3 - Semestre 6/Statistiques/Projet/Donnees/01-03-21/vacsi-tot-reg-2021-03-01-20h15.csv")
vacsi_tot_reg_2021_03_08_17h25 <- read_delim("L3 - Semestre 6/Statistiques/Projet/Donnees/08-03-21/vacsi-tot-reg-2021-03-08-17h25.csv", 
                                             ";", escape_double = FALSE, trim_ws = TRUE)
vacsi_tot_reg_2021_03_15_17h25 <- read_delim("L3 - Semestre 6/Statistiques/Projet/Donnees/15-03-21/vacsi-tot-reg-2021-03-15-17h25.csv", 
                                             ";", escape_double = FALSE, trim_ws = TRUE)
vacsi_tot_reg_2021_03_18_17h25 <- read_delim("L3 - Semestre 6/Statistiques/Projet/Donnees/18-03-21/vacsi-tot-reg-2021-03-18-17h25.csv", 
                                             ";", escape_double = FALSE, trim_ws = TRUE)
vacsi_tot_reg_2021_03_22_17h19 <- read_delim("L3 - Semestre 6/Statistiques/Projet/Donnees/22-03-21/vacsi-tot-reg-2021-03-22-17h19.csv", 
                                             ";", escape_double = FALSE, trim_ws = TRUE)
## ------------------------ ##


# Population total aux regions Ultramarines et Metropolitaines #
#============================================================#
pop_ultra <- sum(head(vacsi_tot_reg_2021_03_22_17h19$pop, 4))
pop_metro <- sum(tail(vacsi_tot_reg_2021_03_22_17h19$pop, 15))
#============================================================#


# Nombre de vaccinees aux regions Ultramarines et Metropolitaines #
#==============================================================================#
nb_vac_ultra_01_03 <- sum(head( vacsi_tot_reg_2021_03_01_20h15$n_tot_dose1, 4))
nb_vac_metro_01_03 <- sum(tail( vacsi_tot_reg_2021_03_01_20h15$n_tot_dose1, 15))

nb_vac_ultra_08_03 <- sum(head( vacsi_tot_reg_2021_03_08_17h25$n_tot_dose1, 4))
nb_vac_metro_08_03 <- sum(tail( vacsi_tot_reg_2021_03_08_17h25$n_tot_dose1, 15))

nb_vac_ultra_15_03 <- sum(head( vacsi_tot_reg_2021_03_15_17h25$n_tot_dose1, 4))
nb_vac_metro_15_03 <- sum(tail( vacsi_tot_reg_2021_03_15_17h25$n_tot_dose1, 15))

nb_vac_ultra_18_03 <- sum(head( vacsi_tot_reg_2021_03_18_17h25$n_tot_dose1, 4))
nb_vac_metro_18_03 <- sum(tail( vacsi_tot_reg_2021_03_18_17h25$n_tot_dose1, 15))

nb_vac_ultra_22_03 <- sum(head( vacsi_tot_reg_2021_03_22_17h19$n_tot_dose1, 4))
nb_vac_metro_22_03 <- sum(tail( vacsi_tot_reg_2021_03_22_17h19$n_tot_dose1, 15))
#==============================================================================#


# Pourcentage vaccinée aux Deux Regions #
#========================================================#
p_vac_ultra_01_03 <- nb_vac_ultra_01_03/pop_ultra * 100
p_vac_metro_01_03 <- nb_vac_metro_01_03/pop_metro * 100

p_vac_ultra_08_03 <- nb_vac_ultra_08_03/pop_ultra * 100
p_vac_metro_08_03 <- nb_vac_metro_08_03/pop_metro * 100

p_vac_ultra_15_03 <- nb_vac_ultra_15_03/pop_ultra * 100
p_vac_metro_15_03 <- nb_vac_metro_15_03/pop_metro * 100

p_vac_ultra_18_03 <- nb_vac_ultra_18_03/pop_ultra * 100
p_vac_metro_18_03 <- nb_vac_metro_18_03/pop_metro * 100

p_vac_ultra_22_03 <- nb_vac_ultra_22_03/pop_ultra * 100
p_vac_metro_22_03 <- nb_vac_metro_22_03/pop_metro * 100
#========================================================#


# Changement des vaccinees par semaine (ex: +25% de vaccinees entre le Mar 08 et Mar 15) #
#===========================================================================================#
difference_p <- function (nb_vac_date1, nb_vac_date2) {
  dif = nb_vac_date2 - nb_vac_date1
  dif = dif/nb_vac_date1 * 100
  return(dif)
}

tab_dif_vac <- data.frame("Date" =
                            c(as.Date("08-03-2021", format="%d-%m-%Y"),
                              as.Date("08-03-2021", format="%d-%m-%Y"),
                              
                              as.Date("15-03-2021", format="%d-%m-%Y"),
                              as.Date("15-03-2021", format="%d-%m-%Y"),
                              
                              as.Date("22-03-2021", format="%d-%m-%Y"),
                              as.Date("22-03-2021", format="%d-%m-%Y")),
                          "Regions" =
                            c("Ultramarine",
                              "Metropolitaine",
                              
                              "Ultramarine",
                              "Metropolitaine",
                              
                              "Ultramarine",
                              "Metropolitaine"),
                          "dif_nb_vac" =
                            c(nb_vac_ultra_08_03 - nb_vac_ultra_01_03,
                              nb_vac_metro_08_03 - nb_vac_metro_01_03,
                              
                              nb_vac_ultra_15_03 - nb_vac_ultra_08_03,
                              nb_vac_metro_15_03 - nb_vac_metro_08_03,
                              
                              nb_vac_ultra_22_03 - nb_vac_ultra_15_03,
                              nb_vac_metro_22_03 - nb_vac_metro_15_03),
                          "dif_p_vac" =
                            c(difference_p(nb_vac_ultra_01_03, nb_vac_ultra_08_03),
                              difference_p(nb_vac_metro_01_03, nb_vac_metro_08_03),
                              
                              difference_p(nb_vac_ultra_08_03, nb_vac_ultra_15_03),
                              difference_p(nb_vac_metro_08_03, nb_vac_metro_15_03),
                              
                              difference_p(nb_vac_ultra_15_03, nb_vac_ultra_22_03),
                              difference_p(nb_vac_metro_15_03, nb_vac_metro_22_03)))
#===========================================================================================#


# Tableau de Vaccinees aux regions Metropolitaines et Ultramarines #
#===============================================================================#
nb_vaccine_ultra_metro <- data.frame("region" = c("Ultramarine", "Metropolitaine"),
                                     
                                     "couv_tot_dose1" = c(p_vac_ultra_22_03, p_vac_metro_22_03))
#===============================================================================#


# Tableau des vaccinees aux regions Metropolitaines et Ultramarines aux dates
#===================================================================================#
dates_nb_vac_ultra_metro <- data.frame("Date" = 
                                         c(as.Date("01-03-2021", format="%d-%m-%Y"),
                                           as.Date("01-03-2021", format="%d-%m-%Y"),
                                           
                                           as.Date("08-03-2021", format="%d-%m-%Y"),
                                           as.Date("08-03-2021", format="%d-%m-%Y"),
                                           
                                           as.Date("15-03-2021", format="%d-%m-%Y"),
                                           as.Date("15-03-2021", format="%d-%m-%Y"),
                                           
                                           as.Date("18-03-2021", format="%d-%m-%Y"),
                                           as.Date("18-03-2021", format="%d-%m-%Y"),
                                           
                                           as.Date("22-03-2021", format="%d-%m-%Y"),
                                           as.Date("22-03-2021", format="%d-%m-%Y")),
                                       "Regions" =
                                         c("Ultramarine",
                                           "Metropolitaine",
                                           
                                           "Ultramarine",
                                           "Metropolitaine",
                                           
                                           "Ultramarine",
                                           "Metropolitaine",
                                           
                                           "Ultramarine",
                                           "Metropolitaine",
                                           
                                           "Ultramarine",
                                           "Metropolitaine"),
                                       "nb_vac" = 
                                         c(nb_vac_ultra_01_03,
                                           nb_vac_metro_01_03,
                                           
                                           nb_vac_ultra_08_03,
                                           nb_vac_metro_08_03,
                                           
                                           nb_vac_ultra_15_03,
                                           nb_vac_metro_15_03,
                                           
                                           nb_vac_ultra_18_03,
                                           nb_vac_metro_18_03,
                                           
                                           nb_vac_ultra_22_03,
                                           nb_vac_metro_22_03),
                                       "p_vac" = 
                                         c(p_vac_ultra_01_03,
                                           p_vac_metro_01_03,
                                           
                                           p_vac_ultra_08_03,
                                           p_vac_metro_08_03,
                                           
                                           p_vac_ultra_15_03,
                                           p_vac_metro_15_03,
                                           
                                           p_vac_ultra_18_03,
                                           p_vac_metro_18_03,
                                           
                                           p_vac_ultra_22_03,
                                           p_vac_metro_22_03))
#===================================================================================#


# Graphe du pourcentage des personnes vaccinées aux regions Metropolitaines et Ultramarines
#======================================================================#
ggplot(aes(x=region, y=couv_tot_dose1), data = nb_vaccine_ultra_metro) +
  geom_bar(stat = "identity") +
  labs(x="Region", y="% de Vaccinées") +
  ylim(0,10)
#======================================================================#


# Graphes de la vaccination de tous les regions #
#======================================================================================================================#
# Graphe du pourcentage des personnes vaccinees dans tous les regions (01-03-21)
ggplot(aes(x=as.factor(reg), y=couv_tot_dose1), data = vacsi_tot_reg_2021_03_01_20h15) +
  geom_bar(stat = "identity", fill=c("red2","red2","red2","red2","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4")) +
  labs(x="Region", y="% de Vaccinees", title = "Pourcentage des Personnes Vaccinees dans toutes les Regions (01-03-21)") +
  ylim(0,13)

# Graphe du pourcentage des personnes vaccinees dans tous les regions (08-03-21)
ggplot(aes(x=reg, y=couv_tot_dose1), data = vacsi_tot_reg_2021_03_08_17h25) +
  geom_bar(stat = "identity", fill=c("red2","red2","red2","red2","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4")) +
  labs(x="Region", y="% de Vaccinees", title = "Pourcentage des Personnes Vaccinees dans toutes les Regions (08-03-21)") +
  ylim(0,13)

# Graphe du pourcentage des personnes vaccinees dans tous les regions (15-03-21)
ggplot(aes(x=reg, y=couv_tot_dose1), data = vacsi_tot_reg_2021_03_15_17h25) +
  geom_bar(stat = "identity", fill=c("red2","red2","red2","red2","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4")) +
  labs(x="Region", y="% de Vaccinees", title = "Pourcentage des Personnes Vaccinees dans toutes les Regions (15-03-21)") +
  ylim(0,13)

# Graphe du pourcentage des personnes vaccinees dans tous les regions (18-03-21)
ggplot(aes(x=reg, y=couv_tot_dose1), data = vacsi_tot_reg_2021_03_18_17h25) +
  geom_bar(stat = "identity",fill=c("red2","red2","red2","red2","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4")) +
  labs(x="Region", y="% de Vaccinees", title = "Pourcentage des Personnes Vaccinees dans toutes les Regions (18-03-21)") +
  ylim(0,13)

# Graphe du pourcentage des personnes vaccinees dans tous les regions (22-03-21)
ggplot(aes(x=reg, y=couv_tot_dose1), data = vacsi_tot_reg_2021_03_22_17h19) +
  geom_bar(stat = "identity",fill=c("red2","red2","red2","red2","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4","blue4")) +
  labs(x="Region", y="% de Vaccinees", title = "Pourcentage des Personnes Vaccinees dans toutes les Regions (22-03-21)") +
  ylim(0,13)
#======================================================================================================================#


# Graphes de l'avancement des vaccins aux deux Regions #
#===============================================================================================================#
plot_nb_ultra = (ggplot(subset(dates_nb_vac_ultra_metro, Regions == "Ultramarine"), aes(x = Date, y = nb_vac)) +
  geom_point() +
  ylim(0, 60000) +
  labs(x="Date", y="Nb de vaccinées", title = "Nombre de vaccinees aux Regions Ultramarines"))

plot_nb_metro = (ggplot(subset(dates_nb_vac_ultra_metro, Regions == "Metropolitaine"), aes(x = Date, y = nb_vac)) +
  geom_point() +
  ylim(0, 6500000) +
  labs(x="Date", y="Nb de vaccinées", title = "Nombre de vaccinees aux Regions Metropolitaines"))

plot_nb_ultra_metro = (ggplot(dates_nb_vac_ultra_metro, aes(x = Date, y = nb_vac)) +
  geom_point(aes(color = Regions)) +
  labs(x="Date", y="Nb de vaccinées", title = "Nombre de vaccinees aux Regions Ultramarines et Metropolitaines"))
#===============================================================================================================#


# Graphes du pourcentage des vaccineess aux deux Regions #
#===============================================================================================================#
plot_p_ultra = (ggplot(subset(dates_nb_vac_ultra_metro, Regions == "Ultramarine"), aes(x = Date, y = p_vac)) +
                geom_point() +
                ylim(0, 10) +
                labs(x="Date", y="Nb de vaccinées", title = "Pourcentage de vaccinees aux Regions Ultramarines"))

plot_p_metro = (ggplot(subset(dates_nb_vac_ultra_metro, Regions == "Metropolitaine"), aes(x = Date, y = p_vac)) +
                geom_point() +
                ylim(0, 10) +
                labs(x="Date", y="Nb de vaccinées", title = "Pourcentage de vaccinees aux Regions Metropolitaines"))

plot_p_ultra_metro = (ggplot(dates_nb_vac_ultra_metro, aes(x = Date, y = p_vac)) +
                      geom_point(aes(color = Regions)) +
                      labs(x="Date", y="Nb de vaccinées", title = "Pourcentage de vaccinees aux Regions Ultramarines et Metropolitaines"))
#===============================================================================================================#

plot_nb_ultra
plot_nb_metro
plot_nb_ultra_metro

plot_p_ultra
plot_p_metro
plot_p_ultra_metro