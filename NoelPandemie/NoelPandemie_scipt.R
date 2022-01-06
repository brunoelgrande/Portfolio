### Noel en pandemie ###
### Bruno Gauthier   ###
### 2021-12-19       ###

# Library
library(gplots)
library(Rmisc)
library(OneStopAnova)
library(nortest)
library(car)
library(multcomp)
library(psych)
library(tidyverse)
library(plotly)
library(Hmisc)
library(ggthemes)
library(reshape2)
library(gridExtra)
library(lubridate)

# Thème pour ggplot
theme <-   theme_fivethirtyeight()+
              theme(axis.title = element_text(size = 16.5), 
              axis.text.x = element_text(size = 12.5, face = "bold"), 
              legend.title = element_text(face = "bold", size = 14),
              axis.line = element_line(size = 0.4, colour = "grey10"), 
              axis.text.y = element_text(size = 15, face = "bold"), 
              legend.text = element_text(size = 14),
              plot.title = element_text(size = 21, colour = "gray15"), 
              plot.subtitle = element_text(size = 15.5, colour = "gray42"), 
              plot.caption = element_text(color = "gray40", face = "italic", size = 11.5),
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"),
              legend.background = element_rect(fill="white"))


# Tri des dates - Analyse de septembre à mars de chaque année
date_2020 <- c("2020-09-01","2021-03-20")      
date_2021 <- c("2021-09-01", "2021-12-16")

# Importer les données brutes et transformation en table allongée pour les CAS
COVID_2020 <- read.table("NoePandemie_Cas_quotidiens_confimes.txt", header = TRUE, sep = "\t") %>%
        mutate(Date = as.Date(Date)) %>%# Correction de format des dates
        filter(Date >= date_2020[1] & Date <= date_2020[2]) %>%
        mutate(Cas.2020 = QC)%>%
        select(Date, Cas.2020) %>%
        mutate(Date = Date %m+% years(1))   # Transférer la date en 2021 pour superposer les courbes sur 2020

COVID_2021 <- read.table("NoelPandemie_Cas_quotidiens_confimes.txt", header = TRUE, sep = "\t") %>%
        mutate(Date = as.Date(Date)) %>%# Correction de format des dates
        filter(Date >= date_2021[1] & Date <= date_2021[2]) %>%
        mutate(Cas.2021 = QC)%>%
        select(Date, Cas.2021)

# Regrouper les 2 jeux de données avec un "full outer join" pour garder toutes les données. 
COVID <- merge(x = COVID_2020, y = COVID_2021, by = "Date", all = TRUE)%>%
         pivot_longer(cols = c("Cas.2020","Cas.2021"), names_to = "Annee", values_to = "Cas")%>%
         mutate(Annee = as.factor(Annee))

Dates <- data.frame(Date.2020 = date_2020,
                   Date.2021 = date_2021)


Cas <- table(COVID$Annee)
df <- merge(x = COVID_2020, y = COVID_2021, by = "Date", all = TRUE)
Cas[2] <- sum(!is.na(df$Cas.2021))

# Tendance dans le temps
ggplot(COVID, aes(x=Date, y=Cas, color = Annee)) + 
        geom_line(size = 1)+
        theme+
        labs(   x = "Dates",
                y = "Cas",
                title = "Nouveaux cas COVID quotidiens",
                subtitle = "par 100 000 habitants au Québec",
                caption = "© Bruno Gauthier")+
        scale_color_discrete(name = "Années", breaks= c("Cas.2020", "Cas.2021"), labels=c("2020", "2021"))


### Analyse statistique

# Filter pour données que nous avons seulement
COVID_stats <- COVID %>%
        filter(Date >= date_2021[1] & Date <= date_2021[2])
levels(COVID_stats$Annee) <- c("2020", "2021")

#Même graphique, mais sans données NA
ggplot(COVID_stats, aes(x=Date, y=Cas, color = Annee)) + 
        geom_line(size = 1)+
        theme+
        labs(   x = "Dates",
                y = "Cas",
                title = "Cas COVID quotidiens au Québec",
                subtitle = "par 100 000 habitants (1er septembre au 16 décembre)",
                caption = "© Bruno Gauthier")+
        scale_color_discrete(name = "Années", breaks= c("2020", "2021"), labels=c("2020", "2021"))


# Boxplot de la distribution par année
COVID_stats %>%
        ggplot(aes(y=Annee, x=Cas, fill = Annee, alpha = 0.9))+
        geom_boxplot(outlier.size = 1.7, 
                     outlier.shape = 20, 
                     lwd = 0.8, 
                     fatten = 1.2,
                     col = "black")+
        stat_summary(fun=mean, geom="point", shape=18, size=6, color="red", fill="red")+
        geom_violin(alpha = 0.1, 
                    fill = "deepskyblue2")+
        theme+
        labs(title = "Distribution des cas COVID au Québec",
             subtitle = "par 100 000 habitants (1er septembre au 16 décembre)",
             y = "", 
             x= "Nouveau cas quotidiens",
             fill = "",
             caption = "© Bruno Gauthier")+
        theme(legend.position = "none")

# Histogramme sans transfo
COVID_stats %>%
        ggplot(aes(x=Cas, color = Annee, fill=Annee))+
        geom_histogram(binwidth=1,alpha = 0.5)+
        theme+
        labs(title = "Cas COVID au Québec",
             subtitle = "par 100 000 habitants (1er septembre au 16 décembre)",
             y = "Nombre", 
             x= "Nouveau cas quotidiens",
             #fill = "",
             caption = "© Bruno Gauthier")

# Transformation par rang
COVID_stats$Cas_Rang <- rank(COVID_stats$Cas)
headTail(COVID_stats)


# Boxplot de la distribution par année après transformation
COVID_stats %>%
        ggplot(aes(y=Annee, x=Cas_Rang, fill = Annee, alpha = 0.9))+
        geom_boxplot(outlier.size = 1.7, 
                     outlier.shape = 20, 
                     lwd = 0.8, 
                     fatten = 1.2,
                     col = "black")+
        stat_summary(fun=mean, geom="point", shape=18, size=6, color="red", fill="red")+
        geom_violin(alpha = 0.1, 
                    fill = "deepskyblue2")+
        theme+
        labs(title = "Distribution des cas COVID au Québec",
             subtitle = "par 100 000 habitants (1er septembre au 16 décembre)",
             y = "", 
             x= "Nouveau cas quotidiens",
             fill = "",
             caption = "© Bruno Gauthier")+
        theme(legend.position = "none")

# Histogramme avec transfo
COVID_stats %>%
        ggplot(aes(x=Cas_Rang, color = Annee, fill=Annee))+
        geom_histogram(binwidth=10,alpha = 0.5)+
        theme+
        labs(title = "Rang des cas COVID au Québec",
             subtitle = "par 100 000 habitants (1er septembre au 16 décembre)",
             y = "Nombre", 
             x= "Nouveau cas quotidiens",
             #fill = "",
             caption = "© Bruno Gauthier")

# Anova sur transfo rang
ANOVA_rang <- aov(Cas_Rang~Annee, data = COVID_stats)
par(mfrow = c(1,2))
plot(ANOVA_rang, which = 1)
plot(ANOVA_rang, which = 2)

# Tests formels
ad.test(residuals(ANOVA_rang))
leveneTest(COVID_stats$Cas_Rang ~ COVID_stats$Annee )

# ANOVA lorsque les variances sont inégales
oneway.test(Cas_Rang~Annee, COVID_stats, var.equal = FALSE)

# Moyennes par années avec les IC à 95%
group.CI(Cas~Annee, COVID_stats, ci= 0.95)