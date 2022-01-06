### Variation du coût des énergies dans le temps'
### Gaz naturel, propane et huile no.2'
### 2022-01-05
### Bruno Gauthier

### https://blog.brunogauthier.net/2022/01/05/energies-temps.html
### https://github.com/brunoelgrande/Portfolio

library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)


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

# Importation données de prix -- Energy Information Administration (www.eia.gov)
ng_import <- read_excel('NG_Res_Price.xlsx') %>% 
  mutate(Date = as.Date(Date))%>%
  as.data.frame()

oil_import <- read_excel('Oil2_Res_Price.xlsx')%>% 
  mutate(Date = as.Date(Date))%>%
  as.data.frame()

prop_import <- read_excel('Prop_Res_Price.xlsx')%>%   
  mutate(Date = as.Date(Date))%>%
  as.data.frame()


# Visualisation des données brutes

## Gaz Naturel


ng_import %>% 
  ggplot(aes(x= Date, y = NG) )+
  geom_area(size=1, color = "DeepSkyBlue", fill = "DeepSkyBlue", alpha = 0.3)+
  theme+
  labs(title = "Gaz naturel - marché résidentiel",
       subtitle = "en USD par mille pieds cube",
       caption = "© Bruno Gauthier",
       y = "Prix",
       x = "Années")+
  scale_y_continuous( labels = scales::label_dollar(prefix = "", suffix = "$"))

summary(ng_import)

## Variation annuelle du gaz naturel

ng_import %>%
  mutate(Year = as.factor(year(Date)),
         Month = (month(Date)))%>%
  filter(Year %in% c(1985,1990,1995,2000,2005,2010,2015,2020,2021))%>%
  ggplot(aes(x= Month, y = NG, color = Year ))+
  geom_line(size=0.8)+
  theme+
  labs(title = "Gaz naturel - variation annuelle",
       subtitle = "en USD par mille pieds cube - 1985 à 2021",
       caption = "© Bruno Gauthier",
       y = "",
       x = "")+
  scale_color_discrete(name = "Années")+
  scale_x_continuous( breaks = 1:12, labels = c("Janv","Fev","Mars","Avil","Mai","Juin","Juil","Aout","Sept","Oct","Nov","Dec"))+
  scale_y_continuous( labels = scales::label_dollar(prefix = "", suffix = "$"))


## Huile no 2

oil_import %>% 
  ggplot(aes(x= Date, y = Oil))+
  geom_area(size=1, color = "red", fill = "red", alpha = 0.3)+
  theme+
  labs(title = "Huile à chauffage résidentiel no.2",
       subtitle = "en USD par gallon",
       caption = "© Bruno Gauthier",
       y = "Prix",
       x = "Années")+
  scale_y_continuous( labels = scales::label_dollar(prefix = "", suffix = "$"))

summary(oil_import)


## Propane

prop_import %>% 
  ggplot(aes(x= Date, y = Prop))+
  geom_area(size=1, color = "orange", fill = "orange", alpha = 0.3)+
  theme+
  labs(title = "Propane - marché résidentiel",
       subtitle = "en USD par gallon",
       caption = "© Bruno Gauthier",
       y = "Prix",
       x = "Années")+
  scale_y_continuous( labels = scales::label_dollar(prefix = "", suffix = "$"))

summary(prop_import)


# Transformation des prix en unités communes


## Conversion gaz naturel : Millier de pieds cube à MMBH
ng_import$NG <- ng_import$NG / 1.037

## Conversion propane : Gallon à MMBH
prop_import$Prop <- prop_import$Prop * 10.917
 
## Conversion huile : Gallon à MMBH
oil_import$Oil <- oil_import$Oil * 7.194


# Création du fichier de données de prix


## Dates à l'étude

## Vérifions les dates couvertes par chacun des jeux de données et trouvons les dates pour lesquelles des données existent dans les 3 jeux de données. 

Date_Debut <- max(c(min(ng_import$Date),
                    min(oil_import$Date),
                    min(prop_import$Date)))

Date_Fin <- min(c(max(ng_import$Date),
                  max(oil_import$Date),
                  max(prop_import$Date)))


Dates_Etude <- c(Date_Debut, Date_Fin) ;Dates_Etude


## Regroupement des données


NG <- pivot_longer(ng_import, cols= "NG", names_to = "Type", values_to = "Price")
OIL <- pivot_longer(oil_import, cols= "Oil", names_to = "Type", values_to = "Price")
PROP <- pivot_longer(prop_import, cols= "Prop", names_to = "Type", values_to = "Price")

Prices <- union_all(union_all(NG,OIL),PROP)%>%
  filter(Date >= Dates_Etude[1] & Date <= Dates_Etude[2]+months(3)) %>%
  arrange(Date)

Prices_wide <- Prices %>%
  pivot_wider(names_from = "Type", values_from = "Price")


# Variation du prix dans le temps

Prices %>%
  ggplot(aes(x= Date, y = Price, color = Type))+
  geom_line(size = 1)+
  theme+
  labs(title = "Coûts des énergies - marché résidentiel",
       subtitle = "en USD par millier de BTU",
       caption = "© Bruno Gauthier",
       y = "Prix",
       x = "Années")+
  scale_color_manual('', 
                     breaks = c('NG', 'Oil', 'Prop'),
                     values = c('NG' = 'DeepSkyBlue', 'Oil' = 'red', 'Prop' = "orange"),
                     labels = c("Gaz Naturel", "Huile 2", "Propane"))+
  scale_y_continuous( labels = scales::label_dollar(prefix = "", suffix = "$")) +
  annotate('rect',  ## add red rectangle
           xmin = as.Date('2001-3-1'),
           xmax = as.Date('2001-11-1'),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.1,
           fill = 'red') +
  annotate('curve', ## add curved arrow
           x = as.Date('1999-1-1'),
           y = 34,
           xend = as.Date('2001-3-1'),
           yend = 40,
           curvature = -0.5,
           arrow = arrow(length = unit(3, 'mm'))) +
  annotate('text', ## add text
           x = as.Date('1998-1-1'),
           y = 30,
           label = 'Récession \n début 2000')+
  
  annotate('rect',
           xmin = as.Date('2007-12-1'),
           xmax = as.Date('2009-6-1'),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.1,
           fill = 'red') +
  annotate('curve',
           x = as.Date('2005-12-1'),
           y = 39,
           xend = as.Date('2007-12-1'),
           yend = 45,
           curvature = -0.5,
           arrow = arrow(length = unit(3, 'mm'))) +
  annotate('text',
           x = as.Date('2005-11-1'),
           y = 35,
           label = 'Grande récession \n 2008') +
  
  annotate('rect',
           xmin = as.Date('2020-2-1'),
           xmax = as.Date('2020-09-01'),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.1,
           fill = 'red') +
  annotate('curve',
           x = as.Date('2018-1-1'),
           y = 39,
           xend = as.Date('2020-2-1'),
           yend = 43,
           curvature = -0.5,
           arrow = arrow(length = unit(3, 'mm'))) +
  annotate('text',
           x = as.Date('2017-1-1'),
           y = 34.5,
           label = 'Crise \n COVID-19')



# Corrélation entre le prix du propane et de l'huile no.2 dans le marché résidentiel


# Régression linéaire
m1 <- lm(Prop ~ Oil, data = Prices_wide)

# Equation de la droite et coefficient R carré
eq <- paste0("Prix Propane = ", round(m1$coefficients[2],3), " * Prix Huile + ", round(m1$coefficients[1], 2), "\n R carré = ", round(summary(m1)$r.squared,3))

# Graphique des points originaux et droite de corrélation
Prices_wide %>%
  ggplot(aes( x = Oil, y = Prop))+
  geom_point(color = "chocolate")+
  scale_y_continuous( labels = scales::label_dollar(prefix = "", suffix = "$"))+
  scale_x_continuous( labels = scales::label_dollar(prefix = "", suffix = "$"))+
  theme+
  labs(title = "Corrélation Huile et Propane",
       subtitle = "en USD par millier de BTU",
       caption = "© Bruno Gauthier",
       y = "Prix - Propane",
       x = "Prix - Huile")+
  geom_smooth(method =lm, color = "darkblue")+
  annotate('text',
           x = 25,
           y = 15,
           label = eq,
           color = "darkblue")  



### Création du graphique animé avec gganimate

# Données qui vont apparaître graduellement dans le temps

# https://www.youtube.com/watch?v=SnCi0s0e4Io 

# install.packages('gifski')
# install.packages('png')

library(gganimate)
library(gapminder)

Graph_Energies <- Prices %>%
  ggplot(aes(x= Date, y = Price, color = Type))+
  geom_line(size = 1)+
  theme+
  labs(title = "Coûts des énergies - marché résidentiel",
       subtitle = "en USD par millier de BTU",
       caption = "© Bruno Gauthier",
       y = "Prix",
       x = "Années")+
  scale_color_manual('', 
                     breaks = c('NG', 'Oil', 'Prop'),
                     values = c('NG' = 'DeepSkyBlue', 'Oil' = 'red', 'Prop' = "orange"),
                     labels = c("Gaz Naturel", "Huile 2", "Propane"))+
  scale_y_continuous( labels = scales::label_dollar(prefix = "", suffix = "$"))+
  geom_point()
+
  scale_x_continuous(breaks = 1990:2100)


# Paramètres de gganimate

Graph_GIF <- Graph_Energies+
  transition_reveal(Date)+
  view_follow(fixed_y = TRUE)


# Sauvegarde du fichier

animate(Graph_GIF, height = 500, width = 800, fps = 30, duration = 10, end_pause = 60, res = 100)
anim_save( "Cout_Energie_Annees_Anime.gif" )

