library (tidyverse)
library(ggplot2)
library(ggridges)

appearences<-read_csv('/Users/anatoscano/Desktop/LCD/appearances.csv')
teams<-read_csv('/Users/anatoscano/Desktop/LCD/teams.csv')
players <- read_csv(file='/Users/anatoscano/Desktop/LCD/players.csv', locale = readr::locale(encoding = "latin1"))
leagues<-read_csv('/Users/anatoscano/Desktop/LCD/leagues.csv')
teamstats<-read_csv('/Users/anatoscano/Desktop/LCD/teamstats.csv')
games<-read_csv('/Users/anatoscano/Desktop/LCD/games.csv')
shots<-read_csv('/Users/anatoscano/Desktop/LCD/shots.csv')

#Buscar los 10 equipos mas goleadores
equipos_goleadores<-group_by(teamstats, teamID) %>%
  summarise(count_goals=sum(goals, na.rm = TRUE))

goles_ordenados=order(equipos_goleadores$count_goals, decreasing = TRUE)
equipos_goleadores=head(equipos_goleadores[goles_ordenados,], n=10)

equipos_goleadores<-inner_join(equipos_goleadores, teams, by='teamID')
ggplot(data=equipos_goleadores)+
  geom_bar(aes(x=reorder(name, count_goals), y=count_goals), stat='identity')+
  labs(title="Top 10 equipos goleadores", x=NULL, y="Cantidad de goles")

#Tiros al arco (10 equipos)
tiros_arco<-group_by(teamstats, teamID) %>%
  summarise(count_tiros=sum(shots, na.rm = TRUE))

tiros_ordenados<-order(tiros_arco$count_tiros, decreasing = TRUE)
tiros_arco<-head(tiros_arco[tiros_ordenados,], n=10)

tiros_arco<-inner_join(tiros_arco, teams, by='teamID')

ggplot(data=tiros_arco)+
  geom_bar(aes(x=reorder(name, count_tiros), y=count_tiros), stat='identity')+
  labs(title="Top 10 equipos en función de los tiros al arco", x=NULL, y="Cantidad de tiros al arco")

#Indice tiros vs goles
indicador_tg<-inner_join(equipos_goleadores, tiros_arco, by='name')
ggplot(data=indicador_tg)+
  geom_point(aes(x=count_tiros, y=count_goals, colour=factor(name)), stat='identity')+
  labs(title="Cantidad de goles en función de la cantidad de tiros al arco", x="Cantidad de tiros al arco", y="Cantidad de goles")
  
#Indice tiros vs goles general
goles_por_equipo<-group_by(teamstats, teamID) %>%
  summarise(count_goals=sum(goals, na.rm = TRUE))
tiros_por_equipo<-group_by(teamstats, teamID) %>%
  summarise(count_tiros=sum(shots, na.rm = TRUE))
indicador_gral<-inner_join(goles_por_equipo, tiros_por_equipo, by='teamID')
indicador_gral<-inner_join(indicador_gral, teams, by='teamID')

ggplot(data=indicador_gral)+
  geom_point(aes(x=count_tiros, y=count_goals), stat='identity')+
  labs(title = "Cantidad de goles en función de la cantidad de tiros al arco", x="Cantidad de tiros al arco", y="Cantidad de goles")

#Top 5 goleadores de La Liga
goleadores<-group_by(appearences, playerID) %>%
  summarise(goles=sum(goals, na.rm = TRUE))

goleadores_ordenados<-order(goleadores$goles, decreasing=TRUE)
goleadores_ordenados<-head(goleadores[goleadores_ordenados,], n=5)

top_goleadores<-inner_join(goleadores_ordenados, players, by='playerID')

ggplot(data=top_goleadores)+
  geom_bar(aes(x=reorder(name, goles), y=goles), stat='identity')+
  labs(title="Top 5 goleadores", x=NULL, y="Cantidad de goles")

#Goles
shot_result<-group_by(shots, shotResult) %>%
  filter(shotResult == "Goal")
names(top_goleadores)
names(top_goleadores)[1]="shooterID"
goles<-semi_join(shot_result, top_goleadores, by="shooterID")
names(players)[1]="shooterID"
goles<-inner_join(goles, players, by='shooterID')

ggplot(data=goles)+
  geom_density_ridges2(aes(x=reorder(shooterID, minute), y=minute), stat = 'identity')

ggplot(goles, aes(x = minute, y = name, fill=factor(name))) + geom_density_ridges()+
  labs(title="Tiempo de goles de los top 5 goleadores de La Liga", x="Tiempo [']", y=NULL)

       