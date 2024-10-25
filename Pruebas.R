library(tidyverse)
library(StatsBombR)
library(ggplot2)
library(SBpitch)
library(grid)

#ID Season 2024 = 282
#ID Euro = 55
#ID Copa America = 223
euro <- FreeCompetitions() %>%
  filter(competition_id==55 & season_id==282)
# copa_america <- FreeCompetitions() %>%
#   filter(competition_id==223 & season_id==282)
partidos_euro <- FreeMatches(euro)
# partidos_america <- FreeMatches(copa_america)
sbData_euro <- free_allevents(MatchesDF = partidos_euro)
#sbData_america <- free_allevents(MatchesDF = partidos_america)
names(sbData_euro)
sbData_euro = allclean(sbData_euro) #Esto limpia los datos
names(sbData_euro)

#======================================================
#                 1.Goals and Shots
#======================================================
shots_goals = sbData_euro %>%
  group_by(team.name) %>%
  summarise(shots=sum(type.name=="Shot", na.rm=TRUE),
  goals = sum(shot.outcome.name=="Goal", na.rm=TRUE)
)
#Repetimos sacando un promedio por partido
shots_goals = sbData_euro %>%
  group_by(team.name) %>%
  summarise(shots=sum(type.name=="Shot", na.rm=TRUE),
  goals = sum(shot.outcome.name=="Goal", na.rm=TRUE),
  shotsPerGame=sum(type.name=="Shot", na.rm=TRUE)/n_distinct(match_id),
  goalsPerGame = sum(shot.outcome.name=="Goal", na.rm=TRUE)/n_distinct(match_id)
)

#======================================================
#                 2.Graficar tiros 
#======================================================
ggplot(data= shots_goals,
       aes(x=reorder(team.name, shotsPerGame),y=shotsPerGame)) +
  geom_bar(stat= "identity", width=0.5) +
  labs(y="Shots per game") +
  theme(axis.title.y = element_blank())+
  scale_y_continuous(expand = c(0,0)) +
  coord_flip()+
  theme_SB()

#======================================================
#           3.Tiros por jugador por 90 mins
#======================================================
player_shots = sbData_euro %>%
  group_by(player.name,player.id) %>%
  summarise(shots=sum(type.name=="Shot",na.rm = TRUE))
player_minutes = get.minutesplayed(sbData_euro)
player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes=sum(MinutesPlayed))
player_shots = left_join(player_shots, player_minutes)
player_shots = player_shots %>% mutate(nineties=minutes/90)
player_shots = player_shots %>% mutate(shots_per90=shots/nineties)

#======================================================
#                 4.Mapa de pases 
#======================================================
datosJugs = unique(sbData_euro[, c("player.id", "player.name")])
datosPos = unique(sbData_euro[, c("position.id", "position.name")])
# Bellingham ID = 30714
passes_box = sbData_euro %>%
  filter(type.name=="Pass" & is.na(pass.outcome.name) & player.id==30714) %>%
  filter(pass.end_location.x>=102 & pass.end_location.y<=62 & pass.end_location.y>=18)
create_Pitch() +
  geom_segment(data= passes_box, aes(x=location.x, y=location.y, 
                                     xend=pass.end_location.x, yend=pass.end_location.y),
               lineend="round", size=0.5,colour="#000000",
               arrow=arrow(length=unit(0.07,"inches"),ends="last",type="open")) +
  labs(title="Jude bellingham, Completed Box Passes", subtitle = "Euro 2024") +
  scale_y_reverse() +
  coord_fixed(ratio = 105/100)
#======================================================
#                 4.Mapa de pases 
#======================================================
passes = sbData_euro %>%
  filter(type.name=="Pass" & is.na(pass.outcome.name) & player.id==30714)
create_Pitch() +
  geom_segment(data= passes, aes(x=location.x, y=location.y, 
                                     xend=pass.end_location.x, yend=pass.end_location.y),
               lineend="round", size=0.5,colour="#000000",
               arrow=arrow(length=unit(0.07,"inches"),ends="last",type="open")) +
  geom_segment(data= passes_box, aes(x=location.x, y=location.y, 
                                     xend=pass.end_location.x, yend=pass.end_location.y),
               lineend="round", size=0.5,colour="#ff0000",
               arrow=arrow(length=unit(0.07,"inches"),ends="last",type="open")) +
  labs(title="Jude bellingham, Completed Box Passes", subtitle = "Euro 2024") +
  scale_y_reverse() +
  coord_fixed(ratio = 105/100)
#======================================================
#               5.xG, xGA usando joining
#======================================================
xGA= sbData_euro%>%
  filter(type.name=="Shot") %>%
  select(shot.key_pass_id,xGA=shot.statsbomb_xg)
shot_assists=left_join(sbData_euro,xGA, by=c("id"="shot.key_pass_id")) %>%
  select(team.name, player.name, player.id, type.name, pass.shot_assist, pass.goal_assist,xGA) %>%
  filter(pass.shot_assist==TRUE|pass.goal_assist==TRUE)

player_xGA = shot_assists %>%
  group_by(player.name, player.id, team.name)%>%
  summarise(xGA=sum(xGA,na.rm=TRUE))
player_xG = sbData_euro %>%
  filter(type.name=="Shot") %>%
  filter(shot.type.name!="Penalty"|is.na(shot.type.name)) %>%
  group_by(player.name, player.id, team.name) %>%
  summarise(xG=sum(shot.statsbomb_xg, na.rm=TRUE)) %>%
  left_join(player_xGA) %>%
  mutate(xG_xGA=sum(xG+xGA, na.rm=TRUE))
player_minutes=get.minutesplayed(sbData_euro)
player_minutes=player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes=sum(MinutesPlayed))
player_xG_xGA=left_join(player_xG, player_minutes) %>%
  mutate(nineties=minutes/90,
         xG_90=round(xG/nineties,2),
         xGA_90=round(xGA/nineties,2),
         xG_xGA90=round(xG_xGA/nineties,2))
chart=player_xG_xGA %>%
  ungroup() %>%
  filter(minutes>=90) %>%
  top_n(n=15, w=xG_xGA90)
chart <- chart%>%
  select(1,9:10)%>%
  pivot_longer(-player.name, names_to="variable",values_to="value") %>%
  filter(variable=="xG_90"|variable=="xGA_90")

ggplot(chart, aes(x=reorder(player.name,value), y=value, fill=fct_rev(variable)))+
  geom_bar(stat="identity",colour="white")+
  labs(title="Expected Goal Contribution",subtitle="Euro 2024",
       x="",y="Per 90",caption="Minimum 60 minutes\nNPxG = Value of shots taken (no penalties)\nxG assisted = Value of shots assisted")+
  theme(axis.text.y=element_text(size=14,color="#333333",family="Source Sans Pro"),
        axis.title=element_text(size=14,color="#333333",family="Source Sans Pro"),
        axis.text.x=element_text(size=14,color="#333333",family="Source Sans Pro"),
        axis.ticks=element_blank(),
        panel.background=element_rect(fill="white",colour="white"),
        plot.background=element_rect(fill="white",colour="white"),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        plot.title=element_text(size=18,color="#333333",family="Source Sans Pro"),
        plot.subtitle=element_text(size=18, color="#333333", family="Source Sans Pro", face="bold"),
        plot.caption=element_text(color="#333333", family="Source Sans Pro", size =10),
        text=element_text(family="Source Sans Pro"),
        legend.title=element_blank(),
        legend.text = element_text(size=14, color="#333333", family="Source Sans Pro"),
        legend.position = "bottom") +
  scale_fill_manual(values=c("#3371AC", "#DC2228"), labels = c( "xG Assisted","NPxG")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,max(chart$value) + 0.3)) + 
  coord_flip()+ 
  guides(fill = guide_legend(reverse = TRUE)) 
#======================================================
#                 6. Gráfica de tiros
#======================================================
heatmap = sbData_euro %>% mutate(location.x=ifelse(location.x>120,120, location.x),
          location.y = ifelse(location.y>80,80, location.y),
          location.x = ifelse(location.x<0,0, location.x),
          location.y = ifelse(location.y<0,0, location.y))
heatmap$xbin <- cut(heatmap$location.x,breaks=seq(from=0, to=120, by=20),include.lowest=TRUE)
heatmap$ybin <- cut(heatmap$location.y,breaks=seq(from=0, to=80, by=20),include.lowest=TRUE)
heatmap = heatmap %>%
  filter(type.name=="Pressure" | duel.type.name=="Tackle" | type.name=="Foul Committed" | 
        type.name=="Interception" | type.name=="Block") %>%
  group_by(team.name) %>%
  mutate(total_DA=n()) %>%
  group_by(team.name,xbin,ybin) %>%
  summarise(total_DA = max(total_DA),
            bin_DA=n(),
            bin_pct=bin_DA/total_DA,
            location.x=median(location.x),
            location.y=median(location.y)) %>%
  group_by(xbin,ybin) %>%
  mutate(league_ave=mean(bin_pct)) %>%
  group_by(team.name,xbin,ybin) %>%
  mutate(diff_vs_ave=bin_pct - league_ave)

#Ahora si a plottear
defensiveactivitycolors <- c("#dc2429","#dc2329","#df272d","#df3238","#e14348","#e44d15",
                             "#e35256", "#e76266", "#e9777b", "#ec8589", "#ec898d", "#ef9195",
                             "#ef9ea1", "#f0a6a9", "#f2abae", "#f4b9bc", "#f8d1d2", "#f9e0e2",
                             "#f7e1e3", "#f5e2e4", "#d4d5d8", "#d1d3d8", "#cdd2d6", "#c8cdd3", "#c0c7cd",
                             "#b9c0c8", "#b5bcc3", "#909ba5", "#8f9aa5", "#818c98", "#798590",
                             "#697785", "#526173", "#435367", "#3a4b60", "#2e4257", "#1d3048",
                             "#11263e", "#11273e", "#0d233a", "#020c16")
ggplot(data= heatmap, aes(x = location.x, y = location.y, fill = diff_vs_ave, group =diff_vs_ave)) +
  geom_bin2d(binwidth = c(20, 20), position = "identity", alpha = 0.9) + #2
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  annotate("point", x = 12 , y = 40, colour = "white", size = 1.05) +
  annotate("point", x = 108 , y = 40, colour = "white", size = 1.05) +
  annotate("path", colour = "white", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  annotate("point", x = 60 , y = 40, colour = "white", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
  annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") + #3
  theme(axis.text.x=element_blank(),
       axis.title.x = element_blank(),
       axis.title.y = element_blank(),
       plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
       plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
       axis.text.y=element_blank(),
       legend.title = element_blank(),
       legend.text=element_text(size=22,family="Source Sans Pro"),
       legend.key.size = unit(1.5, "cm"),
       plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5,
                                 family="Source Sans Pro", colour = "black", hjust = 0.5),
       legend.direction = "vertical",
       axis.ticks=element_blank(),
       plot.background = element_rect(fill = "white"),
       strip.text.x = element_text(size=13,family="Source Sans Pro")) + #4
  scale_y_reverse() + #5
  scale_fill_gradientn(colours = defensiveactivitycolors, trans = "reverse", labels =
                         scales::percent_format(accuracy = 1), limits = c(0.03, -0.03)) + #6
  labs(title = "Where Do Teams Defend vs League Average?", subtitle = "UEFA Euro 2024") + #7
  coord_fixed(ratio = 95/100) + 
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="last",
                                                 length=unit(2.55,"mm")), gp=gpar(col="black", fill=NA, lwd=2.2)),
                    xmin=25, xmax = 95, ymin = -83, ymax = -83) + #9
  facet_wrap(~team.name)+ #10
  guides(fill = guide_legend(reverse = TRUE)) 
#======================================================
#                   7. Mapa de tiros
#======================================================
shots = sbData_euro %>%
  filter(type.name=="Shot"& (shot.type.name!="Penalty"|is.na(shot.type.name)) & player.name=="Harry Kane")
shotmapxgcolors <- c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960", "#FCDC5F",
                     "#F5B94D", "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608", "#BF0000", "#7F0000", "#5F0000")
ggplot() +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  # add penalty spot right
  annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
  annotate("path", colour = "black", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  # add centre spot
  annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_point(data = shots, aes(x = location.x, y = location.y, fill = shot.statsbomb_xg, shape = shot.body_part.name),
             size = 6, alpha = 0.8) +
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.position = "top",
        legend.title=element_text(size=22,family="Source Sans Pro"),
        legend.text=element_text(size=20,family="Source Sans Pro"),
        legend.margin = margin(c(20, 10, -85, 50)),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5, family="Source Sans
Pro", colour = "black", hjust = 0.5),
        legend.direction = "horizontal",
        axis.ticks=element_blank(),
        aspect.ratio = c(65/100),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Source Sans Pro")) +
  labs(title = "Harry Kane, Shot Map", subtitle = "UEFA Euro 2024") + #4
  scale_fill_gradientn(colours = shotmapxgcolors, limit = c(0,0.8), oob=scales::squish, name = "Expected Goals Value") +
  scale_shape_manual(values = c("Head" = 21, "Right Foot" = 23, "Left Foot" = 24), name ="") + #6
  guides(fill = guide_colourbar(title.position = "top"),
         shape = guide_legend(override.aes = list(size = 7, fill = "black"))) + #7
  coord_flip(xlim = c(85, 125))
#======================================================
#             8. Pruebas de mis funciones
#======================================================