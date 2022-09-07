nba <- read.csv("nbahistorical.csv", as.is = T, header = T)
nba <- nba[-c(21,22),]

techs <- read.csv("techfouls.csv",as.is = T, header = T)
techs <- techs[complete.cases(techs), ]

info <- read.csv("all_seasons.csv",as.is=T, header=T)
info <- info[info$draft_year == "2019" | info$draft_year == "1999", ]

library(ggplot2)
library(gridExtra)

#Fouls
ggplot(data = nba, aes (x=Season, y = PF, group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=19, ymax=pmax(PF,0)), fill="chocolate", alpha=0.4) +
  geom_line(color = "chocolate", linetype = "dotted", size = 0.5) +
  geom_point(color = "chocolate", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("Fouls")),
       title = "Average Fouls per Game") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
#----------------------------------------------------------------------------------------------------------------------------
            
#Pace
ggplot(data = nba, aes (x=Season, y = Pace, group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=90, ymax=pmax(Pace,0)), fill="firebrick", alpha=0.4) +
  geom_line(color = "firebrick", linetype = "dotted", size = 0.5) +
  geom_point(color = "firebrick", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("Pace"^"(possesions per 48 minutes)")),
       title = "Average Pace per Game") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
#----------------------------------------------------------------------------------------------------------------------------        
        
#Efficiency (eFG%)
eff <- ggplot(data = nba, aes (x=Season, y = eFG., group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=.47, ymax=pmax(eFG.,0)), fill="aquamarine3", alpha=0.4) +
  geom_line(color = "aquamarine3", linetype = "dotted", size = 0.5) +
  geom_point(color = "aquamarine3", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("Efficiency"^"(eFG%)")),
       title = "Efficiency per Season") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
#Offensive Rebounds
oreb <- ggplot(data = nba, aes (x=Season, y = ORB, group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=9.7, ymax=pmax(ORB,0)), fill="brown1", alpha=0.4) +
  geom_line(color = "brown1", linetype = "dotted", size = 0.5) +
  geom_point(color = "brown1", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("Offensive Rebounds")),
       title = "Offensive Rebounds per Game") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
grid.arrange(eff, oreb,
             ncol = 2, nrow=1)
             
#----------------------------------------------------------------------------------------------------------------------------             
             
#3-Pointers
#3-Pointers Attempted
x3pa <- ggplot(data = nba, aes (x=Season, y = X3PA, group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=14.7, ymax=pmax(X3PA,0)), fill="gold3", alpha=0.4) +
  geom_line(color = "gold3", linetype = "dotted", size = 0.5) +
  geom_point(color = "gold3", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("3-Pointers Attempted")),
       title = "3-Pointers Attempted per Game") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
#3-Pointers Made
x3pm <- ggplot(data = nba, aes (x=Season, y = X3P, group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=5.1, ymax=pmax(X3P,0)), fill="chartreuse3", alpha=0.4) +
  geom_line(color = "chartreuse3", linetype = "dotted", size = 0.5) +
  geom_point(color = "chartreuse3", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("3-Pointers Made")),
       title = "3-Pointers Made per Game") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
#3-Pointer %
x3p. <- ggplot(data = nba, aes (x=Season, y = X3P., group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=.347, ymax=pmax(X3P.,0)), fill="orange", alpha=0.4) +
  geom_line(color = "orange", linetype = "dotted", size = 0.5) +
  geom_point(color = "orange", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("3-Point %")),
       title = "3-Pointer % per Season") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
grid.arrange(x3pa, x3pm, x3p.,
             ncol = 1, nrow = 3)
             
#----------------------------------------------------------------------------------------------------------------------------                       
#Size/Weight             
info$player_weight <- info$player_weight * 2.2
info1 <- info[info$draft_year == "2019",]
info2 <- info[info$draft_year == "1999",]

#WEIGHT FIRST
weight2019 <- ggplot(data = info1, aes(player_weight)) +
  geom_histogram(aes(y=..density..),binwidth=8, color="black",fill="white") +
  geom_density(alpha=.2, fill="#00BCF4") +
  scale_x_continuous(breaks = round(seq(min(info1$player_weight),
                                        max(info1$player_weight, by = 1),10))) +
  geom_vline(aes(xintercept=mean(player_weight)),
             color="blue", linetype="dashed", size=.5) +
  labs(x = expression(paste("Weight"^"(lbs)")), y = expression(paste("Density")),
       title = "Weight of Players in 2019") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
weight1999 <-ggplot(data = info2, aes(player_weight)) +
  geom_histogram(aes(y=..density..),binwidth=8, color="black",fill="white") +
  geom_density(alpha=.2, fill="red") +
  scale_x_continuous(breaks = round(seq(min(info2$player_weight),
                                        max(info2$player_weight, by = 1),10))) +
  geom_vline(aes(xintercept=mean(player_weight)),
             color="red", linetype="dashed", size=.5) +
  labs(x = expression(paste("Weight"^"(lbs)")), y = expression(paste("Density")),
       title = "Weight of Players in 1999") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
weightdensity <- ggplot(data = info, aes(x = player_weight, color = draft_year, fill=draft_year)) +
  geom_density(alpha = .3) +
  scale_x_continuous(breaks = round(seq(min(info$player_weight),
                                        max(info$player_weight, by = 1),10))) +
  geom_vline(aes(xintercept=mean(info1$player_weight)),
             color="blue", linetype="dashed", size=.5) +
  geom_vline(aes(xintercept=mean(info2$player_weight)),
             color="firebrick", linetype="dashed", size=.5) +
  labs(x = expression(paste("Weight"^"(lbs)")), y = expression(paste("Density")),
       title = "Weight Difference between the Two Years") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "black", size = .1),
        panel.grid.minor = element_line(color = "white", size = .1))
        
grid.arrange(weight2019, weight1999,weightdensity, ncol = 1, nrow=3)

#----------------------------------------------------------------------------------------------------------------------------

#HEIGHT
xLabels <- paste(c("5'10","6'0","6'2","6'4","6'6","6'8","6'10","7'0","7'2"))

height2019 <- ggplot(data = info1, aes(player_height)) +
  geom_histogram(aes(y=..density..),binwidth=4, color="black",fill="white") +
  geom_density(alpha=.2, fill="#00BCF4") +
  scale_x_continuous(breaks = c(178,183,188,193,198,203,208,213,218),
                     labels = xLabels) +
  geom_vline(aes(xintercept=mean(player_height)),
             color="blue", linetype="dashed", size=.5) +
  labs(x = expression(paste("Height"^"(ft/in)")), y = expression(paste("Density")),
       title = "Height of Players Drafted in 2019") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
height1999 <-ggplot(data = info2, aes(player_height)) +
  geom_histogram(aes(y=..density..),binwidth=4, color="black",fill="white") +
  geom_density(alpha=.2, fill="red") +
  scale_x_continuous(breaks = c(178,183,188,193,198,203,208,213,218),
                     labels = xLabels) +
  geom_vline(aes(xintercept=mean(player_height)),
             color="red", linetype="dashed", size=.5) +
  labs(x = expression(paste("Height"^"(ft/in)")), y = expression(paste("Density")),
       title = "Height of Players Drafted in 1999") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
heightdensity <- ggplot(data = info, aes(x = player_height, color = draft_year, fill=draft_year)) +
  geom_density(alpha = .3) +
  scale_x_continuous(breaks = c(178,183,188,193,198,203,208,213,218),
                     labels = xLabels) +
  geom_vline(aes(xintercept=mean(info1$player_height)),
             color="blue", linetype="dashed", size=.5) +
  geom_vline(aes(xintercept=mean(info2$player_height)),
             color="firebrick", linetype="dashed", size=.5) +
  labs(x = expression(paste("Height"^"(ft/in)")), y = expression(paste("Density")),
       title = "Height Comparison between the Two Years") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "black", size = .1),
        panel.grid.minor = element_line(color = "white", size = .1))
        
grid.arrange(height2019, height1999,heightdensity, ncol = 1, nrow=3)

grid.arrange(weightdensity, heightdensity, ncol=1,nrow=2)

#----------------------------------------------------------------------------------------------------------------------------

#Points
ggplot(data = nba, aes (x=Season, y = PTS, group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=93.4, ymax=pmax(PTS,0)), fill="darkslateblue", alpha=0.4) +
  geom_line(color = "darkslateblue", linetype = "dotted", size = 0.5) +
  geom_point(color = "darkslateblue", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("Points")),
       title = "Points per Game") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
#----------------------------------------------------------------------------------------------------------------------------
        
#Age
ggplot(data = nba, aes (x=Season, y = Age, group = 1,)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=26, ymax=pmax(Age,0)), fill="darkslateblue", alpha=0.4) +
  geom_line(color = "darkslateblue", linetype = "dotted", size = 0.5) +
  geom_point(color = "darkslateblue", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("Average Age")),
       title = "Average Age per Season") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
#----------------------------------------------------------------------------------------------------------------------------

#FGA, DREB, TREB
#Field Goals Attempted
fga <- ggplot(data = nba, aes (x=Season, y = FGA, group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=79, ymax=pmax(FGA,0)), fill="darkorchid1", alpha=0.4) +
  geom_line(color = "darkorchid1", linetype = "dotted", size = 0.5) +
  geom_point(color = "darkorchid1", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("Field Goals Attempted")),
       title = "Field Goals Attempted per Game") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))        
        
#Defensive Rebounds
dreb <- ggplot(data = nba, aes (x=Season, y = DRB, group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=29.8, ymax=pmax(DRB,0)), fill="deepskyblue1", alpha=0.4) +
  geom_line(color = "deepskyblue1", linetype = "dotted", size = 0.5) +
  geom_point(color = "deepskyblue1", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("Devensive Rebounds")),
       title = "Defensive Rebounds per Game") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
#Total Rebounds
treb <- ggplot(data = nba, aes (x=Season, y = TRB, group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=41, ymax=pmax(TRB,0)), fill="darkorange", alpha=0.4) +
  geom_line(color = "darkorange", linetype = "dotted", size = 0.5) +
  geom_point(color = "darkorange", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("Total Rebounds")),
       title = "Total Rebounds per Game") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
grid.arrange(fga, treb, dreb,
             ncol = 1, nrow=3)
             
#----------------------------------------------------------------------------------------------------------------------------             
             
#ORtg, TOV%, and FGA/FT
#Offensive Rating
ortg <- ggplot(data = nba, aes (x=Season, y = ORtg, group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=102.9, ymax=pmax(ORtg,0)), fill="darkgoldenrod1", alpha=0.4) +
  geom_line(color = "darkgoldenrod1", linetype = "dotted", size = 0.5) +
  geom_point(color = "darkgoldenrod1", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("Offensive Rating"^"(estimate of points scored per 100 possessions)")),
       title = "Average Offensive Rating per Season") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
#TOV%
tov. <- ggplot(data = nba, aes (x=Season, y = TOV., group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=12.3, ymax=pmax(TOV.,0)), fill="red2", alpha=0.4) +
  geom_line(color = "red2", linetype = "dotted", size = 0.5) +
  geom_point(color = "red2", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("Turnover %"^"(an estimate of turnovers committed per 100 plays)")),
       title = "Turnover % per Season") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
#FT/FGA
ft.fga <- ggplot(data = nba, aes (x=Season, y = FT.FGA, group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=0.192, ymax=pmax(FT.FGA,0)), fill="lightslateblue", alpha=0.4) +
  geom_line(color = "lightslateblue", linetype = "dotted", size = 0.5) +
  geom_point(color = "lightslateblue", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("# Free Throws per Field Goal Attempt")),
       title = "Average # of Free Throws per Field Goal Attempt per Season") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1))
        
grid.arrange(ortg, tov., ft.fga,
             ncol = 1, nrow=3)

#----------------------------------------------------------------------------------------------------------------------------

#Techs

par(mfrow=c(1,1))

techmean <- colMeans(techs[,])

t = seq(2004,2021,by = 1)

df <- data.frame(t, techmean)

ggplot(data = df, aes (x=t, y = techmean, group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymin=0.3033333, ymax=pmax(techmean)), fill="indianred1", alpha=0.4) +
  geom_line(color = "indianred1", linetype = "dotted", size = 0.5) +
  geom_point(color = "indianred1", shape = "diamond", size = 2) +
  labs(x = "Season", y = expression(paste("Average Technical Fouls per Game")),
       title = "Average Technial Fouls per Game") +
  theme(panel.background = element_rect(fill = "white",color="black"),
        panel.grid.major = element_line(color = "gray10", size = .1),
        panel.grid.minor = element_line(color = "gray50", size = .1)) +
  scale_x_continuous(breaks = round(seq(min(df$t), max(df$t), by = 1),1))
