#Simulering af sæson
set.seed(2025)
DNK_17_3 <- read_excel("/Users/philipstrommen/Desktop/Bachelor/DNK_17_3.xlsx")
SL<-DNK_17_3 %>% filter(Season == "2024/2025")

#Laver designmatrix
TeamNames<-unique(SL$Home)
Index<-1:length(TeamNames)
Goals<-1:(2*nrow(SL)) 
alfa<-gamma<-rep(0,length(TeamNames))
DesignMatrix<-matrix(0,nrow=2*nrow(SL),ncol=2+2*length(TeamNames))

#Opdaterer designmatrix
for (i in 1:nrow(SL)){
  Goals[2*i-1]<-SL$HG[i]
  Goals[2*i]<-SL$AG[i]
  DesignMatrix[c(2*i-1,2*i),1]<-1
  DesignMatrix[2*i-1,2]<-1
  DesignMatrix[2*i-1,2+Index[TeamNames==SL$Home[i]]]<-1
  DesignMatrix[2*i-1,2+length(TeamNames)+Index[TeamNames==SL$Away[i]]]<--1
  DesignMatrix[2*i,2+Index[TeamNames==SL$Away[i]]]<-1
  DesignMatrix[2*i,2+length(TeamNames)+Index[TeamNames==SL$Home[i]]]<--1
}

#Estimate parameters in the independent Poisson-mode hvor #  [c(1,3,23)] fjernes
testGLM<-glm(Goals ~ DesignMatrix[,c(-1,-3,-15)], family = poisson(link = "log"))

#assigner parametre
#betaAll
betaALL<-testGLM$coefficients[1]

# delta
delta<-testGLM$coefficients[2]

#Angrebs
alfa[2:length(TeamNames)]<-testGLM$coefficients[3:(length(TeamNames)+1)]

#Forsvars
gamma[2:length(TeamNames)]<-testGLM$coefficients[(length(TeamNames)+2):(2*length(TeamNames))]

#samlet
Parametre<-list(hold=TeamNames,hjemmebanefordel=delta,betaALL=betaALL, alfa=alfa, gamma=gamma )

# Angrebs og forsvars sættes op mod hinanden
Alle_parametre<-data.frame(Hold=TeamNames,alfa=alfa, gamma=gamma)

#Hent den færdige stilling af grundspille
home_points <- SL %>%
  group_by(Home) %>%
  summarise(Points = sum(ifelse(Res == "H", 3, ifelse(Res == "D", 1, 0)))) %>%
  rename(Team = Home)

away_points <- SL %>%
  group_by(Away) %>%
  summarise(Points = sum(ifelse(Res == "A", 3, ifelse(Res == "D", 1, 0)))) %>%
  rename(Team = Away)

# Kombiner hjemme- og udepoint til samlet stilling
current_standings <- bind_rows(home_points, away_points) %>%
  group_by(Team) %>%
  summarise(TotalPoints = sum(Points)) %>%
  arrange(desc(TotalPoints))

# Vælg de øverste 6 hold til mesterskabsspillet
TeamNames_Mester <- current_standings$Team[1:6]

# Vælg de nederste 6 hold til nedrykningsspillet
TeamNames_Nedrykning <- current_standings$Team[7:12]

# Gem de nuværende point fra grundspillet for begge grupper
initial_points_mester <- current_standings %>%
  filter(Team %in% TeamNames_Mester) %>%
  select(Team, TotalPoints)

initial_points_nedrykning <- current_standings %>%
  filter(Team %in% TeamNames_Nedrykning) %>%
  select(Team, TotalPoints)

#HDAPROB
HDAprob<- function(Home, Away, betaALL,delta,alfa,gamma) {
  lamH<-exp(betaALL+delta+alfa[Index[TeamNames==Home]]-gamma[Index[TeamNames==Away]])
  lamA<-exp(betaALL+alfa[Index[TeamNames==Away]]-gamma[Index[TeamNames==Home]])
  pH<-pD<-0
  
  for (i in 0:20){
    pD<-pD+dpois(i,lamH)*dpois(i,lamA)
    for (j in 0:i) if (j < i) pH<-pH+dpois(i,lamH)*dpois(j,lamA)
  }
  
  return(c(pH,pD,1-pH-pD))
  
}

# Funktion til at simulere en sæson ----
simulate_season <- function(TeamNames, HDAprob) {
  results <- matrix(0, nrow = length(TeamNames), ncol = 3, dimnames = list(TeamNames, c("Wins", "Draws", "Losses")))
  
  for (i in 1:length(TeamNames)) {
    for (j in 1:length(TeamNames)) {
      if (i != j) {
        probs <- HDAprob(TeamNames[i], TeamNames[j], betaALL, delta, alfa, gamma)
        result <- sample(c("HomeWin", "Draw", "AwayWin"), 1, prob = probs)
        
        if (result == "HomeWin") {
          results[i, "Wins"] <- results[i, "Wins"] + 1
          results[j, "Losses"] <- results[j, "Losses"] + 1
        } else if (result == "AwayWin") {
          results[i, "Losses"] <- results[i, "Losses"] + 1
          results[j, "Wins"] <- results[j, "Wins"] + 1
        } else {
          results[i, "Draws"] <- results[i, "Draws"] + 1
          results[j, "Draws"] <- results[j, "Draws"] + 1
        }
      }
    }
  }
  
  return(results)
}

# ---- 4. Simuler mesterskabsspillet 1000 gange ----
all_results_mester <- list()
for (season in 1:1000) {
  season_results_mester <- simulate_season(TeamNames_Mester, HDAprob)
  all_results_mester[[season]] <- season_results_mester
}

# ---- 5. Simuler nedrykningsspillet 1000 gange ----
all_results_nedrykning <- list()
for (season in 1:1000) {
  season_results_nedrykning <- simulate_season(TeamNames_Nedrykning, HDAprob)
  all_results_nedrykning[[season]] <- season_results_nedrykning
}

# ---- 6. Opsummer resultater for mesterskabsspillet ----
total_results_mester <- matrix(0, nrow = length(TeamNames_Mester), ncol = 3, 
                               dimnames = list(TeamNames_Mester, c("Wins", "Draws", "Losses")))

for (season_results_mester in all_results_mester) {
  total_results_mester <- total_results_mester + season_results_mester
}

average_results_mester <- total_results_mester / 1000
points_mester <- (average_results_mester[, "Wins"] * 3) + average_results_mester[, "Draws"]
simulated_points_mester <- data.frame(Team = TeamNames_Mester, SimulatedPoints = round(points_mester))

# ---- 7. Opsummer resultater for nedrykningsspillet ----
total_results_nedrykning <- matrix(0, nrow = length(TeamNames_Nedrykning), ncol = 3, 
                                   dimnames = list(TeamNames_Nedrykning, c("Wins", "Draws", "Losses")))

for (season_results_nedrykning in all_results_nedrykning) {
  total_results_nedrykning <- total_results_nedrykning + season_results_nedrykning
}

average_results_nedrykning <- total_results_nedrykning / 1000
points_nedrykning <- (average_results_nedrykning[, "Wins"] * 3) + average_results_nedrykning[, "Draws"]
simulated_points_nedrykning <- data.frame(Team = TeamNames_Nedrykning, SimulatedPoints = round(points_nedrykning))

# ---- 8. Kombiner grundspil og slutspil for at få den estimerede endelige stilling ----
final_standings_mester <- initial_points_mester %>%
  left_join(simulated_points_mester, by = "Team") %>%
  mutate(FinalPoints = TotalPoints + SimulatedPoints) %>%
  arrange(desc(FinalPoints))

final_standings_nedrykning <- initial_points_nedrykning %>%
  left_join(simulated_points_nedrykning, by = "Team") %>%
  mutate(FinalPoints = TotalPoints + SimulatedPoints) %>%
  arrange(desc(FinalPoints))

# ---- 9. Udskriv endelige tabeller ----
print("Mesterskabsspil:")
print(final_standings_mester)

print("Nedrykningsspil:")
print(final_standings_nedrykning)

