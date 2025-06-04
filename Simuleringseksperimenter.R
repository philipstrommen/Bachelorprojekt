#Simuleringseksperimenter
#Indledende
SL <- read_excel("/Users/philipstrommen/Desktop/Bachelor/Superliga_slut.xlsx")
SL <- SL %>% select(-c(BFECH, BFECD, BFECA))
SL<-SL %>% filter(Season == "2024/2025")
M<-nrow(SL)

TeamNames<-unique(SL$Home)
Index<-1:length(TeamNames)
Goals<-1:(2*nrow(SL)) 
betaOFF<-betaDEF<-rep(0,length(TeamNames))
DesignMatrix<-matrix(0,nrow=2*nrow(SL),ncol=2+2*length(TeamNames))

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

#Estimate parameters in the independent Poisson-mode 
testGLM<-glm(Goals ~ DesignMatrix[,c(-1,-3,-15)], family = poisson(link = "log"))
# [,c(1,3,23)] fjernes
# 1: intercept, 2: hjemmebanefordel, 3: første holds angrebsparameter, 15: første holds forsvarsparameter

#These are now the true parameters
betaALL<-testGLM$coefficients[1]
betaHOME<-testGLM$coefficients[2]
betaOFF[2:length(TeamNames)]<-testGLM$coefficients[3:(length(TeamNames)+1)]
betaDEF[2:length(TeamNames)]<-testGLM$coefficients[(length(TeamNames)+2):(2*length(TeamNames))]

# Calculate home win/dram/Away win probabilities for a specific match
HDAprob<- function(Home, Away, betaALL,betaHOME,betaOFF,betaDEF) {
  lamH<-exp(betaALL+betaHOME+betaOFF[Index[TeamNames==Home]]-betaDEF[Index[TeamNames==Away]])
  lamA<-exp(betaALL+betaOFF[Index[TeamNames==Away]]-betaDEF[Index[TeamNames==Home]])
  pH<-pD<-0
  
  for (i in 0:20){
    pD<-pD+dpois(i,lamH)*dpois(i,lamA)
    for (j in 0:i) if (j < i) pH<-pH+dpois(i,lamH)*dpois(j,lamA)
  }
  
  return(c(pH,pD,1-pH-pD))
  
}

SL<-cbind(SL,matrix(0,nrow=nrow(SL),ncol=3))
N<-ncol(SL)



cut<-0.035 #Bookmaker's profit margin

#Bookmakers set odds according to true probabilities
for (i in 1:nrow(SL)) {
  temp <- HDAprob(SL[i,6], SL[i,7], betaALL, betaHOME, betaOFF, betaDEF)
  SL[i, (N-2):N] <- 1 / (temp * (1 + cut))
}


#Strategi 1
set.seed(2025)
# Strategi 1

nSeasons<-1000

RandomWealth<-Wealth<-ExpectedWealth<-matrix(nrow(SL),ncol=nrow(SL)+1,nrow=nSeasons)

lam<-SimGoals<-rep(0,2*nrow(SL))

Probs<-matrix(0,nrow=nrow(SL),ncol=3)
NetWin<-Result<-Bet<-rep(0,ncol(SL)) 
lam<- exp(DesignMatrix%*%c(betaALL,betaHOME,betaOFF,betaDEF)) # \lambda=exp(X\beta)
NEWbetaOFF<-NEWbetaDEF<-rep(0,length(TeamNames))

Begin<-as.vector(c(betaALL,betaHOME,betaOFF[2:12],betaDEF[2:12]))
OptTime<-PFTime<-rep(0,nSeasons)

for (k in 1:nSeasons){
  
  #Simuler nye mål til ny sæson
  SimGoals<-rpois(length(lam),lam)
  
  #. estimer nye parametre fra den nye sæson
  
  OptTime[k]<-system.time(NEWtestGLM<-glm(SimGoals ~ DesignMatrix[,c(-1,-3,-15)],start=Begin, family = poisson(link = "log")))[3]
  NEWbetaALL<-NEWtestGLM$coefficients[1]
  NEWbetaHOME<-NEWtestGLM$coefficients[2]
  NEWbetaOFF[2:length(TeamNames)]<-NEWtestGLM$coefficients[3:(length(TeamNames)+1)]
  NEWbetaDEF[2:length(TeamNames)]<-NEWtestGLM$coefficients[(length(TeamNames)+2):(2*length(TeamNames))]
  
  Resample1<-FALSE #Bet w/ new estimate on a new season simulated with new estimate
  Resample2<-FALSE #Bet w/ new estimate on a season simulated with true parameter
  # If these are both false: Bet w/ new estimate the season that gave this new estimate
  
  
  if (Resample1){
    for (i in 1:nrow(SL)){
      lamH<-exp(NEWbetaALL+NEWbetaHOME+NEWbetaOFF[Index[TeamNames==SL[i,6]]]-NEWbetaDEF[Index[TeamNames==SL[i,7]]])
      lamA<-exp(NEWbetaALL+NEWbetaOFF[Index[TeamNames==SL[i,7]]]-NEWbetaDEF[Index[TeamNames==SL[i,6]]])
      SimGoals[c(2*i-1,2*i)]<-rpois(2,c(lamH,lamA))
    }
  }
  
  if (Resample2){
    for (i in 1:nrow(SL)){
      lamH<-exp(betaALL+betaHOME+betaOFF[Index[TeamNames==SL[i,6]]]-betaDEF[Index[TeamNames==SL[i,7]]])
      lamA<-exp(betaALL+betaOFF[Index[TeamNames==SL[i,7]]]-betaDEF[Index[TeamNames==SL[i,6]]])
      SimGoals[c(2*i-1,2*i)]<-rpois(2,c(lamH,lamA))
    }
  }
  
  dummy<-0
  
  PFTime[k]<-system.time( 
    for(j in 1:nrow(SL)) {
      Probs<-HDAprob(SL[j,6],SL[j,7],NEWbetaALL,NEWbetaHOME,NEWbetaOFF,NEWbetaDEF)
      Result<-(SimGoals[2*j-1] > SimGoals[2*j]) + 2*(SimGoals[2*j-1] == SimGoals[2*j])+3*(SimGoals[2*j-1] < SimGoals[2*j])
      # Bet 1 on the result with the highest expected return -- it that is >0, else don't bet
      Bet<-which.max((Probs*SL[j,(N-2):N]-1))*(max(Probs*SL[j,(N-2):N])-1 > 0)
      # benchmark, sanity check: Bet 1 uniformly randomly 
      RandomBet<-floor(3*runif(1))+1
      NetWin<- (Bet > 0)*(SL[j,N-3+Bet]*(Result==Bet)-1) 
      Wealth[k,j+1]<-Wealth[k,j]+NetWin
      #How much we think (assuming our model is correct) our wealth will increase
      ExpectedWealth[k,j+1]<-ExpectedWealth[k,j]+(Bet > 0)*(max(Probs*SL[j,(N-2):N]-1)) 
      RandomWin<-(SL[j,N-3+RandomBet]*(Result==RandomBet)-1)
      RandomWealth[k,j+1]<-RandomWealth[k,j]+RandomWin
    }
  )[3]
  
  print(k)
}

RunningWealth<-RunningRandomWealth<-matrix(0,nrow=1,ncol=nrow(SL)+1)
RunningExpectedWealth<-matrix(0,nrow=1,ncol=nrow(SL)+1)

#Den gennemsnitlige formue efter i kampe i de 1000 sæsonner tages her
for (i in 1:(nrow(SL)+1)){
  dummy1<-sort(Wealth[,i])
  dummy2<-sort(RandomWealth[,i])
  RunningWealth[,i]<-c(mean(Wealth[,i]))
  RunningExpectedWealth[,i]<- mean(ExpectedWealth[,i])
  RunningRandomWealth[,i]<-c(mean(RandomWealth[,i]))
}


# Farver
col_real <- "forestgreen"
col_expected <- "darkorange"
col_random <- "steelblue"


# Y-akse spænd
yrange <- c(min(RunningWealth, RunningRandomWealth), max(RunningWealth, RunningRandomWealth))

# Plot
main_title <- paste(nSeasons, "simulationer ud fra", M, "kampe")

plot(0:nrow(SL), RunningWealth[1,], type = 'l',
     xlab = "Kampnummer", ylab = "Formue",
     xlim = c(0, 250), ylim = yrange,
     lwd = 2, col = col_real, main = main_title, cex.main = 1.1)

# Tilføj forventet formue
lines(0:nrow(SL), RunningExpectedWealth[1,], col = col_expected, lwd = 2)

# Tilføj tilfældig formue
lines(0:nrow(SL), RunningRandomWealth[1,], col = col_random, lwd = 2)

# Tilføj hældninger
slope_real <- round(mean(diff(RunningWealth[1,], 1)), 2)
slope_expected <- round(mean(diff(RunningExpectedWealth[1,], 1)), 2)
slope_random <- round(mean(diff(RunningRandomWealth[1,], 1)), 2)

# Legende
legend("topleft", legend = c(
  paste("Formue (hældning =", slope_real, ")"),
  paste("Forventet formue (hældning =", slope_expected, ")"),
  paste("Tilfældig formue (hældning =", slope_random, ")")
), col = c(col_real, col_expected, col_random), lwd = 2, cex = 0.8, bty = "n")



#Strategi 2
set.seed(2025)
for (k in 1:nSeasons){
  
  #Simulate a new season with the true parameters  
  SimGoals<-rpois(length(lam),lam)
  
  #Estimate paramaters from the newly simulated season
  
  OptTime[k]<-system.time(NEWtestGLM<-glm(SimGoals ~ DesignMatrix[,c(-1,-3,-15)],start=Begin, family = poisson(link = "log")))[3]
  NEWbetaALL<-NEWtestGLM$coefficients[1]
  NEWbetaHOME<-NEWtestGLM$coefficients[2]
  NEWbetaOFF[2:length(TeamNames)]<-NEWtestGLM$coefficients[3:(length(TeamNames)+1)]
  NEWbetaDEF[2:length(TeamNames)]<-NEWtestGLM$coefficients[(length(TeamNames)+2):(2*length(TeamNames))]
  
  Resample1<-FALSE #Bet w/ new estimate on a new season simulated with new estimate
  Resample2<-TRUE #Bet w/ new estimate on a season simulated with true parameter
  # If these are both false: Bet w/ new estimate the season that gave this new estimate
  
  
  if (Resample1){
    for (i in 1:nrow(SL)){
      lamH<-exp(NEWbetaALL+NEWbetaHOME+NEWbetaOFF[Index[TeamNames==SL[i,6]]]-NEWbetaDEF[Index[TeamNames==SL[i,7]]])
      lamA<-exp(NEWbetaALL+NEWbetaOFF[Index[TeamNames==SL[i,7]]]-NEWbetaDEF[Index[TeamNames==SL[i,6]]])
      SimGoals[c(2*i-1,2*i)]<-rpois(2,c(lamH,lamA))
    }
  }
  
  if (Resample2){
    for (i in 1:nrow(SL)){
      lamH<-exp(betaALL+betaHOME+betaOFF[Index[TeamNames==SL[i,6]]]-betaDEF[Index[TeamNames==SL[i,7]]])
      lamA<-exp(betaALL+betaOFF[Index[TeamNames==SL[i,7]]]-betaDEF[Index[TeamNames==SL[i,6]]])
      SimGoals[c(2*i-1,2*i)]<-rpois(2,c(lamH,lamA))
    }
  }
  
  dummy<-0
  
  PFTime[k]<-system.time( 
    for(j in 1:nrow(SL)) {
      Probs<-HDAprob(SL[j,6],SL[j,7],NEWbetaALL,NEWbetaHOME,NEWbetaOFF,NEWbetaDEF)
      Result<-(SimGoals[2*j-1] > SimGoals[2*j]) + 2*(SimGoals[2*j-1] == SimGoals[2*j])+3*(SimGoals[2*j-1] < SimGoals[2*j])
      # Bet 1 on the result with the highest expected return -- it that is >0, else don't bet
      Bet<-which.max((Probs*SL[j,(N-2):N]-1))*(max(Probs*SL[j,(N-2):N])-1 > 0)
      # benchmark, sanity check: Bet 1 uniformly randomly 
      RandomBet<-floor(3*runif(1))+1
      NetWin<- (Bet > 0)*(SL[j,N-3+Bet]*(Result==Bet)-1) 
      Wealth[k,j+1]<-Wealth[k,j]+NetWin
      #How much we think (assuming our model is correct) our wealth will increase
      ExpectedWealth[k,j+1]<-ExpectedWealth[k,j]+(Bet > 0)*(max(Probs*SL[j,(N-2):N]-1)) 
      RandomWin<-(SL[j,N-3+RandomBet]*(Result==RandomBet)-1)
      RandomWealth[k,j+1]<-RandomWealth[k,j]+RandomWin
    }
  )[3]
  
  print(k)
}

RunningWealth<-RunningRandomWealth<-matrix(0,nrow=3,ncol=nrow(SL)+1)
RunningExpectedWealth<-matrix(0,nrow=1,ncol=nrow(SL)+1)

#Calculate average weath after i (1...M) matches -- and 2.5% and 97.5% fractiles
for (i in 1:(nrow(SL)+1)){
  dummy1<-sort(Wealth[,i])
  dummy2<-sort(RandomWealth[,i])
  RunningWealth[,i]<-c(mean(Wealth[,i]))
  RunningExpectedWealth[,i]<- mean(ExpectedWealth[,i])
  RunningRandomWealth[,i]<-c(mean(RandomWealth[,i]))
}


# Farver
col_real <- "forestgreen"
col_expected <- "darkorange"
col_random <- "steelblue"

# Y-akse spænd
yrange <- c(min(RunningWealth, RunningRandomWealth,RunningExpectedWealth), max(RunningWealth, RunningRandomWealth,RunningExpectedWealth))

# Titel
main_title <- paste(nSeasons, "simulationer ud fra", M, "kampe")

# Plot formue (gennemsnit) baseret på runde 1 i RunningWealth
plot(0:nrow(SL), RunningWealth[1,], type = 'l',
     xlab = "Kampnummer", ylab = "Formue",
     xlim = c(0, 250), ylim = yrange,
     lwd = 2, col = col_real, main = main_title, cex.main = 1.1)

# Tilføj forventet formue
lines(0:nrow(SL), RunningExpectedWealth[1,], col = col_expected, lwd = 2)

# Tilføj tilfældig formue
lines(0:nrow(SL), RunningRandomWealth[1,], col = col_random, lwd = 2)

# Beregn hældninger
slope_real <- round(mean(diff(RunningWealth[1,])), 2)
slope_expected <- round(mean(diff(RunningExpectedWealth[1,])), 2)
slope_random <- round(mean(diff(RunningRandomWealth[1,])), 2)

# Tilføj legende
legend("topleft", legend = c(
  paste("Formue (hældning =", slope_real, ")"),
  paste("Forventet formue (hældning =", slope_expected, ")"),
  paste("Tilfældig formue (hældning =", slope_random, ")")
), col = c(col_real, col_expected, col_random), lwd = 2, cex = 0.8, bty = "n")


#Strategi 3
for (k in 1:nSeasons){
  
  #Simulate a new season with the true parameters  
  SimGoals<-rpois(length(lam),lam)
  
  #Estimate paramaters from the newly simulated season
  
  OptTime[k]<-system.time(NEWtestGLM<-glm(SimGoals ~ DesignMatrix[,c(-1,-3,-15)],start=Begin, family = poisson(link = "log")))[3]
  NEWbetaALL<-NEWtestGLM$coefficients[1]
  NEWbetaHOME<-NEWtestGLM$coefficients[2]
  NEWbetaOFF[2:length(TeamNames)]<-NEWtestGLM$coefficients[3:(length(TeamNames)+1)]
  NEWbetaDEF[2:length(TeamNames)]<-NEWtestGLM$coefficients[(length(TeamNames)+2):(2*length(TeamNames))]
  
  Resample1<-TRUE #Bet w/ new estimate on a new season simulated with new estimate
  Resample2<-FALSE #Bet w/ new estimate on a season simulated with true parameter
  # If these are both false: Bet w/ new estimate the season that gave this new estimate
  
  
  if (Resample1){
    for (i in 1:nrow(SL)){
      lamH<-exp(NEWbetaALL+NEWbetaHOME+NEWbetaOFF[Index[TeamNames==SL[i,6]]]-NEWbetaDEF[Index[TeamNames==SL[i,7]]])
      lamA<-exp(NEWbetaALL+NEWbetaOFF[Index[TeamNames==SL[i,7]]]-NEWbetaDEF[Index[TeamNames==SL[i,6]]])
      SimGoals[c(2*i-1,2*i)]<-rpois(2,c(lamH,lamA))
    }
  }
  
  if (Resample2){
    for (i in 1:nrow(SL)){
      lamH<-exp(betaALL+betaHOME+betaOFF[Index[TeamNames==SL[i,6]]]-betaDEF[Index[TeamNames==SL[i,7]]])
      lamA<-exp(betaALL+betaOFF[Index[TeamNames==SL[i,7]]]-betaDEF[Index[TeamNames==SL[i,6]]])
      SimGoals[c(2*i-1,2*i)]<-rpois(2,c(lamH,lamA))
    }
  }
  
  dummy<-0
  
  PFTime[k]<-system.time( # It must be possible to speed up this by vectorization
    for(j in 1:nrow(SL)) {
      Probs<-HDAprob(SL[j,6],SL[j,7],NEWbetaALL,NEWbetaHOME,NEWbetaOFF,NEWbetaDEF)
      Result<-(SimGoals[2*j-1] > SimGoals[2*j]) + 2*(SimGoals[2*j-1] == SimGoals[2*j])+3*(SimGoals[2*j-1] < SimGoals[2*j])
      # Bet 1 on the result with the highest expected return -- it that is >0, else don't bet
      Bet<-which.max((Probs*SL[j,(N-2):N]-1))*(max(Probs*SL[j,(N-2):N])-1 > 0)
      # benchmark, sanity check: Bet 1 uniformly randomly 
      RandomBet<-floor(3*runif(1))+1
      NetWin<- (Bet > 0)*(SL[j,N-3+Bet]*(Result==Bet)-1) 
      Wealth[k,j+1]<-Wealth[k,j]+NetWin
      #How much we think (assuming our model is correct) our wealth will increase
      ExpectedWealth[k,j+1]<-ExpectedWealth[k,j]+(Bet > 0)*(max(Probs*SL[j,(N-2):N]-1)) 
      RandomWin<-(SL[j,N-3+RandomBet]*(Result==RandomBet)-1)
      RandomWealth[k,j+1]<-RandomWealth[k,j]+RandomWin
    }
  )[3]
  
  print(k)
}

RunningWealth<-RunningRandomWealth<-matrix(0,nrow=1,ncol=nrow(SL)+1)
RunningExpectedWealth<-matrix(0,nrow=1,ncol=nrow(SL)+1)

#Calculate average weath after i (1...M) matches -- and 2.5% and 97.5% fractiles
for (i in 1:(nrow(SL)+1)){
  dummy1<-sort(Wealth[,i])
  dummy2<-sort(RandomWealth[,i])
  RunningWealth[,i]<-c(mean(Wealth[,i]))
  RunningExpectedWealth[,i]<- mean(ExpectedWealth[,i])
  RunningRandomWealth[,i]<-c(mean(RandomWealth[,i]))
}


col_real <- "forestgreen"
col_expected <- "darkorange"
col_random <- "steelblue"

yrange<-c(min(RunningWealth,RunningRandomWealth,RunningExpectedWealth),max(RunningWealth,RunningRandomWealth,RunningExpectedWealth))

# Titel
title <- paste(nSeasons, "simulationer ud fra", M, "kampe")

# Plot formue (middelværdi)
plot(0:nrow(SL), RunningWealth[1,], type = 'l',
     xlab = "Kampnummer", ylab = "Formue",
     xlim = c(0, 250), ylim = yrange,
     lwd = 2, col = col_real, main = title, cex.main = 1.1)

# Forventet formue
lines(0:nrow(SL), RunningExpectedWealth[1,], col = col_expected, lwd = 2)

# Tilfældig formue
lines(0:nrow(SL), RunningRandomWealth[1,], col = col_random, lwd = 2)

# Beregn hældninger fra midterlinjer
slope_real <- round(mean(diff(RunningWealth[1,])), 2)
slope_expected <- round(mean(diff(RunningExpectedWealth[1,])), 2)
slope_random <- round(mean(diff(RunningRandomWealth[1,])), 2)

# Tilføj legende
legend("topleft", legend = c(
  paste("Formue (hældning =", slope_real, ")"),
  paste("Forventet formue (hældning =", slope_expected, ")"),
  paste("Tilfældig formue (hældning =", slope_random, ")")
), col = c(col_real, col_expected, col_random), lwd = 2, cex = 0.8, bty = "n")

