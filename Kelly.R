#Kelly
set.seed(2025)

SL <- read_excel("/Users/philipstrommen/Desktop/Bachelor/Superliga_slut.xlsx")

head(SL)
SL<-data.frame(SL %>% filter(Season == "2024/2025"))
M<-nrow(SL)


TeamNames<-unique(SL$Home)
Index<-1:length(TeamNames)
Goals<-1:(2*nrow(SL)) 
alfa<-gamma<-rep(0,length(TeamNames))
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

#Estimate prameters in the independet Poisson-mode
testGLM<-glm(Goals ~ DesignMatrix[,c(-1,-3,-15)], family = poisson(link = "log"))
# 1: intercept, 2: hjemmebanefordel, 3: første holds angrebsparameter, 15: første holds forsvarsparameter

#These are now the true parameters
betaALL<-testGLM$coefficients[1]
delta<-testGLM$coefficients[2]
alfa[2:length(TeamNames)]<-testGLM$coefficients[3:(length(TeamNames)+1)]
gamma[2:length(TeamNames)]<-testGLM$coefficients[(length(TeamNames)+2):(2*length(TeamNames))]

# Calculate home win/dram/Away win probabilities for a specific match
HDAprob<- function(Home, Away, betaALL,delta,alfa,gamma) {
  nMatches<-length(Home)
  pD<-pH<-pA<-rep(0,nMatches)
  MaxGoals<-10
  for (i in 1:nMatches){
    lamH<-exp(betaALL+delta+alfa[Index[TeamNames==Home[i]]]-gamma[Index[TeamNames==Away[i]]])
    lamA<-exp(betaALL+alfa[Index[TeamNames==Away[i]]]-gamma[Index[TeamNames==Home[i]]])
    pMatrix<-dpois(0:MaxGoals,lamH)%*%t(dpois(0:MaxGoals,lamA))
    pD[i]<-sum(diag(pMatrix))
    pH[i]<-sum(pMatrix[lower.tri(pMatrix)])
    pA[i]<-sum(pMatrix[upper.tri(pMatrix)])
    scale<-pH[i]+pD[i]+pA[i]
    pH[i]<-pH[i]/scale; pD[i]<-pD[i]/scale; pA[i]<-pA[i]/scale
  }
  return(cbind(pH,pD,pA))
  
}


Odds<-matrix(0,nrow=nrow(SL),ncol=3)
HDAprob(SL[i,6], SL[i,7],betaALL,delta,alfa,gamma)

cut<-0.035  #Bookmaker's profit margin

#Bookmakers set odds according to true probabilities
for (i in 1:nrow(SL)){
  temp<-HDAprob(SL[i,6], SL[i,7],betaALL,delta,alfa,gamma)
  Odds[i,]<-1 / (temp * (1 + cut))
}


#Simulate seasons and betting strategies

nSeasons<-1000

RandomWealth<-Wealth<-ExpectedWealth<-matrix(nrow(SL),ncol=nrow(SL)+1,nrow=nSeasons)

lam<-SimGoals<-rep(0,2*nrow(SL))

lam<- exp(DesignMatrix%*%c(betaALL,delta,alfa,gamma))
NEWalfa<-NEWgamma<-rep(0,length(TeamNames))

Probs<-Temp<-rep(0,3)

Begin<-as.vector(c(betaALL,delta,alfa[2:length(TeamNames)],gamma[2:length(TeamNames)]))

for (k in 1:nSeasons){
  
  #Simulate a new season with the true parameters  
  SimGoals<-rpois(length(lam),lam)
  
  #Estimate parameters from the newly simulated season
  
  NEWtestGLM<-glm(SimGoals ~ DesignMatrix[,c(-1,-3,-15)],start=Begin, family = poisson(link = "log"))
  
  
  NEWbetaALL<-NEWtestGLM$coefficients[1]
  NEWdelta<-NEWtestGLM$coefficients[2]
  NEWalfa[2:length(TeamNames)]<-NEWtestGLM$coefficients[3:(length(TeamNames)+1)]
  NEWgamma[2:length(TeamNames)]<-NEWtestGLM$coefficients[(length(TeamNames)+2):(2*length(TeamNames))]
  
  
  Resample1<-FALSE #Bet w/ new estimate on a new season simulated with new estimate
  Resample2<-TRUE #Bet w/ new estimate on a season simulated with true parameter
  # If these are both false: Bet w/ new estimate the season that gave this new estimate
  
  wNew<-0
  
  for (i in 1:nrow(SL)){
    lamH<-wNew*exp(NEWbetaALL+NEWdelta+NEWalfa[Index[TeamNames==SL[i,6]]]-NEWgamma[Index[TeamNames==SL[i,7]]])+(1-wNew)*exp(betaALL+delta+alfa[Index[TeamNames==SL[i,6]]]-gamma[Index[TeamNames==SL[i,7]]])
    lamA<-wNew*exp(NEWbetaALL+NEWalfa[Index[TeamNames==SL[i,7]]]-NEWgamma[Index[TeamNames==SL[i,6]]])+(1-wNew)*exp(betaALL+alfa[Index[TeamNames==SL[i,7]]]-gamma[Index[TeamNames==SL[i,6]]])
    SimGoals[c(2*i-1,2*i)]<-rpois(2,c(lamH,lamA))
  }
  
  
  Ai<-2*(1:nrow(SL))
  Hi<-Ai-1
  AllProbs<-HDAprob(SL[,6],SL[,7],NEWbetaALL,NEWdelta,NEWalfa,NEWgamma)
  AllResults<-(SimGoals[Hi] > SimGoals[Ai])+2*(SimGoals[Hi] == SimGoals[Ai])+3*(SimGoals[Hi] < SimGoals[Ai])
  
  for(j in 1:nrow(SL)) {
    Probs<-AllProbs[j,]
    Result<-AllResults[j]
    # benchmark, sanity check: Bet 1 uniformly randomly 
    RandomBet<-floor(3*runif(1))+1
    RandomWin<-(Odds[j,RandomBet]*(Result==RandomBet)-1)
    RandomWealth[k,j+1]<-RandomWealth[k,j]+RandomWin
    # Bet Kelly on the result with the highest expected return -- if that is >0, else don't bet
    Temp<-Probs*Odds[j,]
    Kelly<-(Probs*(Odds[j,]-1)-(1-Probs))/(Odds[j,]-1)
    Faster<-max(Temp)
    print(Faster)
    NetWin<-0
    ExpectedWealth[k,j+1]<-ExpectedWealth[k,j]
    if(Faster > 1){ 
      Bet<-which.max(Temp)
      Size <- Wealth[k,j] * Kelly[Bet]
      # Size<-1
      NetWin<- (Odds[j,Bet]*(Result==Bet)-1)*Size 
      #How much we think (assuming our model is correct) our wealth will increase
      ExpectedWealth[k,j+1]<-ExpectedWealth[k,j+1]+(Faster-1)*Size 
    }
    Wealth[k,j+1]<-Wealth[k,j]+NetWin
    
  }
  
}


RunningWealth<-RunningRandomWealth<-matrix(0,nrow=3,ncol=nrow(SL)+1)
RunningExpectedWealth<-matrix(0,nrow=1,ncol=nrow(SL)+1)

#Calculate average wealth after i (1...M) matches -- and 2.5% and 97.5% fractiles

for (i in 1:(nrow(SL)+1)){
  dummy1<-sort(Wealth[,i])
  dummy2<-sort(RandomWealth[,i])
  RunningWealth[,i]<-c(dummy1[round(0.025*nSeasons)], mean(Wealth[,i]),dummy1[round(0.975*nSeasons)])
  RunningExpectedWealth[,i]<- mean(ExpectedWealth[,i])
  RunningRandomWealth[,i]<-c(dummy2[round(0.025*nSeasons)], mean(RandomWealth[,i]),dummy2[round(0.975*nSeasons)])
}


# Tilføj vækstrater 

RunningWealth<-matrix(0,nrow=2,ncol=nrow(SL)+1)

for (i in 1:(nrow(SL)+1)){
  RunningWealth[,i]<-c(sd(Wealth[,i]), mean(Wealth[,i]))
}


# Growth rates
t<-1:193
t2<-1:192
# RunningWealth
LogRunningWealth <- log(RunningWealth)
GrowthRatesWealth <- ((LogRunningWealth[2,]-LogRunningWealth[2,1])/t) 

#logaritmisk formue
log_wealth <- log(RunningWealth[2,])

# Plot log-formue
plot(0:nrow(SL), log_wealth, type='l', col="darkgreen", lwd=2,
     xlab="Kampnummer", ylab="log(Formue)", 
     main="Logaritmisk formue ved Kelly-strategi")

mean(diff(log_wealth))

#væksrate
# Beregn log-formue
log_wealth <- log(RunningWealth[2,])

# Beregn vækstrater (asymptotisk gennemsnit pr. kamp)
growth_rates <- (log_wealth - log_wealth[1]) / (0:(length(log_wealth)-1))
growth_rates[1] <- NA  # Undgå division med nul

# Gennemsnitlig vækstrate
avg_growth <- mean(growth_rates, na.rm = TRUE)

# Plot vækstrater
plot(0:nrow(SL), growth_rates, type='l', lwd=2, col="deepskyblue2",
     xlab="Kampnummer", ylab="Vækstrate",
     main="Vækstrate baseret på log-formue ved Kelly-strategi")
abline(h=avg_growth, col="red", lwd=2, lty=2)

# Tekst på grafen - rykket højere op
text(100, avg_growth + 0.00118, "Gennemsnitlig vækstrate", adj=0, cex=0.8)
text(100, avg_growth + 0.00090, paste("= ", round(avg_growth, 5)), adj=0, cex=0.8)

# Tilføj undertekst
mtext(paste(nSeasons, "simuleringer ud fra", M, "kampe"), side=3, adj=0, cex=0.8)


#ekspempel
# Parametre for én kamp
p <- 0.5                 # sandsynlighed for gevinst
odds <- 1.25             # UK-odds (dvs. "odds_decimal - 1")

# Kelly-funktion: log-vækstrate som funktion af alpha
kelly_growth <- function(alpha) {
  p * log((1 - alpha) + alpha * (odds + 1)) + 
    (1 - p) * log(1 - alpha)
}

# Find maksimum
opt <- optimize(kelly_growth, interval = c(0, 0.25), maximum = TRUE)

# Sekvens af alpha-værdier
alpha_vals <- seq(0, 0.25, by = 0.001)
growth_vals <- sapply(alpha_vals, kelly_growth)

# Plot
plot(alpha_vals, growth_vals, type = "l", lwd = 2, col = "black",
     xlab = "Andel af formue væddet", ylab = "Vækstrate",
     main = "Asymptotisk vækstrate af formue")

# Tilføj linjer og tekst
abline(h = 0, col = "gray60", lty = 2)                             # Neutral vækstrate (0)
abline(h = opt$objective, col = "deepskyblue2", lwd = 2)          # Kelly-vækstrate
abline(v = opt$maximum, col = "deepskyblue2", lwd = 2)            # Kelly-andel

# Tekstforklaring
text(0.2, 0.0055, "Kelly vækstrate", col = "deepskyblue2", cex = 0.8)
text(0.2, 0.0035, "Vækstrate", col = "black", cex = 0.8)
text(opt$maximum, -0.008, "Kelly andel", col = "deepskyblue2", cex = 0.8)

