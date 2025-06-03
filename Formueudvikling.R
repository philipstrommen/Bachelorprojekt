#Formueudvikling
#24/25
DNK_17_3 <- read_excel("/Users/philipstrommen/Desktop/Bachelor/DNK_17_3.xlsx")
SL<-DNK_17_3 %>% filter(Season == "2023/2024")
M<-nrow(SL)




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


#These are now the true parameters
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

Alle_parametre24 <- data.frame(
  Hold = c("Midtjylland", "Lyngby", "Aarhus", "Sonderjyske", "Silkeborg", "Nordsjaelland",
           "Viborg", "Vejle", "Randers FC", "Brondby", "FC Copenhagen", "Aalborg"),
  alfa = c(0.00000000, -0.53434679, -0.38783712, -0.60235150, -0.45225075, -0.05839367,
           -0.57529286, -0.78255705, -0.49516691, -0.05566519, 0.01018012, -0.88447128),
  gamma = c(0.00000000, -0.31925572, -0.01351351, -0.21277803, -0.09000640, 0.24456837,
            -0.21565067, 0.09200298, -0.23468115, 0.21514084, 0.12020742, -0.42706744)
)




# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2024/2025")



ProbTable<- function(Alle_parametre,HT,AT){
  teams<-Alle_parametre$Hold
  delta <- delta
  betaALL <- betaALL
  a<- which(teams==HT)
  b<-which(teams==AT)
  #a/A svarer til hjemmemål, b/B svarer til udemål
  lambda_a<-exp(betaALL+Alle_parametre[a,]$alfa-Alle_parametre[b,]$gamma+delta)
  lambda_b<-exp(betaALL+Alle_parametre[b,]$alfa-Alle_parametre[a,]$gamma)
  A<-as.numeric()
  B<-as.numeric()
  for (i in 0:6){
    A[(i+1)]<-dpois(i,lambda_a)
    B[(i+1)]<-dpois(i,lambda_b)
  }
  A[8]<-1-sum(A[1:7])
  B[8]<-1-sum(B[1:7])
  name<-c("0","1","2","3","4","5","6","7+")
  zero<-mat.or.vec(8,1)
  C<-data.frame(row.names=name)
  for (j in 1:8){
    for (k in 1:8) {
      C[j,k]<-A[k]*B[j] # simultan fordeling
    }
  }
  colnames(C)<- name
  return(round(C*100,2))
}



AllProb <- function(Alle_parametre,ProbTable) {
  teams<-Alle_parametre$Hold
  delta<-delta
  n <- length(teams)
  name<-c("Hjemmehold","Udehold","Hjemmesejr","Uafgjort","Udesejr")
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {#hold kan ikke spille mod sig selv
        #ProbTable finder ssh-tabeller for mulige slutresultater
        ssh<-ProbTable(Alle_parametre,teams[i],teams[j])
        #ResultProbs laver ssh-tabel om til samlede ssh'er for ude-,hjemmesejr og uafgjort
        ResultProbabilities<-ResultProbs(ssh)
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- ResultProbabilities$HomeWin
        C[row,4] <- ResultProbabilities$Draw
        C[row,5] <- ResultProbabilities$AwayWin
        row <- row + 1
      }
    }
  }
  colnames(C)<- name
  return(C)
}

ResultProbs<-function(probs){
  R<-matrix(0,3,1)
  n<-length(probs)
  for (i in 1:n){
    for (j in 1:n) {
      if (i>j){ #altså summen af indgangene under diagonalen=AW
        R[3]<-R[3]+probs[i,j]
      } else{ #summen af ssh'er i diagonaln=D
        if (i==j){
          R[2]<-R[2]+probs[i,j]
        }else { #summen af resterende, altså indgangene over diagonalen=HW
          R[1]<-R[1]+probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1],Draw=R[2],AwayWin=R[3]))
}

AllSSH <- AllProb(Alle_parametre24, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]


# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1, strategi[,5], 0)


zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
formue1 <- plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)


#23/24
DNK_17_3 <- read_excel("/Users/philipstrommen/Desktop/Bachelor/DNK_17_3.xlsx")
SL<-DNK_17_3 %>% filter(Season == "2022/2023")
M<-nrow(SL)

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

# 1: intercept, 2: hjemmebanefordel, 3: første holds angrebsparameter, 15: første holds forsvarsparameter
qr(DesignMatrix[,c(-1,-3,-15)])$rank
head(DesignMatrix)
head(SL)
summary(testGLM)

#These are now the true parameters
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

Alle_parametre_ny <- data.frame(
  Hold = c("Midtjylland", "Lyngby", "Viborg", "FC Copenhagen", "Brondby", "Odense",
           "Aarhus", "Randers FC", "Hvidovre IF", "Vejle", "Silkeborg", "Nordsjaelland"),
  alfa = c(0.00000000, -0.5790059, -0.1686674, 0.1931593, -0.0000377, -0.1114784,
           -0.1967073, -0.2007359, -0.4634908, -0.4570078, -0.1905625, -0.0089727),
  gamma = c(0.00000000, -0.1947841, 0.1887942, 0.1298641, -0.2349347, -0.3234213,
            0.3076481, -0.1080498, -0.1197290, -0.3741151, -0.2345294, 0.1622380)
)





# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2023/2024") %>% slice(1:132)


ProbTable<- function(Alle_parametre,HT,AT){
  teams<-Alle_parametre$Hold
  delta <- delta
  betaALL <- betaALL
  a<- which(teams==HT)
  b<-which(teams==AT)
  #a/A svarer til hjemmemål, b/B svarer til udemål
  lambda_a<-exp(betaALL+Alle_parametre[a,]$alfa-Alle_parametre[b,]$gamma+delta)
  lambda_b<-exp(betaALL+Alle_parametre[b,]$alfa-Alle_parametre[a,]$gamma)
  A<-as.numeric()
  B<-as.numeric()
  for (i in 0:6){
    A[(i+1)]<-dpois(i,lambda_a)
    B[(i+1)]<-dpois(i,lambda_b)
  }
  A[8]<-1-sum(A[1:7])
  B[8]<-1-sum(B[1:7])
  name<-c("0","1","2","3","4","5","6","7+")
  zero<-mat.or.vec(8,1)
  C<-data.frame(row.names=name)
  for (j in 1:8){
    for (k in 1:8) {
      C[j,k]<-A[k]*B[j] # simultan fordeling
    }
  }
  colnames(C)<- name
  return(round(C*100,2))
}


AllProb <- function(Alle_parametre,ProbTable) {
  teams<-Alle_parametre$Hold
  delta<-delta
  n <- length(teams)
  name<-c("Hjemmehold","Udehold","Hjemmesejr","Uafgjort","Udesejr")
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {#hold kan ikke spille mod sig selv
        #ProbTable finder ssh-tabeller for mulige slutresultater
        ssh<-ProbTable(Alle_parametre,teams[i],teams[j])
        #ResultProbs laver ssh-tabel om til samlede ssh'er for ude-,hjemmesejr og uafgjort
        ResultProbabilities<-ResultProbs(ssh)
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- ResultProbabilities$HomeWin
        C[row,4] <- ResultProbabilities$Draw
        C[row,5] <- ResultProbabilities$AwayWin
        row <- row + 1
      }
    }
  }
  colnames(C)<- name
  return(C)
}

ResultProbs<-function(probs){
  R<-matrix(0,3,1)
  n<-length(probs)
  for (i in 1:n){
    for (j in 1:n) {
      if (i>j){ #altså summen af indgangene under diagonalen=AW
        R[3]<-R[3]+probs[i,j]
      } else{ #summen af ssh'er i diagonaln=D
        if (i==j){
          R[2]<-R[2]+probs[i,j]
        }else { #summen af resterende, altså indgangene over diagonalen=HW
          R[1]<-R[1]+probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1],Draw=R[2],AwayWin=R[3]))
}

AllSSH <- AllProb(Alle_parametre_ny, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]



# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1, strategi[,5], 0)


zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)


#22/23
DNK_17_3 <- read_excel("/Users/philipstrommen/Desktop/Bachelor/DNK_17_3.xlsx")
SL<-DNK_17_3 %>% filter(Season == "2021/2022")
M<-nrow(SL)

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

# 1: intercept, 2: hjemmebanefordel, 3: første holds angrebsparameter, 15: første holds forsvarsparameter
qr(DesignMatrix[,c(-1,-3,-15)])$rank
head(DesignMatrix)
head(SL)
summary(testGLM)

#These are now the true parameters
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

Alle_parametre_ny <- data.frame(
  Hold = c("Midtjylland", "Nordsjaelland", "Lyngby", "FC Copenhagen", "Aarhus", 
           "Silkeborg", "Aalborg", "Randers FC", "Brondby", "Horsens", 
           "Viborg", "Odense"),
  alfa = c(0.00000000, -0.53606344, -0.70392037, -0.09427796, -0.75396456,
           -0.07793063, -0.21214141, -0.47470751, -0.37087048, -0.82510627,
           -0.38343387, -0.36709742),
  gamma = c(0.00000000, -0.4237473, -0.6508888, 0.5679026, -0.3132380,
            -0.1011986, -0.2719169, -0.1762711, -0.1635539, -0.5350235,
            -0.3352567, -0.4229458)
)





# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2022/2023") %>% slice(1:132)


ProbTable<- function(Alle_parametre,HT,AT){
  teams<-Alle_parametre$Hold
  delta <- delta
  betaALL <- betaALL
  a<- which(teams==HT)
  b<-which(teams==AT)
  #a/A svarer til hjemmemål, b/B svarer til udemål
  lambda_a<-exp(betaALL+Alle_parametre[a,]$alfa-Alle_parametre[b,]$gamma+delta)
  lambda_b<-exp(betaALL+Alle_parametre[b,]$alfa-Alle_parametre[a,]$gamma)
  A<-as.numeric()
  B<-as.numeric()
  for (i in 0:6){
    A[(i+1)]<-dpois(i,lambda_a)
    B[(i+1)]<-dpois(i,lambda_b)
  }
  A[8]<-1-sum(A[1:7])
  B[8]<-1-sum(B[1:7])
  name<-c("0","1","2","3","4","5","6","7+")
  zero<-mat.or.vec(8,1)
  C<-data.frame(row.names=name)
  for (j in 1:8){
    for (k in 1:8) {
      C[j,k]<-A[k]*B[j] # simultan fordeling
    }
  }
  colnames(C)<- name
  return(round(C*100,2))
}


AllProb <- function(Alle_parametre,PProbTable) {
  teams<-Alle_parametre$Hold
  delta<-delta
  n <- length(teams)
  name<-c("Hjemmehold","Udehold","Hjemmesejr","Uafgjort","Udesejr")
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {#hold kan ikke spille mod sig selv
        #ProbTable finder ssh-tabeller for mulige slutresultater
        ssh<-PProbTable(Alle_parametre,teams[i],teams[j])
        #ResultProbs laver ssh-tabel om til samlede ssh'er for ude-,hjemmesejr og uafgjort
        ResultProbabilities<-ResultProbs(ssh)
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- ResultProbabilities$HomeWin
        C[row,4] <- ResultProbabilities$Draw
        C[row,5] <- ResultProbabilities$AwayWin
        row <- row + 1
      }
    }
  }
  colnames(C)<- name
  return(C)
}

ResultProbs<-function(probs){
  R<-matrix(0,3,1)
  n<-length(probs)
  for (i in 1:n){
    for (j in 1:n) {
      if (i>j){ #altså summen af indgangene under diagonalen=AW
        R[3]<-R[3]+probs[i,j]
      } else{ #summen af ssh'er i diagonaln=D
        if (i==j){
          R[2]<-R[2]+probs[i,j]
        }else { #summen af resterende, altså indgangene over diagonalen=HW
          R[1]<-R[1]+probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1],Draw=R[2],AwayWin=R[3]))
}

AllSSH <- AllProb(Alle_parametre_ny, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]



# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1, strategi[,5], 0)


zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)

#21/22
DNK_17_3 <- read_excel("/Users/philipstrommen/Desktop/Bachelor/DNK_17_3.xlsx")
SL<-DNK_17_3 %>% filter(Season == "2020/2021")
M<-nrow(SL)

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

# 1: intercept, 2: hjemmebanefordel, 3: første holds angrebsparameter, 15: første holds forsvarsparameter
qr(DesignMatrix[,c(-1,-3,-15)])$rank
head(DesignMatrix)
head(SL)
summary(testGLM)

#These are now the true parameters
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


Alle_parametre_ny <- data.frame(
  Hold = c("Sonderjyske", "Silkeborg", "Viborg", "Brondby", "Odense",
           "Aarhus", "Midtjylland", "FC Copenhagen", "Randers FC",
           "Vejle", "Aalborg", "Nordsjaelland"),
  alfa = c(0.00000000, -0.38094841, -0.18452865, 0.30727969, -0.14407445,
           0.11434264, 0.27626428, 0.40070051, 0.00412151,
           -0.06452131, -0.04363819, 0.21192363),
  gamma = c(0.00000000, -0.16769812, -0.25119570, 0.32490564, 0.22418742,
            0.25149050, 0.47068653, -0.02302876, 0.36447432,
            -0.03308176, 0.17631681, 0.04460566)
)




# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2021/2022") %>% slice(1:132)


ProbTable<- function(Alle_parametre,HT,AT){
  teams<-Alle_parametre$Hold
  delta <- delta
  betaALL <- betaALL
  a<- which(teams==HT)
  b<-which(teams==AT)
  #a/A svarer til hjemmemål, b/B svarer til udemål
  lambda_a<-exp(betaALL+Alle_parametre[a,]$alfa-Alle_parametre[b,]$gamma+delta)
  lambda_b<-exp(betaALL+Alle_parametre[b,]$alfa-Alle_parametre[a,]$gamma)
  A<-as.numeric()
  B<-as.numeric()
  for (i in 0:6){
    A[(i+1)]<-dpois(i,lambda_a)
    B[(i+1)]<-dpois(i,lambda_b)
  }
  A[8]<-1-sum(A[1:7])
  B[8]<-1-sum(B[1:7])
  name<-c("0","1","2","3","4","5","6","7+")
  zero<-mat.or.vec(8,1)
  C<-data.frame(row.names=name)
  for (j in 1:8){
    for (k in 1:8) {
      C[j,k]<-A[k]*B[j] # simultan fordeling
    }
  }
  colnames(C)<- name
  return(round(C*100,2))
}

AllProb <- function(Alle_parametre,PProbTable) {
  teams<-Alle_parametre$Hold
  delta<-delta
  n <- length(teams)
  name<-c("Hjemmehold","Udehold","Hjemmesejr","Uafgjort","Udesejr")
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {#hold kan ikke spille mod sig selv
        #ProbTable finder ssh-tabeller for mulige slutresultater
        ssh<-PProbTable(Alle_parametre,teams[i],teams[j])
        #ResultProbs laver ssh-tabel om til samlede ssh'er for ude-,hjemmesejr og uafgjort
        ResultProbabilities<-ResultProbs(ssh)
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- ResultProbabilities$HomeWin
        C[row,4] <- ResultProbabilities$Draw
        C[row,5] <- ResultProbabilities$AwayWin
        row <- row + 1
      }
    }
  }
  colnames(C)<- name
  return(C)
}

ResultProbs<-function(probs){
  R<-matrix(0,3,1)
  n<-length(probs)
  for (i in 1:n){
    for (j in 1:n) {
      if (i>j){ #altså summen af indgangene under diagonalen=AW
        R[3]<-R[3]+probs[i,j]
      } else{ #summen af ssh'er i diagonaln=D
        if (i==j){
          R[2]<-R[2]+probs[i,j]
        }else { #summen af resterende, altså indgangene over diagonalen=HW
          R[1]<-R[1]+probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1],Draw=R[2],AwayWin=R[3]))
}

AllSSH <- AllProb(Alle_parametre_ny, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]



# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1, strategi[,5], 0)


zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)




# NU MED KRAV TIL FORVENTET AFKAST
#for 24/25
#indledende
DNK_17_3 <- read_excel("/Users/philipstrommen/Desktop/Bachelor/DNK_17_3.xlsx")
SL<-DNK_17_3 %>% filter(Season == "2023/2024")
M<-nrow(SL)
head(SL)
#view(SL)



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

#These are now the true parameters
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

AllProb <- function(Alle_parametre,PProbTable) {
  teams<-Alle_parametre$Hold
  delta<-delta
  n <- length(teams)
  name<-c("Hjemmehold","Udehold","Hjemmesejr","Uafgjort","Udesejr")
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {#hold kan ikke spille mod sig selv
        #ProbTable finder ssh-tabeller for mulige slutresultater
        ssh<-PProbTable(Alle_parametre,teams[i],teams[j])
        #ResultProbs laver ssh-tabel om til samlede ssh'er for ude-,hjemmesejr og uafgjort
        ResultProbabilities<-ResultProbs(ssh)
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- ResultProbabilities$HomeWin
        C[row,4] <- ResultProbabilities$Draw
        C[row,5] <- ResultProbabilities$AwayWin
        row <- row + 1
      }
    }
  }
  colnames(C)<- name
  return(C)
}

ResultProbs<-function(probs){
  R<-matrix(0,3,1)
  n<-length(probs)
  for (i in 1:n){
    for (j in 1:n) {
      if (i>j){ #altså summen af indgangene under diagonalen=AW
        R[3]<-R[3]+probs[i,j]
      } else{ #summen af ssh'er i diagonaln=D
        if (i==j){
          R[2]<-R[2]+probs[i,j]
        }else { #summen af resterende, altså indgangene over diagonalen=HW
          R[1]<-R[1]+probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1],Draw=R[2],AwayWin=R[3]))
}


ProbTable<- function(Alle_parametre,HT,AT){
  teams<-Alle_parametre$Hold
  delta <- delta
  betaALL <- betaALL
  a<- which(teams==HT)
  b<-which(teams==AT)
  #a/A svarer til hjemmemål, b/B svarer til udemål
  lambda_a<-exp(betaALL+Alle_parametre[a,]$alfa-Alle_parametre[b,]$gamma+delta)
  lambda_b<-exp(betaALL+Alle_parametre[b,]$alfa-Alle_parametre[a,]$gamma)
  A<-as.numeric()
  B<-as.numeric()
  for (i in 0:6){
    A[(i+1)]<-dpois(i,lambda_a)
    B[(i+1)]<-dpois(i,lambda_b)
  }
  A[8]<-1-sum(A[1:7])
  B[8]<-1-sum(B[1:7])
  name<-c("0","1","2","3","4","5","6","7+")
  zero<-mat.or.vec(8,1)
  C<-data.frame(row.names=name)
  for (j in 1:8){
    for (k in 1:8) {
      C[j,k]<-A[k]*B[j] # simultan fordeling
    }
  }
  colnames(C)<- name
  return(round(C*100,2))
}


#0%
Alle_parametre24 <- data.frame(
  Hold = c("Midtjylland", "Lyngby", "Aarhus", "Sonderjyske", "Silkeborg", "Nordsjaelland",
           "Viborg", "Vejle", "Randers FC", "Brondby", "FC Copenhagen", "Aalborg"),
  alfa = c(0.00000000, -0.53434679, -0.38783712, -0.60235150, -0.45225075, -0.05839367,
           -0.57529286, -0.78255705, -0.49516691, -0.05566519, 0.01018012, -0.88447128),
  gamma = c(0.00000000, -0.31925572, -0.01351351, -0.21277803, -0.09000640, 0.24456837,
            -0.21565067, 0.09200298, -0.23468115, 0.21514084, 0.12020742, -0.42706744)
)



# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2024/2025")



AllSSH <- AllProb(Alle_parametre24, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]


# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1, strategi[,5], 0)



zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)

#5%
Alle_parametre24 <- data.frame(
  Hold = c("Midtjylland", "Lyngby", "Aarhus", "Sonderjyske", "Silkeborg", "Nordsjaelland",
           "Viborg", "Vejle", "Randers FC", "Brondby", "FC Copenhagen", "Aalborg"),
  alfa = c(0.00000000, -0.53434679, -0.38783712, -0.60235150, -0.45225075, -0.05839367,
           -0.57529286, -0.78255705, -0.49516691, -0.05566519, 0.01018012, -0.88447128),
  gamma = c(0.00000000, -0.31925572, -0.01351351, -0.21277803, -0.09000640, 0.24456837,
            -0.21565067, 0.09200298, -0.23468115, 0.21514084, 0.12020742, -0.42706744)
)



# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2024/2025")

AllSSH <- AllProb(Alle_parametre24, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]


# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1.05, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1.05, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1.05, strategi[,5], 0)



zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)


#10%
Alle_parametre24 <- data.frame(
  Hold = c("Midtjylland", "Lyngby", "Aarhus", "Sonderjyske", "Silkeborg", "Nordsjaelland",
           "Viborg", "Vejle", "Randers FC", "Brondby", "FC Copenhagen", "Aalborg"),
  alfa = c(0.00000000, -0.53434679, -0.38783712, -0.60235150, -0.45225075, -0.05839367,
           -0.57529286, -0.78255705, -0.49516691, -0.05566519, 0.01018012, -0.88447128),
  gamma = c(0.00000000, -0.31925572, -0.01351351, -0.21277803, -0.09000640, 0.24456837,
            -0.21565067, 0.09200298, -0.23468115, 0.21514084, 0.12020742, -0.42706744)
)



# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2024/2025")

AllSSH <- AllProb(Alle_parametre24, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]


# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1.1, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1.1, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1.1, strategi[,5], 0)



zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)


#15%
Alle_parametre24 <- data.frame(
  Hold = c("Midtjylland", "Lyngby", "Aarhus", "Sonderjyske", "Silkeborg", "Nordsjaelland",
           "Viborg", "Vejle", "Randers FC", "Brondby", "FC Copenhagen", "Aalborg"),
  alfa = c(0.00000000, -0.53434679, -0.38783712, -0.60235150, -0.45225075, -0.05839367,
           -0.57529286, -0.78255705, -0.49516691, -0.05566519, 0.01018012, -0.88447128),
  gamma = c(0.00000000, -0.31925572, -0.01351351, -0.21277803, -0.09000640, 0.24456837,
            -0.21565067, 0.09200298, -0.23468115, 0.21514084, 0.12020742, -0.42706744)
)



# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2024/2025")

AllSSH <- AllProb(Alle_parametre24, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]


# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1.15, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1.15, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1.15, strategi[,5], 0)



zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)


#20%
Alle_parametre24 <- data.frame(
  Hold = c("Midtjylland", "Lyngby", "Aarhus", "Sonderjyske", "Silkeborg", "Nordsjaelland",
           "Viborg", "Vejle", "Randers FC", "Brondby", "FC Copenhagen", "Aalborg"),
  alfa = c(0.00000000, -0.53434679, -0.38783712, -0.60235150, -0.45225075, -0.05839367,
           -0.57529286, -0.78255705, -0.49516691, -0.05566519, 0.01018012, -0.88447128),
  gamma = c(0.00000000, -0.31925572, -0.01351351, -0.21277803, -0.09000640, 0.24456837,
            -0.21565067, 0.09200298, -0.23468115, 0.21514084, 0.12020742, -0.42706744)
)



# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2024/2025")

AllSSH <- AllProb(Alle_parametre24, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]


# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1.2, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1.2, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1.2, strategi[,5], 0)



zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)


#30%
Alle_parametre24 <- data.frame(
  Hold = c("Midtjylland", "Lyngby", "Aarhus", "Sonderjyske", "Silkeborg", "Nordsjaelland",
           "Viborg", "Vejle", "Randers FC", "Brondby", "FC Copenhagen", "Aalborg"),
  alfa = c(0.00000000, -0.53434679, -0.38783712, -0.60235150, -0.45225075, -0.05839367,
           -0.57529286, -0.78255705, -0.49516691, -0.05566519, 0.01018012, -0.88447128),
  gamma = c(0.00000000, -0.31925572, -0.01351351, -0.21277803, -0.09000640, 0.24456837,
            -0.21565067, 0.09200298, -0.23468115, 0.21514084, 0.12020742, -0.42706744)
)



# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2024/2025")

AllSSH <- AllProb(Alle_parametre24, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]


# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1.3, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1.3, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1.3, strategi[,5], 0)



zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)


#Nu for sæson 21/22
DNK_17_3 <- read_excel("/Users/philipstrommen/Desktop/Bachelor/DNK_17_3.xlsx")
SL<-DNK_17_3 %>% filter(Season == "2020/2021")
M<-nrow(SL)
head(SL)
#view(SL)



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

#These are now the true parameters
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

AllProb <- function(Alle_parametre,PProbTable) {
  teams<-Alle_parametre$Hold
  delta<-delta
  n <- length(teams)
  name<-c("Hjemmehold","Udehold","Hjemmesejr","Uafgjort","Udesejr")
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {#hold kan ikke spille mod sig selv
        #ProbTable finder ssh-tabeller for mulige slutresultater
        ssh<-PProbTable(Alle_parametre,teams[i],teams[j])
        #ResultProbs laver ssh-tabel om til samlede ssh'er for ude-,hjemmesejr og uafgjort
        ResultProbabilities<-ResultProbs(ssh)
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- ResultProbabilities$HomeWin
        C[row,4] <- ResultProbabilities$Draw
        C[row,5] <- ResultProbabilities$AwayWin
        row <- row + 1
      }
    }
  }
  colnames(C)<- name
  return(C)
}

ResultProbs<-function(probs){
  R<-matrix(0,3,1)
  n<-length(probs)
  for (i in 1:n){
    for (j in 1:n) {
      if (i>j){ #altså summen af indgangene under diagonalen=AW
        R[3]<-R[3]+probs[i,j]
      } else{ #summen af ssh'er i diagonaln=D
        if (i==j){
          R[2]<-R[2]+probs[i,j]
        }else { #summen af resterende, altså indgangene over diagonalen=HW
          R[1]<-R[1]+probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1],Draw=R[2],AwayWin=R[3]))
}


ProbTable<- function(Alle_parametre,HT,AT){
  teams<-Alle_parametre$Hold
  delta <- delta
  betaALL <- betaALL
  a<- which(teams==HT)
  b<-which(teams==AT)
  #a/A svarer til hjemmemål, b/B svarer til udemål
  lambda_a<-exp(betaALL+Alle_parametre[a,]$alfa-Alle_parametre[b,]$gamma+delta)
  lambda_b<-exp(betaALL+Alle_parametre[b,]$alfa-Alle_parametre[a,]$gamma)
  A<-as.numeric()
  B<-as.numeric()
  for (i in 0:6){
    A[(i+1)]<-dpois(i,lambda_a)
    B[(i+1)]<-dpois(i,lambda_b)
  }
  A[8]<-1-sum(A[1:7])
  B[8]<-1-sum(B[1:7])
  name<-c("0","1","2","3","4","5","6","7+")
  zero<-mat.or.vec(8,1)
  C<-data.frame(row.names=name)
  for (j in 1:8){
    for (k in 1:8) {
      C[j,k]<-A[k]*B[j] # simultan fordeling
    }
  }
  colnames(C)<- name
  return(round(C*100,2))
}


#0%
Alle_parametre_ny <- data.frame(
  Hold = c("Sonderjyske", "Silkeborg", "Viborg", "Brondby", "Odense",
           "Aarhus", "Midtjylland", "FC Copenhagen", "Randers FC",
           "Vejle", "Aalborg", "Nordsjaelland"),
  alfa = c(0.00000000, -0.38094841, -0.18452865, 0.30727969, -0.14407445,
           0.11434264, 0.27626428, 0.40070051, 0.00412151,
           -0.06452131, -0.04363819, 0.21192363),
  gamma = c(0.00000000, -0.16769812, -0.25119570, 0.32490564, 0.22418742,
            0.25149050, 0.47068653, -0.02302876, 0.36447432,
            -0.03308176, 0.17631681, 0.04460566)
)




# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2021/2022") %>% slice(1:132)

AllProb <- function(Alle_parametre,PProbTable) {
  teams<-Alle_parametre$Hold
  delta<-delta
  n <- length(teams)
  name<-c("Hjemmehold","Udehold","Hjemmesejr","Uafgjort","Udesejr")
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {#hold kan ikke spille mod sig selv
        #ProbTable finder ssh-tabeller for mulige slutresultater
        ssh<-PProbTable(Alle_parametre,teams[i],teams[j])
        #ResultProbs laver ssh-tabel om til samlede ssh'er for ude-,hjemmesejr og uafgjort
        ResultProbabilities<-ResultProbs(ssh)
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- ResultProbabilities$HomeWin
        C[row,4] <- ResultProbabilities$Draw
        C[row,5] <- ResultProbabilities$AwayWin
        row <- row + 1
      }
    }
  }
  colnames(C)<- name
  return(C)
}

ResultProbs<-function(probs){
  R<-matrix(0,3,1)
  n<-length(probs)
  for (i in 1:n){
    for (j in 1:n) {
      if (i>j){ #altså summen af indgangene under diagonalen=AW
        R[3]<-R[3]+probs[i,j]
      } else{ #summen af ssh'er i diagonaln=D
        if (i==j){
          R[2]<-R[2]+probs[i,j]
        }else { #summen af resterende, altså indgangene over diagonalen=HW
          R[1]<-R[1]+probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1],Draw=R[2],AwayWin=R[3]))
}

AllSSH <- AllProb(Alle_parametre_ny, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]



# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1, strategi[,5], 0)


zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)


#5%
Alle_parametre_ny <- data.frame(
  Hold = c("Sonderjyske", "Silkeborg", "Viborg", "Brondby", "Odense",
           "Aarhus", "Midtjylland", "FC Copenhagen", "Randers FC",
           "Vejle", "Aalborg", "Nordsjaelland"),
  alfa = c(0.00000000, -0.38094841, -0.18452865, 0.30727969, -0.14407445,
           0.11434264, 0.27626428, 0.40070051, 0.00412151,
           -0.06452131, -0.04363819, 0.21192363),
  gamma = c(0.00000000, -0.16769812, -0.25119570, 0.32490564, 0.22418742,
            0.25149050, 0.47068653, -0.02302876, 0.36447432,
            -0.03308176, 0.17631681, 0.04460566)
)




# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2021/2022") %>% slice(1:132)

AllProb <- function(Alle_parametre,PProbTable) {
  teams<-Alle_parametre$Hold
  delta<-delta
  n <- length(teams)
  name<-c("Hjemmehold","Udehold","Hjemmesejr","Uafgjort","Udesejr")
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {#hold kan ikke spille mod sig selv
        #ProbTable finder ssh-tabeller for mulige slutresultater
        ssh<-PProbTable(Alle_parametre,teams[i],teams[j])
        #ResultProbs laver ssh-tabel om til samlede ssh'er for ude-,hjemmesejr og uafgjort
        ResultProbabilities<-ResultProbs(ssh)
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- ResultProbabilities$HomeWin
        C[row,4] <- ResultProbabilities$Draw
        C[row,5] <- ResultProbabilities$AwayWin
        row <- row + 1
      }
    }
  }
  colnames(C)<- name
  return(C)
}

ResultProbs<-function(probs){
  R<-matrix(0,3,1)
  n<-length(probs)
  for (i in 1:n){
    for (j in 1:n) {
      if (i>j){ #altså summen af indgangene under diagonalen=AW
        R[3]<-R[3]+probs[i,j]
      } else{ #summen af ssh'er i diagonaln=D
        if (i==j){
          R[2]<-R[2]+probs[i,j]
        }else { #summen af resterende, altså indgangene over diagonalen=HW
          R[1]<-R[1]+probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1],Draw=R[2],AwayWin=R[3]))
}

AllSSH <- AllProb(Alle_parametre_ny, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]



# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1.05, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1.05, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1.05, strategi[,5], 0)


zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)


#10%
Alle_parametre_ny <- data.frame(
  Hold = c("Sonderjyske", "Silkeborg", "Viborg", "Brondby", "Odense",
           "Aarhus", "Midtjylland", "FC Copenhagen", "Randers FC",
           "Vejle", "Aalborg", "Nordsjaelland"),
  alfa = c(0.00000000, -0.38094841, -0.18452865, 0.30727969, -0.14407445,
           0.11434264, 0.27626428, 0.40070051, 0.00412151,
           -0.06452131, -0.04363819, 0.21192363),
  gamma = c(0.00000000, -0.16769812, -0.25119570, 0.32490564, 0.22418742,
            0.25149050, 0.47068653, -0.02302876, 0.36447432,
            -0.03308176, 0.17631681, 0.04460566)
)




# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2021/2022") %>% slice(1:132)

AllProb <- function(Alle_parametre,PProbTable) {
  teams<-Alle_parametre$Hold
  delta<-delta
  n <- length(teams)
  name<-c("Hjemmehold","Udehold","Hjemmesejr","Uafgjort","Udesejr")
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {#hold kan ikke spille mod sig selv
        #ProbTable finder ssh-tabeller for mulige slutresultater
        ssh<-PProbTable(Alle_parametre,teams[i],teams[j])
        #ResultProbs laver ssh-tabel om til samlede ssh'er for ude-,hjemmesejr og uafgjort
        ResultProbabilities<-ResultProbs(ssh)
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- ResultProbabilities$HomeWin
        C[row,4] <- ResultProbabilities$Draw
        C[row,5] <- ResultProbabilities$AwayWin
        row <- row + 1
      }
    }
  }
  colnames(C)<- name
  return(C)
}

ResultProbs<-function(probs){
  R<-matrix(0,3,1)
  n<-length(probs)
  for (i in 1:n){
    for (j in 1:n) {
      if (i>j){ #altså summen af indgangene under diagonalen=AW
        R[3]<-R[3]+probs[i,j]
      } else{ #summen af ssh'er i diagonaln=D
        if (i==j){
          R[2]<-R[2]+probs[i,j]
        }else { #summen af resterende, altså indgangene over diagonalen=HW
          R[1]<-R[1]+probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1],Draw=R[2],AwayWin=R[3]))
}

AllSSH <- AllProb(Alle_parametre_ny, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]



# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1.1, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1.1, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1.1, strategi[,5], 0)


zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)


#15%
Alle_parametre_ny <- data.frame(
  Hold = c("Sonderjyske", "Silkeborg", "Viborg", "Brondby", "Odense",
           "Aarhus", "Midtjylland", "FC Copenhagen", "Randers FC",
           "Vejle", "Aalborg", "Nordsjaelland"),
  alfa = c(0.00000000, -0.38094841, -0.18452865, 0.30727969, -0.14407445,
           0.11434264, 0.27626428, 0.40070051, 0.00412151,
           -0.06452131, -0.04363819, 0.21192363),
  gamma = c(0.00000000, -0.16769812, -0.25119570, 0.32490564, 0.22418742,
            0.25149050, 0.47068653, -0.02302876, 0.36447432,
            -0.03308176, 0.17631681, 0.04460566)
)




# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2021/2022") %>% slice(1:132)

AllProb <- function(Alle_parametre,PProbTable) {
  teams<-Alle_parametre$Hold
  delta<-delta
  n <- length(teams)
  name<-c("Hjemmehold","Udehold","Hjemmesejr","Uafgjort","Udesejr")
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {#hold kan ikke spille mod sig selv
        #ProbTable finder ssh-tabeller for mulige slutresultater
        ssh<-PProbTable(Alle_parametre,teams[i],teams[j])
        #ResultProbs laver ssh-tabel om til samlede ssh'er for ude-,hjemmesejr og uafgjort
        ResultProbabilities<-ResultProbs(ssh)
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- ResultProbabilities$HomeWin
        C[row,4] <- ResultProbabilities$Draw
        C[row,5] <- ResultProbabilities$AwayWin
        row <- row + 1
      }
    }
  }
  colnames(C)<- name
  return(C)
}

ResultProbs<-function(probs){
  R<-matrix(0,3,1)
  n<-length(probs)
  for (i in 1:n){
    for (j in 1:n) {
      if (i>j){ #altså summen af indgangene under diagonalen=AW
        R[3]<-R[3]+probs[i,j]
      } else{ #summen af ssh'er i diagonaln=D
        if (i==j){
          R[2]<-R[2]+probs[i,j]
        }else { #summen af resterende, altså indgangene over diagonalen=HW
          R[1]<-R[1]+probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1],Draw=R[2],AwayWin=R[3]))
}

AllSSH <- AllProb(Alle_parametre_ny, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]



# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1.15, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1.15, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1.15, strategi[,5], 0)


zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)


#20%
Alle_parametre_ny <- data.frame(
  Hold = c("Sonderjyske", "Silkeborg", "Viborg", "Brondby", "Odense",
           "Aarhus", "Midtjylland", "FC Copenhagen", "Randers FC",
           "Vejle", "Aalborg", "Nordsjaelland"),
  alfa = c(0.00000000, -0.38094841, -0.18452865, 0.30727969, -0.14407445,
           0.11434264, 0.27626428, 0.40070051, 0.00412151,
           -0.06452131, -0.04363819, 0.21192363),
  gamma = c(0.00000000, -0.16769812, -0.25119570, 0.32490564, 0.22418742,
            0.25149050, 0.47068653, -0.02302876, 0.36447432,
            -0.03308176, 0.17631681, 0.04460566)
)




# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2021/2022") %>% slice(1:132)

AllProb <- function(Alle_parametre,PProbTable) {
  teams<-Alle_parametre$Hold
  delta<-delta
  n <- length(teams)
  name<-c("Hjemmehold","Udehold","Hjemmesejr","Uafgjort","Udesejr")
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {#hold kan ikke spille mod sig selv
        #ProbTable finder ssh-tabeller for mulige slutresultater
        ssh<-PProbTable(Alle_parametre,teams[i],teams[j])
        #ResultProbs laver ssh-tabel om til samlede ssh'er for ude-,hjemmesejr og uafgjort
        ResultProbabilities<-ResultProbs(ssh)
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- ResultProbabilities$HomeWin
        C[row,4] <- ResultProbabilities$Draw
        C[row,5] <- ResultProbabilities$AwayWin
        row <- row + 1
      }
    }
  }
  colnames(C)<- name
  return(C)
}

ResultProbs<-function(probs){
  R<-matrix(0,3,1)
  n<-length(probs)
  for (i in 1:n){
    for (j in 1:n) {
      if (i>j){ #altså summen af indgangene under diagonalen=AW
        R[3]<-R[3]+probs[i,j]
      } else{ #summen af ssh'er i diagonaln=D
        if (i==j){
          R[2]<-R[2]+probs[i,j]
        }else { #summen af resterende, altså indgangene over diagonalen=HW
          R[1]<-R[1]+probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1],Draw=R[2],AwayWin=R[3]))
}

AllSSH <- AllProb(Alle_parametre_ny, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]



# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1.2, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1.2, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1.2, strategi[,5], 0)


zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)


#30%
Alle_parametre_ny <- data.frame(
  Hold = c("Sonderjyske", "Silkeborg", "Viborg", "Brondby", "Odense",
           "Aarhus", "Midtjylland", "FC Copenhagen", "Randers FC",
           "Vejle", "Aalborg", "Nordsjaelland"),
  alfa = c(0.00000000, -0.38094841, -0.18452865, 0.30727969, -0.14407445,
           0.11434264, 0.27626428, 0.40070051, 0.00412151,
           -0.06452131, -0.04363819, 0.21192363),
  gamma = c(0.00000000, -0.16769812, -0.25119570, 0.32490564, 0.22418742,
            0.25149050, 0.47068653, -0.02302876, 0.36447432,
            -0.03308176, 0.17631681, 0.04460566)
)




# --- Start hovedkørsel ---

SL <- DNK_17_3 %>% filter(Season == "2021/2022") %>% slice(1:132)

AllProb <- function(Alle_parametre,PProbTable) {
  teams<-Alle_parametre$Hold
  delta<-delta
  n <- length(teams)
  name<-c("Hjemmehold","Udehold","Hjemmesejr","Uafgjort","Udesejr")
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {#hold kan ikke spille mod sig selv
        #ProbTable finder ssh-tabeller for mulige slutresultater
        ssh<-PProbTable(Alle_parametre,teams[i],teams[j])
        #ResultProbs laver ssh-tabel om til samlede ssh'er for ude-,hjemmesejr og uafgjort
        ResultProbabilities<-ResultProbs(ssh)
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- ResultProbabilities$HomeWin
        C[row,4] <- ResultProbabilities$Draw
        C[row,5] <- ResultProbabilities$AwayWin
        row <- row + 1
      }
    }
  }
  colnames(C)<- name
  return(C)
}

ResultProbs<-function(probs){
  R<-matrix(0,3,1)
  n<-length(probs)
  for (i in 1:n){
    for (j in 1:n) {
      if (i>j){ #altså summen af indgangene under diagonalen=AW
        R[3]<-R[3]+probs[i,j]
      } else{ #summen af ssh'er i diagonaln=D
        if (i==j){
          R[2]<-R[2]+probs[i,j]
        }else { #summen af resterende, altså indgangene over diagonalen=HW
          R[1]<-R[1]+probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1],Draw=R[2],AwayWin=R[3]))
}

AllSSH <- AllProb(Alle_parametre_ny, ProbTable)
AllSSH[,3:5] <- AllSSH[,3:5] / 100
ssh_model <- cbind(AllSSH[,1:2], AllSSH[,3:5])


SSmodel <- ssh_model[order(ssh_model$Hjemmehold, ssh_model$Udehold), ]
SL_sorted <- SL[order(SL$Home, SL$Away), ]



# Brug odds og resultater direkte fra SL
BookOdds <- SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA", "Res")]

winner <- BookOdds$Res

# SSH * odds = forventet afkast
colnames(SSmodel) <- c("Home", "Away", "SSH_H", "SSH_D", "SSH_A")

# Merge SSH og odds
strategi <- SSmodel %>%
  inner_join(SL_sorted[, c("Home", "Away", "PSCH", "PSCD", "PSCA")], by = c("Home", "Away")) %>%
  mutate(
    H = SSH_H * PSCH,
    D = SSH_D * PSCD,
    A = SSH_A * PSCA
  ) %>%
  select(Home, Away, H, D, A)



# Vælg bedste bet
HWS <- ifelse(strategi[,3] > 1.3, strategi[,3], 0)
DS <- ifelse(strategi[,4] > 1.3, strategi[,4], 0)
AWS <- ifelse(strategi[,5] > 1.3, strategi[,5], 0)


zero <- matrix(0, 132, 1)
BestBet <- data.frame(HomeTeam = SSmodel[,1], AwayTeam = SSmodel[,2], SSHOdds = zero, BestBet = zero)
for (i in 1:132){
  BestBet[i,3] <- max(HWS[i], DS[i], AWS[i], 0, na.rm = TRUE)
  if (is.na(BestBet[i,3]) || BestBet[i,3] == 0){
    BestBet[i,4] <- "NoBet"
  } else if (BestBet[i,3] == DS[i]){
    BestBet[i,4] <- "D"
  } else if (BestBet[i,3] == AWS[i]){
    BestBet[i,4] <- "A"
  } else if (BestBet[i,3] == HWS[i]){
    BestBet[i,4] <- "H"
  }
}

# Beregn formue
formue <- 0
formue_vektor <- numeric(132)
for (i in 1:132){
  if (winner[i]=="A" & BestBet[i,4]=="A"){
    formue <- formue + BookOdds[i,5] - 1
  } else if (winner[i]=="D" & BestBet[i,4]=="D"){
    formue <- formue + BookOdds[i,4] - 1
  } else if (winner[i]=="H" & BestBet[i,4]=="H"){
    formue <- formue + BookOdds[i,3] - 1
  } else if (BestBet[i,4] != "NoBet"){
    formue <- formue - 1
  }
  formue_vektor[i] <- formue
}

# Plot formue
plot(0:131, formue_vektor, type='l', xlab="Kampnummer", ylab="Formue", lwd=2, main="Formueudvikling")
abline(h=0, lty=2)

