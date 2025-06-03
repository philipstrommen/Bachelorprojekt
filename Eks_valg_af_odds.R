#Eksempel valg af odds
DNK_17_3 <- read_excel("/Users/philipstrommen/Desktop/Bachelor/DNK_17_3.xlsx")
SL<-DNK_17_3 
SL<-SL %>% filter(Season == "2024/2025")


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

#HDA
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

#odds og ssh
Pin_odds<-data.frame(SL[,6:13]) %>% filter(Home == "FC Copenhagen" & Away == "Lyngby")
Pin_ssh<-1/Pin_odds[,6:8]*100
s<-HDAprob("FC Copenhagen", "Lyngby", betaALL, delta, alfa, gamma)*100
s/100*Pin_odds[6:8]-1
Pin_odds[6:8]
s
Pin_ssh

