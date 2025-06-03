#Modelopbyggelse GLM til at estimere parametre
#Data
DNK_17_3 <- read_excel("/Users/philipstrommen/Desktop/Bachelor/DNK_17_3.xlsx")
SL<-DNK_17_3 %>% filter(Season == "2024/2025")
M<-nrow(SL)

#Tjekker hjemmebanefordel
hjemmebane<-data.frame(Hjemmesejre=length(which(SL$Res=="H"))/nrow(SL),
                       Uafgjorte=length(which(SL$Res=="D"))/nrow(SL),
                       Udesejre=length(which(SL$Res=="A"))/nrow(SL))
print(hjemmebane)

#Laver designmatrice
TeamNames<-unique(SL$Home)
Index<-1:length(TeamNames)
Goals<-1:(2*nrow(SL)) 
alfa<-gamma<-rep(0,length(TeamNames))
DesignMatrix<-matrix(0,nrow=2*nrow(SL),ncol=2+2*length(TeamNames))

#Opdaterer designmatrice
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

#Tjekker rank af designmatrice
qr(DesignMatrix)$rank

#GLM Aarhus som ref og fjerner intercept
testGLM<-glm(Goals ~ DesignMatrix[,c(-1,-3,-15)], family = poisson(link = "log"))

# summary
summary(testGLM)

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


# Regn ssh for specifikke stillinger
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

Ssh<-ProbTable(Alle_parametre, "FC Copenhagen", "Vejle")
view(Ssh)

#HDA prob funktion til at regne ssh for 1 X 2
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

HDAprob("FC Copenhagen", "Vejle", betaALL,delta,alfa,gamma)
