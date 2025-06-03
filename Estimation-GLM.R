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

# Sorteres efter angrebsevne
all<-Alle_parametre[order(-Alle_parametre$alfa), ]


# Plot parametre
Alle_parametre$Hold <- factor(Alle_parametre$Hold, levels = Alle_parametre$Hold)
Alle_parametre_long <- Alle_parametre %>%
  gather(key = "Parameter", value = "Værdi", -Hold)


myplot1 <- ggplot(Alle_parametre_long, aes(x = Hold, y = Værdi, fill = Parameter)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Hold", y = "Værdi") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("alfa" = "blue", "gamma" = "red"))

myplot1
