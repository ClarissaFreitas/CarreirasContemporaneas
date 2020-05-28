#Abrindo a biblioteca
 library(psych)
 library(lavaan)
 library(semPlot)

#Cleaning Dataset.R:
  #Abrindo a biblioteca
    library(haven)
    library(psych)
    library(skimr)
    library(tidyverse)

  #Entendendo o banco de dados
    carreira <- read_sav("C:\\Users\\Marco2\\Desktop\\Gabriel\\Mestrado\\Artigos e capítulos\\Em andamento\\Artigo - Carreiras contemporâneas\\bancosintegrados.sav")
    glimpse(carreira)

    #Nome das variáveis
      #AEO  = Autoeficácia Ocupacional
      #ACSF = Atitude de Carreira sem Fronteiras
      #ACP  = Atitude de Carreira Proteana
      #PPC  = Escala de Parâmetros de Carreira Caleidoscópica
      #EEC  = Engajamento com a Carreira

    #A variável ID parece estranha, é melhor olhá-la de perto
      carreira <- carreira %>% arrange(ID)
      carreira

      #ID está significando "Concordo/Discordo" ao invés de identificar os participantes
      #Criando uma nova ID
        id <- 1:300
        carreira <- cbind(carreira, id)
        glimpse(carreira)
      #Pronto! Agora vou selecionar "id" (em minúsculo) e prosseguir

  #Selecionando apenas as sete variáveis principais, já convertendo para números
    carreira <- carreira %>%
      select(id, Sexo, AEO01:EEC09)

  #Análises descritivas iniciais
    skim(carreira)

#Análises Fatoriais Confirmatórias
#AEO = Autoeficácia Ocupacional
  model_AEO <- ' AEO =~ AEO01 + AEO02 + AEO03 + AEO04 + AEO05 + AEO06 '
  fit_AEO <- cfa(model = model_AEO, data = carreira, 
                 ordered = TRUE, estimator = "WLSMV")
  summary(fit_AEO, fit.measures = TRUE, standardized = TRUE)
  AEO_loadings <- inspect(fit_AEO, what = "std")[["lambda"]]
  #Factor loadings entre .535 e .727
  #CFI = .982, TLI = .970
  #RMSEA = .062 (entre .023 e .1)
  #SRMR = .035

#ACSF = Atitude de Carreira sem Fronteiras
  model_ACSF <- '
  MP =~ ACSF05 + ACSF06 + ACSF03 + ACSF04 + ACSF07 + ACSF08
  MF =~ ACSF12 + ACSF13 + ACSF11 + ACSF10
  '
  fit_ACSF <- cfa(model = model_ACSF, data = carreira, 
                  ordered = TRUE, estimator = "WLSMV")
  summary(fit_ACSF, fit.measures = TRUE, standardized = TRUE)
  ACSF_loadings <- inspect(fit_ACSF, what = "std")[["lambda"]]
  #Factor loadings entre .508 e .823
  #CFI = .929, TLI = .958
  #RMSEA = .106 (entre .089 e .124)
  #SRMR = .082

#ACP  = Atitude de Carreira Proteana
  model_ACP <- '
  AG =~ ACP05 + ACP03 + ACP06 + ACP04 + ACP02 + ACP07
  DV =~ ACP11 + ACP09 + ACP14 + ACP12
  ACP12 ~~ ACP14
  '
  #Retirado o item 01, de acordo com "http://pepsic.bvsalud.org/pdf/gerais/v2n2/v2n2a11.pdf"
  fit_ACP <- cfa(model = model_ACP, data = carreira, 
                 ordered = TRUE, estimator = "WLSMV")
  summary(fit_ACP, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)
  ACP_loadings <- inspect(fit_ACP, what = "std")[["lambda"]]
  ACP_loadings
  #Factor loadings entre .465 e .839
  #CFI = .933, TLI = .909
  #RMSEA = 0.098 (entre .080 e .116)
  #SRMR = .069

  #Caso se queira um modelo com ACP geral
  model_ACP_geral <- '
  ACP =~ ACP05 + ACP03 + ACP06 + ACP04 + ACP02 + ACP07 + ACP13 + ACP11 + ACP09 + ACP12
  ACP12 ~~ ACP13
  '
  #Retirado o item 01, de acordo com "http://pepsic.bvsalud.org/pdf/gerais/v2n2/v2n2a11.pdf"
  #Retirado item 14 por apresentar factor loading = .261
  fit_ACP_geral <- cfa(model = model_ACP_geral, data = carreira, 
                       ordered = TRUE, estimator = "WLSMV")
  summary(fit_ACP_geral, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)
  ACP_loadings_geral <- inspect(fit_ACP_geral, what = "std")[["lambda"]]
  #Factor loadings entre .305 e .838
  #CFI = .881, TLI = .842
  #RMSEA = .130 (entre .113 e .148)
  #SRMR = .087

#PPC  = Escala de Parâmetros de Carreira Caleidoscópica
  model_PPC <- '
  AUTEN =~ PPC01 + PPC02 + PPC05 + PPC07 + PPC10 + PPC17 + PPC19
  BALAN =~ PPC04 + PPC08 + PPC06 + PPC11 + PPC15 + PPC18
  CRESC =~ PPC03 + PPC09 + PPC14 + PPC12 + PPC13
  PPC14 ~~ PPC12
  '
  #Retirado item 16 por apresentar factor loading = .222
  fit_PPC <- cfa(model = model_PPC, data = carreira, 
                 ordered = TRUE, estimator = "WLSMV")
  summary(fit_PPC, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)
  PPC_loadings <- inspect(fit_PPC, what = "std")[["lambda"]]
  #Factor loadings entre .222 e .803
  #CFI = .903, TLI = .886
  #RMSEA = 0.70 (entre .060 e .079)
  #SRMR = .066

#EEC  = Engajamento com a Carreira
  model_EEC <- ' EEC =~ EEC01 + EEC02 + EEC03 + EEC04 + EEC05 + EEC06 + EEC07 + EEC08 + EEC09 '
  fit_EEC <- cfa(model = model_EEC, data = carreira, 
                 ordered = TRUE, estimator = "WLSMV")
  summary(fit_EEC, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)
  EEC_loadings <- inspect(fit_EEC, what = "std")[["lambda"]]
  #Factor loadings entre .527 e .821
  #CFI = .920, TLI = .894
  #RMSEA entre .140 e .178
  #SRMR = .067

# Criação dos escores z por item de cada participante
  #AEO
    AEO_predict <- predict(fit_AEO)
    carreira <- cbind(carreira, AEO_predict)
  #ACSF
    ACSF_predict <- predict(fit_ACSF)
    carreira <- cbind(carreira, ACSF_predict)
  #ACP
    ACP_predict <- predict(fit_ACP)
    carreira <- cbind(carreira, ACP_predict)
  #ACP_geral
    ACP_geral_predict <- predict(fit_ACP_geral)
    carreira <- cbind(carreira, ACP_geral_predict)
  #PPC
    PPC_predict <- predict(fit_PPC)
    carreira <- cbind(carreira, PPC_predict)
  #EEC
    EEC_predict <- predict(fit_EEC)
    carreira <- cbind(carreira, EEC_predict)
  
#Novo banco com todas alterações anteriores
  write.csv(carreira, "carreira_fatoriais.csv") 

#Novo banco apenas para a análise de redes
  network <- carreira %>% select(AEO:EEC)


#### Calcular Escores Brutos
```{r}
ListaEscores23 <- list(
Burnout = c("BAT_WEX1", "BAT_WEX2", "BAT_WEX3", "BAT_WEX4", "BAT_WEX5", "BAT_WEX6", "BAT_WEX7", "BAT_WEX8", "BAT_WMD1", "BAT_WMD2", "BAT_WMD3", "BAT_WMD4", "BAT_WMD5", "BAT_WCC1", "BAT_WCC2", "BAT_WCC3", "BAT_WCC4", "BAT_WCC5", "BAT_WCE1", "BAT_WCE2", "BAT_WCE3", "BAT_WCE4", "BAT_WCE5"),
Exhaustion = c("BAT_WEX1", "BAT_WEX2", "BAT_WEX3", "BAT_WEX4", "BAT_WEX5", "BAT_WEX6", "BAT_WEX7", "BAT_WEX8"),
MentalDistance = c("BAT_WMD1", "BAT_WMD2", "BAT_WMD3", "BAT_WMD4", "BAT_WMD5"),
EmotionalImpairment = c("BAT_WCE1", "BAT_WCE2", "BAT_WCE3", "BAT_WCE4", "BAT_WCE5"),
CognitiveImpairment = c("BAT_WCC1", "BAT_WCC2", "BAT_WCC3", "BAT_WCC4", "BAT_WCC5"),
WorkOverload = c("JDRQ_DWO1", "JDRQ_DWO2", "JDRQ_DWO3", "JDRQ_DWO4"),
RoleConflict = c("JDRQ_DRC1", "JDRQ_DRC2", "JDRQ_DRC3"),
InterpersonalConflicts = c("JDRQ_DC1", "JDRQ_DC2", "JDRQ_DC3", "JDRQ_DC4"),
RoleClarity = c("JDRQ_RRC1", "JDRQ_RRC2", "JDRQ_RRC3"),
CoworkersSupport = c("JDRQ_RCS1", "JDRQ_RCS2", "JDRQ_RCS3"),
JobControl = c("JDRQ_RJC1", "JDRQ_RJC2", "JDRQ_RJC3", "JDRQ_RJC4", "JDRQ_RJC5", "JDRQ_RJC6", "JDRQ_RJC7"),
WorkEngagement = c("UWES_1", "UWES_2", "UWES_3", "UWES_4", "UWES_5", "UWES_6", "UWES_7", "UWES_8", "UWES_9"),
LifeSatisfaction = c("SV1", "SV2", "SV3", "SV4", "SV5"))

# Soma os escores
Escores23 <- scoreItems(ListaEscores23,Rel_BAT23)

#View(Escores23)

#Escores23$scores

# Correlações
cor(Escores23$scores, method="kendall")

#For a more simple analysis, I used pearson correlation
  #but you can substitute for spearman of kendall
  #See help(cor)
cor_data = cor(Escores23$scores, method = c("pearson"))

#Here's a correlogram
corrplot(cor_data)

#Rounding output to 2 digits only
rounded = round(cor_data, 2)

#To hide the upper triangle use the following syntax
upper<-rounded
upper[upper.tri(rounded)]<-""
upper<-as.data.frame(upper)
upper


#An APA table format
  #If you put the filename = something it'll create a document with the table

apa.cor.table(Escores23$scores, filename= "Correlation_Tables.doc", table.number=1,
              show.conf.interval = FALSE, landscape = TRUE)
```

```{r}
ListaEscores12 <- list(
Burnout = c("BAT_WEX1", "BAT_WEX3", "BAT_WEX4", "BAT_WMD1", "BAT_WMD2", "BAT_WMD4", "BAT_WCC1", "BAT_WCC4", "BAT_WCC5", "BAT_WCE1", "BAT_WCE2", "BAT_WCE4"),
Exhaustion = c("BAT_WEX1", "BAT_WEX3", "BAT_WEX4"),
MentalDistance = c("BAT_WMD1", "BAT_WMD2", "BAT_WMD4"),
EmotionalImpairment = c("BAT_WCE1", "BAT_WCE2", "BAT_WCE4"),
CognitiveImpairment = c("BAT_WCC1", "BAT_WCC4", "BAT_WCC5"),
WorkOverload = c("JDRQ_DWO1", "JDRQ_DWO2", "JDRQ_DWO3", "JDRQ_DWO4"),
RoleConflict = c("JDRQ_DRC1", "JDRQ_DRC2", "JDRQ_DRC3"),
InterpersonalConflicts = c("JDRQ_DC1", "JDRQ_DC2", "JDRQ_DC3", "JDRQ_DC4"),
RoleClarity = c("JDRQ_RRC1", "JDRQ_RRC2", "JDRQ_RRC3"),
CoworkersSupport = c("JDRQ_RCS1", "JDRQ_RCS2", "JDRQ_RCS3"),
JobControl = c("JDRQ_RJC1", "JDRQ_RJC2", "JDRQ_RJC3", "JDRQ_RJC4", "JDRQ_RJC5", "JDRQ_RJC6", "JDRQ_RJC7"),
WorkEngagement = c("UWES_1", "UWES_2", "UWES_3", "UWES_4", "UWES_5", "UWES_6", "UWES_7", "UWES_8", "UWES_9"),
LifeSatisfaction = c("SV1", "SV2", "SV3", "SV4", "SV5"))

# Soma os escores
Escores12 <- scoreItems(ListaEscores12,Rel_BAT12)

#View(Escores23)

#Escores23$scores

cor(Escores12$scores, method="kendall")

# Syntaxe do Rafael Bastos para fazer as tabelas em APA
#For a more simple analysis, I used pearson correlation
  #but you can substitute for spearman of kendall
  #See help(cor)
cor_data12 = cor(Escores12$scores, method = c("pearson"))

#Here's a correlogram
corrplot(cor_data12)

#Rounding output to 2 digits only
rounded12 = round(cor_data12, 2)

#To hide the upper triangle use the following syntax
upper12<-rounded12
upper12[upper.tri(rounded12)]<-""
upper12<-as.data.frame(upper12)
upper12


#An APA table format 
  #If you put the filename = something it'll create a document with the table

apa.cor.table(Escores12$scores, filename= "Correlation_Tables20.doc", table.number=1,
              show.conf.interval = T, landscape = F)

```
