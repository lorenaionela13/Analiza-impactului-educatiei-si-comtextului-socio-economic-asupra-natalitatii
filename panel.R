rm(list = ls()) 
directory <- "C:\\Users\\Lorena\\Desktop\\panel"

PackageNames <- c("tidyverse","gplots","plm","readxl","foreign","lmtest")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}
data <- read_excel("C:\\Users\\Lorena\\Desktop\\panel\\panel.xlsx")
# Statistici descriptive
summary(data)

# Declararea setului de date de tip panel
pd.df <- pdata.frame(data, index = c("State","Year"), drop.index = TRUE)

# Corelatia 
dev.off()
coplot(Nr_of_Births ~ Year|State, type="l", data=data)


# Heterogeneitatea presupune ca exista diferente intre unitatile studiate

# Explorarea heterogeneitatii in sectiunea transversala
# Graficul traseaza un interval de incredere de 95% in jurul mediilor
# Tari cu rata foarte mare si tari cu rata foarte mica => avem heterogeneitate transversala
plotmeans(Nr_of_Births ~ State, main = 'Heterogeneitate in randul tarilor', data = data)


# Explorarea heterogeneitatii in sectiunea temporala
# Ani cu rata mare si ani cu rata mica => avem heterogeneitate temporala, 
# dar mai mica decat in cazul heterogeneitatii transversale
plotmeans(Nr_of_Births ~ Year, main = 'Heterogeneitate in timp', data = data)

# Model OLS - model clasic de regresie liniara
# Nu ia in calcul heterogeneitatea intre spatiu si timp
ols <- lm(Nr_of_Births ~ Edu_code+ Avg_Age+ Avg_Weight 
            , data)
summary(ols) #output
yhat <- ols$fitted # valori estimate
ggplot(data, aes(x=Edu_code, y=Nr_of_Births))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  theme_bw()
colnames(data)


# Model FE (cu efecte fixe) 
fe <- plm(Nr_of_Births ~ Edu_code+ Avg_Age + Avg_Weight , data, index = c('State','Year'),
          model = 'within')
summary(fe)

re <- plm(Nr_of_Births ~ Edu_code+ Avg_Age + Avg_Weight , data, index = c('State','Year'),
          model = 'between')
summary(re)


# Testul Hausmann il utilizam pentru a decide intre FE si RE
# H0: model cu efecte random 
# H1: model cu efecte fixe
phtest(fe,re) # 0.1286 >0.05 => se recomanda RE

# Testarea efectelor fixe in timp
fixed.time <- plm(Nr_of_Births ~ Edu_code + factor(Year), data=data, index=c("State",
                                                                       "Year"), model="within")

# H0:  nu sunt necesare efectele fixe in timp
# H1:  sunt necesare efectele fixe in timp

pFtest(fixed.time, fe) # p-value < 0.05 => se recomanda folosirea efectelor fixe in timp
plmtest(fe, c('time'), type = 'bp') # p-value >0.05  => nu este nevoie sa se utilizeze efecte fixe in timp 
# Cele doua teste sunt inconcluzive => vom alege varianta in care nu avem efecte fixe in timp

# Testarea efectelor aleatorii cu Breusch-Pagan Lagrange Multiplier
# Testul ne ajuta sa decidem intre RE si OLS 
pool <- plm(Nr_of_Births ~ Edu_code, data=data, index=c("State", "Year"), model="pooling")
summary(pool)

# H0: variatiile in timp sunt 0
# H1: variatiile in timp sunt diferite de 0
plmtest(pool, type=c("bp")) # p-value < 0.05 => respingem ipoteza nula
# variatiile in timp sunt diferite de 0 => efectele aleatorii sunt adecvate a.i.
# exista diferente semnificative intre tari

# Testarea dependentei transversale folosind testul Breusch-Pagan LM si  testul Parasan CD
# Ipoteze teste
# H0: reziduurile intre entitati nu sunt corelate
# H1: reziduurile intre entitati sunt corelate

pcdtest(fe, test = 'lm') #2.2e-16 <0.05 => dependenta transversala
pcdtest(fe, test = 'cd') #  0.4529>0.05 =>nu exista  dependenta transversala


# Testarea autocorelarii - Breusch-Godfrey/Wooldridge test 


# H0: Nu exista autocorelate
# H1: autocorelarea este prezenta
pbgtest(fe) # 2.027e-08 <0.05 => avem autocorelare 


# Testarea heteroschedasticitatii cu testul Breusch-Pagan
# H0: homoschedasticitate
# H1: heteroschedasticitate
bptest(Nr_of_Births ~ Edu_code + factor(State), data = data, studentize=F)
# deoarece2.2e-16 <0.05 => avem heteroschedasticitate


# Testarea efectelor random 
pFtest(re, ols) # 0.5898> 0.05 => nu se recomanda efecte random.
plmtest(re, c('time'), type = 'bp') #  0.1299 > 0.05 => nu se recomanda efecte random
# in cazul in care testul Hausmann recomanda efectele random

# H0: Nu exista autocorelate
# H1: autocorelarea este prezenta
pbgtest(re) # 0.4479 > 0.05 => nu exista autocorelare

# Testarea heteroschedasticitatii cu testul Breusch-Pagan
# H0: homoschedasticitate
# H1: heteroschedasticitate
bptest(Nr_of_Births ~ Edu_code + factor(Year), data = data, studentize=F) 
#0.6482> 0.6482=> nu exista hetero
