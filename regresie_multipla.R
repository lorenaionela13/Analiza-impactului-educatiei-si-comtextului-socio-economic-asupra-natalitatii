rm(list = ls()) 
directory <- "C:\\Users\\Lorena\\Desktop\\proiect-eco"

PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich","tseries", 
                  "olsrr","Boruta","glmnet", "moments","whitestrap","ggplot2","DataCombine","car","caret","mltools", "strucchange","splines","mgcv")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

SETDATECROSS <- read.csv(paste0(directory, "/setdateCross.csv"))

names(SETDATECROSS)

SETDATECROSS %<>% select(Nr_of_Births,Edu_Level._Code,Avg_Age,Avg_Weight,Gender,State)

SETDATECROSS %>% 
  select(Nr_of_Births, Edu_Level._Code, Avg_Age, Avg_Weight,Gender) %>% 
  head(50)


SETDATECROSS %>% 
  select(Nr_of_Births, Edu_Level._Code, Avg_Age, Avg_Weight,Gender) %>% 
  str

SETDATECROSS %>% 
  select(Nr_of_Births, Edu_Level._Code, Avg_Age, Avg_Weight,Gender) %>% 
  stargazer(type = "text")


model_multiplu <- lm(Nr_of_Births ~ Edu_Level._Code + Avg_Age + Avg_Weight+Gender, SETDATECROSS)
summary(model_multiplu)


library(dplyr)


data_transformed <- SETDATECROSS %>%
  mutate(
    Log_Edu_Level = log(pmax(Edu_Level._Code, 1)),           
    Log_Avg_Age = log(pmax(Avg_Age, 1)),                      
    Log_Avg_Weight = log(pmax(Avg_Weight, 1))                  
  ) %>%
  select(Nr_of_Births, Edu_Level._Code, Avg_Age, Avg_Weight, Log_Edu_Level, Log_Avg_Age, Log_Avg_Weight)


print(data_transformed)

model_multiplu <- lm(Nr_of_Births ~ Log_Edu_Level + Log_Avg_Age + Log_Avg_Weight, data = data_transformed)
summary(model_multiplu)


# Afisarea coeficientilor
coef(model_multiplu)
model_multiplu$coefficients



SETDATECROSS %<>% mutate(Nr_of_Birthshat = fitted(model_multiplu),
                  uhat = residuals(model_multiplu))
SETDATECROSS %>% 
  select(Nr_of_Births, Nr_of_Birthshat, uhat) %>% 
  head(50)


SETDATECROSS %>% 
  select(Nr_of_Births, Nr_of_Birthshat, uhat) %>%
  stargazer(type = "text")

summary(model_multiplu)$r.squared


# Afisarea lui R-squared ajustat
summary(model_multiplu)$adj.r.squared

# Criteriul Akaike pentru modelul cu toate variabilele
aic <- AIC(model_multiplu)
cat("AIC (Akaike):", aic, "/n")


# Criteriul Schwarz pentru modelul cu toate variabilele
bic <- BIC(model_multiplu)
cat("BIC (Schwarz):", bic, "/n")


# Verificarea normalitatii variabilelor

# Histograma variabilei Nr_of_Births
ggplot(data = SETDATECROSS) +
  theme_bw() +
  geom_histogram(mapping = aes(x = Nr_of_Births), col = 'grey') + 
  xlab('Nr_of_Births') + 
  ylab('Count') +
  ggtitle('Histograma variabilei Nr_of_Births') + 
  theme(plot.title = element_text(hjust = 0.5)) 


# Testul Shapiro Wilk pentru normalitate
# H0: distributie normala, Ha: distributie nenormala
shapiro.test(SETDATECROSS$Nr_of_Births)

shapiro.test(SETDATECROSS$uhat)  #p-values <0.05
jarque.bera.test(SETDATECROSS$uhat)#p-values <0.05


old_test_normality(model_multiplu)


# Testul Jarque-Bera pentru normalitate
# H0: distributie normala, Ha: distributie nenormala
jarque.bera.test(SETDATECROSS$Nr_of_Births)
jarque.bera.test(SETDATECROSS$uhat)
ols_test_normality(model_multiplu)

plot(model_multiplu)
ols_plot_resid_fit(model_multiplu)


ols_plot_cooksd_bar(model_multiplu)
ols_plot_cooksd_chart(model_multiplu)



# Testul Breusch-Pagan 
bptest(model_multiplu)# 0.02585 <0.5 -->rez heteroscedastice
# Testul White 
white_test(model_multiplu) #0.011346 <0.5 -->heteroscedasticitate


# Testul Durbin-Watson (ordinul 1)

# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate

dwtest(model_multiplu) # p-value < 0.1 => reziduurile sunt autocorelate

# Testul Breusch-Godfrey (ordin superior)

# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate

bgtest(model_multiplu) # p-value < 0.1 
bgtest(model_multiplu, order = 2) # =>
bgtest(model_multiplu, order = 3)
# reziduurile sunt autocorelate si la lag superior


acf(model_multiplu$residuals) # autocorelarea a disparut
residuals <- resid(model_multiplu)
ols_plot_cooksd_bar(model_multiplu)

hist(residuals, 
     main = "Residual Histogram", 
     xlab = "Residuals", 
     ylab = "Frequency",
     col = "lightblue")






data_transformed <- SETDATECROSS %>%
  mutate(
    Gender_Dummy = ifelse(Gender == "M", 1, 0),
    Log_Edu_Level = log(pmax(Edu_Level._Code, 1)),
  ) %>%
  select(Nr_of_Births, Edu_Level._Code, Avg_Age, Avg_Weight, Log_Edu_Level, Gender_Dummy)  

model_multiplu <- lm(Nr_of_Births ~ Log_Edu_Level + Gender_Dummy, data = data_transformed)
summary(model_multiplu)


model_multiplu_interactii <- lm(Nr_of_Births ~ Log_Edu_Level * Gender_Dummy, data = data_transformed)
summary(model_multiplu_interactii)




library(tidyverse)
library(caret)  
library(mltools) 
library(MLmetrics)

prognoza_noua <- data.frame(
  Edu_Level._Code = c(3),  
  Gender = c(0)            
)

prognoza_noua_transf <- prognoza_noua %>%
  mutate(
    Log_Edu_Level = log(pmax(Edu_Level._Code, 1))
  )

# Crearea variabilei dummy pentru Gender
prognoza_noua_transf$Gender_Dummy <- ifelse(prognoza_noua_transf$Gender == 1, 1, 0)


y_pred <- predict(model_multiplu, newdata = prognoza_noua_transf)

# Afișarea valorii prognozate
y_pred



# Împărțirea datelor în set de antrenare și set de testare
set.seed(123)
training.samples <- SETDATECROSS$Nr_of_Births %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- SETDATECROSS[training.samples, ]
test.data <- SETDATECROSS[-training.samples, ]

# Construirea modelului îmbunătățit
data_transformed <- SETDATECROSS %>%
  mutate(
    Gender_Dummy = ifelse(Gender == "M", 1, 0),
    Log_Edu_Level = log(pmax(Edu_Level._Code, 1))
  ) %>%
  select(Nr_of_Births, Edu_Level._Code, Avg_Age, Avg_Weight, Log_Edu_Level, Gender_Dummy)

model_multiplu <- lm(Nr_of_Births ~ Log_Edu_Level + Gender_Dummy, data = data_transformed)
summary(model_multiplu)

prognoza<- data.frame(
  Edu_Level._Code = c(1,3,6,8), 
  Gender = c(0)           
)
prognoza <- prognoza %>%
  mutate(
    Gender_Dummy = ifelse(Gender == "M", 1, 0),
    Log_Edu_Level = log(pmax(Edu_Level._Code, 1))
  ) %>%
  select(Log_Edu_Level, Gender_Dummy)

y_pred_scenariu <- predict(model_multiplu, newdata = prognoza)
y_pred_scenariu
new.specificatii <- data.frame(Log_Edu_Level = c(1,2), Gender_Dummy = rep(1,2))
predict(model_multiplu,newdata=new.specificatii,se.fit=TRUE,interval="confidence",level=0.99)

#TESTUL F
# Testul F pentru semnificatia coeficientului comun este utilizat pentru a testa 
# daca mai multi coeficienti sunt impreuna semnificativ diferiti de zero.

# Generarea termenilor de intteractiune
data_transformed %<>% mutate(femaleXEdu_Level._Code = Gender_Dummy*Edu_Level._Code,
                  femaleXAvg_Age = Gender_Dummy*Avg_Age,
                  femaleXAvg_Weight = Gender_Dummy*Avg_Weight)


# H0: delta0=0 si delta1=0 si delta2=0 si delta3=0

model_9 <- lm(Nr_of_Births ~ Edu_Level._Code + Avg_Age + Avg_Weight, data_transformed)
ssr_r <- sum(resid(model_9)^2)
print(ssr_r)

model_10 <-lm(Nr_of_Births ~ Edu_Level._Code + Avg_Age + Avg_Weight+Gender_Dummy+
                femaleXEdu_Level._Code+femaleXAvg_Age+femaleXAvg_Weight, data_transformed)
ssr_ur <- sum(resid(model_10)^2)
print(ssr_ur)
q <- 4

# Calcularea F_stat 
# df_resid reprezinta gradele de libertate ale reziduurilor din modelul nerestrictionat (n-k-1)
df_resid <- model_10$df.residual
(F_stat <- ((ssr_r - ssr_ur)/q) / (ssr_ur/df_resid))

# F-critical value
qf(0.95, q, df_resid)
# DACA F-stat > F-critical atunci ipoteza nula este respinsa, 
# iar coeficientii sunt in comun semnificativi

# p-value pentru F-test
(F_pvalue <- pf(F_stat, q, df_resid, lower.tail = F))
# DACA F p-value<0.05 atunci ipoteza nula este respinsa, iar coeficientii sunt in comun semnificativi

# F-test folozind comenzile R
linearHypothesis(model_10, c('Gender_Dummy        = 0', 
                             'femaleXEdu_Level._Code   = 0',
                             'femaleXAvg_Age  = 0',
                             'femaleXAvg_Weight = 0'))

summary(model_10)

print("----------------------------------CHOW----------------------------------------------------------")
library(tidyverse) 
library(stargazer) 
library(magrittr) 
library(car) 
library(strucchange) 
library(ggplot2) 
library(caret) 
library(splines)
library(mgcv)



ggplot(data_transformed, aes(x =Avg_Weight , y = Nr_of_Births)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

chow <- data.frame(x = c(1, 1, 2, 3, 4, 4, 5, 5, 6, 7, 7, 8, 8, 9, 10, 10,
                         11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 20, 20),
                   y = c(3, 5, 6, 10, 13, 15, 17, 14, 20, 23, 25, 27, 30, 30, 31,
                         33, 32, 32, 30, 32, 34, 34, 37, 35, 34, 36, 34, 37, 38, 36))

ggplot(chow, aes(x = x, y = y)) +
  geom_point(col='steelblue', size=3)

sctest(chow$y ~ chow$x, type = "Chow")
names(data_transformed)
sctest(data_transformed$Nr_of_Births ~ data_transformed$Edu_Level._Code, type = "Chow", point = 10) 
sctest(data_transformed$Nr_of_Births ~ data_transformed$Avg_Age, type = "Chow", point = 10)
sctest(data_transformed$Nr_of_Births ~ data_transformed$Avg_Weight, type = "Chow", point = 10) 

 model_11 <- model_9
 summary(model_11)
 k <- 3
 n <- nobs(model_11)
 ssr_r <- sum(resid(model_11)^2)
 print(ssr_r)

# Modelul nerestrictionat pentru females: 
 model_12 <- update(model_11, subset = (Gender_Dummy == 1))  # Selectăm doar observațiile pentru femei
 ssr1 <- sum(resid(model_12)^2) 
 print(ssr1)
# Modelul nerestrictionat pentru males: 
 model_13 <- update(model_11, subset = (Gender_Dummy == 0))  # Selectăm doar observațiile pentru bărbați
 ssr2 <- sum(resid(model_13)^2) 
print(ssr2)

# Calcularea Chow F-statistic
 (F_stat <- ((ssr_r-(ssr1+ssr2))/(k+1)) / ((ssr1+ssr2)/(n-2*(k+1))))
# Valoare F-critical
 qf(0.95, k+1, n-2*(k+1))


# p-value pentru F-test
 (F_pvalue <- pf(F_stat, k+1, n-2*(k+1), lower.tail = F))

print("--------------------------------------------------------------------------------------------")


# Regresia Ridge - este un model care se foloseste cu precadere atunci cand 
# exista multicoliniaritate in date 
# SSR = sum((y-yfit)^2)
# Regresia ridge incearca sa minimizeze SSR + lambda*sum(beta^2)
# lambda*sum(beta^2) se mai numeste si shrinkage penalty 
# lambda ia valoarea a.i. sa produca cea mai mica valoare pt MSE 


SETDATECROSS <- read.csv(paste0(directory, "/setdateCross.csv"))


model0 <- lm(Nr_of_Births ~Edu_Level._Code+ Avg_Age + Avg_Weight + Gender_Dummy ,data_transformed)
names(model0)
summary(model0)
prognoza <- data.frame(Edu_Level._Code = c(3),
                       Avg_Age = c(27),
                       Avg_Weight = c(3106),
                       Gender_Dummy = c(1))
y_pred_scenariu <- predict(model0, newdata = prognoza)
y_pred_scenariu

y <- data_transformed$Nr_of_Births
x <- data.matrix(data_transformed[, c('Edu_Level._Code', 'Avg_Age', 'Avg_Weight', 'Gender_Dummy')])

model <- glmnet(x, y, alpha = 0)
summary(model)

cv_model <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_model$lambda.min
best_lambda 

plot(cv_model) 

best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model) 

plot(model, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)


y_predicted <- predict(model, s = best_lambda, newx = x)


sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq 

print("------------------------------------------------------------------------")

model <- glmnet(x, y, alpha = 1)
cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda # 379.0506

plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) 

plot(model, xvar = "lambda",label=T)
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)
y_predicted

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq 

#######################################################################

# Elastic net regression - functioneaza similar cu Ridge si LASSO doar ca 
# adauga ambele penalitati SSR + lambda*sum(beta^2) + lambda*sum(|beta|)

model <- cv.glmnet(x, y, alpha = 0.5)
cv_model <- cv.glmnet(x, y, alpha = 0.5)
best_lambda <- cv_model$lambda.min
best_lambda #629.3889

plot(cv_model) 


best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model) 

# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)
y_predicted



# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq 

# Algoritmul Boruta 

convert <- c(5:10)
data_transformed[convert] <- data.frame(apply(data_transformed[convert], 2, as.factor))

library(Boruta)
 set.seed(111)
boruta.bank_train <- Boruta(Nr_of_Births~., data = data_transformed, doTrace = 2)
 print(boruta.bank_train)


getSelectedAttributes(boruta.bank_train, withTentative = F)


 model_boruta <- lm(Nr_of_Births ~ Edu_Level._Code + Avg_Age + Avg_Weight, data_transformed)
 summary(model_boruta) 
 model_boruta1 <- lm(Nr_of_Births ~ Edu_Level._Code + Avg_Age + Avg_Weight+Gender_Dummy, data_transformed)
 summary(model_boruta1) 

 