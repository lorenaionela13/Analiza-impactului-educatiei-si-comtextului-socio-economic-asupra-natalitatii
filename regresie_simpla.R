rm(list = ls()) 
directory <- "C:\\Users\\Lorena\\Desktop\\proiect-eco"



PackageNames <- c("tidyverse", "stargazer", "magrittr")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}
SETDATECROSS <- read.csv(paste0(directory, "/setdateCross.csv"))


SETDATECROSS %<>% select(Nr_of_Births,Edu_Level._Code)

names(SETDATECROSS)


str(SETDATECROSS)
# Statistici descriptive
stargazer(SETDATECROSS, type = "text")

head(SETDATECROSS, 20)


cor(SETDATECROSS)

model_SETDATECROSS <- lm(Nr_of_Births ~ Edu_Level._Code, data = SETDATECROSS)
ls()
model_SETDATECROSS$coefficients['Edu_Level._Code']
summary(model_SETDATECROSS)

# Graficul observatiilor cu dreapta estimata
plot(x = SETDATECROSS$Edu_Level._Code, y = SETDATECROSS$Nr_of_Births)
abline(a = model_SETDATECROSS$coefficients['(Intercept)'], 
       b = model_SETDATECROSS$coefficients['Edu_Level._Code'],
       col = 'purple')



ggplot(data = SETDATECROSS, mapping = aes(x = Edu_Level._Code, y = Nr_of_Births)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 


stargazer(SETDATECROSS, type = "text")

# Reziduuri
SETDATECROSS %<>% mutate(uhat = residuals(model_SETDATECROSS))
stargazer(SETDATECROSS, type = "text")
ggplot(SETDATECROSS, aes(x = Edu_Level._Code)) +
  geom_point(aes(y = Nr_of_Births, col = 'Nr_of_Births - actual value')) +
  geom_point(aes(y = uhat, col = 'Residual uhat')) +
  xlab('Return on equity')

head(SETDATECROSS, 10)


SETDATECROSS %<>% mutate(Nr_of_Birthshat = fitted(model_SETDATECROSS))
stargazer(SETDATECROSS, type = "text")
ggplot(data = SETDATECROSS, mapping = aes(x = Edu_Level._Code)) +
  geom_point(mapping = aes(y = Nr_of_Births, color = 'Nr_of_Births - actual value')) +
  geom_point(mapping = aes(y = Nr_of_Birthshat, color = 'Nr_of_Birthshat - predicted value')) + 
  xlab('Return on equity')


# Graficul valorilor si reziduurilor reale si previzionate
ggplot(SETDATECROSS, aes(x = Edu_Level._Code)) +
  geom_point(aes(y = Nr_of_Births, color = 'Nr_of_Births - actual value')) +
  geom_point(aes(y = Nr_of_Birthshat, color = 'Nr_of_Birthshat - predicted value')) +
  geom_point(aes(y = uhat, color = 'Residual uhat')) +
  geom_smooth(aes(y = Nr_of_Births, color = 'Fitted line'), 
              method = "lm", se = FALSE) +
  xlab('Return on equity')

wage1 %<>% mutate(wagehat = fitted(model_wage1),
                  uhat = resid(model_wage1))
ggplot(wage1, aes(x = educ)) +
  geom_point(aes(y = wage, color = 'Wage - actual value')) +
  geom_point(aes(y = wagehat, color = 'Wagehat - predicted value')) +
  geom_point(aes(y = uhat, color = 'Residual uhat')) +
  geom_smooth(aes(y = wage, color = 'Fitted line'), 
              method = "lm", se = FALSE)


str(model_SETDATECROSS)
str(summary(model_SETDATECROSS))

summary(model_SETDATECROSS)$r.squared

summary(model_SETDATECROSS)$adj.r.squared

nobs(model_SETDATECROSS)

aic <- AIC(model_SETDATECROSS)
cat("AIC (Akaike):", aic, "/n")

# Criteriul Schwarz pentru modelul simplu
bic <- BIC(model_SETDATECROSS)
cat("BIC (Schwarz):", bic, "/n")


# Testul Breusch-Pagan 
bptest(model_SETDATECROSS) # 0.1015>0.1 -->reziduri homoscedastice
# Testul White 
white_test(model_SETDATECROSS) #0.2329 >0.1 --> homoscedastice

dwtest(model_SETDATECROSS) # 0.513 >0.1 --> rez nu sunt autocorelate

ols_plot_resid_fit(model_SETDATECROSS)
ols_plot_cooksd_bar(model_SETDATECROSS)

acf(model_SETDATECROSS$residuals) # autocorelarea a disparut

# Testul Breusch-Godfrey (ordin superior)

bgtest(model_SETDATECROSS) # 0.9151
bgtest(model_SETDATECROSS, order = 2) #0.7233
bgtest(model_SETDATECROSS, order = 3) #0.3551 


residuals <- resid(model_SETDATECROSS)

hist(residuals, 
     main = "Residual Histogram", 
     xlab = "Residuals", 
     ylab = "Frequency",
     col = "lightblue")


