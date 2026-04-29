# pacotes para análise 
library(dbplyr)
library(dplyr)
library(plm)
library(tidyverse)
library("nloptr")
library("lme4")
library(rstatix)
library(car)
library(ggplot2)
library(ggpubr)

install.packages("ggpubr")
# importar dados 
df1 <- read.csv(file = "/home/jara/Downloads/exercicio_2.csv" , header = TRUE, sep = ",")
head(df1)

#organizar dados (separação dos grupos)
df2 <- select(df1,Grupo,Vitamina_C)
head(df2)


## separação por grupos 

df_A <- df2%>%
  filter(Grupo =="A") 

df_A

df_B <- df2%>%
  filter(Grupo =="B") 

df_B

df_C <- df2%>%
  filter(Grupo =="C") 

df_C

df_D <- df2%>%
  filter(Grupo =="D") 

df_D

# 1 passo : teste de normalidade, grupo com  valores abaixo de 30 amostras


## teste normalidade

shap_A <- shapiro.test(df_A$Vitamina_C)
shap_A
capture.output(shap_A	, file = "shap_A.txt")

getwd()

shap_B <- shapiro.test(df_B$Vitamina_C)
shap_B
capture.output(shap_B	, file = "shap_B.txt")


shap_C <- shapiro.test(df_C$Vitamina_C)
shap_C
capture.output(shap_C	, file = "shap_D.txt")

shap_D <- shapiro.test(df_D$Vitamina_C)
shap_D
capture.output(shap_D	, file = "shap_D.txt")

##From the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.

## normalidade ok : há outros parametros que devem ser verificados , como visualização de histograma etc.
## leituras recomendadas
## http://www.sthda.com/english/wiki/normality-test-in-r
## https://www.statology.org/test-for-normality-in-r/

# 2 passo: teste de homocedasticidade -  teste F


homocedasticidade <- with(df2, leveneTest(Vitamina_C, Grupo))

homocedasticidade



# 3 passo: teste de ANOVA one way 

# Compute the analysis of variance
res.aov <- aov(Vitamina_C ~ Grupo, data = df2)
summary(res.aov)

TukeyHSD(res.aov)

## http://www.sthda.com/english/wiki/one-way-anova-test-in-r


# visualizar resultados

tukey <- df2 %>%
  tukey_hsd(Vitamina_C ~ Grupo)

tukey


tukey <- tukey %>%
  filter(p.adj < 0.05)

tukey <- df2 %>%
  tukey_hsd(Vitamina_C ~ Grupo) %>%
  add_y_position()

ggplot(df2, aes(x = Grupo, y = Vitamina_C, fill = Grupo)) +
  geom_boxplot() +
  stat_pvalue_manual(tukey, label = "p.adj.signif") +
  theme_minimal()
