# R para bioinformática 
<i> Encontros com aluno de doutarado no estudo de R para análises na área da saúde </i>

<p> <strong> DIA 01 </strong> <p>

<strong>1. </strong>Baixar R e RStudio em https://posit.co/download/rstudio-desktop;

<strong>2. </strong> Pacotes de R usados no encontro:
```r

library(dbplyr)
library(dplyr)
library(plm)
library(tidyverse)
library("nloptr")
library("lme4")
library(rstatix)
library(car)

# caso não possuir, instalar usando comando install.packages(nomedopacote)
```

<strong>3. </strong> Entender as interfaces do RStudio

![Fluxograma](https://www.est.ufmg.br/~cristianocs/Pacotes2021/PainelLegenda.png)
   
<strong>4.</strong> Como inserir seus dados de pesquisa? 
<table>
  <tr>
    <td align="center">📥<br><b>Importar dados</b></td>
    <td>➜</td>
    <td align="center">🗂️<br><b>Organizar dados</b></td>
    <td>➜</td>
    <td align="center">⚙️<br><b>Transformar dados</b></td>
    <td>➜</td>
    <td align="center">📊<br><b>Visualizar resultados</b></td>
  </tr>
</table>

<strong> Importar dados </strong> 

```r
df1 <- read.csv(file = "/home/jara/Downloads/dia01.csv" , header = TRUE, sep = ";")
head(df1)
```

<strong> Organizar dados </strong> 

```r
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
```


<strong> Transformar dads </strong> 

```r

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

## normalidade ok : há outros parametros que devem ser verificados , como visualização de histograma etc.
## leituras recomendadas
## http://www.sthda.com/english/wiki/normality-test-in-r
## https://www.statology.org/test-for-normality-in-r/

# 2 passo: teste de homocedasticidade -  teste F

homocedasticidade <- with(df2, leveneTest(Vitamina_C, Grupo))

homocedasticidade

## https://programa-r.forumeiros.com/t111-solucao-para-problemas-com-a-funcao-levenetest

# 3 passo: teste de ANOVA one way 

# Compute the analysis of variance
res.aov <- aov(Vitamina_C ~ Grupo, data = df2)
summary(res.aov)

TukeyHSD(res.aov)

## http://www.sthda.com/english/wiki/one-way-anova-test-in-r
```

<strong> Visualizar dados </strong> 

```r
df1 <- read.csv(file = "/home/jara/Downloads/exercicio_1.csv" , header = TRUE, sep = ";")
head(df1)
```


<p> <strong> DIA 01 </strong> <p>

O teste F de homocedasticidade (2 grupos) verifica se as variâncias de dois ou mais grupos são iguais (homogêneas), sendo fundamental para validar ANOVA e testes t. Ele calcula a razão entre variâncias; um valor \(F\) próximo de 1 sugere homocedasticidade, enquanto valores altos indicam heterocedasticidade. A hipótese nula assume variâncias iguais.

O  Teste de Levene  (3 ou mais grupos) é um método estatístico para avaliar a  igualdade de variâncias  entre dois ou mais grupos. Ele avalia se a variabilidade dos dados é consistente entre as categorias, o que é uma premissa fundamental em muitos  testes paramétricos . O Teste de Levene determina se os grupos apresentam variabilidade semelhante comparando os desvios em relação à média (ou mediana).O teste F de homocedasticidade (2 grupos) verifica se as variâncias de dois ou mais grupos são iguais (homogêneas), sendo fundamental para validar ANOVA e testes t. Ele calcula a razão entre variâncias; um valor \(F\) próximo de 1 sugere homocedasticidade, enquanto valores altos indicam heterocedasticidade. A hipótese nula assume variâncias iguais.




