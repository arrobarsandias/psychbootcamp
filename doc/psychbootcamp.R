## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(psychbootcamp)

## ---- echo=FALSE--------------------------------------------------------------
table_header <- c(
  "Referencia", 
  "Datos", 
  "Características", 
  "Muestra", 
  "Variables", 
  "Análisis")
table_body <- c(
  "Collado, S., Fidalgo, C., Rodríguez-Rey, R., & Sorrel, M. A. (2022). Development and validation of the self-efficacy for writing and defending academic texts scale. Psicología Educativa. https://doi.org/10.5093/psed2022a15",
  "https://osf.io/7bv3a/", 
  "8 ítems en formato de respuesta graduada de 5 categorías", 
  "418 estudiantes universitarios españoles; recogida online; muestreo no probabilístico", 
  "Codebook",
  "Análisis de ítems; Fiabilidad; Validez: Estructura interna; Validez: Relación con otras variables; Interpretación de puntuaciones
")

table_frame <- data.frame(table_body)
row.names(table_frame) <- table_header
table <- kableExtra::kable_styling(kableExtra::column_spec(knitr::kable(table_frame, format="html"), 1, bold = TRUE), bootstrap_options = c("condensed"))
gsub("<thead>.*</thead>", "", table)

## ---- echo = TRUE, message= FALSE, warning = FALSE----------------------------
library(cdmTools)
library(CTT)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(MVN)
library(psych)

## -----------------------------------------------------------------------------
dat <- psychbootcamp::SEAT
dim(dat)

## -----------------------------------------------------------------------------
SEAT <- 1:8 # SEAT
STAIE <- 9:13 # STAI-Estado
EFGEN <- 14:23 # Autoeficacia general
EFESC <- 24:39 # Autoeficacia escritura
EFHAB <- 40:49 # Autoeficacia hablar
ANTFG <- 50:57 # Ansiedad TFG
COHAB <- 58:69 # Confianza hablar
STAIR <- 70:74 # STAI-Rasgo

## -----------------------------------------------------------------------------
item.freq <- psych::response.frequencies(dat[, SEAT])
round(item.freq, 3)

## -----------------------------------------------------------------------------
item.stat <- psych::describe(dat[, SEAT])
item.stat

## -----------------------------------------------------------------------------
item.cor <- cor(dat[, SEAT], method = "pearson", use = "complete.obs")
round(item.cor, 2)
# psych::corPlot(dat[, SEAT]) # para sacar un gráfico

## -----------------------------------------------------------------------------
item.ctt <- CTT::itemAnalysis(dat[, SEAT])$itemReport

## -----------------------------------------------------------------------------
item.scale <- cor(cbind(rowSums(dat[, ANTFG]), dat[, SEAT]))[-1, 1]
round(cbind(item.freq, item.ctt[, -1], "rjY" = item.scale), 2)

## -----------------------------------------------------------------------------
kmo <- psych::KMO(dat[, SEAT])
kmo

## -----------------------------------------------------------------------------
mvn.test <- MVN::mvn(dat[, SEAT])
mvn.test$multivariateNormality

## -----------------------------------------------------------------------------
abs(mvn.test$Descriptives$Skew) > 3
abs(mvn.test$Descriptives$Kurtosis) > 3

## ---- message=FALSE-----------------------------------------------------------
pa <- cdmTools::paK(dat = dat[, SEAT], cor = "cor", verbose = FALSE) 
pa$sug.K
pa$plot

## ---- message=FALSE-----------------------------------------------------------
efa1 <- psych::fa(r = dat[, SEAT], nfactors = 1, fm = "ml")
efa2 <- psych::fa(r = dat[, SEAT], nfactors = 2, fm = "ml", rotate = "oblimin")

## ---- message=FALSE-----------------------------------------------------------
efa1$chi # valor del estadistico chi-cuadrado
efa1$dof # grados de libertad
1 - pchisq(q = efa1$chi, df = efa1$dof) # valor p asociado al estadistico chi-cuadrado

## ---- message=FALSE-----------------------------------------------------------
mean(abs(residuals(efa1, diag = FALSE)) >= 0.05, na.rm = TRUE)

## ---- message=FALSE-----------------------------------------------------------
sqrt(mean(residuals(efa1, diag = FALSE)^2, na.rm = TRUE))

## ---- message=FALSE-----------------------------------------------------------
print(efa1$loadings, cut = 0)

## ---- message=FALSE-----------------------------------------------------------
comp.efa <- data.frame(chi = c(efa1$chi, efa2$chi),
                       df = c(efa1$dof, efa2$dof),
                       p.value = c(1 - pchisq(q = efa1$chi, df = efa1$dof),
                                   1 - pchisq(q = efa2$chi, df = efa2$dof)),
                       SRMR = c(sqrt(mean(residuals(efa1, diag = FALSE)^2, na.rm = TRUE)),
                                sqrt(mean(residuals(efa2, diag = FALSE)^2, na.rm = TRUE))),
                       prop.res.05 = c(mean(abs(residuals(efa1, diag = FALSE)) >= 0.05, na.rm = TRUE), 
                                       mean(abs(residuals(efa2, diag = FALSE)) >= 0.05, na.rm = TRUE)),
                       prop.var = c(efa1$Vaccounted[2], efa2$Vaccounted[3,2]),
                       prop.load.03 = c(mean(abs(efa1$loadings[1:8]) >= 0.3), 
                                        mean(abs(efa2$loadings[c(1:5, 14:16)]) >= 0.3)),
                       row.names = c("EFA1", "EFA2"))
round(comp.efa, 2)

## -----------------------------------------------------------------------------
int.consistency <- CTT::itemAnalysis(dat[, SEAT])
int.consistency$alpha

## -----------------------------------------------------------------------------
sH <- psych::splitHalf(dat[, SEAT], raw = TRUE)
sH$meanr

## -----------------------------------------------------------------------------
dat_S <- data.frame("SEAT" = rowSums(dat[, SEAT]),
                    "EFGEN" = rowSums(dat[, EFGEN]),
                    "EFESC" = rowSums(dat[, EFESC]),
                    "EFHAB" = rowSums(dat[, EFHAB]),
                    "COHAB" = rowSums(dat[, COHAB]),
                    "ANTFG" = rowSums(dat[, ANTFG]),
                    "STAIE" = rowSums(dat[, STAIE]),
                    "STAIR" = rowSums(dat[, STAIR]))
colnames(dat_S) <- c("SEAT", "EFGEN", "EFESC", "EFHAB", "COHAB", "ANTFG", "STAIE", "STAIR")
criterion.cor <- cor(dat_S, method = "pearson", use = "complete.obs")
round(criterion.cor, 2)

## ---- message=FALSE-----------------------------------------------------------
tot_n_1 <- rowSums(dat[, SEAT])
baremo_n_1 <- data.frame(table(tot_n_1))
baremo_n_1$percent <- round((baremo_n_1$Freq/sum(baremo_n_1$Freq))*100, 1)
baremo_n_1$cumpercent <- round(cumsum(baremo_n_1$percent), 1)
baremo_n_1$rangocent <- round((baremo_n_1$cumpercent-baremo_n_1$percent)+(baremo_n_1$percent/2), 0)
baremo_n_1$zscore <- round((as.numeric(levels(baremo_n_1$tot_n_1)) - mean(tot_n_1))/sd(tot_n_1), 2)
baremo_n_1$tscore <- round(10*baremo_n_1$zscore + 50, 0)
baremo_n_1$dscore <- round(20*baremo_n_1$zscore + 50, 0)
baremo_n_1$rangocent[baremo_n_1$rangocent > 99] <- 99
baremo_n_1$rangocent[baremo_n_1$rangocent < 1] <- 1
baremo_n_1$tscore[baremo_n_1$tscore > 90] <- 90
baremo_n_1$tscore[baremo_n_1$tscore < 10] <- 10
baremo_n_1$dscore[baremo_n_1$dscore > 100] <- 100
baremo_n_1$dscore[baremo_n_1$dscore < 0] <- 0
baremo_n_1

## ---- message=FALSE-----------------------------------------------------------
ggpubr::gghistogram(tot_n_1, fill = "darkgray", xlab = "Sum Score", ylab = "Count", xlim = c(0, 40), bins = 20) + 
  ggplot2::scale_x_continuous(breaks = seq(0, 40, by = 4))

