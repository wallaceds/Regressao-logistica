base = read.csv("kag_risk_factors_cervical_cancer.csv")

base$STDs..Time.since.first.diagnosis = NULL
base$STDs..Time.since.last.diagnosis = NULL

summary (base)

sapply(base, class)

base[base=="?"]<-NA

#convertando de factor para numérico
base$First.sexual.intercourse <- as.numeric(as.character(base$First.sexual.intercourse))
base$Smokes <- as.numeric(as.character(base$Smokes))
base$Smokes..packs.year. <- as.numeric(as.character(base$Smokes..packs.year.))
base$Hormonal.Contraceptives..years. <- as.numeric(as.character(base$Hormonal.Contraceptives..years.))
base$IUD..years. <- as.numeric(as.character(base$IUD..years.))
base$STDs..number. <- as.numeric(as.character(base$STDs..number.))
base$STDs.cervical.condylomatosis <- as.numeric(as.character(base$STDs.cervical.condylomatosis))
base$STDs.vulvo.perineal.condylomatosis <- as.numeric(as.character(base$STDs.vulvo.perineal.condylomatosis))
base$STDs.pelvic.inflammatory.disease <- as.numeric(as.character(base$STDs.pelvic.inflammatory.disease))
base$STDs.molluscum.contagiosum <- as.numeric(as.character(base$STDs.molluscum.contagiosum))
base$STDs.HIV <- as.numeric(as.character(base$STDs.HIV))
base$STDs.HPV <- as.numeric(as.character(base$STDs.HPV))
base$Number.of.sexual.partners <- as.numeric(as.character(base$Number.of.sexual.partners))
base$Num.of.pregnancies <- as.numeric(as.character(base$Num.of.pregnancies))
base$Smokes..years. <- as.numeric(as.character(base$Smokes..years.))
base$Hormonal.Contraceptives <- as.numeric(as.character(base$Hormonal.Contraceptives))
base$IUD <- as.numeric(as.character(base$IUD))
base$STDs <- as.numeric(as.character(base$STDs))
base$STDs.condylomatosis <- as.numeric(as.character(base$STDs.condylomatosis))
base$STDs.vaginal.condylomatosis <- as.numeric(as.character(base$STDs.vaginal.condylomatosis))
base$STDs.syphilis <- as.numeric(as.character(base$STDs.syphilis))
base$STDs.genital.herpes <- as.numeric(as.character(base$STDs.genital.herpes))
base$STDs.AIDS <- as.numeric(as.character(base$STDs.AIDS))
base$STDs.Hepatitis.B <- as.numeric(as.character(base$STDs.Hepatitis.B))
base$Smokes <- as.numeric(as.character(base$Smokes))

# conferindo média e onde estão as NA's
mean(base$First.sexual.intercourse, na.rm = TRUE)
base[is.na(base$First.sexual.intercourse),]

base$First.sexual.intercourse = ifelse(is.na(base$First.sexual.intercourse), mean(base$First.sexual.intercourse, na.rm = TRUE), base$First.sexual.intercourse)
base$Number.of.sexual.partners = ifelse(is.na(base$Number.of.sexual.partners), mean(base$Number.of.sexual.partners, na.rm = TRUE), base$Number.of.sexual.partners)
base$Num.of.pregnancies = ifelse(is.na(base$Num.of.pregnancies), mean(base$Num.of.pregnancies, na.rm = TRUE), base$Num.of.pregnancies)
base$Smokes = ifelse(is.na(base$Smokes), mean(base$Smokes, na.rm = TRUE), base$Smokes)
base$Smokes..years. = ifelse(is.na(base$Smokes..years.), mean(base$Smokes..years., na.rm = TRUE), base$Smokes..years.)
base$Smokes..packs.year. = ifelse(is.na(base$Smokes..packs.year.), mean(base$Smokes..packs.year., na.rm = TRUE), base$Smokes..packs.year.)
base$Hormonal.Contraceptives = ifelse(is.na(base$Hormonal.Contraceptives), mean(base$Hormonal.Contraceptives, na.rm = TRUE), base$Hormonal.Contraceptives)
base$Hormonal.Contraceptives..years. = ifelse(is.na(base$Hormonal.Contraceptives..years.), mean(base$Hormonal.Contraceptives..years., na.rm = TRUE), base$Hormonal.Contraceptives..years.)
base$IUD = ifelse(is.na(base$IUD), mean(base$IUD, na.rm = TRUE), base$IUD)
base$IUD..years. = ifelse(is.na(base$IUD..years.), mean(base$IUD..years., na.rm = TRUE), base$IUD..years.)
base$STDs = ifelse(is.na(base$STDs), mean(base$STDs, na.rm = TRUE), base$STDs)
base$STDs..number. = ifelse(is.na(base$STDs..number.), mean(base$STDs..number., na.rm = TRUE), base$STDs..number.)
base$STDs.condylomatosis = ifelse(is.na(base$STDs.condylomatosis), mean(base$STDs.condylomatosis, na.rm = TRUE), base$STDs.condylomatosis)
base$STDs.cervical.condylomatosis = ifelse(is.na(base$STDs.cervical.condylomatosis), mean(base$STDs.cervical.condylomatosis, na.rm = TRUE), base$STDs.cervical.condylomatosis)
base$STDs.vaginal.condylomatosis = ifelse(is.na(base$STDs.vaginal.condylomatosis), mean(base$STDs.vaginal.condylomatosis, na.rm = TRUE), base$STDs.vaginal.condylomatosis)
base$STDs.vulvo.perineal.condylomatosis = ifelse(is.na(base$STDs.vulvo.perineal.condylomatosis), mean(base$STDs.vulvo.perineal.condylomatosis, na.rm = TRUE), base$STDs.vulvo.perineal.condylomatosis)
base$STDs.syphilis = ifelse(is.na(base$STDs.syphilis), mean(base$STDs.syphilis, na.rm = TRUE), base$STDs.syphilis)
base$STDs.pelvic.inflammatory.disease = ifelse(is.na(base$STDs.pelvic.inflammatory.disease), mean(base$STDs.pelvic.inflammatory.disease, na.rm = TRUE), base$STDs.pelvic.inflammatory.disease)
base$STDs.genital.herpes = ifelse(is.na(base$STDs.genital.herpes), mean(base$STDs.genital.herpes, na.rm = TRUE), base$STDs.genital.herpes)
base$STDs.molluscum.contagiosum = ifelse(is.na(base$STDs.molluscum.contagiosum), mean(base$STDs.molluscum.contagiosum, na.rm = TRUE), base$STDs.molluscum.contagiosum)
base$STDs.AIDS = ifelse(is.na(base$STDs.AIDS), mean(base$STDs.AIDS, na.rm = TRUE), base$STDs.AIDS) 
base$STDs.HIV = ifelse(is.na(base$STDs.HIV), mean(base$STDs.HIV, na.rm = TRUE), base$STDs.HIV)
base$STDs.Hepatitis.B = ifelse(is.na(base$STDs.Hepatitis.B), mean(base$STDs.Hepatitis.B, na.rm = TRUE), base$STDs.Hepatitis.B)
base$STDs.HPV = ifelse(is.na(base$STDs.HPV), mean(base$STDs.HPV, na.rm = TRUE), base$STDs.HPV)

# Divisão entre treinamento e teste
library(caTools)
set.seed(1)
divisao = sample.split(base$Biopsy, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

classificador = glm(formula = Biopsy ~ ., family = gaussian(), data = base_treinamento)
probabilidades = predict(classificador, type = 'response', newdata = base_teste[-34])
previsoes = ifelse(probabilidades > 0.5, 1, 0)
matriz_confusao = table(base_teste[, 34], previsoes)
library(caret)
confusionMatrix(matriz_confusao)

# ZeroR - base line classifier (linha base)
table(base_teste$income)

