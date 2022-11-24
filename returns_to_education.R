#Importo base de datos
base <- eph::get_microdata(2005,1)

#Filtro base por ingresos positivos y edad entre 18 y 70
library(tidyverse)
base <- base %>% filter(base$P21>0 & base$PP3E_TOT > 0 & base$CH06 >18 & base$CH06 < 70)

#Construir:
# lnwage (log del ingreso por hora)
base$salario_semanal <- base$P21/4
base$lnwage <- log(base$salario_semanal/base$PP3E_TOT)
# edupi (si individuo no tiene educación o primaria incompleta)
base$edupi <- ifelse(base$NIVEL_ED==1|base$NIVEL_ED==7, 1, 0)
# edup (primaria completa)
base$edup <- ifelse(base$NIVEL_ED==2, 1, 0)
# edusi (secundaria incompleta)
base$edusi <-  ifelse(base$NIVEL_ED==3, 1, 0)
# edus (secundaria completa)
base$edus <-  ifelse(base$NIVEL_ED==4, 1, 0)
# eduui (universitaria incompleta)
base$eduui <-  ifelse(base$NIVEL_ED==5, 1, 0)
# eduu (universitaria completa)
base$eduu <-  ifelse(base$NIVEL_ED==6, 1, 0)
# yearse (años de educación): =3 si no educación o primaria incompleta, =7 si primaria completa, =9.5 si secundaria incompleta, =12 si secundaria completa, =14 si universitaria incompleta, =17 si universitaria completa.

base <- base %>% mutate(yearse= case_when(NIVEL_ED== 1 | NIVEL_ED== 7 ~ 3, 
                                          NIVEL_ED== 2 ~ 7, 
                                          NIVEL_ED== 3 ~ 9.5, 
                                          NIVEL_ED== 4 ~ 12, 
                                          NIVEL_ED== 5 ~ 14, 
                                          NIVEL_ED== 6 ~ 17))
# Edad (edad del individuo)
base$Edad <- base$CH06
# Edad2 (edad al cuadrado)
base$Edad2 <- (base$CH06)^2
# Exper (experiencia, calculada como age-yearse-6)
base$Exper <- as.numeric(base$Edad)-as.numeric(base$yearse)-6
# Exper2 (experiencia al cuadrado)
base$Exper2 <- (base$Exper)^2
# Patrón (si es patrón en su ocupación principal)
base$Patron <- ifelse(base$CAT_OCUP==1, 1, 0) 
# Independiente (si es cuenta propista o trabajador familiar en la ocupación principal)
base$Independiente <- ifelse(base$CAT_OCUP==2 |base$CAT_OCUP==4, 1, 0) 

#Armo regresión
mod1 <- lm(lnwage ~ yearse + Exper + Exper2, base)
summary(mod1)
stargazer::stargazer(mod1, type="latex")

#Exporto resultado de regresión a HTML
library(jtools)
if (requireNamespace("huxtable")) {
  export_summs(mod1,
               model.names = "Model 1",
               scale = TRUE, robust = TRUE)
}

export_summs(mod1)

# Otros modelos
mod2 <- lm(lnwage ~ yearse + Edad + Edad2, base)
summary(mod2)

mod3 <- lm(lnwage ~ edup + edusi + edus + eduui+ eduu + Exper + Exper2, base)
summary(mod3)

mod4 <- lm(lnwage ~ edup + edusi + edus + eduui+ eduu + Edad + Edad2,  base)
summary(mod4)

mod5 <- lm(lnwage ~ edup + edusi + edus + eduui+ eduu + Exper + Exper2 + Patron + Independiente, base)
summary(mod5)

stargazer::stargazer(mod1, mod2, mod3, mod4, mod5, type="text")



#Mincer
mod <- lm(lnwage ~ Exper + Exper2 +  edup + edusi + edus + eduui+ eduu, base)
summary(mod)
stargazer::stargazer(mod, type="latex")

# Mincer varones
base_varones <- base %>% filter(base$CH04==1)
mod2 <- lm(lnwage ~ Exper + Exper2 +  edup + edusi + edus + eduui+ eduu, base_varones)
summary(mod2)

# Mincer mujeres
base_mujeres <- base %>% filter(base$CH04==2)
mod3 <- lm(lnwage ~ Exper + Exper2 +  edup + edusi + edus + eduui+ eduu, base_mujeres)
summary(mod3)

# Mincer region GBA
base_gba <- base %>% filter(base$REGION==01)
mod4 <- lm(lnwage ~ Exper + Exper2 +  edup + edusi + edus + eduui+ eduu, base_gba)
summary(mod4)

# Mincer region NOA
base_noa <- base %>% filter(base$REGION==40)
mod5 <- lm(lnwage ~ Exper + Exper2 +  edup + edusi + edus + eduui+ eduu, base_noa)
summary(mod5)

# Mincer region NEA
base_nea <- base %>% filter(base$REGION==41)
mod6 <- lm(lnwage ~ Exper + Exper2 +  edup + edusi + edus + eduui+ eduu, base_nea)
summary(mod6)

# Mincer region CUYO
base_cuyo <- base %>% filter(base$REGION==42)
mod7 <- lm(lnwage ~ Exper + Exper2 +  edup + edusi + edus + eduui+ eduu, base_cuyo)
summary(mod7)

# Mincer region PAMPEANA
base_pampeana <- base %>% filter(base$REGION==43)
mod8 <- lm(lnwage ~ Exper + Exper2 +  edup + edusi + edus + eduui+ eduu, base_pampeana)
summary(mod8)

# Mincer region PATAGONIA
base_patagonia <- base %>% filter(base$REGION==44)
mod9 <- lm(lnwage ~ Exper + Exper2 +  edup + edusi + edus + eduui+ eduu, base_nea)
summary(mod9)

stargazer::stargazer(mod2, mod3, mod4, mod5, mod6, mod7, mod8,mod9, type="text")


mod9$coefficients

data<- data.table::data.table(varones = mod2$coefficients,
                              mujeres = mod3$coefficients, 
                              GBA = mod4$coefficients, 
                              NOA = mod5$coefficients, 
                              NEA = mod6$coefficients, 
                              Cuyo = mod7$coefficients, 
                              Pampeana = mod8$coefficients, 
                              Patagonia= mod9$coefficients)

data2 <- data[-c(1), ] 

#Tabla retornos a la educación 2004-2022
table <- data.table::data.table(year = as.numeric(c("2004", "2005", "2006", "2007", "2008", "2009", "2010", 
                                         "2011", "2012", "2013", "2014", "2015", "2016", "2017", 
                                         "2018", "2019", "2020", "2021", "2022")), 
                                returns = as.numeric(c("9.63", "9.68", "10", "10", "9.16", "8.87", "8.77", 
                                            "8.34", "8.02", "7.83", "7.42", "7.59", "8.58", "8.5", 
                                            "8.1", "8.2", "8.61", "8.35", "8")))


ggplot(table) +
  aes(x = year, y = returns) +
  geom_line(size = 0.5, colour = "#112446") +
  labs(
    x = "Año",
    y = "Tasa de retorno (%)",
    title = "Tasa de Retornos a la Educación",
    subtitle = "Desde 2004 hasta 2022",
    caption = "Elaboración propia a partir de la EPH (INDEC)"
  ) +
  theme_minimal()


mean(base$yearse)

#Tabla años de escolarización promedio 1950-2020
educ_att <- data.table::data.table(year = as.numeric(c("1950", "1955", "1960", "1965", "1970", "1975", "1980", 
                                                    "1985", "1990", "1995", "2000", "2005", "2010", "2015", 
                                                    "2020")), 
                                returns = as.numeric(c("4.85", "5.20", "5.67", "5.94", "6.31", "6.85", "7.30", 
                                                       "7.85", "8.37", "8.64", "8.73", "9.38", "10.00", "11.50", 
                                                       "11.80")))

library(ggplot2)
ggplot(educ_att) +
  aes(x = year, y = returns) +
  geom_line(size = 0.5, colour = "#112446") +
  labs(
    x = "Año",
    y = "Años de Escolarización",
    title = "Años de Escolarización Promedio",
    subtitle = "Para Argentina desde 1950 hasta 2020",
    caption = "Fuente: Elaboración propia a partir de los datos de Barro & Lee"
  ) +
  theme_minimal()
