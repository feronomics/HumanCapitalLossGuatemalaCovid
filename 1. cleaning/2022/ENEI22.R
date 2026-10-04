## IMPACTO DE LA PANDEMIA EN RESULTADOS LABORALES

##Cargar paquetes
library(foreign)
library(dplyr)
library(VIM)
library(mice)

### ENEI 2022

##Cargar los datos:
enei22 <- read.spss("ENEI_2022.sav", to.data.frame = TRUE)

### ====================== CONTROL =========================================
## Seleccionamos variables de interes:
control22 <- subset(enei22, select = c(
  areag, factor, P03A02, P03A03, P03A06, P04A05A, P05C26, 
  P05C27C, P05C28B, P05E01A, pea, Ocupados, formalidad))

## Renombramos las variables:
control22_renamed <- control22 %>% rename(
  area = areag,
  SEXO = P03A02,
  EDAD = P03A03,
  ETNIA = P03A06,
  NIVEL_EDUC = P04A05A,
  SALARIO = P05C26,
  HORAS_EXTRA = P05C27C,
  COMISIONES = P05C28B,
  HORAS = P05E01A)

## Creamos la variable ingreso:
control22_renamed <- control22_renamed %>%
  mutate(INGRESO = rowSums(across(c(SALARIO, HORAS_EXTRA, COMISIONES)), na.rm=T)) %>%
  select(-SALARIO, -HORAS_EXTRA, -COMISIONES) 

## Transformamos las variables de interes:
control22_transformed <- control22_renamed %>% mutate(
  area = ifelse(area == "Urbana", 1, 0),
  SEXO = ifelse(SEXO == "Mujer", 1, 0),
  ETNIA = ifelse(ETNIA == "Ladino" | ETNIA == "Extranjero", 0, 1),
  pea = ifelse(is.na(pea), 0, pea),
  Ocupados = case_when(Ocupados == "Población ocupada" ~ 1, TRUE ~ 0), 
  formalidad = case_when(formalidad == "Informal" ~ 1, TRUE ~ 0),
  HORAS = ifelse(is.na(HORAS), 0, HORAS)) %>% rename(
  urban = area,
  weight = factor,
  woman = SEXO, 
  age = EDAD,
  indigenous = ETNIA,
  educ_level = NIVEL_EDUC,
  occupied = Ocupados,
  informal = formalidad, 
  income = INGRESO,
  hours = HORAS)

## Filtrar las cohortes de interés:
control22_filtered <- control22_transformed %>% filter(
  age == 21, educ_level %in% c("Diversificado", "Superior", "Maestría", "Doctorado") )

## Verificamos si hay datos ausentes:
md.pattern(control22_filtered)

## Creamos variables treatment, post y año
control22_clean <- control22_filtered %>% mutate(
  post = 1,
  treatment = 0,
  year = 2022)

## exportamos resultado: 
write.csv(control22_clean, "control22.csv")


### ====================== TRATAMIENTO =========================================
## Seleccionamos variables de interes:
treat22 <- subset(enei22, select = c(
  areag, factor, P03A02, P03A03, P03A06, P04A05A, P05C26, 
  P05C27C, P05C28B, P05E01A, pea, Ocupados, formalidad))

## Renombramos las variables:
treat22_renamed <- treat22 %>% rename(
  area = areag,
  SEXO = P03A02,
  EDAD = P03A03,
  ETNIA = P03A06,
  NIVEL_EDUC = P04A05A,
  SALARIO = P05C26,
  HORAS_EXTRA = P05C27C,
  COMISIONES = P05C28B,
  HORAS = P05E01A)

## Creamos la variable ingreso:
treat22_renamed <- treat22_renamed %>%
  mutate(INGRESO = rowSums(across(c(SALARIO, HORAS_EXTRA, COMISIONES)), na.rm=T)) %>%
  select(-SALARIO, -HORAS_EXTRA, -COMISIONES) 

## Transformamos las variables de interes:
treat22_transformed <- treat22_renamed %>% mutate(
  area = ifelse(area == "Urbana", 1, 0),
  SEXO = ifelse(SEXO == "Mujer", 1, 0),
  ETNIA = ifelse(ETNIA == "Ladino" | ETNIA == "Extranjero", 0, 1),
  pea = ifelse(is.na(pea), 0, pea),
  Ocupados = case_when(Ocupados == "Población ocupada" ~ 1, TRUE ~ 0), 
  formalidad = case_when(formalidad == "Informal" ~ 1, TRUE ~ 0),
  HORAS = ifelse(is.na(HORAS), 0, HORAS)) %>% rename(
    urban = area,
    weight = factor,
    woman = SEXO, 
    age = EDAD,
    indigenous = ETNIA,
    educ_level = NIVEL_EDUC,
    occupied = Ocupados,
    informal = formalidad, 
    income = INGRESO,
    hours = HORAS)

## Filtrar las cohortes de interés:
treat22_filtered <- treat22_transformed %>% filter(
  age == 20, educ_level %in% c("Diversificado", "Superior", "Maestría", "Doctorado") )

## Verificamos si hay datos ausentes:
md.pattern(treat22_filtered)

## Creamos variables treatment, post y año
treat22_clean <- treat22_filtered %>% mutate(
  post = 1,
  treatment = 1,
  year = 2022)

## exportamos resultado: 
write.csv(treat22_clean, "treat22.csv")


