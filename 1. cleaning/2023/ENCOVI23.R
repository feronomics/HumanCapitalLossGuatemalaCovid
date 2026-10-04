## IMPACTO DE LA PANDEMIA EN RESULTADOS LABORALES

##Cargar paquetes
library(foreign)
library(dplyr)
library(VIM)
library(mice)

### ENCOVI 2023

##Cargar los datos:
encovi23 <- read.spss("ENCOVI_23.sav", to.data.frame = TRUE)

### ====================== CONTROL =========================================
## Seleccionamos variables de interes:
control23 <- subset(encovi23, select = c(
  AREA, FACTOR, PPA02, PPA03, P04A07, P06B26A, P10C27, 
  P10C28C, P10C29B, P10E01A, PEA, OCUPADOS, FORMAL_INFORMAL))

## Renombramos las variables:
control23_renamed <- control23 %>% rename(
  SEXO = PPA02,
  EDAD = PPA03,
  ETNIA = P04A07,
  NIVEL_EDUC = P06B26A,
  SALARIO = P10C27,
  HORAS_EXTRA = P10C28C,
  COMISIONES = P10C29B,
  HORAS = P10E01A)

## Creamos la variable ingreso:
control23_renamed <- control23_renamed %>%
  mutate(INGRESO = rowSums(across(c(SALARIO, HORAS_EXTRA, COMISIONES)), na.rm=T)) %>%
  select(-SALARIO, -HORAS_EXTRA, -COMISIONES) 

## Transformamos las variables de interes:
control23_transformed <- control23_renamed %>% mutate(
  AREA = ifelse(AREA == "Urbana", 1, 0),
  SEXO = ifelse(SEXO == "Mujer", 1, 0),
  ETNIA = ifelse(ETNIA == "Ladino / mestizo?" | ETNIA == "Extranjero?", 0, 1),
  PEA = ifelse(is.na(PEA), 0, PEA),
  OCUPADOS = case_when(OCUPADOS == "Población ocupada" ~ 1, TRUE ~ 0), 
  FORMAL_INFORMAL = case_when(FORMAL_INFORMAL == "Informal" ~ 1, TRUE ~ 0),
  HORAS = ifelse(is.na(HORAS), 0, HORAS)) %>% rename(
  urban = AREA,
  weight = FACTOR,
  woman = SEXO, 
  age = EDAD,
  indigenous = ETNIA,
  educ_level = NIVEL_EDUC,
  active = PEA,
  occupied = OCUPADOS,
  informal = FORMAL_INFORMAL, 
  ingreso = INGRESO,
  hours = HORAS)

## Filtrar las cohortes de interés:
control23_filtered <- control23_transformed %>% filter(
  age == 22, educ_level %in% c("Diversificado", "Universitario", "Maestría", "Doctorado") )

## Verificamos si hay datos ausentes:
md.pattern(control23_filtered)

## Creamos variables treatment, post y año
control23_clean <- control23_filtered %>% mutate(
  post = 1,
  treatment = 0,
  year = 2023)

## exportamos resultado: 
write.csv(control23_clean, "control23.csv")


### ====================== TRATAMIENTO =========================================
## Seleccionamos variables de interes:
treat23 <- subset(encovi23, select = c(
  AREA, FACTOR, PPA02, PPA03, P04A07, P06B26A, P10C27, 
  P10C28C, P10C29B, P10E01A, PEA, OCUPADOS, FORMAL_INFORMAL))

## Renombramos las variables:
treat23_renamed <- treat23 %>% rename(
  SEXO = PPA02,
  EDAD = PPA03,
  ETNIA = P04A07,
  NIVEL_EDUC = P06B26A,
  SALARIO = P10C27,
  HORAS_EXTRA = P10C28C,
  COMISIONES = P10C29B,
  HORAS = P10E01A)

## Creamos la variable ingreso:
treat23_renamed <- treat23_renamed %>%
  mutate(INGRESO = rowSums(across(c(SALARIO, HORAS_EXTRA, COMISIONES)), na.rm=T)) %>%
  select(-SALARIO, -HORAS_EXTRA, -COMISIONES) 

## Transformamos las variables de interes:
treat23_transformed <- treat23_renamed %>% mutate(
  AREA = ifelse(AREA == "Urbana", 1, 0),
  SEXO = ifelse(SEXO == "Mujer", 1, 0),
  ETNIA = ifelse(ETNIA == "Ladino / mestizo?" | ETNIA == "Extranjero?", 0, 1),
  PEA = ifelse(is.na(PEA), 0, PEA),
  OCUPADOS = case_when(OCUPADOS == "Población ocupada" ~ 1, TRUE ~ 0), 
  FORMAL_INFORMAL = case_when(FORMAL_INFORMAL == "Informal" ~ 1, TRUE ~ 0),
  HORAS = ifelse(is.na(HORAS), 0, HORAS)) %>% rename(
    urban = AREA,
    weight = FACTOR,
    woman = SEXO, 
    age = EDAD,
    indigenous = ETNIA,
    educ_level = NIVEL_EDUC,
    active = PEA,
    occupied = OCUPADOS,
    informal = FORMAL_INFORMAL, 
    income = INGRESO,
    hours = HORAS)

## Filtrar las cohortes de interés:
treat23_filtered <- treat23_transformed %>% filter(
  age == 21, educ_level %in% c("Diversificado", "Universitario", "Maestría", "Doctorado") )

## Verificamos si hay datos ausentes:
md.pattern(treat23_filtered)

## Creamos variables treatment, post y año
treat23_clean <- treat23_filtered %>% mutate(
  post = 1,
  treatment = 1,
  year = 2023)

## exportamos resultado: 
write.csv(treat23_clean, "treat23.csv")


