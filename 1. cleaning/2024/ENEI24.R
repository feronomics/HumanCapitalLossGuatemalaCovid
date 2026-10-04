## IMPACTO DE LA PANDEMIA EN RESULTADOS LABORALES

##Cargar paquetes
library(foreign)
library(dplyr)
library(VIM)
library(mice)

### ENEI 2024

##Cargar los datos:
enei24 <- read.spss("ENEI_2024.sav", to.data.frame = TRUE)

## Seleccionamos variables de interes:
treat24 <- subset(enei24, select = c(
  P00A10, FACTOR, P02A02, P02A03, P02A08, P03A03A, P05D01, 
  P05D02C, P05D03B, P05C15A, P05C15B, P05C15C, P05C15D, 
  P05C15E, P05C15F, P05C15G, PEA, OCUPADOS, FORMAL_INFORMAL))

## Renombramos las variables:
treat24_renamed <- treat24 %>%
  mutate(across(c(P05C15A:P05C15G), ~ as.numeric(unlist(.x)))) %>%
  rename(
    area = P00A10,
    SEXO = P02A02,
    EDAD = P02A03,
    ETNIA = P02A08,
    NIVEL_EDUC = P03A03A,
    SALARIO = P05D01,
    HORAS_EXTRA = P05D02C,
    COMISIONES = P05D03B, 
    LUNES = P05C15A, 
    MARTES = P05C15B, 
    MIERCOLES = P05C15C,
    JUEVES = P05C15D,
    VIERNES = P05C15E,
    SABADO = P05C15F,
    DOMINGO = P05C15G)

## Creamos la variable ingreso:
treat24_renamed <- treat24_renamed %>%
  mutate(INGRESO = rowSums(across(c(SALARIO, HORAS_EXTRA, COMISIONES)), na.rm=T),
         HORAS = rowSums(across(c(LUNES, MARTES, MIERCOLES, JUEVES, VIERNES, SABADO, DOMINGO)), na.rm=T)) %>%
  select(-SALARIO, -HORAS_EXTRA, -COMISIONES, -LUNES, -MARTES, 
         -MIERCOLES, -JUEVES, -VIERNES, -SABADO, -DOMINGO) 

## Transformamos las variables de interes:
treat24_transformed <- treat24_renamed %>% mutate(
  area = ifelse(area == "Urbana", 1, 0),
  SEXO = ifelse(SEXO == "Mujer", 1, 0),
  ETNIA = ifelse(ETNIA == "Ladino" | ETNIA == "Extranjero", 0, 1),
  PEA = ifelse(is.na(PEA), 0, PEA),
  OCUPADOS = case_when(OCUPADOS == "Población ocupada" ~ 1, TRUE ~ 0), 
  FORMAL_INFORMAL = case_when(FORMAL_INFORMAL == "Informal" ~ 1, TRUE ~ 0),
  HORAS = ifelse(is.na(HORAS), 0, HORAS)) %>% rename(
    urban = area,
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
treat24_filtered <- treat24_transformed %>% filter(
  age == 22, educ_level %in% c("DIVERSIFICADO", "SUPERIOR", "MAESTRÍA", "DOCTORADO") )

## Verificamos si hay datos ausentes:
md.pattern(treat24_filtered)

## Creamos variables treatment, post y año
treat24_clean <- treat24_filtered %>% mutate(
  post = 1,
  treatment = 1,
  year = 2024)

## exportamos resultado: 
write.csv(treat24_clean, "treat24.csv")


