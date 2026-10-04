## IMPACTO DE LA PANDEMIA EN RESULTADOS LABORALES

##Cargar paquetes
library(foreign)
library(dplyr)
library(VIM)
library(mice)

### ENEI 2018

##Cargar los datos:
enei18 <- read.spss("ENEI_2018.sav", to.data.frame = TRUE)

## Seleccionamos variables de interes:
control18 <- subset(enei18, select = c(
  AREA, FACTOR, PPA02, PPA03, PPA06, P03A02, P03A03, P03A04A, P04C10, 
  P04C11C, P04C12B, P04C28A, P04C28B, P04C28C, P04C28D, P04C28E, 
  P04C28F, P04C28G, PEA, OCUPADOS, FORMAL_INFORMAL))

## Renombramos las variables:
control18_renamed <- control18 %>% rename(
  SEXO = PPA02,
  EDAD = PPA03,
  ETNIA = PPA06,
  INSCRITO = P03A02,
  TIPO_ESCUELA = P03A03,
  NIVEL_EDUC = P03A04A,
  SALARIO = P04C10,
  HORAS_EXTRA = P04C11C,
  COMISIONES = P04C12B,
  LUNES = P04C28A,
  MARTES = P04C28B, 
  MIERCOLES = P04C28C,
  JUEVES = P04C28D,
  VIERNES = P04C28E,
  SABADO = P04C28F,
  DOMINGO = P04C28G)

## Creamos la variable ingreso y horas trabajadas:
control18_renamed <- control18_renamed %>%
  mutate(INGRESO = rowSums(across(c(SALARIO, HORAS_EXTRA, COMISIONES)), na.rm=T), 
         HORAS = rowSums(across(c(LUNES,MARTES,MIERCOLES,JUEVES,VIERNES,SABADO,DOMINGO)),na.rm=T)) %>%
  select(-SALARIO, -HORAS_EXTRA, -COMISIONES, -LUNES, -MARTES, 
         -MIERCOLES, -JUEVES, -VIERNES, -SABADO, -DOMINGO) 

## Transformamos las variables de interes:
control18_transformed <- control18_renamed %>% mutate(
  AREA = ifelse(AREA == "Urbana", 1, 0),
  SEXO = ifelse(SEXO == "Mujer", 1, 0),
  ETNIA = ifelse(ETNIA == "Ladino" | ETNIA == "Extranjero", 0, 1),
  INSCRITO = case_when(INSCRITO == "Si" ~ 1, TRUE ~ 0),
  TIPO_ESCUELA = case_when(TIPO_ESCUELA == "Público" ~ 1, TRUE ~ 0), 
  PEA = ifelse(is.na(PEA), 0, PEA),
  OCUPADOS = case_when(OCUPADOS == "Población ocupada" ~ 1, TRUE ~ 0), 
  FORMAL_INFORMAL = case_when(FORMAL_INFORMAL == "Informal" ~ 1, TRUE ~ 0)) %>% rename(
  urban = AREA,
  weight = FACTOR,
  women = SEXO, 
  age = EDAD,
  indigenous = ETNIA,
  enrolled = INSCRITO,
  public_school = TIPO_ESCUELA, 
  educ_level = NIVEL_EDUC,
  active = PEA, 
  occupied = OCUPADOS,
  informal = FORMAL_INFORMAL, 
  labor_income = INGRESO,
  hours = HORAS)

## Filtrar las cohortes de interés:
control18_filtered <- control18_transformed %>% filter(
  age == 17, enrolled == 1, educ_level == "Diversificado")

## Verificamos si hay datos ausentes:
md.pattern(control18_filtered)

## Creamos variables treatment, post y año
control18_clean <- control18_filtered %>% mutate(
  post = 0,
  treatment = 0,
  year = 2018)

## exportamos resultado: 
write.csv(control18_clean, "control18.csv")
