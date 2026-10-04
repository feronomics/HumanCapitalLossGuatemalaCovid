## IMPACTO DE LA PANDEMIA EN RESULTADOS LABORALES

##Cargar paquetes
library(dplyr)

### MERGING

##Cargar los datos:

### Grupo control
control18 <- read.table("control18.csv", sep=",", header=T)
control21 <- read.table("control21.csv", sep=",", header=T)
control22 <- read.table("control22.csv", sep=",", header=T)
control23 <- read.table("control23.csv", sep=",", header=T)

### Grupo tratamiento
treat19 <- read.table("treat19.csv", sep=",", header=T)
treat22 <- read.table("treat22.csv", sep=",", header=T)
treat23 <- read.table("treat23.csv", sep=",", header=T)
treat24 <- read.table("treat24.csv", sep=",", header=T)

### Filtramos
control18 <- control18 %>% select(-enrolled, -public_school) %>% rename(woman = women, income = labor_income)
treat19 <- treat19 %>% select(-enrolled, -public_school)

control22 <- control22 %>% rename(active = pea)
treat22 <- treat22 %>% rename(active = pea)
control23 <- control23 %>% rename(income = ingreso)

### Unimos
data <- bind_rows(control18, control21, control22, control23, treat19, treat22, treat23, treat24)
data <- data[,-1]

### Homogeneizamos nivel educativo
data <- data %>% mutate(
  educ_level = case_when(
    educ_level %in% c("Diversificado", "DIVERSIFICADO") ~ "High School",
    educ_level %in% c("Superior", "Universitario", "SUPERIOR") ~ "University", 
    TRUE ~ NA))

### Exportamos data set de resultado
write.csv(data, "data_clean.csv")
