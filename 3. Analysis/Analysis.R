## IMPACTO DE LA PANDEMIA EN RESULTADOS LABORALES

##Cargar paquetes
library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)
library(margins)
library(marginaleffects)
library(webshot2)
library(gridExtra)
library(ggplot2)


### ANALYSIS

### Cargar los datos
data <- read.table("data_clean.csv", sep=",", header=T)

### Inflacion anual
cpi <- data.frame(year = c(2018, 2019, 2021, 2022, 2023, 2024), 
                  cpi = c(75.68, 78.79, 84.39, 86.81, 95.23, 98.86), 
                  gdppc = c(3.1, 6.1, 9.8, 9.2, 8.8, 6.0))

### Convertimos los ingresos a terminos reales
data <- left_join(data, cpi, by = "year")
data <- data %>% mutate(real_income = income * 75.68 /cpi)

data1 <- data %>% filter(year < 2023)
data1 <- data1 %>% filter(!(year == 2022 & treatment == 0)) 

workers <- data1 %>% filter(real_income > 0)
table(workers[, c("post", "treatment")])

### Modelos con ingreso

### =============== Regresion DiD convencional ============================
m1 <- lm(real_income ~ treatment + post + treatment * post, data1, weights = weight)
se1 <- sqrt(diag(vcovHC(m1, type = "HC1")))
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))

m1a <- lm(hours ~ treatment + post + treatment * post, data1, weights = weight)
se1a <- sqrt(diag(vcovHC(m1a, type = "HC1")))
coeftest(m1a, vcov = vcovHC(m1a, type = "HC1"))

m1b <- lm(occupied ~ treatment + post + treatment * post, data1, weights = weight)
se1b <- sqrt(diag(vcovHC(m1b, type = "HC1")))
coeftest(m1b, vcov = vcovHC(m1b, type = "HC1"))

m1c <- lm(real_income ~ treatment + post + treatment * post, workers, weights = weight)
se1c <- sqrt(diag(vcovHC(m1c, type = "HC1")))
coeftest(m1c, vcov = vcovHC(m1c, type = "HC1"))

m1d <- lm(hours ~ treatment + post + treatment * post, workers, weights = weight)
se1d <- sqrt(diag(vcovHC(m1d, type = "HC1")))
coeftest(m1d, vcov = vcovHC(m1d, type = "HC1"))

### =============== Introducción de Controles ==============================
m2 <- lm(real_income ~ urban + woman + indigenous + treatment + post + treatment*post , data1, weights = weight)
se2 <- sqrt(diag(vcovHC(m2, type = "HC1")))
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))

m2a <- lm(hours ~ urban + woman + indigenous + treatment + post + treatment*post , data1, weights = weight)
se2a <- sqrt(diag(vcovHC(m2a, type = "HC1")))
coeftest(m2a, vcov = vcovHC(m2a, type = "HC1"))

m2b <- lm(occupied ~ urban + woman + indigenous + treatment + post + treatment*post , data1, weights = weight)
se2b <- sqrt(diag(vcovHC(m2b, type = "HC1")))
coeftest(m2b, vcov = vcovHC(m2b, type = "HC1"))

m2c <- lm(real_income ~ urban + woman + indigenous + treatment + post + treatment*post , workers, weights = weight)
se2c <- sqrt(diag(vcovHC(m2c, type = "HC1")))
coeftest(m2c, vcov = vcovHC(m2c, type = "HC1"))

m2d <- lm(hours ~ urban + woman + indigenous + treatment + post + treatment*post , workers, weights = weight)
se2d <- sqrt(diag(vcovHC(m2d, type = "HC1")))
coeftest(m2d, vcov = vcovHC(m2d, type = "HC1"))

### ================ DID con interacciones ==================================
m3 <- lm(real_income ~ urban + woman + indigenous + treatment + post + treatment * post + 
           treatment:post:woman + treatment:post:indigenous + treatment:post:urban, data1, weights = weight)
se3 <- sqrt(diag(vcovHC(m3, type = "HC1")))
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))

m3a <- lm(hours ~ urban + woman + indigenous + treatment + post + treatment * post + 
           treatment:post:woman + treatment:post:indigenous + treatment:post:urban, data1, weights = weight)
se3a <- sqrt(diag(vcovHC(m3a, type = "HC1")))
coeftest(m3a, vcov = vcovHC(m3a, type = "HC1"))

m3b <- lm(occupied ~ urban + woman + indigenous + treatment + post + treatment * post + 
           treatment:post:woman + treatment:post:indigenous + treatment:post:urban, data1, weights = weight)
se3b <- sqrt(diag(vcovHC(m3b, type = "HC1")))
coeftest(m3b, vcov = vcovHC(m3b, type = "HC1"))

m3c <- lm(real_income ~ urban + woman + indigenous + treatment + post + treatment * post + 
           treatment:post:woman + treatment:post:indigenous + treatment:post:urban, workers, weights = weight)
se3c <- sqrt(diag(vcovHC(m3c, type = "HC1")))
coeftest(m3c, vcov = vcovHC(m3c, type = "HC1"))

m3d <- lm(hours ~ urban + woman + indigenous + treatment + post + treatment * post + 
            treatment:post:woman + treatment:post:indigenous + treatment:post:urban, workers, weights = weight)
se3d <- sqrt(diag(vcovHC(m3d, type = "HC1")))
coeftest(m3d, vcov = vcovHC(m3d, type = "HC1"))

### ================= DID Dinamico ========================================
data_dinamic <- data %>% mutate(age = as.factor(age), year = as.factor(year))
workers_dinamic <- data_dinamic %>% filter(real_income > 0)

m4 <- lm(real_income ~ urban + woman + indigenous + treatment + age + treatment*age, data_dinamic, weights = weight)
se4 <- sqrt(diag(vcovHC(m4, type = "HC1")))
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))

m4a <- lm(hours ~ urban + woman + indigenous + treatment + age + treatment*age, data_dinamic, weights = weight)
se4a <- sqrt(diag(vcovHC(m4a, type = "HC1")))
coeftest(m4a, vcov = vcovHC(m4a, type = "HC1"))

m4b <- lm(occupied ~ urban + woman + indigenous + treatment + age + treatment*age, data_dinamic, weights = weight)
se4b <- sqrt(diag(vcovHC(m4b, type = "HC1")))
coeftest(m4b, vcov = vcovHC(m4b, type = "HC1"))

m4c <- lm(real_income ~ urban + woman + indigenous + treatment + age + treatment*age, workers_dinamic, weights = weight)
se4c <- sqrt(diag(vcovHC(m4c, type = "HC1")))
coeftest(m4c, vcov = vcovHC(m4c, type = "HC1"))

m4d <- lm(hours ~ urban + woman + indigenous + treatment + age + treatment*age, workers_dinamic, weights = weight)
se4d <- sqrt(diag(vcovHC(m4d, type = "HC1")))
coeftest(m4d, vcov = vcovHC(m4d, type = "HC1"))

stargazer(m1, m2, m3, m4,
          type = "html",
          se = list(se1, se2, se3, se4),
          intercept.bottom =  FALSE, 
          out = "table1.html")

stargazer(m1a, m2a, m3a, m4a,
          type = "html",
          se = list(se1a, se2a, se3a, se4a),
          intercept.bottom =  FALSE, 
          out = "table2.html")

stargazer(m1b, m2b, m3b, m4b,
          type = "html",
          se = list(se1b, se2b, se3b, se4b),
          intercept.bottom =  FALSE, 
          out = "table3.html")

stargazer(m1c, m2c, m3c, m4c,
          type = "html",
          se = list(se1c, se2c, se3c),
          intercept.bottom =  FALSE, 
          out = "table4.html")

stargazer(m1d, m2d, m3d, m4d,
          type = "html",
          se = list(se1d, se2d, se3d, se4d),
          intercept.bottom =  FALSE, 
          out = "table5.html")

webshot("table1.html", "table1.png", zoom = 2, vwidth = 900)
webshot("table2.html", "table2.png", zoom = 2, vwidth = 900)
webshot("table3.html", "table3.png", zoom = 2, vwidth = 900)
webshot("table4.html", "table4.png", zoom = 2, vwidth = 900)
webshot("table5.html", "table5.png", zoom = 2, vwidth = 900)

### =========================================================================
### =========================================================================
### =========================================================================
### 

# MDE
calc_mde <- function(se, alpha = 0.05, power_target = 0.8){
  z_alpha <- qnorm(1 - alpha/2)
  z_power <- qnorm(power_target)
  mde <- (z_alpha + z_power) * se
  return(mde)
}

### --- Put all models in a list ---

models <- list(
  m1 = m1, m1a = m1a, m1b = m1b, m1c = m1c, m1d = m1d,
  m2 = m2, m2a = m2a, m2b = m2b, m2c = m2c, m2d = m2d,
  m3 = m3, m3a = m3a, m3b = m3b, m3c = m3c, m3d = m3d)

### --- Loop through and extract results --- 
results <- data.frame()

for(name in names(models)){
  mod <- models[[name]]
  vc <- vcovHC(mod, type = "HC1")
  se <- sqrt(diag(vc))
  
  # Find coefficient of interest depending on model
  if("treatment:post" %in% names(coef(mod))){
    coef_name <- "treatment:post"
  } else {
    next
  }
  
  beta_hat <- coef(mod)[coef_name]
  se_hat   <- se[coef_name]
  
  # Calculate 95% confidence interval
  ci_lower <- beta_hat - qnorm(0.975) * se_hat
  ci_upper <- beta_hat + qnorm(0.975) * se_hat
  
  # Calculate MDE (signed)
  mde <- sign(beta_hat) * calc_mde(se_hat)
  
  results <- rbind(results, data.frame(
    model = name,
    coef = beta_hat,
    se = se_hat,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper,
    MDE = mde
  ))
}

# Add a "group" column based on suffix
results$variable <- ifelse(grepl("a$", results$model), "Hours",
                           ifelse(grepl("b$", results$model), "Occupied",
                                  ifelse(grepl("c$", results$model), "Real Income (workers)",
                                         ifelse(grepl("d$", results$model), "Hours (workers)", "Real Income"))))

# Add a "type" column based on model number (m1, m2, m3)
results$type <- ifelse(grepl("^m1", results$model), "2x2 DiD",
                       ifelse(grepl("^m2", results$model), "2x2 DiD with Controls",
                              ifelse(grepl("^m3", results$model), "DiD with Interactions", NA)))

# Reorder columns for stargazer
results_for_table <- results[, c("variable", "type", "coef", "se", "CI_Lower", "CI_Upper", "MDE")]

# Sort so you have: type → group → model
results_for_table <- results_for_table[order(results_for_table$variable, results_for_table$type), ]
colnames(results_for_table) <- c("Outcome", "Model", "Coefficient", "SE", "CI Lower", "CI Upper", "MDE")

# Use stargazer to make a text or HTML table
stargazer(results_for_table, summary = FALSE, rownames = FALSE, type = "html", out = "power.html")
webshot("power.html", "power.png", zoom = 2, vwidth = 900)

stargazer(results_for_table, summary = FALSE, rownames = FALSE, type = "text")

