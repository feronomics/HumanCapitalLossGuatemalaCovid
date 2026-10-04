library(dplyr)
library(ggplot2)
library(gridExtra)
#=======================================================================
# BALANCE TESTS

### Cargar los datos
data <- read.table("data_clean.csv", sep=",", header=T)
cpi <- data.frame(year = c(2018, 2019, 2021, 2022, 2023, 2024), 
                  cpi = c(75.68, 78.79, 84.39, 86.81, 95.23, 98.86), 
                  gdppc = c(3.1, 6.1, 9.8, 9.2, 8.8, 6.0))
data <- left_join(data, cpi, by = "year")
data <- data %>% mutate(real_income = income * 75.68 /cpi)
data1 <- data %>% filter(year < 2023)
data1 <- data1 %>% filter(!(year == 2022 & treatment == 0)) 

# Pre-period
df_pre <- subset(data1, post == 0)

df <- subset(df_pre, select=c("indigenous", "weight", "treatment"))

vars <- c("urban", "woman", "indigenous", "occupied", "informal", "active", "real_income", "hours")
var_labels <- c("Urban", "Woman", "Indigenous", "Occupied", "Informal", "Active", "Income", "Hours")

# Weighted mean
w_mean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

# Standardized Mean Difference
smd <- function(x, treat, w = NULL) {
  if(is.null(w)) {
    m0 <- mean(x[treat==0], na.rm=TRUE)
    m1 <- mean(x[treat==1], na.rm=TRUE)
    s0 <- sd(x[treat==0], na.rm=TRUE)
    s1 <- sd(x[treat==1], na.rm=TRUE)
  } else {
    m0 <- w_mean(x[treat==0], w[treat==0])
    m1 <- w_mean(x[treat==1], w[treat==1])
    s0 <- sqrt(sum(w[treat==0]*(x[treat==0]-m0)^2)/sum(w[treat==0]))
    s1 <- sqrt(sum(w[treat==1]*(x[treat==1]-m1)^2)/sum(w[treat==1]))
  }
  sp <- sqrt((s0^2 + s1^2)/2)
  (m1 - m0)/sp
}

# Function to create table for stargazer
make_table <- function(weighted=FALSE) {
  tbl <- data.frame(
    Variable = var_labels,
    Control = NA,
    Treatment = NA,
    SMD = NA
  )
  
  for(i in seq_along(vars)) {
    var <- vars[i]
    if(weighted) {
      tbl$Control[i] <- w_mean(df_pre[[var]][df_pre$treatment==0], df_pre$weight[df_pre$treatment==0])
      tbl$Treatment[i] <- w_mean(df_pre[[var]][df_pre$treatment==1], df_pre$weight[df_pre$treatment==1])
      tbl$SMD[i] <- smd(df_pre[[var]], df_pre$treatment, w=df_pre$weight)
    } else {
      tbl$Control[i] <- mean(df_pre[[var]][df_pre$treatment==0], na.rm=TRUE)
      tbl$Treatment[i] <- mean(df_pre[[var]][df_pre$treatment==1], na.rm=TRUE)
      tbl$SMD[i] <- smd(df_pre[[var]], df_pre$treatment)
    }
  }
  
  tbl <- tbl %>% mutate(across(-Variable, ~round(., 3)))
  return(tbl)
}

# Create weighted and unweighted tables
tab_w <- make_table(weighted=TRUE)
tab_uw <- make_table(weighted=FALSE)

# Add blank row before unweighted for spacing
tab_blank <- data.frame(Variable="", Control=NA, Treatment=NA, SMD=NA)

# Combine tables with a blank row in between
combined <- rbind(
  data.frame(Variable="Unweighted", Control=NA, Treatment=NA, SMD=NA),
  tab_uw,
  tab_blank,
  data.frame(Variable="Weighted", Control=NA, Treatment=NA, SMD=NA),
  tab_w
)

# Stargazer output
stargazer(combined, type="html", summary=FALSE, rownames=FALSE, out = "balance_table.html")
webshot("balance_table.html", "balance_table.png", zoom = 2, vwidth = 300)

# ==================================================================
#Graphs

# Function for weighted mean & se
weighted_mean_se <- function(x, w) {
  # Weighted mean
  m <- sum(w * x) / sum(w)
  # Effective sample size (Kish approximation)
  n_eff <- (sum(w))^2 / sum(w^2)
  # Weighted standard error (binomial approx)
  se <- sqrt(m * (1 - m) / n_eff)
  return(c(mean=m, se=se))
}

# Categorical plots with WEIGHTS
cat_vars <- c("urban", "woman", "indigenous", "occupied", "informal", "active")
cat_plots <- lapply(cat_vars, function(v) {
  df_sum <- df_pre %>%
    group_by(treatment) %>%
    summarise(
      tmp = list(weighted_mean_se(.data[[v]], weight)),
      .groups="drop"
    ) %>%
    tidyr::unnest_wider(tmp) %>%
    rename(prop=mean)
  
  ggplot(df_sum, aes(x=factor(treatment), y=prop, fill=factor(treatment))) +
    geom_bar(stat="identity", position="dodge") +
    geom_errorbar(aes(ymin = prop - 1.96*se, ymax = prop + 1.96*se), width=0.2) +
    scale_fill_manual(values=c("gray70","steelblue")) +
    scale_x_discrete(labels = c("0" = "Control", "1" = "Treatment")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) +
    labs(title=v, x=NULL, y="Proportion") +
    theme_minimal() +  theme(legend.position = "none") 
})

# Continuous plots (weighted densities)
cont_vars <- c("real_income", "hours")
cont_plots <- lapply(cont_vars, function(v) {
  ggplot(df_pre, aes_string(x=v, color="factor(treatment)", fill="factor(treatment)", weight="weight")) +
    geom_density(alpha=0.3) +
    scale_color_manual(values=c("black","steelblue"), name="Treatment") +
    scale_fill_manual(values=c("gray70","steelblue"), guide=FALSE) +
    labs(title=v, x=NULL, y="Density") +
    theme_minimal()
})

# Arrange plots: first 6 categorical, then 2 continuous
g1 <- grid.arrange(grobs=c(cat_plots), ncol=2)
g2 <-grid.arrange(grobs=c(cont_plots), ncol=2)

ggsave("cat_plots.png", plot = g1, width = 7, height = 10)
ggsave("cont_plots.png", plot = g2, width = 7, height = 3)
