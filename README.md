# HumanCapitalLossGuatemalaCovid
This repo contains the replicability package for the working paper: The Effects of Human Capital Loss in Guatemala during Covid-19 Pandemic on Youth's Labor outcomes

The paper answers to the research question: does the loss of human capital associated to the COVID-19 pandemic caused a negative effect on youth labor market outcomes. The empirical strategy focus on the inter-cohort differential exposure to the pandemic: teens born in 2001 graduated from high school in 2019 (approximately) whereas the teens born in 2002 had to study their last year of high school during the outbreak. 

# Cleaning
Raw databases from the National Institute of Statistics Income and Employment households surveys. There is a folder for each year used in the analysis and a script that selects the relevant variables and separates observations in both treatment and control groups.

# Merging 
In this folder I merge the databases from each year in one final analysis-ready dataset, combining all treatment and control group observations.

# Analysis 
Run the DiD analysis and event study specifications for the relevant outcomes. Export regression tables using the package stargazer in html format and then took a picture to obtain a PNG file for uploading in the paper using the webshot R package. 

