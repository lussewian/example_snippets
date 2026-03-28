#' Outcomes model
#' Author: Wian Lusse
#' Date: 2026/18/03
#' 
#' Runs models with different predictors and summarises in a single docuement

#' Import packages
library(dplyr)
library(jtools)


var <- ""


formula_list <- list(
  m1 = glue::glue("{var} ~ a + b"),
  m2 = glue::glue("{var} ~ b + c"),
  m3 = glue::glue("{var} ~ a + b + c"),
  m4 = glue::glue("{var} ~ a + b + c + d")
)

jj <- 1

for(ii in 1:length(formula_list)){
 
  ff <- as.formula(formula_list[[ii]])
  
  logit_mod <- glm(ff, data = tbl,family = "binomial")
  assign(glue::glue("Model{ii}"), logit_mod, envir = .GlobalEnv)
}

summary <- export_summs(Model1, Model2, Model3, Model4, exp = TRUE)

