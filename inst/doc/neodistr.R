## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, 
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)


## ----eval=FALSE, echo=TRUE----------------------------------------------------
# # From CRAN
# install.packages("neodistr")
# 
# # From GitHub
# #devtools::install_github("madsyair/neodistr")

## ----message=FALSE, echo=TRUE,eval=TRUE---------------------------------------
 library(rstan)
  library(brms)
  library(bayesplot)
  library(loo)
  library(neodistr)
  # Set Stan options for faster computation
  options(mc.cores = min(2, parallel::detectCores()))
  rstan_options(auto_write = TRUE)

## ----load_packages, message=FALSE,echo=FALSE----------------------------------
library(neodistr)

# Load other packages conditionally

  library(rstan)
  library(brms)
  library(bayesplot)
  library(loo)
  
  # Set Stan options for faster computation
  options(mc.cores = min(2, parallel::detectCores()))
  rstan_options(auto_write = TRUE)


## ----eval=FALSE,echo=TRUE-----------------------------------------------------
# #install.packages("R.utils")
# #library(R.utils)
# set.seed(400)
# precomp_dir <- system.file("data_precomputed", package = "neodistr")
# # Simulate data from FOSSEP distribution
# y <- rfossep(50, 0, 1, 2, 2)  # Simulated data from fossep distribution
# df <- data.frame(y)
# #saveRDS(y, file.path(precomp_dir, "fossep_data.rds")) # Save pre-computed data
# #gzip(file.path(precomp_dir, "fossep_data.rds"))
# # Basic exploration
# summary(y)
# hist(y, main = "Simulated FOSSEP Data", xlab = "Y", col = "skyblue", breaks = 20)

## ----eval=TRUE,echo=FALSE-----------------------------------------------------
# this data simulation generated from the fossep distribution
#y <- rfossep(100, 0, 1, 2, 2)  # Simulated data from fossep distribution
#df <- data.frame(y)
precomp_dir <- system.file("data_precomputed", package = "neodistr")
y <- readRDS(file.path(precomp_dir, "fossep_data.rds")) # Load pre-computed data
df <- data.frame(y)
# Basic exploration
summary(y)
hist(y, main = "Simulated FOSSEP Data", xlab = "Y", col = "skyblue", breaks = 20)

## -----------------------------------------------------------------------------
# Define formula for parameter estimation
formula <- brms::bf(y ~ 1)

# Choose priors
prior <- c(
  set_prior("lognormal(0,0.5)", class = "alpha"),
  set_prior("lognormal(log(2),0.5)", class = "beta"),
  set_prior("normal(0,1)", class = "sigma"),
  set_prior("normal(0,1)", class = "Intercept") # mu
)

## ----prior_check_fossep, eval=FALSE-------------------------------------------
# # Run prior predictive check (computationally intensive)
# ppc_fit_fossep <- bnrm(
#   formula = formula,
#   data = df,
#   family = fossep(),
#   prior = prior,
#   sample_prior = "only",
#   chains = 2,           # Reduced for demo
#   cores = 1,            # Single core for consistency
#   iter = 2000,          # Reduced iterations
#   warmup = 1000,
#   refresh = 0           # Suppress output
# )

## ----prior_check_fossep_pre,eval=FALSE----------------------------------------
# # Show prior predictive check plot
# ppc_plot_fossep <- pp_check(ppc_fit_fossep, type = "dens_overlay")
# #saveRDS(ppc_plot_fossep, file.path(precomp_dir, "ppc_plot_fossep.rds")) # Save pre-computed plot file.path(precomp_dir, "fossep_data.rds")
# print(ppc_plot_fossep)
# 

## ----prior_check_pre_computed, eval=TRUE,echo=FALSE---------------------------
# Load pre-computed prior predictive check results
ppc_plot_fossep <- readRDS(file.path(precomp_dir, "ppc_plot_fossep.rds")) # Load pre-computed plot
print(ppc_plot_fossep)
# Show prior predictive check plot

## ----fit_fossep_bnrm, eval=FALSE----------------------------------------------
# # Fit the model (computationally intensive)
# fit_fossep_bnrm <- bnrm(
#   formula = formula,
#   data = df,
#   family = fossep(),
#   prior = prior,
#   chains = 2,
#   cores = 1,
#   iter = 2000,
#   warmup = 1000,
#   seed = 123,
#   refresh = 0
# )
# #saveRDS(fit_fossep_bnrm, file.path(precomp_dir, "fit_fossep_bnrm.rds")) # Save pre-computed fit

## ----show_fossep_results, eval=FALSE------------------------------------------
#   summary(fit_fossep_bnrm)
# 
#   # Convergence diagnostics
#   trace_plot <- mcmc_trace(fit_fossep_bnrm)
#   print(trace_plot)
# 
#   # Posterior predictive check
#   pp_plot <- pp_check(fit_fossep_bnrm)
#   #saveRDS(pp_plot, file.path(precomp_dir, "pp_plot_fossep.rds")) # Save pre-computed plot
#   print(pp_plot)
# 

## ----show_fossep_results_pre, eval=TRUE,echo=FALSE----------------------------
# precomputed results
 fit_fossep_bnrm <- readRDS(file.path(precomp_dir, "fit_fossep_bnrm.rds")) # Load pre-computed fit
  summary(fit_fossep_bnrm)
  # Convergence diagnostics
  trace_plot <- mcmc_trace(fit_fossep_bnrm, facet_args = list(ncol = 2),pars = c("alpha", "beta", "sigma", "Intercept"))
  print(trace_plot)
# Read pre-computed plot
  # Posterior predictive check
  
  pp_plot<-readRDS(file.path(precomp_dir,"pp_plot_fossep.rds")) # Load pre-computed plot
  print(pp_plot)

## ----eval=FALSE---------------------------------------------------------------
# set.seed(123)
# x <- runif(50)
# e <- rjsep(50, 0, 0.1, 0.8,2)  # Simulated errors from JSEP distribution
# y <- 0.5 + 0.8*x + e
# xy<-cbind(y,x)
# 
# df_reg <- data.frame(y, x)
# # Basic exploration
# plot(x, y, main = "Regression Data", pch = 19, col = "steelblue")
# #save pre-computed data
# #saveRDS(xy, file.path(precomp_dir,"jsep_data.rds")) # Save pre-computed data

## ----eval=TRUE, echo=FALSE----------------------------------------------------
xy <- readRDS(file.path(precomp_dir,"jsep_data.rds")) # Load pre-computed data
df_reg<-data.frame(xy)
x<- df_reg$x
y<- df_reg$y
# Basic exploration
plot(x, y, main = "Regression Data", pch = 19, col = "steelblue")


## ----eval=TRUE----------------------------------------------------------------
# Define regression formula
formula_reg <- brms::bf(y ~ x)

# Priors for regression
prior_reg <- c(
  set_prior("lognormal(log(2),0.25)", class = "alpha"),
  set_prior("lognormal(log(2),0.25)", class = "beta"),
  set_prior("normal(0,1)", class = "sigma"),
  set_prior("normal(0,1)", class = "Intercept"),
  set_prior("normal(0,1)", class = "b")
)

## ----pc_check_jsep_reg, eval=FALSE--------------------------------------------
# # Fit regression model
# fit_ppc_jsep <- bnrm(
#   formula = formula_reg,
#   data = df_reg,
#   family = jsep(),
#   prior = prior_reg,
#   chains = 2,
#   cores = 1,
#   iter = 2000,
#   sample_prior = "only", # Prior predictive check
#   warmup = 1000,
#   seed = 123,
#   refresh = 0
# )
# # prior predictive check fir jesp
# 
# 
# ppc_plot_jsep<- pp_check(fit_ppc_jsep, type = "dens_overlay", nsamples = 200)
# #saveRDS(ppc_plot_jsep, file.path(precomp_dir,"ppc_plot_jsep.rds")) # Save pre-computed plot
# print(ppc_plot_jsep)
# # Show prior predictive check plot

## ----ppc_plot_jsep, eval=TRUE,echo=FALSE,message=FALSE------------------------


ppc_plot_jsep <- readRDS(file.path(precomp_dir,"ppc_plot_jsep.rds")) # Load pre-computed plot
print(ppc_plot_jsep)
# Show prior predictive check plot

## ----fit_jsep_bnrm, eval=FALSE------------------------------------------------
# fit_jsep_bnrm <- bnrm(
#   formula = formula_reg,
#   data = df_reg,
#   family = jsep(),
#   prior = prior_reg,
#   chains = 2,
#   cores = 1,
#   iter = 2000,
#    warmup = 1000,
#   seed = 123,
#   refresh = 0
# )
# #saveRDS(fit_jsep_bnrm, file.path(precomp_dir, "fit_jsep_bnrm.rds")) # Save pre-computed fit

## ----show_jsep_results, eval=FALSE--------------------------------------------
# 
# summary(fit_jsep_bnrm)
# # Convergence diagnostics
#   trace_plot <- mcmc_trace(fit_jsep_bnrm, facet_args = list(ncol = 2),pars = c("alpha", "beta", "sigma", "Intercept","b_x"))
#   print(trace_plot)
# 
#   # Posterior predictive check
#   pp_plot <- pp_check(fit_jsep_bnrm)
# 
#  #saveRDS(pp_plot, file.path(precomp_dir,"pp_plot_jsep.rds")) # Save pre-computed plot
# 
#   # Model evaluation
#   loo_result <- loo(fit_jsep_bnrm, moment_match = TRUE)
#   #saveRDS(loo_result, file.path(precomp_dir,"loo_result_jsep.rds")) # Save pre-computed loo result
#   print(loo_result)
# 

## ----show_jsep_results_pre, eval=TRUE, echo=FALSE-----------------------------
fit_jsep_bnrm <- readRDS(file.path(precomp_dir,"fit_jsep_bnrm.rds")) # Load pre-computed fit  
 
summary(fit_jsep_bnrm)
# Convergence diagnostics
  trace_plot <- mcmc_trace(fit_jsep_bnrm, facet_args = list(ncol = 2),pars = c("alpha", "beta", "sigma", "Intercept","b_x"))
  print(trace_plot)
  # Posterior predictive check
  pp_plot <- readRDS(file.path(precomp_dir,"pp_plot_jsep.rds")) # Load pre-computed 
  print(pp_plot)
  
  # Model evaluation
  loo_result <- readRDS(file.path(precomp_dir,"loo_result_jsep.rds")) # Load pre-computed loo result
  print(loo_result)
 

## ----brms_demo, eval=FALSE----------------------------------------------------
# # Define custom family for brms
# fossep_custom <- brms_custom_family("fossep")
# 
# # Fit model with brms
# fit_brms_demo <- brm(
#   y ~ 1,
#   data = df,
#   family = fossep_custom$custom_family,
#   stanvars = fossep_custom$stanvars_family,
#   chains = 2,
#   cores = 2,
#   iter = 4000,
#   warmup = 1000,
#   seed = 123,
#   refresh = 0
# )
# summary(fit_brms_demo)
# 

## ----stan_demo, eval=FALSE----------------------------------------------------
# # Prepare data for Stan
# stan_data <- list(
#   n = length(y),
#   y = y
# )
# 
# # Simple Stan model (abbreviated for vignette)
# stan_code <- paste0(
#   "functions { ", stanf_fossep(vectorize = TRUE), " }",
#   "data { int<lower=1> n; vector[n] y; }",
#   "parameters { real mu; real<lower=0> sigma; real<lower=0> alpha; real<lower=0> beta; }",
#   "model { y ~ fossep(rep_vector(mu, n), sigma, alpha, beta); }"
# )
# 
# # Fit Stan model
# fit_stan_demo <- stan(
#   model_code = stan_code,
#   data = stan_data,
#   chains = 2,
#   cores = 1,
#   iter = 1000,
#   warmup = 500,
#   refresh = 0
# )
# summary(fit_stan_demo, pars = c("mu", "sigma", "alpha", "beta"))

