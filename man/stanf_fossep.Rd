% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stanf_fossep.R
\name{stanf_fossep}
\alias{stanf_fossep}
\title{Stan function of Fernandez-Osiewalski-Steel Skew Exponential Power Distribution}
\usage{
stanf_fossep(vectorize = TRUE)
}
\arguments{
\item{vectorize}{logical; if TRUE, Vectorize Stan code of Fernandez-Osiewalski-Steel Skew Exponential Power distribution are given 
The default value of this parameter is TRUE}
}
\value{
\code{fossep_lpdf} gives stan's code of the log of density, \code{fossep_cdf} gives stan's code of the distribution
function, \code{fossep_lcdf} gives stan's code of the log of distribution function and \code{fossep_lccdf} 
gives the stans's code of complement of log distribution function (1-fossep_lcdf)
}
\description{
Stan code of fossep distribution for custom distribution in stan
}
\details{
Fernandez-Osiewalski-Steel Skew Exponential Power Distribution has density:
\deqn{f(y |\mu,\sigma,\alpha,\beta) = \frac{c}{\sigma} \exp \left( - \frac{1}{2} \left| v z \right|^\beta \right) \quad \text{if } y < \mu}
\deqn{f(y |\mu,\sigma,\alpha,\beta) = \frac{c}{\sigma} \exp \left( - \frac{1}{2} \left| \frac{v}{z} \right|^\beta \right) \quad \text{if } y \ge \mu}
\deqn{ \text{where } -\infty < y < \infty, \ -\infty < \mu < \infty, \ \sigma > 0, \ \alpha > 0, \ \beta > 0}
\deqn{ z = \frac{y - \mu}{\sigma}}
\deqn{ c = v \beta \left[ (1 + v^2) 2^{\frac{1}{\beta}} \Gamma \left( \frac{1}{\beta} \right) \right]^{-1}}
 
This function gives stan code  of log density, cumulative distribution, log of cumulative distribution, 
log complementary cumulative distribution of Fernandez-Osiewalski-Steel Skew Exponential Power Distribution
}
\examples{
\dontrun{
library (neodistr)
library (rstan)

# inputting data
set.seed(400)
dt <- neodistr::rfossep(100,mu=0, sigma=1, alpha = 2, beta = 2) # random generating fossep data
dataf <- list(
 n = 100,
 y = dt
 )
 
 
#### Vector
## Calling the function of the neonormal distribution that is available in the package.
func_code_vector<-paste(c("functions{",neodistr::stanf_fossep(vectorize=TRUE),"}"),collapse="\n")

# Define Stan Model Code
model_vector <-"
    data{
      int<lower=1> n;
      vector[n] y;
    }
    parameters{
      real mu;
      real <lower=0> sigma;
      real <lower=0> alpha;
      real <lower=0>beta;
    }
    model {
      y ~ fossep(rep_vector(mu,n),sigma, alpha, beta);
      mu ~ cauchy (0,1);
      sigma ~ cauchy (0, 1);
      alpha ~ lognormal(0,2.5);
      beta ~ lognormal(0,2.5);
      
    }
 "
 
 # Merge stan model code and selected neo-normal stan function
fit_code_vector <- paste (c(func_code_vector,model_vector,"\n"), collapse = "\n")

# Create the model using Stan Function
fit2 <- stan(
    model_code = fit_code_vector,  # Stan Program
    data = dataf,                  # named list data
    chains = 2,                    # number of markov chains
    warmup = 5000,                 # total number of warmup iterarions per chain
    iter = 10000,                  # total number of iterations iterarions per chain
    cores = 2,                     # number of cores (could use one per chain)
    control = list(                # control sampel behavior
      adapt_delta = 0.99
    ),
    refresh = 1000                 # progress has shown if refresh >=1, else no progress shown
)

# Showing the estimation result of the parameters that were executed using the Stan file
print(fit2, pars = c("mu", "sigma", "alpha", "beta", "lp__"), probs=c(.025,.5,.975))
 }
}
\references{
Fernandez, C., Osiewalski, J., & Steel, M. F. (1995) Modeling and inference with v-spherical distributions. 
Journal of the American Statistical Association, 90(432), pp 1331-1340

Rigby, R.A. and Stasinopoulos, M.D. and Heller, G.Z. and De Bastiani, F. (2019) Distributions for Modeling Location, 
Scale, and Shape: Using GAMLSS in R.CRC Press
}
\author{
Almira Utami and Achmad Syahrul Choir
}
