# Power analysis - reliability and sample size

Reliability is defined as the ratio of true score variance to observed score variance
rho(X) = sigma(T)^2 / sigma(X)^2 = 1 - sigma(E)^2/sigma(X)^2
sigma(T)^2 is the variance of the true score
sigma(X)^2 is the variance of the observed score
sigma(E)^2 is the variance of the error score

sigma(T) = sigma(X)*sqrt(rho(X))
sigma(X) = sigma(T)/sqrt(rho(X))

true effect size:     d(T) = (mu_1 - mu_0)/sigma(T) 
observed effect size: d(X) = (mu_1 - mu_0)/sigma(X) = (mu_1 - mu_0)/(sigma(T)/sqrt(rho(X))) = d(T)*sqrt(rho(X)) 

Ref: Kanyongo et al. (2017) Reliability and Statistical Power: How Measurement Fallibility Affects Power and Required Sample Sizes for Several Parametric and Nonparametric Statistics


## Interactions between reliability, sample size and effect size

## Two sample t-test
![figure](https://github.com/TingsterX/power__reliability_sample_size/blob/master/figure.jpeg "power analysis")


