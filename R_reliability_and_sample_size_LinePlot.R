# The power, effect size, reliability, sample size analysis
# Reliability is defined as the ratio of true score variance to observed score variance
# rho(X) = sigma(T)-square / sigma(X)-square = 1 - sigma(E)-square/sigma(X)-square
# sigma(T)-square is the variance of the true score
# sigma(X)-square is the variance of the observed score
# sigma(E)-square is the variance of the error score
# sigma(T) = sigma(X)*sqrt(rho(X))
# sigma(X) = sigma(T)/sqrt(rho(X))

# true effect size:     d(T) = (mu_1 - mu_0)/sigma(T) = 
# observed effect size: d(X) = (mu_1 - mu_0)/sigma(X) = d(T)*sqrt(rho(X)) 


#####################################################################
# Install the Power Analysis package in R
# > install.packages('pwr')
# > library(pwr)
#####################################################################
# Start the Power analysis
#####################################################################
library(pwr)
library(plotly)
library(reshape)
library(RColorBrewer)

#####################################################################

setwd('~/Reliability/Power')

#####################################################################
#### paired-t test
#####################################################################
rho <- seq(0.1, 1, by=0.01)
effect_sizes <- c(0.2, 0.5, 0.8)
effect_sizes_labels <- c('small=0.2', 'medium=0.5', 'large=0.8')
powers <- c(0.7, 0.8, 0.9)

sample_size <- array(0, dim=c(3, 3, length(rho)))
# calculate the sample size
for (a in 1:3){
  # power
  power_for_test <- as.numeric(powers[a])
  for (b in 1:3){
    # effect size
    effect_size <- as.numeric(effect_sizes[b])
    for (i in 1:length(rho)){
      d_observed <- effect_size*sqrt(rho[i])
      output <- pwr.t.test(d=d_observed, power=power_for_test, sig.level=0.05, type="paired", alternative="two.sided")
      sample_size[a, b, i] = ceiling(output$n)
    }
  }
}


for (a in 1:3){
  # power
  power_for_test <- sprintf("%s", powers[a])
  for (b in 1:3){
    # effect size
    effect_size <- sprintf("%s", effect_sizes_labels[b])
    df1 <- data.frame(reliability=rho, sample_size=sample_size[a,b,])
    df1$power=power_for_test
    df1$effect_size=effect_size
    df1$group = sprintf("power=%s, %s", power_for_test, effect_size)
    if (a==1&&b==1){
      df_all <- df1
    }
    else{
      df_all <- rbind(df_all, df1)
    }
  }
}

df_all$log_sample_size = log10(df_all$sample_size)
head(df_all)
df_all <- df_all[df_all$sample_size<=2200,]

g <- ggplot(df_all, aes(x=reliability, y=sample_size, col=effect_size, linetype = power)) +
  geom_line(size=1.5) +
  xlim(0, 1) + ylim(10, 2200) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  scale_y_continuous(breaks=seq(0,2200,100))
g + theme(axis.text=element_text(size=30),
          axis.title=element_text(size=30)) 
ggsave("Rplot_line_paired_t_test.png", width = 30, height = 40, units = "cm")




#####################################################################
#### two-sample t test
#####################################################################

rho <- seq(0.1, 1, by=0.01)
effect_sizes <- c(0.2, 0.5, 0.8)
effect_sizes_labels <- c('small=0.2', 'medium=0.5', 'large=0.8')
powers <- c(0.7, 0.8, 0.9)

sample_size <- array(0, dim=c(3, 3, length(rho)))
# calculate the sample size
for (a in 1:3){
  # power
  power_for_test <- as.numeric(powers[a])
  for (b in 1:3){
    # effect size
    effect_size <- as.numeric(effect_sizes[b])
    for (i in 1:length(rho)){
      d_observed <- effect_size*sqrt(rho[i])
      output <- pwr.t.test(d=d_observed, power=power_for_test, sig.level=0.05, type="two.sample", alternative="two.sided")
      sample_size[a, b, i] = ceiling(output$n)
    }
  }
}

for (a in 1:3){
  # power
  power_for_test <- sprintf("%s", powers[a])
  for (b in 1:3){
    # effect size
    effect_size <- sprintf("%s", effect_sizes_labels[b])
    df1 <- data.frame(reliability=rho, sample_size=sample_size[a,b,])
    df1$power=power_for_test
    df1$effect_size=effect_size
    df1$group = sprintf("power=%s, %s", power_for_test, effect_size)
    if (a==1&&b==1){
      df_all <- df1
    }
    else{
      df_all <- rbind(df_all, df1)
    }
  }
}

df_all$log_sample_size = log10(df_all$sample_size)
head(df_all)
df_all <- df_all[df_all$sample_size<=2200,]
g <- ggplot(df_all, aes(x=reliability, y=sample_size, col=effect_size, linetype = power)) +
  geom_line(size=2)+
  xlim(0.1, 1) + ylim(10, 2200) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  scale_y_continuous(breaks=seq(0,2200,100))
g + theme(axis.text=element_text(size=30),
          axis.title=element_text(size=30))
ggsave("Rplot_line_2sample_t_test.png", width = 30, height = 40, units = "cm")


#####################################################################
#### anova
# pwr.anova.test(k=3, f=0.1, power=power_for_test, sig.level=0.05)
#####################################################################
rho <- seq(0.1, 1, by=0.01)
effect_sizes <- c(0.1, 0.25, 0.4)
effect_sizes_labels <- c('small=0.1', 'medium=0.25', 'large=0.4')
powers <- c(0.7, 0.8, 0.9)

sample_size <- array(0, dim=c(3, 3, length(rho)))
# calculate the sample size
for (a in 1:3){
  # power
  power_for_test <- as.numeric(powers[a])
  for (b in 1:3){
    # effect size
    effect_size <- as.numeric(effect_sizes[b])
    for (i in 1:length(rho)){
      d_observed <- effect_size*sqrt(rho[i])
      output <- pwr.anova.test(k=3, f=d_observed, power=power_for_test, sig.level=0.05)
      sample_size[a, b, i] = ceiling(output$n)
    }
  }
}


for (a in 1:3){
  # power
  power_for_test <- sprintf("%s", powers[a])
  for (b in 1:3){
    # effect size
    effect_size <- sprintf("%s", effect_sizes_labels[b])
    df1 <- data.frame(reliability=rho, sample_size=sample_size[a,b,])
    df1$power=power_for_test
    df1$effect_size=effect_size
    df1$group = sprintf("power=%s, %s", power_for_test, effect_size)
    if (a==1&&b==1){
      df_all <- df1
    }
    else{
      df_all <- rbind(df_all, df1)
    }
  }
}

df_all$log_sample_size = log10(df_all$sample_size)
head(df_all)
df_all <- df_all[df_all$sample_size<=2200,]
g <- ggplot(df_all, aes(x=reliability, y=sample_size, col=effect_size, linetype = power)) +
  geom_line(size=1.5)+
  xlim(0.1, 1) + ylim(10, 2200) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  scale_y_continuous(breaks=seq(0,2200,100))
g + theme(axis.text=element_text(size=30),
          axis.title=element_text(size=30)) 
ggsave("Rplot_line_anova.png", width = 30, height = 40, units = "cm")