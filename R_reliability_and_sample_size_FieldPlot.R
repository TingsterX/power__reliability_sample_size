# The power, effect size, reliability, sample size analysis
# Reliability is defined as the ratio of true score variance to observed score variance
# rho(X) = sigma(T)-square / sigma(X)-square = 1 - sigma(E)-square/sigma(X)-square
# sigma(T)-square is the variance of the true score
# sigma(X)-square is the variance of the observed score
# sigma(E)-square is the variance of the error score
# sigma(T) = sigma(X)*sqrt(rho(X))
# sigma(X) = sigma(T)/sqrt(rho(X))

# true effect size:     d(T) = (mu_1 - mu_0)/sigma(T) 
# observed effect size: d(X) = (mu_1 - mu_0)/sigma(X) = (mu_1 - mu_0)/(sigma(T)/sqrt(rho(X))) = d(T)*sqrt(rho(X)) 

# An example:
# In the following Power analysis, for example, independent-t test, 
# To achieve 0.7 power when the sample effect size is 0.2, the required sample size is calculated as:
# > pwr.t.test(d=0.2,power=0.7,sig.level=0.05,type="two.sample",alternative="two.sided")
# then, the required sample size n is 310.
# But in the above case, we assume that the reliability is 1
# meaning that the effect size d=0.2 represent the true effect size we can get from the sample
# If the reliability is 0.9 instead of 1, meaning the real sample effect size d=0.2*sqrt(0.9)
# > pwr.t.test(d=0.2*sqrt(0.9),power=0.7,sig.level=0.05,type="two.sample",alternative="two.sided")
# then, the required sample size n is 344, is more than 310.

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
# Paired-T Test
# The basic power analysis
# The effect size (Cohen'd) is mean/sd, or t/sqrt(N)
# > pwr.t.test(d=0.2,power=0.8,sig.level=0.05,type="paired",alternative="two.sided")
# > pwr.t.test(d=0.5,power=0.8,sig.level=0.05,type="paired",alternative="two.sided")
# > pwr.t.test(d=0.8,power=0.8,sig.level=0.05,type="paired",alternative="two.sided")
#####################################################################

rho <- seq(0.1, 1, by=0.01)
effect_size <- seq(0.15, 0.8, by=0.01)
power_for_test <- 0.8

n_pairedt <- matrix(0, length(rho), length(effect_size))
dimnames(n_pairedt) = list(rho, effect_size)
# calculate the sample size
for (i in 1:length(rho)){
  for (j in 1:length(effect_size)){
    d_observed <- effect_size[j]*sqrt(rho[i])
    #cat(sprintf("reliability=%0.2f, effect size=%0.2f, observed effect size =%0.2f\n", rho[i], effect_size[j], d_observed))
    output <- pwr.t.test(d=d_observed, power=power_for_test, sig.level=0.05, type="paired", alternative="two.sided")
    n_pairedt[i, j] = ceiling(output$n)
          }
}

df_pairedt <- melt(n_pairedt)
colnames(df_pairedt) <- c('reliability', 'effect_size', 'sample_size')

#####################################################################
# Two-Sample-T Test
# The basic power analysis
# The effect size (Cohen'd) is mean/sd, or t/sqrt(N)
# > pwr.t.test(d=0.2,power=0.7,sig.level=0.05,type="two.sample",alternative="two.sided")
# > pwr.t.test(d=0.5,power=0.7,sig.level=0.05,type="two.sample",alternative="two.sided")
# > pwr.t.test(d=0.8,power=0.7,sig.level=0.05,type="two.sample",alternative="two.sided")
#####################################################################

rho <- seq(0.1, 1, by=0.01)
effect_size <- seq(0.15, 0.8, by=0.01)
power_for_test <- 0.8
n_2sample_t <- matrix(0, length(rho), length(effect_size))
dimnames(n_2sample_t) = list(rho, effect_size)
# calculate the sample size
for (i in 1:length(rho)){
  for (j in 1:length(effect_size)){
    d_observed <- effect_size[j]*sqrt(rho[i])
    #cat(sprintf("reliability=%0.2f, effect size=%0.2f, observed effect size =%0.2f\n", rho[i], effect_size[j], d_observed))
    output <- pwr.t.test(d=d_observed, power=power_for_test, sig.level=0.05, type="two.sample", alternative="two.sided")
    n_2sample_t[i,j] <- ceiling(output$n)
  }
}

df_2samplet <- melt(n_2sample_t)
colnames(df_2samplet) <- c('reliability', 'effect_size', 'sample_size')

#####################################################################
# ANOVA Test
# The basic power analysis
# The effect size (Cohen'f) 
# > pwr.anova.test(k=3, f=0.1, power=power_for_test, sig.level=0.05)
# > pwr.anova.test(k=3, f=d_observed, power=power_for_test, sig.level=0.05)
# > pwr.anova.test(k=3, f=d_observed, power=power_for_test, sig.level=0.05)
#####################################################################
rho <- seq(0.1, 1, by=0.01)
effect_size <- seq(0.075, 0.4, by=0.01)
power_for_test <- 0.8

n_anova <- matrix(0, length(rho), length(effect_size))
dimnames(n_anova) = list(rho, effect_size)
# calculate the sample size
for (i in 1:length(rho)){
  for (j in 1:length(effect_size)){
    d_observed <- effect_size[j]*sqrt(rho[i])
    #cat(sprintf("reliability=%0.2f, effect size=%0.2f, observed effect size =%0.2f\n", rho[i], effect_size[j], d_observed))
    output <- pwr.anova.test(k=3, f=d_observed, power=power_for_test, sig.level=0.05)
    n_anova[i,j] <- ceiling(output$n)
  }
}

df_anova <- melt(n_anova)
colnames(df_anova) <- c('reliability', 'effect_size_f', 'sample_size')

#####################################################################
#
#
#                             plot
#
#
#####################################################################


#####################################################################
# plot paired-t
#####################################################################
df <- df_pairedt
xaxis <- list(title= 'reliability', tickfont = list(size = 40), titlefont = list(size = 40), automargin = TRUE)
yaxis <- list(title= 'effect size', tickfont = list(size = 40), titlefont = list(size = 40), automargin = TRUE)
#
white2red.colors <- colorRampPalette(c("white","red", "red"))
red2blue.colors <- colorRampPalette(c('red', "mediumblue"))
cmap <- c(white2red.colors(250), red2blue.colors(5))

p<- plot_ly(df, x=~reliability, y=~effect_size, z=~sample_size, type="contour", 
            autocontour = F, 
            contours = list(
              showlabels = TRUE,
              labelfont = list(size=35)
            ),
            ncontours = 30,
            line = list(smoothing = 2),
            colors = cmap,
            zmin=10, zmax=1500,
            width = 1200, height = 1100) %>% 
  layout(
    title = "Sample Size (Power=0.8, paired t-test)",
    xaxis = xaxis, yaxis = yaxis)
colorbar(p, limits=c(10, 1500))
##############################
fout <- "Rplot_2dContours_PairedTtest__power0.8.png"
plotly_IMAGE(p, width=1300, height=1300, format="png", scale=1, out_file = fout)
##############################
df_fair <- df[ which(df$effect_size<=0.5 & df$reliability < 0.4), ]
#
red2blue.colors <- colorRampPalette(c('white', 'indianred1', "mediumblue"))
cmap <- c(red2blue.colors(200))
#
p<- plot_ly(df_fair, x=~reliability, y=~effect_size, z=~sample_size, type="contour", 
            autocontour = F, 
            contours = list(
              showlabels = TRUE,
              labelfont = list(size=40)
            ),
            ncontours = 30,
            line = list(smoothing = 2),
            colors = cmap,
            #reversescale = T,
            zmin=10, zmax=2000,
            width = 600, height = 1100) %>% 
  layout(
    title = "Sample Size (Power=0.8, paired t-test)",
    xaxis = xaxis, yaxis = yaxis)
colorbar(p, limits=c(10, 2000))
##############################
fout <- "Rplot_2dContours_PairedTtest_power0.8__fair_reliability.png"
plotly_IMAGE(p, width=700, height=1200, format="png", scale=1, out_file = fout)
##############################


#####################################################################
# two sample t
#####################################################################

df <- df_2samplet
xaxis <- list(title= 'reliability', tickfont = list(size = 40), titlefont = list(size = 40), automargin = TRUE)
yaxis <- list(title= 'effect size', tickfont = list(size = 40), titlefont = list(size = 40), automargin = TRUE)
#
white2red.colors <- colorRampPalette(c("white","red", "red"))
red2blue.colors <- colorRampPalette(c('red', "mediumblue"))
cmap <- c(white2red.colors(250), red2blue.colors(5))

p<- plot_ly(df, x=~reliability, y=~effect_size, z=~sample_size, type="contour", 
            autocontour = F, 
            contours = list(
              showlabels = TRUE,
              labelfont = list(size=35)
            ),
            ncontours = 30,
            line = list(smoothing = 2),
            colors = cmap,
            zmin=10, zmax=1500,
            width = 1200, height = 1100) %>% 
  layout(
    title = "Sample Size (Power=0.8, two-sample t-test)",
    xaxis = xaxis, yaxis = yaxis)
colorbar(p, limits=c(10, 1500))
##############################
fout <- "Rplot_2dContours_TwoSampleTtest__power0.8.png"
plotly_IMAGE(p, width=1300, height=1300, format="png", scale=1, out_file = fout)
##############################

df_fair <- df[ which(df$effect_size<=0.5 & df$reliability < 0.4), ]
#
red2blue.colors <- colorRampPalette(c('white', 'indianred1', "mediumblue"))
cmap <- c(red2blue.colors(200))
#
p<- plot_ly(df_fair, x=~reliability, y=~effect_size, z=~sample_size, type="contour", 
            autocontour = F, 
            contours = list(
              showlabels = TRUE,
              labelfont = list(size=40)
            ),
            ncontours = 30,
            line = list(smoothing = 2),
            colors = cmap,
            #reversescale = T,
            zmin=10, zmax=3000,
            width = 600, height = 1100) %>% 
  layout(
    title = "Sample Size (Power=0.8, two-sample t-test)",
    xaxis = xaxis, yaxis = yaxis)
colorbar(p, limits=c(10, 3000))
##############################
fout <- "Rplot_2dContours_TwoSampleTtest_power0.8__fair_reliability.png"
plotly_IMAGE(p, width=700, height=1200, format="png", scale=1, out_file = fout)
##############################




#####################################################################
# plot anova
#####################################################################
df <- df_anova
xaxis <- list(title= 'reliability', tickfont = list(size = 40), titlefont = list(size = 40), automargin = TRUE)
yaxis <- list(title= 'effect size', tickfont = list(size = 40), titlefont = list(size = 40), automargin = TRUE)
#
white2red.colors <- colorRampPalette(c("white","red", "red"))
red2blue.colors <- colorRampPalette(c('red', "mediumblue"))
cmap <- c(white2red.colors(250), red2blue.colors(5))
#
p<- plot_ly(df, x=~reliability, y=~effect_size_f, z=~sample_size, type="contour", 
            autocontour = F, 
            contours = list(
              showlabels = TRUE,
              labelfont = list(size=35)
            ),
            ncontours = 30,
            line = list(smoothing = 2),
            colors = cmap,
            zmin=10, zmax=1500,
            width = 1200, height = 1100) %>% 
  layout(
    title = "Sample Size (Power=0.8, one-way ANOVA)",
    xaxis = xaxis, yaxis = yaxis)
colorbar(p, limits=c(10, 2000))
##############################
fout <- "Rplot_2dContours_ANOVA_power0.8.png"
plotly_IMAGE(p, width=1500, height=1500, format="png", scale=1, out_file = fout)
##############################
ceiling(1200*0.25/0.4)*2
ceiling(1100*0.3/0.9)*2

df_fair <- df[ which(df$effect_size_f<=0.25 & df$reliability < 0.4), ]
#
red2blue.colors <- colorRampPalette(c('white', 'indianred1', "mediumblue"))
cmap <- c(red2blue.colors(200))
#
p<- plot_ly(df_fair, x=~reliability, y=~effect_size_f, z=~sample_size, type="contour", 
            autocontour = F, 
            contours = list(
              showlabels = TRUE,
              labelfont = list(size=40)
            ),
            ncontours = 30,
            line = list(smoothing = 2),
            colors = cmap,
            #reversescale = T,
            zmin=10, zmax=2500,
            width = 600, height = 1100) %>% 
  layout(
    title = "Sample Size (Power=0.8, one-way ANOVA)",
    xaxis = xaxis, yaxis = yaxis)
colorbar(p, limits=c(10, 3000))
##############################
fout <- "Rplot_2dContours_ANOVA_power0.8__fair_reliability.png"
plotly_IMAGE(p, width=700, height=1200, format="png", scale=1, out_file = fout)
##############################



