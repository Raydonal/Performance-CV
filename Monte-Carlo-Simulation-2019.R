# ---------------------------------------------------------------------------------------
# --------------- Reproducible Research  -----------------------------------------------
# Script to reproduce the results on paper:
# Performance of  some estimators of relative variability
#
# Created by Raydonal Ospina
# Modified by: Raydonal Ospina
#
# Raydonal  15/11/2017
# Updated   27/01/2019
# Contact: raydonal@de.ufpe.br
# ---------------------------------------------------------------------------------------

# # ---------------------------------------------------------------------------------------
# # Verify the packages
# if(!require(install.load)){install.packages("install.load", dep=TRUE); require("install.load")}
# # devtools::install_github("hrbrmstr/ggalt")
#
# # load the packages
# install_load("magrittr", "tidyr", "dplyr", "ggplot2", "leaflet",
#              "reshape", "reshape2", "grid", "gridExtra", "data.table",
#              "latex2exp", "gamlss", "plot3D", "compiler", "moments",
#              "tables", "fitdistrplus", "RColorBrewer", "lmomco", "lattice",
#              "seleniumPipes", "RSelenium", "readxl", "mailR",
#              "sp",  "stringr", "latticeExtra", "plyr",  #"reshapeGUI",
#              "gridBase", "gridExtra", "entropy", "EntropyEstimation",
#              "infotheo", "distrEx", "gamlss", "betareg", "readxl", "haven",
#              "survey", "sampling", "ncf", "spdep", "data.table", "compiler",
#              "plot3D", "tidyverse", "tolerance")
library(tidyverse)
library(compiler)
library(gridExtra)
library(gamlss.dist)
library(gamlss)
library(data.table)
library(tolerance)
library(purrr)

#
# if (!require("devtools")){install.packages("devtools"); devtools::install_github("shiny")}
# # ---------------------------------------------------------------------------------------
#
#
# # ---------------------------------------------------------------------------------------
# #### Functions
# # ---------------------------------------------------------------------------------------
# ## median
# median <- function(x) as.numeric(quantile(x, probs = .5))
# median <- cmpfun(median)
# # ---------------------------------------------------------------------------------------
#
# # ---------------------------------------------------------------------------------------
# ## classical coefficient of variation
# old <- function(x) sd(x)/mean(x)
# old <- cmpfun(old)
# # ---------------------------------------------------------------------------------------
#
# # ---------------------------------------------------------------------------------------
# ## edith
# edith <- function(x){
#   q <- quantile(x, probs = c(.25, .75))
#   as.numeric((q[2] - q[1]) / (q[2] + q[1]))
# }
# edith <- cmpfun(edith)
# # ---------------------------------------------------------------------------------------
#
# # ---------------------------------------------------------------------------------------
# ## edith type 8
# edith8 <- function(x){
#   q <- quantile(x, probs = c(.25, .75), type = 8)
#   as.numeric((q[2] - q[1]) / (q[2] + q[1]))
# }
# edith8 <- cmpfun(edith8)
# # ---------------------------------------------------------------------------------------
#
# # ---------------------------------------------------------------------------------------
# ## mad/median
# madmedian <- function(x){
#   mdn <- median(x)
#   MAD <- 1.4826 * median(abs(x - mdn))
#   MAD/mdn
# }
# madmedian <- cmpfun(madmedian)
# # ---------------------------------------------------------------------------------------
#
# # ---------------------------------------------------------------------------------------
# ## mnad/median
# MnADmedian <- function(x){
#   mdn <- median(x)
#   MnAD <- mean(abs(x - mdn))
#   MnAD/mdn
# }
# MnADmedian <- cmpfun(MnADmedian)
# # ---------------------------------------------------------------------------------------
#
# # ---------------------------------------------------------------------------------------
# ## all together
# cvs <- function(x)  c(old(x), edith(x), edith8(x), madmedian(x), MnADmedian(x))
# cvs <- cmpfun(cvs)
# # ---------------------------------------------------------------------------------------
#
#
# ## PLOTS
# # ---------------------------------------------------------------------------------------
#
# ## Plot of CV performance in ggplot2
#
# plot.CV.1 <- function(data=data, # data base
#                     name.title="", # title of plot
#                     name.distribution="", # Name of output files
#                     ...
#                     )
# {
#   set.seed(11)
#   data$Estimator <- factor(data$Estimator, levels=c("CQV7","CQV8","CVMAD","CQVMnAD"),
#                                 labels=c(expression(bolditalic(CQV)[7]),
#                                          expression(bolditalic(CQV)[8]),
#                                          expression(bolditalic(CQV)[MAD]),
#                                          expression(bolditalic(CQV)[MnAD]))
#                            )
#
#   pl <-  ggplot(data, aes(x=true,  y=gamma, fill=n, color=n, size=n))
#   pl <-pl+ geom_jitter(alpha=0.6, shape=21, width = 0.05, height = 0.05)+
#     facet_wrap(~Estimator, ncol=4, labeller = label_parsed) +
#     scale_colour_brewer(palette = "Set1")+
#     scale_fill_brewer(palette = "Set1") #+scale_color_brewer(palette = "Spectral")
#     # geom_point(shape=1)
#   pl <- pl +  theme(
#     legend.key = element_rect(colour = "white",
#                               fill = "white"),
#     legend.key.size = unit(1.1, "cm"),
#     legend.text = element_text(face = "bold",
#                                size=18),
#     legend.title = element_text(size=18, face="bold", hjust = 0.5),
#     panel.grid.major = element_line(colour = "gray70",
#                                     linetype = "dotted"),
#     # panel.grid.minor = element_line(colour = "red", linetype = "dotted"),
#     panel.background = element_rect(fill = "white",
#                                     colour="black"),
#     panel.spacing = unit(1.1, "cm"),
#     strip.text.x = element_text(size=18,
#                                 hjust=0.5,
#                                 vjust=0.5,
#                                 face="bold", lineheight = 0.5),
#
#     strip.background = element_rect(colour="black", fill="gray98"),
#     axis.text=element_text(size=17, face="plain", colour="gray24"),
#
#     axis.text.x  = element_text(angle=0, vjust=0.5, size=17),
#     axis.title=element_text(size=22,face="plain"),
#     plot.title = element_text(size = 18, colour = "black", face="bold", hjust=0.5),
#     # legend.position = c(0.85, 0.85)
#     legend.position ="right", #c(0.1, 0.85)
#     legend.direction="vertical",
#     legend.title.align=0.5
#
#   ) +  guides(fill = guide_legend(label.position = "right", keywidth = 0.8,
#                                   label.hjust=0.5, title.hjust=0.5))
#
#   pl <- pl +    labs(x = expression(theta), y=expression(gamma), title=paste(name.title))
#
#   # cartesian coordinate
#   a = pl+scale_x_continuous(breaks=seq(0.0, 1.0, 0.2), limits=c(0,1), expand=c(0.02,0.02))
#   # a  <- a+scale_y_continuous(expand = expand_scale(mult = c(0, .1)))#scale_y_continuous(expand=c(0.1,0.1))
#
#   # polar coordinate
#   b = pl+coord_polar(theta = "x", start = 0, direction=1)+
#     scale_x_continuous(breaks=seq(0.0, 1.0, 0.1), limits=c(0,1), expand=c(0,0))
#
#   m = list(a=a,b=b)
#
#   # save the plots
#   ggsave(paste0(name.distribution,"-1.pdf"), m$a, width=16, height = 5)
#   ggsave(paste0(name.distribution,"-1-Polar.pdf"), m$b, width=16, height = 5)
#
#   # return the plots for modifications
#   return(m)
#
# }
#
#
# plot.CV.2 <- function(data=data, # data base
#                       name.title="", # title of plot
#                       name.distribution="", # Name of output files
#                       ...
# )
# {
#   set.seed(11)
#   data$Estimator <- factor(data$Estimator, levels=c("CQV7","CQV8","CVMAD","CQVMnAD"),
#                            labels=c(expression(bolditalic(CQV)[7]),
#                                     expression(bolditalic(CQV)[8]),
#                                     expression(bolditalic(CQV)[MAD]),
#                                     expression(bolditalic(CQV)[MnAD])))
#
#   pl <-  ggplot(data, aes(x=true,  y=gamma, fill=n, color=n, size=n))
#   pl <-pl+ geom_jitter(alpha=0.6, shape=21, width = 0.05, height = 0.05)+
#     facet_wrap(~Estimator, ncol=4, labeller = label_parsed)
#   pl <- pl +  theme(
#     legend.key = element_rect(colour = "white",
#                               fill = "white"),
#     legend.key.size = unit(1.1, "cm"),
#     legend.text = element_text(face = "bold",
#                                size=18),
#     legend.title = element_text(size=18, face="bold", hjust = 0.5),
#     panel.grid.major = element_line(colour = "gray70",
#                                     linetype = "dotted"),
#     # panel.grid.minor = element_line(colour = "red", linetype = "dotted"),
#     panel.background = element_rect(fill = "white",
#                                     colour="black"),
#     panel.spacing = unit(1.1, "cm"),
#     strip.text.x = element_text(size=18,
#                                 hjust=0.5,
#                                 vjust=0.5,
#                                 face="bold", lineheight = 0.5),
#
#     strip.background = element_rect(colour="black", fill="gray98"),
#     axis.text=element_text(size=17, face="plain", colour="gray24"),
#
#     axis.text.x  = element_text(angle=0, vjust=0.5, size=17),
#     axis.title=element_text(size=22,face="plain"),
#     plot.title = element_text(size = 18, colour = "black", face="bold", hjust=0.5),
#     # legend.position = c(0.85, 0.85)
#     legend.position ="right", #c(0.1, 0.85)
#     legend.direction="vertical",
#     legend.title.align=0.5
#
#   ) +  guides(fill = guide_legend(label.position = "right", keywidth = 0.8,
#                                   label.hjust=0.5, title.hjust=0.5))
#
#   pl <- pl +    labs(x = expression(theta), y=expression(gamma), title=paste(name.title))
#
#   # cartesian coordinate
#   a = pl+scale_x_continuous(breaks=seq(0.0, 1.0, 0.2), limits=c(0,1), expand=c(0.02,0.02))
#   # a  <- a+scale_y_continuous(expand = expand_scale(mult = c(0, .1)))#scale_y_continuous(expand=c(0.1,0.1))
#
#   # polar coordinate
#   b = pl+coord_polar(theta = "x", start = 0, direction=1)+
#     scale_x_continuous(breaks=seq(0.0, 1.0, 0.1), limits=c(0,1), expand=c(0,0))
#
#   m = list(a=a,b=b)
#
#   # save the plots
#   ggsave(paste0(name.distribution,"-2.pdf"), m$a, width=16, height = 5)
#   ggsave(paste0(name.distribution,"-2-Polar.pdf"), m$b, width=16, height = 5)
#
#   # return the plots for modifications
#   return(m)
#
# }
#
#
# #-----------------------------------------
# plot.CV.3 <- function(data=data, # data base
#                       name.title="", # title of plot
#                       name.distribution="", # Name of output files
#                       ...
# )
# {
#  # set.seed(11)
#   data$Estimator <- factor(data$Estimator, levels=c("CQV7","CQV8","CVMAD","CQVMnAD"),
#                            labels=c(expression(bolditalic(CQV)[7]),
#                                     expression(bolditalic(CQV)[8]),
#                                     expression(bolditalic(CV)[MAD]),
#                                     expression(bolditalic(CV)[MnAD]))
#   )
#
#   pl <-  ggplot(data, aes(x = true,  y=lgamma, fill = n, color = n, size = n))
#   pl <- pl + geom_point(alpha = 0.7, shape=21) + #, width = 0.05, height = 0.5) +
#     facet_wrap(~Estimator, ncol = 4, labeller = label_parsed) +
#     scale_color_manual(values = c("blue",
#                                   "red",
#                                   "green",
#                                   "brown",
#                                   "orange")) +
#     scale_fill_manual(values = c("blue",
#                                   "red",
#                                   "green",
#                                   "brown",
#                                   "orange"))+
#     geom_hline(aes(yintercept = 0), col="red")
#     #
#     #
#     # scale_colour_brewer(palette = "Set1")+
#     # scale_fill_brewer(palette = "Set1") #+scale_color_brewer(palette = "Spectral")
#   # geom_point(shape=1)
#   pl <- pl +  theme(
#     legend.key = element_rect(colour = "white",
#                               fill = "white"),
#     legend.key.size = unit(1.1, "cm"),
#     legend.text = element_text(face = "bold",
#                                size=18),
#     legend.title = element_text(size=18, face="bold", hjust = 0.5),
#     panel.grid.major = element_line(colour = "gray70",
#                                     linetype = "dotted"),
#     # panel.grid.minor = element_line(colour = "red", linetype = "dotted"),
#     panel.background = element_rect(fill = "white",
#                                     colour="black"),
#     panel.spacing = unit(1.1, "cm"),
#     strip.text.x = element_text(size=18,
#                                 hjust=0.5,
#                                 vjust=0.5,
#                                 face="bold", lineheight = 0.5),
#
#     strip.background = element_rect(colour="black", fill="gray98"),
#     axis.text=element_text(size=17, face="plain", colour="gray24"),
#
#     axis.text.x  = element_text(angle=0, vjust=0.5, size=17),
#     axis.title=element_text(size=22,face="plain"),
#     plot.title = element_text(size = 18, colour = "black", face="bold", hjust=0.5),
#     # legend.position = c(0.85, 0.85)
#     legend.position ="right", #c(0.1, 0.85)
#     legend.direction="vertical",
#     legend.title.align=0.5
#
#   ) +  guides(fill = guide_legend(label.position = "right", keywidth = 0.8,
#                                   label.hjust=0.5, title.hjust=0.5))
#
#   pl <- pl +    labs(x = expression(theta), y=expression(log[10](gamma)), title=paste(name.title))
#
#   # cartesian coordinate
#   a = pl+scale_x_continuous(breaks=seq(0.0, 1.0, 0.2), limits=c(0,1), expand=c(0.02,0.02))
#   # a  <- a+scale_y_continuous(limits=c(0,max(data_long$gamma)),
#   #                          breaks  = seq(0,max(data_long$gamma), by = 2000),
#   #                          expand = c(0,0))
#   #
#
#   # polar coordinate
#   b = pl+coord_polar(theta = "x", start = 0, direction=1)+
#     scale_x_continuous(breaks=seq(0.0, 1.0, 0.1), limits=c(0,1), expand=c(0,0))
#
#
#   m = list(a=a,b=b)
#
#   # save the plots
#   ggsave(paste0(name.distribution,"-4.pdf"), m$a, width=16, height = 5)
#   ggsave(paste0(name.distribution,"-4-Polar.pdf"), m$b, width=16, height = 5)
#
#   # return the plots for modifications
#   return(m)
#
# }
#

#
# # #################################################
# #
# #-----------------------------------------
# plot.CV.4 <- function(data=data, # data base
#                       name.title="", # title of plot
#                       name.distribution="", # Name of output files
#                       ...
# )
# {
#   # set.seed(11)
#   data$Estimator <- factor(data$Estimator, levels=c("CQV7","CQV8","CVMAD","CQVMnAD"),
#                            labels=c(expression(bolditalic(CQV)[7]),
#                                     expression(bolditalic(CQV)[8]),
#                                     expression(bolditalic(CV)[MAD]),
#                                     expression(bolditalic(CV)[MnAD]))
#   )
#
#   pl <-  ggplot(data, aes(x = true,  y=lgamma,  shape=n))
#
#   pl <- pl + geom_point(alpha = 0.7, color = "black", size=1) + #, width = 0.05, height = 0.5) +
#     facet_wrap(~Estimator, ncol = 4, labeller = label_parsed) +
#      scale_shape_manual(values = c(0,1,2,5,6))
#     #                               "red",
#     #                               "green",
#     #                               "brown",
#     #                               "orange")) +
#     # scale_fill_manual(values = c("blue",
#     #                              "red",
#     #                              "green",
#     #                              "brown",
#     #                              "orange"))+
#     geom_hline(aes(yintercept = 0), col="red")
#   #
#   #
#   # scale_colour_brewer(palette = "Set1")+
#   # scale_fill_brewer(palette = "Set1") #+scale_color_brewer(palette = "Spectral")
#   # geom_point(shape=1)
#   pl <- pl +  theme(
#     legend.key = element_rect(colour = "white",
#                               fill = "white"),
#     legend.key.size = unit(1.1, "cm"),
#     legend.text = element_text(face = "bold",
#                                size=18),
#     legend.title = element_text(size=18, face="bold", hjust = 0.5),
#     panel.grid.major = element_line(colour = "gray70",
#                                     linetype = "dotted"),
#     # panel.grid.minor = element_line(colour = "red", linetype = "dotted"),
#     panel.background = element_rect(fill = "white",
#                                     colour="black"),
#     panel.spacing = unit(1.1, "cm"),
#     strip.text.x = element_text(size=18,
#                                 hjust=0.5,
#                                 vjust=0.5,
#                                 face="bold", lineheight = 0.5),
#
#     strip.background = element_rect(colour="black", fill="gray98"),
#     axis.text=element_text(size=17, face="plain", colour="gray24"),
#
#     axis.text.x  = element_text(angle=0, vjust=0.5, size=17),
#     axis.title=element_text(size=22,face="plain"),
#     plot.title = element_text(size = 18, colour = "black", face="bold", hjust=0.5),
#     # legend.position = c(0.85, 0.85)
#     legend.position ="right", #c(0.1, 0.85)
#     legend.direction="vertical",
#     legend.title.align=0.5
#
#   ) +  guides(fill = guide_legend(label.position = "right", keywidth = 0.8,
#                                   label.hjust=0.5, title.hjust=0.5))
#
#   pl <- pl +    labs(x = expression(theta), y=expression(log[10](gamma)), title=paste(name.title))
#
#   # cartesian coordinate
#   a = pl+scale_x_continuous(breaks=seq(0.0, 1.0, 0.2), limits=c(0,1), expand=c(0.02,0.02))
#   # a  <- a+scale_y_continuous(limits=c(0,max(data_long$gamma)),
#   #                          breaks  = seq(0,max(data_long$gamma), by = 2000),
#   #                          expand = c(0,0))
#   #
#
#   # polar coordinate
#   b = pl+coord_polar(theta = "x", start = 0, direction=1)+
#     scale_x_continuous(breaks=seq(0.0, 1.0, 0.1), limits=c(0,1), expand=c(0,0))
#
#
#   m = list(a=a,b=b)
#
#   # save the plots
#   ggsave(paste0(name.distribution,"-4.pdf"), m$a, width=16, height = 5)
#   ggsave(paste0(name.distribution,"-4-Polar.pdf"), m$b, width=16, height = 5)
#
#   # return the plots for modifications
#   return(m)
#
# }
#
#

#
# # ---------------------------------------------------------------------------------------
#
#
# # ---------------------------------------------------------------------------------------
# ## Monte Carlo Simulation
#
# ## Parameters of simulation
#
# ## random seed
#  set.seed(121)
#
# # Sample size
# n <- c(10, 25, 50, 100, 200)
#
# # mean
# mu <- c(0.1, 0.4, 0.7, 1, 5,  15,  30)
#
# # dispersion
# s <- c(.1, 0.3,  0.6, 1, 3, 5)
#
#
# ## number of Monte Carlo replicates
# B <- 10000
# # ---------------------------------------------------------------------------------------
#
#
# ptm <- proc.time()
#
# #########################################################################################
# ## ------------------------
# ##  Normal
# # E(X) = mu
# # VarX = sigma^2
# ## ------------------------
#
# params <- expand.grid(n = n, mu = mu, s = s)
# NROW(params)
# params$true <- with(params, s/mu)
#
# params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
# save(params, file="Normal-params.Rdata")
#
# L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
# colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
#
# for(i in 1:NROW(params)){
#   r <- as.numeric(unlist(params[i,]))
#   ## replicates
#   res <- parallel:::mclapply(1:B, function(i){
#     x <- rnorm(n = r[1], mean = r[2], s = r[3])
#     cvs(x)
#   }, mc.cores = 4)
#   res <- do.call(rbind, res)
#   L[i, ] <- colMeans((res-r[4])^2, na.rm = TRUE)
#   if(i %% 10 == 0) cat(i, " ")
#   #  if(i == NROW(params)) cat("\n")
# }
# out <- cbind(params, L)
#
# # Add the Gamma measures
# out$CQV7=out$edith/out$old
# out$CQV8=out$edith8/out$old
# out$CVMAD=out$MADmedian/out$old
# out$CQVMnAD=out$MnADmedian/out$old
#
# save(out, file="Normal-Sim-Wide.Rdata")
#
# load("Normal-Sim-Wide.Rdata")
#
# data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
# data_long$n=as.factor(data_long$n)
# # data_long$m=as.factor(data_long$m)
# # data_long$p=as.factor(format(data_long$p, digits=2)  )
# data_long$lgamma = log10(data_long$gamma)
# data_long$ltrue = log10(data_long$true)
# # remove the infnite values
# data_long <- data_long[!is.infinite((data_long$gamma)), ]
#
#
# save(data_long, file="Normal-Sim-Long.Rdata")
#
# Normal = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
# save(Normal, file="Normal-Table.Rdata")
#
# # v.1 = plot.CV.1(data=data_long, name.distribution="Normal")
# # v.2 = plot.CV.2(data=data_long, name.distribution="Normal")
# load("Normal-Sim-Long.Rdata")
# v.3 = plot.CV.3(data=data_long, name.distribution="Normal")
# #########################################################################################
#
#
#
# #########################################################################################
# ## ------------------------
# ##  Normal Contaminated 5% lambda=3
# # E(X) = mu
# # VarX = sigma^2
# ## ------------------------
#
# params <- expand.grid(n = n, mu = mu, s = s)
# NROW(params)
# params$true <- with(params, s/mu)
#
# params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
# save(params, file="Normal-5-params.Rdata")
#
# L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
# colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
#
# for(i in 1:NROW(params)){
#   r <- as.numeric(unlist(params[i,]))
#   ## replicates
#   res <- parallel:::mclapply(1:B, function(i){
#     x <- (1-(5/100))*rnorm(n = r[1], mean = r[2], s = r[3])+
#       (5/100)*rnorm(n = r[1], mean = r[2], s = sqrt(3)*r[3])
#     cvs(x)
#   }, mc.cores = 4)
#   res <- do.call(rbind, res)
#   L[i, ] <- colMeans((res-r[4])^2, na.rm = TRUE)
#   if(i %% 10 == 0) cat(i, " ")
#   #  if(i == NROW(params)) cat("\n")
# }
# out <- cbind(params, L)
#
# # Add the Gamma measures
# out$CQV7=out$edith/out$old
# out$CQV8=out$edith8/out$old
# out$CVMAD=out$MADmedian/out$old
# out$CQVMnAD=out$MnADmedian/out$old
#
# save(out, file="Normal-5-Sim-Wide.Rdata")
#
# load("Normal-5-Sim-Wide.Rdata")
#
# data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
# data_long$n=as.factor(data_long$n)
# # data_long$m=as.factor(data_long$m)
# # data_long$p=as.factor(format(data_long$p, digits=2)  )
# data_long$lgamma = log10(data_long$gamma)
# data_long$ltrue = log10(data_long$true)
# # remove the infnite values
# data_long <- data_long[!is.infinite((data_long$gamma)), ]
#
#
# save(data_long, file="Normal-5-Sim-Long.Rdata")
#
# Normal.5 = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
# save(Normal.5, file="Normal.5-Table.Rdata")
#
# # v.1 = plot.CV.1(data=data_long, name.distribution="Normal-5")
# # v.2 = plot.CV.2(data=data_long, name.distribution="Normal-5")
#
#
# load("Normal-5-Sim-Long.Rdata")
# v.3 = plot.CV.3(data=data_long, name.distribution="Normal-5")
# #########################################################################################
#
#
#
#
# #########################################################################################
# ## ------------------------
# ##  Normal Contaminated 10% lambda=3
# # E(X) = mu
# # VarX = sigma^2
# ## ------------------------
#
# params <- expand.grid(n = n, mu = mu, s = s)
# NROW(params)
# params$true <- with(params, s/mu)
#
# params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
# save(params, file="Normal-10-params.Rdata")
#
# L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
# colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
#
# for(i in 1:NROW(params)){
#   r <- as.numeric(unlist(params[i,]))
#   ## replicates
#   res <- parallel:::mclapply(1:B, function(i){
#     x <- (1-(10/100))*rnorm(n = r[1], mean = r[2], s = r[3])+
#       (10/100)*rnorm(n = r[1], mean = r[2], s = sqrt(3)*r[3])
#     cvs(x)
#   }, mc.cores = 4)
#   res <- do.call(rbind, res)
#   L[i, ] <- colMeans((res-r[4])^2, na.rm = TRUE)
#   if(i %% 10 == 0) cat(i, " ")
#   #  if(i == NROW(params)) cat("\n")
# }
# out <- cbind(params, L)
#
# # Add the Gamma measures
# out$CQV7=out$edith/out$old
# out$CQV8=out$edith8/out$old
# out$CVMAD=out$MADmedian/out$old
# out$CQVMnAD=out$MnADmedian/out$old
#
#  save(out, file="Normal-10-Sim-Wide.Rdata")
#
#  load("Normal-10-Sim-Wide.Rdata")
#
# data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
# data_long$n=as.factor(data_long$n)
# # data_long$m=as.factor(data_long$m)
# # data_long$p=as.factor(format(data_long$p, digits=2)  )
# data_long$lgamma = log10(data_long$gamma)
# data_long$ltrue = log10(data_long$true)
# # remove the infnite values
# data_long <- data_long[!is.infinite((data_long$gamma)), ]
#
#
# save(data_long, file="Normal-10-Sim-Long.Rdata")
#
# Normal.10 = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
# save(Normal.10, file="Normal.10-Table.Rdata")
#
# # v.1 = plot.CV.1(data=data_long, name.distribution="Normal-10")
# # v.2 = plot.CV.2(data=data_long, name.distribution="Normal-10")
#
# load("Normal-10-Sim-Long.Rdata")
# v.3 = plot.CV.3(data=data_long, name.distribution="Normal-10")
# #########################################################################################
#
#
#
# #########################################################################################
# ## ------------------------
# ##  Normal Contaminated 15% lambda=3
# # E(X) = mu
# # VarX = sigma^2
# ## ------------------------
#
# params <- expand.grid(n = n, mu = mu, s = s)
# NROW(params)
# params$true <- with(params, s/mu)
#
# params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
# save(params, file="Normal-15-params.Rdata")
#
# L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
# colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
#
# for(i in 1:NROW(params)){
#   r <- as.numeric(unlist(params[i,]))
#   ## replicates
#   res <- parallel:::mclapply(1:B, function(i){
#     x <- (1-(15/100))*rnorm(n = r[1], mean = r[2], s = r[3])+
#       (15/100)*rnorm(n = r[1], mean = r[2], s = sqrt(3)*r[3])
#     cvs(x)
#   }, mc.cores = 4)
#   res <- do.call(rbind, res)
#   L[i, ] <- colMeans((res-r[4])^2, na.rm = TRUE)
#   if(i %% 10 == 0) cat(i, " ")
#   #  if(i == NROW(params)) cat("\n")
# }
# out <- cbind(params, L)
#
# # Add the Gamma measures
# out$CQV7=out$edith/out$old
# out$CQV8=out$edith8/out$old
# out$CVMAD=out$MADmedian/out$old
# out$CQVMnAD=out$MnADmedian/out$old
#
#  save(out, file="Normal-15-Sim-Wide.Rdata")
# load("Normal-15-Sim-Wide.Rdata")
#
# data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
# data_long$n=as.factor(data_long$n)
# # data_long$m=as.factor(data_long$m)
# # data_long$p=as.factor(format(data_long$p, digits=2)  )
# data_long$lgamma = log10(data_long$gamma)
# data_long$ltrue = log10(data_long$true)
# # remove the infnite values
# data_long <- data_long[!is.infinite((data_long$gamma)), ]
#
#
# save(data_long, file="Normal-15-Sim-Long.Rdata")
#
# Normal.15 = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
# save(Normal.15, file="Normal.15-Table.Rdata")
#
#
# # v.1 = plot.CV.1(data=data_long, name.distribution="Normal-15")
# # v.2 = plot.CV.2(data=data_long, name.distribution="Normal-15")
# load("Normal-15-Sim-Long.Rdata")
# v.3 = plot.CV.3(data=data_long, name.distribution="Normal-15")
# #########################################################################################
#
#
# #########################################################################################
# ## ------------------------
# ##  Normal Contaminated 20% lambda=3
# # E(X) = mu
# # VarX = sigma^2
# ## ------------------------
#
# params <- expand.grid(n = n, mu = mu, s = s)
# NROW(params)
# params$true <- with(params, s/mu)
#
# params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
# save(params, file="Normal-20-params.Rdata")
#
# L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
# colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
#
# for(i in 1:NROW(params)){
#   r <- as.numeric(unlist(params[i,]))
#   ## replicates
#   res <- parallel:::mclapply(1:B, function(i){
#     x <- (1-(20/100))*rnorm(n = r[1], mean = r[2], s = r[3])+
#       (20/100)*rnorm(n = r[1], mean = r[2], s = sqrt(3)*r[3])
#     cvs(x)
#   }, mc.cores = 4)
#   res <- do.call(rbind, res)
#   L[i, ] <- colMeans((res-r[4])^2, na.rm = TRUE)
#   if(i %% 10 == 0) cat(i, " ")
#   #  if(i == NROW(params)) cat("\n")
# }
# out <- cbind(params, L)
#
# # Add the Gamma measures
# out$CQV7=out$edith/out$old
# out$CQV8=out$edith8/out$old
# out$CVMAD=out$MADmedian/out$old
# out$CQVMnAD=out$MnADmedian/out$old
#
#  save(out, file="Normal-20-Sim-Wide.Rdata")
# load("Normal-20-Sim-Wide.Rdata")
# data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
# data_long$n=as.factor(data_long$n)
# # data_long$m=as.factor(data_long$m)
# # data_long$p=as.factor(format(data_long$p, digits=2)  )
# data_long$lgamma = log10(data_long$gamma)
# data_long$ltrue = log10(data_long$true)
# # remove the infnite values
# data_long <- data_long[!is.infinite((data_long$gamma)), ]
#
#
# save(data_long, file="Normal-20-Sim-Long.Rdata")
#
# Normal.20 = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
# save(Normal.20, file="Normal.20-Table.Rdata")
#
#
# # v.1 = plot.CV.1(data=data_long, name.distribution="Normal-20")
# # v.2 = plot.CV.2(data=data_long, name.distribution="Normal-20")
#
# load("Normal-20-Sim-Long.Rdata")
# v.3 = plot.CV.3(data=data_long, name.distribution="Normal-20")
# #########################################################################################
#
#
#
#
# #########################################################################################
# ## ------------------------
# ## Lognormal
# # Here  μ=mu and σ=sigma are the mean and standard deviation of the logarithm.
# # alpha = E(X) = exp(μ + 1/2 σ^2)
# # beta = Var(X) = exp(2*μ + σ^2)*(exp(σ^2) - 1)
# # The coefficient of variation is sqrt(exp(σ^2) - 1)
# ## ------------------------
#
#
# # location <- log(m^2 / sqrt(s^2 + m^2))
# # shape <- sqrt(log(1 + (s^2 / m^2)))
# #
#
# # function alpha as (mu,s) functions
# func.alpha <- function(m,s) {
# log(m^2 / sqrt(s^2 + m^2))
#   #exp(mu+(s^2/2))
#   }
#
# # function beta as (mu,s) functions
# func.beta <- function(m,s) {
# sqrt(log(1 + (s^2 / m^2)))
#   #exp(2*mu+s^2)*(exp(s^2)-1)
#   }
#
# #alpha
# alpha <- outer(mu, s,  Vectorize(func.alpha))
# # True value of alpha
# alpha <- unique(sort(alpha))
#
# #beta
# beta <- outer(mu, s,  Vectorize(func.beta))
#
# # True value of beta
# beta <- unique(sort(beta))
#
# params <- expand.grid(n = n, a= alpha, b = sqrt(beta))
#
# params$true <- with(params, {
#   mu <- a
#   s <- b
#   true <- s/mu
# })
#
# params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
#
# save(params, file="LogNormal-params.Rdata")
#
# L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
# colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
#
# for(i in 1:NROW(params)){
#   r <- as.numeric(unlist(params[i,]))
#   ## replicates
#   res <- parallel:::mclapply(1:B, function(i){
#     x <-rlnorm(n = r[1], meanlog = r[2], sdlog = r[3])
#     cvs(x)
#   }, mc.cores = 4)
#   res <- do.call(rbind, res)
#   L[i, ] <- colMeans((res-r[4])^2, na.rm = TRUE)
#  # if(i %% 10 == 0)
#     cat(i, " ")
#   #  if(i == NROW(params)) cat("\n")
# }
#
# out <- cbind(params, L)
#
# # Add the Gamma measures
# out$CQV7=out$edith/out$old
# out$CQV8=out$edith8/out$old
# out$CVMAD=out$MADmedian/out$old
# out$CQVMnAD=out$MnADmedian/out$old
#
# save(out, file="LogNormal-Sim-Wide.Rdata")
#
# data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
# data_long$n=as.factor(data_long$n)
# # data_long$m=as.factor(data_long$m)
# # data_long$p=as.factor(format(data_long$p, digits=2)  )
# data_long$lgamma = -log10(data_long$gamma)
# data_long$ltrue = -log10(data_long$true)
#
# save(data_long, file="LogNormal-Sim-Long.Rdata")
#
# Lognormal = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
# save(Lognormal, file="LogNormal-Table.Rdata")
#
# # v.1 = plot.CV.1(data=data_long, name.distribution="Normal")
# # v.2 = plot.CV.2(data=data_long, name.distribution="Normal")
# load("LogNormal-Sim-Long.Rdata")
# v.3 = plot.CV.3(data=data_long, name.distribution="Lognormal")
#
# #########################################################################################
#
#
#
#
# #########################################################################################
# ## ------------------------
# ## shifted-Exponential f(x)=1/β e^-(x−μ)/β
# # Here  μ=mu and σ=sigma are the mean and standard deviation of the logarithm.
# # alpha = E(X) = μ+β and
# # beta = Var(X) = β^2
# ## ------------------------
#
#
# # location <- log(m^2 / sqrt(s^2 + m^2))
# # shape <- sqrt(log(1 + (s^2 / m^2)))
# #
#
# # function alpha as (mu,s) functions
# func.alpha <- function(m,s) {
# m+s
#   #exp(mu+(s^2/2))
# }
#
# # function beta as (mu,s) functions
# func.beta <- function(m,s) {
#  s^2
#   #exp(2*mu+s^2)*(exp(s^2)-1)
# }
#
# #alpha
# alpha <- outer(mu, s,  Vectorize(func.alpha))
# # True value of alpha
# alpha <- unique(sort(alpha))
#
# #beta
# beta <- outer(mu, s,  Vectorize(func.beta))
#
# # True value of beta
# beta <- unique(sort(beta))
#
# params <- expand.grid(n = n, a= alpha, b = sqrt(beta))
#
# params$true <- with(params, {
#   mu <- a
#   s <- b
#   true <- s/mu
# })
#
# params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
#
# save(params, file="ShiftExpo-params.Rdata")
#
# L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
# colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
#
# for(i in 1:NROW(params)){
#   r <- as.numeric(unlist(params[i,]))
#   ## replicates
#   res <- parallel:::mclapply(1:B, function(i){
#     x <-r2exp(n = r[1], rate = r[3], shift = r[2])
#     cvs(x)
#   }, mc.cores = 4)
#   res <- do.call(rbind, res)
#   L[i, ] <- colMeans((res-r[4])^2, na.rm = TRUE)
#   # if(i %% 10 == 0)
#   cat(i, " ")
#   #  if(i == NROW(params)) cat("\n")
# }
#
# out <- cbind(params, L)
#
# # Add the Gamma measures
# out$CQV7=out$edith/out$old
# out$CQV8=out$edith8/out$old
# out$CVMAD=out$MADmedian/out$old
# out$CQVMnAD=out$MnADmedian/out$old
#
# save(out, file="ShiftExpo-Sim-Wide.Rdata")
#
# data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
# data_long$n=as.factor(data_long$n)
# # data_long$m=as.factor(data_long$m)
# # data_long$p=as.factor(format(data_long$p, digits=2)  )
# data_long$lgamma = -log10(data_long$gamma)
# data_long$ltrue = -log10(data_long$true)
#
# save(data_long, file="ShiftExpo-Sim-Long.Rdata")
#
# ShiftExponential = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
# save(ShiftExponential, file="ShiftExpo-Table.Rdata")
#
# # v.1 = plot.CV.1(data=data_long, name.distribution="Normal")
# # v.2 = plot.CV.2(data=data_long, name.distribution="Normal")
# load("ShiftExpo-Sim-Long.Rdata")
# v.3 = plot.CV.3(data=data_long, name.distribution="ShiftExpo")
#
# #########################################################################################
#
#
#
#
#
# #########################################################################################
# ## ---------------------------------------------------------------------------------------
# ##  Binomial case
# # E(X) = m*p = mu
# # Var(X) = m*p*(1-p) = sigma^2 =>
# # p = 1-(sigma^2/ mu)
# # m = [mu / p],   [.] is the integer part function
# ## ---------------------------------------------------------------------------------------
#
#
# # function p as mu and s function
# func.p <- function(m,s) {1 -(s^2/m)}
#
# # Probability with valid restrictio 0 < s^2/mu < 1
# p <- outer(mu, s, Vectorize(func.p))
# # True value of p
# p <- unique(sort(p[p>0]))
#
# # m experiments to exit - Binomial(m,p)
# func.m <- function(m, p) {floor(mu/p)}
#
# # Probability with valid restriction 0 < s^2/mu < 1
# m <- unique(sort(outer(mu, p, func.m)))
# m <- m[m>1]
#
# # Grid parameter - Binomial distribution
# # params <- expand.grid(m = m, p = p)
#
# params <- expand.grid(n = n, m = m, p = p)
#
# # true parameter value CV
# params$true <- with(params, sqrt(m*p*(1-p)) / (m*p))
#
# params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
# save(params, file="Binomial-params.Rdata")
#
# # Monte Carlo Scheme
# L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
# colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
# for(i in 1:NROW(params)){
#   r <- as.numeric(unlist(params[i,]))
#   ## replicates
#   res <- parallel:::mclapply(1:B, function(i){
#   x <- rbinom(n = r[1], size = r[2], p = r[3])
#   cvs(x)
#   }, mc.cores = 4)
#   res <- do.call(rbind, res)
#   L[i,] <- colMeans((res-r[4])^2, na.rm = TRUE)
#   if(i %% 5 == 0) cat(i, " ")
#   #  if(i == NROW(params)) cat("\n")
# }
# out <- cbind(params, L)
#
#   # Add the Gamma measures
#   out$CQV7=out$edith/out$old
#   out$CQV8=out$edith8/out$old
#   out$CVMAD=out$MADmedian/out$old
#   out$CQVMnAD=out$MnADmedian/out$old
#
#    save(out, file="Binomial-Sim-Wide.Rdata")
#   load("Binomial-Sim-Wide.Rdata")
#
#   data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
#   data_long$n=as.factor(data_long$n)
#   data_long$m=as.factor(data_long$m)
#   data_long$p=as.factor(format(data_long$p, digits=2)  )
#   data_long$lgamma = log10(data_long$gamma)
#   data_long$ltrue = log10(data_long$true)
#   # remove the infnite values
#   data_long <- data_long[!is.infinite((data_long$gamma)), ]
#
#   save(data_long, file="Binomial-Sim-Long.Rdata")
#
#   Binomial = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
#   save(Binomial, file="Binomial-Table.Rdata")
#
#   # v.1 = plot.CV.1(data=data_long, name.distribution="Binomial")
#   # v.2 = plot.CV.2(data=data_long, name.distribution="Binomial")
#   #
#   load("Binomial-Sim-Long.Rdata")
#   v.3 = plot.CV.3(data=data_long, name.distribution="Binomial")
#   #########################################################################################
#
#
#   #########################################################################################
#   ## ------------------------
#   ##  Uniform
#   # U~U(a,b)
#   # E(U) = 0.5*(a+b) = mu
#   # Var(U) =  1/12 * (b-a)^2 = sigma^2 =>
#   # X~(mu-sigma*sqrt(3), mu+sigma*sqrt(3))
#   # E(X) = mu
#   # VarX = sigma^2
#   ## ------------------------
#
#
#   # function p as mu and s function
#   func.a <- function(m,s) {m - (s*sqrt(3))}
#   func.b <- function(m,s) {m + (s*sqrt(3))}
#
#   # lim inf
#   a <- as.vector(outer(mu, s, Vectorize(func.a)))
#   # lim sup
#   b <- as.vector(outer(mu, s, Vectorize(func.b)))
#   params <- data.frame(a,b)
#   params <- cbind(n=sort(rep(n, length(a))), params)
#
#   # true parameter value CV
#   params$true <- with(params, sqrt((1/12)*(b-a)^2) / (0.5*(a+b)))
#   params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
#   save(params, file="Uniform-params.Rdata")
#
#   # Monte Carlo Scheme
#   L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
#   colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
#   for(i in 1:NROW(params)){
#     r <- as.numeric(unlist(params[i,]))
#     ## replicates
#     res <- parallel:::mclapply(1:B, function(i){
#       x <- runif(r[1], min =r[2] , max=r[3])
#       cvs(x)
#     }, mc.cores = 4)
#     res <- do.call(rbind, res)
#     L[i,] <- colMeans((res-r[4])^2, na.rm = TRUE)
#     if(i %% 5 == 0) cat(i, " ")
#     #  if(i == NROW(params)) cat("\n")
#   }
#   out <- cbind(params, L)
#
#
#   # Add the Gamma measures
#   out$CQV7=out$edith/out$old
#   out$CQV8=out$edith8/out$old
#   out$CVMAD=out$MADmedian/out$old
#   out$CQVMnAD=out$MnADmedian/out$old
#
#   save(out, file="Unifor-Sim-Wide.Rdata")
#
#   load("Unifor-Sim-Wide.Rdata")
#   data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
#   data_long$n=as.factor(data_long$n)
#   # data_long$m=as.factor(data_long$m)
#   # data_long$p=as.factor(format(data_long$p, digits=2)  )
#   data_long$lgamma = log10(data_long$gamma)
#   data_long$ltrue = log10(data_long$true)
#   # remove the infnite values
#   data_long <- data_long[!is.infinite((data_long$gamma)), ]
#
#
#   save(data_long, file="Uniform-Sim-Long.Rdata")
#
#
#   Uniform = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
#   save(Uniform, file="Uniforme-Table.Rdata")
#
#
#   # v.1 = plot.CV.1(data=data_long, name.distribution="Uniform")
#   # v.2 = plot.CV.2(data=data_long, name.distribution="Uniform")
#
#   load("Uniform-Sim-Long.Rdata")
#
#   v.3 = plot.CV.3(data=data_long, name.distribution="Uniform")
#
# #########################################################################################
#
#
#   #########################################################################################
#   ## ------------------------
#   ##  Poisson
#   # E(X) = mu
#   # Var(X) = mu^2
#   ## ------------------------
#
#   # function p as mu and s function
#   func.lambda <- function(m,s) {1/(s/m)^2}
#
#   # lambda as function of mu and sigma
#   lambda <- unique(sort(outer(mu, s, Vectorize(func.lambda))))
#
#   params <- expand.grid(n = n, lambda = lambda)
#   params$true <- with(params, {
#     mu <- lambda
#     s <- sqrt(lambda)
#     s/mu
#   })
#
#   params <- params[(0 <= params$true) & (params$true <= 1),  ]
#   save(params, file="Poisson-params.Rdata")
#
#   L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
#   colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
#     for(i in 1:NROW(params)){
#     r <- as.numeric(unlist(params[i,]))
#     ## replicates
#     res <- parallel:::mclapply(1:B, function(i){
#       x <- rpois(n = r[1], lambda = r[2])
#       cvs(x)
#     }, mc.cores = 4)
#     res <- do.call(rbind, res)
#     L[i, ] <- colMeans((res-r[3])^2, na.rm = TRUE)
#     if(i %% 10 == 0) cat(i, " ")
#     #  if(i == NROW(params)) cat("\n")
#   }
#   out <- cbind(params, L)
#
#   # Add the Gamma measures
#   out$CQV7=out$edith/out$old
#   out$CQV8=out$edith8/out$old
#   out$CVMAD=out$MADmedian/out$old
#   out$CQVMnAD=out$MnADmedian/out$old
#
#   save(out, file="Poisson-Sim-Wide.Rdata")
# load("Poisson-Sim-Wide.Rdata")
#   data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
#   data_long$n=as.factor(data_long$n)
#   # data_long$m=as.factor(data_long$m)
#   # data_long$p=as.factor(format(data_long$p, digits=2)  )
#   data_long$lgamma = log10(data_long$gamma)
#   data_long$ltrue = log10(data_long$true)
#   # remove the infnite values
#   data_long <- data_long[!is.infinite((data_long$gamma)), ]
#
#
#   save(data_long, file="Poisson-Sim-Long.Rdata")
#
#
#   Poisson = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
#   save(Poisson, file="Poisson-Table.Rdata")
#
#
#
#   # v.1 = plot.CV.1(data=data_long, name.distribution="Poisson")
#   # v.2 = plot.CV.2(data=data_long, name.distribution="Poisson")
#   #
#   load("Poisson-Sim-Long.Rdata")
#   v.3 = plot.CV.3(data=data_long, name.distribution="Poisson")
#
#   #########################################################################################
#   ## ------------------------
#   ##  Beta case
#   ## ------------------------
#
#
#   # function q as (mu,s) functions
#   func.qshape <- function(mu,s) {
#     mu.star =  mu #boot::inv.logit(mu) #(m - min(mu))/(max(mu)-min(mu))
#     s2.star = s^2 #boot::inv.logit(s^2)  #(s - min(s))/(max(s)-min(s))
#     p =  mu.star*((mu.star*(1-mu.star)/ s2.star) -1)
#     }
#
#   # function p as (mu, s) functions
#   func.pshape <- function(mu,s) {
#     mu.star =  mu #boot::inv.logit(mu) #(m - min(mu))/(max(mu)-min(mu))
#     s2.star = s^2 #boot::inv.logit(s^2)  #(s - min(s))/(max(s)-min(s))
#     p =  (1-mu.star)*((mu.star*(1-mu.star)/ s2.star) -1)
#   }
#
#   q <- outer(mu, s,  Vectorize(func.qshape))
#   # True value of p
#   q <- unique(sort(q[q>0]))
#
#
#   p <- outer(mu, s,  Vectorize(func.pshape))
#   # True value of p
#   p <- unique(sort(p[p>0]))
#
#   params <- expand.grid(n = n, a = p, b = q)
#
#
#   params$true <- with(params, {
#     mu <- a/(a+b)
#     s <- sqrt(a*b/((a+b)^2*(a+b+1)))
#     true <- s/mu
#   })
#
#   params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
#   save(params, file="Beta-params.Rdata")
#
#   L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
#   colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
#   for(i in 1:NROW(params)){
#     r <- as.numeric(unlist(params[i,]))
#     ## replicates
#     res <- parallel:::mclapply(1:B, function(i){
#       x <- rbeta(n = r[1], shape1 = r[2], shape2 = r[3])
#       cvs(x)
#     }, mc.cores = 4)
#     res <- do.call(rbind, res)
#     L[i,] <- colMeans((res-r[4])^2, na.rm = TRUE)
#     if(i %% 10 == 0) cat(i, " ")
#     #  if(i == NROW(params)) cat("\n")
#   }
#   out <- cbind(params, L)
#
#
#   # Add the Gamma measures
#   out$CQV7=out$edith/out$old
#   out$CQV8=out$edith8/out$old
#   out$CVMAD=out$MADmedian/out$old
#   out$CQVMnAD=out$MnADmedian/out$old
#
#   save(out, file="Beta-Sim-Wide.Rdata")
# load("Beta-Sim-Wide.Rdata")
#   data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
#   data_long$n=as.factor(data_long$n)
#   # data_long$m=as.factor(data_long$m)
#   # data_long$p=as.factor(format(data_long$p, digits=2)  )
#   data_long$lgamma = log10(data_long$gamma)
#   data_long$ltrue = log10(data_long$true)
#   # remove the infnite values
#   data_long <- data_long[!is.infinite((data_long$gamma)), ]
#
#
#   save(data_long, file="Beta-Sim-Long.Rdata")
#
#
#   Beta = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
#   save(Beta, file="Beta-Table.Rdata")
#
#
#
#   # v.1 = plot.CV.1(data=data_long, name.distribution="Beta")
#   # v.2 = plot.CV.2(data=data_long, name.distribution="Beta")
#   #
#
#   load("Beta-Sim-Long.Rdata")
#
#   v.3 = plot.CV.3(data=data_long, name.distribution="Beta")
#
#
#
#   #########################################################################################
#
#
#   #######################################################################################
#   # ------------------------
#   #  Exponential
#   # ------------------------
#   # E(X) = 1/lambda
#   # Var(X) = 1/lambda^2
#   ## ------------------------
#
#   # function lamda as mu and s function - Using the idea of Poisson distribution
#   func.lambda <- function(m,s) {1/(s/m)}
#
#   # lambda as function of mu and sigma
#   lambda <- unique(sort(outer(mu, s, Vectorize(func.lambda))))
#
#   params <- expand.grid(n = n, lambda = lambda)
#   params$true <- with(params, {
#     mu <- lambda
#     s <- lambda
#     s/mu
#   })
#
#   params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
#   save(params, file="Exponentil-params.Rdata")
#
#   L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
#   colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
#   for(i in 1:NROW(params)){
#     r <- as.numeric(unlist(params[i,]))
#     ## replicates
#     res <- parallel:::mclapply(1:B, function(i){
#       x <- rexp(r[1], 1/r[2])
#       cvs(x)
#     }, mc.cores = 4)
#     res <- do.call(rbind, res)
#     L[i,] <- colMeans((res-1)^2, na.rm = TRUE)
#     if(i %% 5 == 0) cat(i, " ")
#     #  if(i == NROW(params)) cat("\n")
#   }
#   out <- cbind(params, L)
#
#   # Add the Gamma measures
#   out$CQV7=out$edith/out$old
#   out$CQV8=out$edith8/out$old
#   out$CVMAD=out$MADmedian/out$old
#   out$CQVMnAD=out$MnADmedian/out$old
#
#   save(out, file="Exponential-Sim-Wide.Rdata")
#
# load("Exponential-Sim-Wide.Rdata")
#   data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
#   data_long$n=as.factor(data_long$n)
#   # data_long$m=as.factor(data_long$m)
#   # data_long$p=as.factor(format(data_long$p, digits=2)  )
#   data_long$lgamma = log10(data_long$gamma)
#   data_long$ltrue = log10(data_long$true)
#   # remove the infnite values
#   data_long <- data_long[!is.infinite((data_long$gamma)), ]
#
#
#   save(data_long, file="Exponential-Sim-Long.Rdata")
#
#
#   Exponencial = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
#   save(Exponencial, file="Exponencial-Table.Rdata")
#
#
#
#   # v.1 = plot.CV.1(data=data_long, name.distribution="Exponential")
#   # v.2 = plot.CV.2(data=data_long, name.distribution="Exponential")
#   #
#   load("Exponential-Sim-Long.Rdata")
#
#   v.3 = plot.CV.3(data=data_long, name.distribution="Exponential")
#
#
#
# #########################################################################################
#
# #########################################################################################
# # ------------------------
# #  Gamma
# # ------------------------
#
#   # function alpha as (mu,s) functions
#   func.alpha <- function(mu,s) {mu^2/s^2}
#
#   # function alpha as (mu,s) functions
#   func.beta <- function(mu,s) {mu/s^2}
#
#
#   #alpha
#   alpha <- outer(mu, s,  Vectorize(func.alpha))
#   # True value of alpha
#   alpha <- unique(sort(alpha[alpha>0]))
#
#   #beta
#   beta <- outer(mu, s,  Vectorize(func.beta))
#
#   # True value of beta
#   beta <- unique(sort(beta[beta>0]))
#
#   params <- expand.grid(n = n, a= alpha, b = beta)
#
#   params$true <- with(params, {
#   mu <- a/b
#   s <- sqrt(a/b^2)
#   true <- s/mu
#   })
#
# params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
# save(params, file="Gamma-params.Rdata")
#
# L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
# colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
# for(i in 1:NROW(params)){
#   r <- as.numeric(unlist(params[i,]))
#   ## replicates
#   res <- parallel:::mclapply(1:B, function(i){
#     x <- rgamma(n = r[1], shape = r[2], rate = r[3])
#     cvs(x)
#   }, mc.cores = 4)
#   res <- do.call(rbind, res)
#   L[i,] <- colMeans((res-r[4])^2, na.rm = TRUE)
#   if(i %% 5 == 0) cat(i, " ")
#   #  if(i == NROW(params)) cat("\n")
# }
# out <- cbind(params, L)
#
#
# # Add the Gamma measures
# out$CQV7=out$edith/out$old
# out$CQV8=out$edith8/out$old
# out$CVMAD=out$MADmedian/out$old
# out$CQVMnAD=out$MnADmedian/out$old
#
# save(out, file="Gamma-Sim-Wide.Rdata")
# load("Gamma-Sim-Wide.Rdata")
# data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
# data_long$n=as.factor(data_long$n)
# # data_long$m=as.factor(data_long$m)
# # data_long$p=as.factor(format(data_long$p, digits=2)  )
# data_long$lgamma = log10(data_long$gamma)
# data_long$ltrue = log10(data_long$true)
# # remove the infnite values
# data_long <- data_long[!is.infinite((data_long$gamma)), ]
#
# save(data_long, file="Gamma-Sim-Long.Rdata")
#
# Gamma = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
# save(Gamma, file="Gamma-Table.Rdata")
#
# # v.1 = plot.CV.1(data=data_long, name.distribution="Gamma")
# # v.2 = plot.CV.2(data=data_long, name.distribution="Gamma")
#
# load("Gamma-Sim-Long.Rdata")
# v.3 = plot.CV.3(data=data_long, name.distribution="Gamma")
#
#
# #########################################################################################

# #########################################################################################
# # ------------------------
# #  Chi^2 case
# # ------------------------
#
# # function nu as mu and s function
# func.nu <- function(m,s) {floor(2/(s/m)^2)}
#
# nu <- outer(mu, s,  Vectorize(func.nu))
# #   # True value of p
#    nu <- unique(sort(nu[nu>0]))
# #
# # # lambda as function of mu and sigma
# # nu <- unique(sort(outer(mu, s, Vectorize(func.nu))))
#
# params <- expand.grid(n = n, nu = nu)
# params$true <- with(params, {
#   mu <- nu
#   s <- sqrt(2*nu)
#   s/mu
# })
#
# params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
# save(params, file="Chisquare-params.Rdata")
#
# L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
# colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
# for(i in 1:NROW(params)){
#   r <- as.numeric(unlist(params[i,]))
#   ## replicates
#   res <- parallel:::mclapply(1:B, function(i){
#   x <- rchisq(n = r[1], df = r[2])
#   cvs(x)
#   }, mc.cores = 4)
#   res <- do.call(rbind, res)
#   L[i,] <- colMeans((res-r[3])^2, na.rm = TRUE)
#   if(i %% 5 == 0) cat(i, " ")
#   #  if(i == NROW(params)) cat("\n")
#   }
# out <- cbind(params, L)
#
# # Add the Gamma measures
# out$CQV7=out$edith/out$old
# out$CQV8=out$edith8/out$old
# out$CVMAD=out$MADmedian/out$old
# out$CQVMnAD=out$MnADmedian/out$old
#
# save(out, file="Chisquare-Sim-Wide.Rdata")
# load("Chisquare-Sim-Wide.Rdata")
#
# data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
# data_long$n=as.factor(data_long$n)
# # data_long$m=as.factor(data_long$m)
# # data_long$p=as.factor(format(data_long$p, digits=2)  )
# data_long$lgamma = log10(data_long$gamma)
# data_long$ltrue = log10(data_long$true)
# # remove the infnite values
# data_long <- data_long[!is.infinite((data_long$gamma)), ]
#
#
# save(data_long, file="Chisquare-Sim-Long.Rdata")
#
# Chisquare = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
# save(Chisquare, file="Chisquare-Table.Rdata")
#
# # v.1 = plot.CV.1(data=data_long, name.distribution="Chisquare")
# # v.2 = plot.CV.2(data=data_long, name.distribution="Chisquare")
#
# load("Chisquare-Sim-Long.Rdata")
#
# v.3 = plot.CV.3(data=data_long, name.distribution="Chisquare")

# #########################################################################################
# # ------------------------------
# #     Ex-Gaussian  --- see Ueda's and Box-Cox's paper
# # ------------------------------
#
# # shape parameter
# nu <- c(0.7, 7, 14)
#
# # function alpha as (mu,s) functions
# func.mus <- function(mu,v) {mu-v}
#
# # function alpha as (mu,s) functions s^2 > v^2
# func.ss <- function(s,v) {s^2-v^2}
#
# #mus
# mu.s <- outer(mu, nu,  Vectorize(func.mus))
#
# # mus
# mu.s<- unique(sort(mu.s)) #[alpha>0]))
#
# #sigma
# sigma.s <- outer(s, nu,  Vectorize(func.ss))
#
# # True value of beta
# sigma.s <- unique(sort(sigma.s[sigma.s>0]))
#
# sigma.s <- sqrt(sigma.s)
#
#
# params <- expand.grid(n = n, mu = mu.s, sigma = sigma.s, nu = nu)
#
# params$true <- with(params, {
#   media <- mu + nu
#   s <- sqrt(sigma^2 + nu^2)
#   s/media
# })
#
# params <- params[(0 <= params$true) & (params$true <= 1),  ]
#
# save(params, file="ExGaussian-params.Rdata")
#
# L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
# colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
# for(i in 1:NROW(params)){
#   r <- as.numeric(unlist(params[i,]))
#   ## replicates
#   res <- parallel:::mclapply(1:B, function(i){
#     x <- rexGAUS(n = r[1], mu = r[2], sigma = r[3], nu = r[4])
#     cvs(x)
#   }, mc.cores = 4)
#   res <- do.call(rbind, res)
#   L[i,] <- colMeans((res-r[5])^2, na.rm = TRUE)
#   if(i %% 5 == 0) cat(i, " ")
#   #  if(i == NROW(params)) cat("\n")
# }
# out <- cbind(params, L)
#
# # Add the Gamma measures
# out$CQV7=out$edith/out$old
# out$CQV8=out$edith8/out$old
# out$CVMAD=out$MADmedian/out$old
# out$CQVMnAD=out$MnADmedian/out$old
#
# save(out, file="ExGaussian-Sim-Wide.Rdata")
# load("ExGaussian-Sim-Wide.Rdata")
# data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
# data_long$n=as.factor(data_long$n)
# # data_long$m=as.factor(data_long$m)
# # data_long$p=as.factor(format(data_long$p, digits=2)  )
# data_long$lgamma = log10(data_long$gamma)
# data_long$ltrue = log10(data_long$true)
# # remove the infnite values
# data_long <- data_long[!is.infinite((data_long$gamma)), ]
#
#
# save(data_long, file="ExGaussian-Sim-Long.Rdata")
#
# ExGaussian = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
# save(ExGaussian, file="ExGaussian-Table.Rdata")
#
# v.1 = plot.CV.1(data=data_long, name.distribution="ExGaussian")
# v.2 = plot.CV.2(data=data_long, name.distribution="ExGaussian")
#
# load("ExGaussian-Sim-Long.Rdata")
#
# v.3 = plot.CV.3(data=data_long, name.distribution="ExGaussian")

# proc.time() - ptm
#
# print(ptm)
#########################################################################################

# load("Normal-Table.Rdata")
# load("Normal.5-Table.Rdata")
# load("Normal.10-Table.Rdata")
# load("Normal.15-Table.Rdata")
# load("Normal.20-Table.Rdata")
# load("Uniforme-Table.Rdata")
# 
# load("Binomial-Table.Rdata")
# 
# load("Poisson-Table.Rdata")
# load("Beta-Table.Rdata")
# load("Gamma-Table.Rdata")
# load("Exponencial-Table.Rdata")
# load("Chisquare-Table.Rdata")
# load("ShiftExpo-Table.Rdata")
# load("ExGaussian-Table.Rdata")
# load("LogNormal-Table.Rdata")
#
#
# #---------------------------------------------------------------------------
#
#
# #
# # Place supergroups, more_bands, and more_artists into a list
# tt <- as.data.frame(list(Normal, Normal.5, Normal.10, Normal.15, Normal.20) %>%
#   reduce(left_join, by = c("n" = "n","Estimator" = "Estimator")))
#
# sink(file="tabela-Normal.txt")
# knitr::kable(
#   tt, caption = 'Normal', format="latex", digits=2,
#   booktabs = TRUE
# )
# sink()
# #---------------------------------------------------------------------------
#
# #---------------------------------------------------------------------------
# # Place supergroups, more_bands, and more_artists into a list
# tt.1 <- list(Normal, Binomial, Uniform, Poisson, Beta,
#            Exponencial, Gamma,  Chisquare, ShiftExponential, ExGaussian, Lognormal) %>%
#   reduce(left_join, by = c("n" = "n","Estimator" = "Estimator"))
# 
# sink(file="tabela-Distribuicoes.txt")
# knitr::kable(
#   tt.1, caption = 'Distributions', format="latex", digits=2,
#   booktabs = TRUE
# )
# sink()
# #---------------------------------------------------------------------------

#---------------------------------------------------------------------------

# Data long

#load("Normal-Sim-Long.Rdata")
# load("Normal.5-Sim-Long.Rdata")
# load("Normal.10-Sim-Long.Rdata")
# load("Normal.15-Sim-Long.Rdata")
# load("Normal.20-Sim-Long.Rdata")
# load("Uniform-Sim-Long.Rdata")
# load("Binomial-Sim-Long.Rdata")
# load("Poisson-Sim-Long.Rdata")
# load("Beta-Sim-Long.Rdata")
# load("Gamma-Sim-Long.Rdata")
# load("Exponential-Sim-Long.Rdata")
# load("Chisquare-Sim-Long.Rdata")
# load("ShiftExpo-Sim-Long.Rdata")
# load("ExGaussian-Sim-Long.Rdata")
# load("LogNormal-Sim-Long.Rdata")




# #### Load this lines for each distribution above - summary of log10(gamma)
# Carefull with the index of covariates
## library(tidyverse)
## library(data.table)

# DT <- data.table(data_long)
# DT$lgamma <- DT$lgamma
# 
# # Min Values
# DT[DT[, .I[which.min(lgamma)], by=Estimator]$V1][, c(1,2,3, 4, 10, 11,12)]
# 
# # Max Values
# DT[DT[, .I[which.max(lgamma)], by=Estimator]$V1][, c(1,2,3, 4, 10, 11,12)]
#   #---------------------------------------------------------------------------
# 
# 
# # # eficient
# #
# # ef = DT[DT[, .I[which(lgamma<0.4)], by=Estimator]$V1][, c(1,2,3, 4, 10, 11,12)]
# #
# # # a = data_long %>% group_by(n, Estimator) %>% summarise(med=median(lgamma))
# #
#  b = data_long %>% group_by(Estimator, n) %>% summarise(med=median(lgamma))
# #
#  s = spread(b, Estimator, med)
# #
#  knitr::kable(
#    s, caption = 'Distributions', format="latex", digits=2,
#    booktabs = TRUE
#  )
# #
