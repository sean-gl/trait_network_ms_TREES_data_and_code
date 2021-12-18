            
#___ USE FOR 12_2021 RUNS ___#

###_____ summary figures ______###
###_____ summary figures ______###
###_____ summary figures ______###

# rm(list=ls())
library(doBy)

setwd("/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/data") 

# turn on the below lines to use 11_3_2020 runs (vpmax = 40 or 60)
# df0 <- read.csv("cleaned_mo_wet_11_3_2020.csv") 
# df1 <- read.csv("cleaned_mo_dry_11_3_2020.csv") 
# df2 <- read.csv("cleaned_yu_wet_11_3_2020.csv") 
# df3 <- read.csv("cleaned_yu_dry_11_3_2020.csv") 
# df4 <- read.csv("cleaned_irrigated_11_3_2020.csv") 

# turn on below lines to use 12_2021 runs (vpmax = 90 or 130)
# df0 <- read.csv("cleaned_mo_wet_12_2021.csv") 
# df1 <- read.csv("cleaned_mo_dry_12_2021.csv") 
# df2 <- read.csv("cleaned_yu_wet_12_2021.csv") 
# df3 <- read.csv("cleaned_yu_dry_12_2021.csv") 
# df4 <- read.csv("cleaned_irrigated_12_2021.csv") 

# turn on below lines to use 12_2021_v2 runs (vpmax = 60 or 120)
df0 <- read.csv("cleaned_mo_wet_12_2021_v2.csv") 
df1 <- read.csv("cleaned_mo_dry_12_2021_v2.csv") 
df2 <- read.csv("cleaned_yu_wet_12_2021_v2.csv") 
df3 <- read.csv("cleaned_yu_dry_12_2021_v2.csv") 
df4 <- read.csv("cleaned_irrigated_12_2021_v2.csv") 


# check max values
max(df0$NPP_sum); max(df1$NPP_sum); max(df2$NPP_sum); max(df3$NPP_sum); max(df4$NPP_sum)
max(df0$cum_trans_season); max(df1$cum_trans_season); max(df2$cum_trans_season); max(df3$cum_trans_season); max(df4$cum_trans_season)
max(df0$repro_sum); max(df1$repro_sum); max(df2$repro_sum); max(df3$repro_sum); max(df4$repro_sum)
max(df0$precip_sum); max(df1$precip_sum); max(df2$precip_sum); max(df3$precip_sum); max(df4$precip_sum)
max(df0$simET_sum); max(df1$simET_sum); max(df2$simET_sum); max(df3$simET_sum); max(df4$simET_sum)

# check min NPP
min(df0$NPP_sum); min(df1$NPP_sum); min(df2$NPP_sum); min(df3$NPP_sum); min(df4$NPP_sum)


# remove bad NPP_sum data... spikes to 10e+95 on one run
df0$NPP_sum <- ifelse(df0$NPP_sum > 500000, 0, df0$NPP_sum)
df1$NPP_sum <- ifelse(df1$NPP_sum > 500000, 0, df1$NPP_sum)
df2$NPP_sum <- ifelse(df2$NPP_sum > 500000, 0, df2$NPP_sum)
df3$NPP_sum <- ifelse(df3$NPP_sum > 500000, 0, df3$NPP_sum)
df4$NPP_sum <- ifelse(df4$NPP_sum > 500000, 0, df4$NPP_sum)

# combine all dfs
comb <- rbind.data.frame(df0, df1, df2, df3, df4)

# change la_ra name to max_LAI
colnames(comb)[colnames(comb)=="la_ra"] <- "max_LAI"
# change max_LAI contrasts to "high" and "low"
comb$max_LAI <- ifelse(comb$max_LAI=="narrow", "low", "high")
comb$max_LAI <- as.factor(comb$max_LAI) # change to factor

# check runs
crap <- summaryBy(NPP_sum ~ simulation, data=comb, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI', 'vpmax',
                      'soil_water', 'soil_texture', 'climate'))
head(crap[order(crap$NPP_sum),])
crap[order(crap$NPP_sum),]

# checking treatment/trait effects
# nrow(crap)
# eff_high <- subset(crap, eff=="high"); nrow(eff_high)
# eff_low <- subset(crap, eff=="low"); nrow(eff_low)
# mean(eff_high$NPP_sum)
# mean(eff_low$NPP_sum)

# remove infinite theta values
comb$theta_all <- (comb$theta1.max + comb$theta2.max + comb$theta3.max + comb$theta4.max)/4
max(comb$theta_all); min(comb$theta_all)
comb$theta_all <- ifelse(comb$theta_all > 0.5, 0.5, comb$theta_all)
comb$theta_all <- ifelse(comb$theta_all < 0, 0, comb$theta_all)

# calculating mean stats
# cp <- subset(comb, climate == "mo_wet"); nrow(cp)
# hp <- subset(comb, climate == "yu_wet"); nrow(hp)
# (mean(cp$NPP_sum) - mean(hp$NPP_sum))/mean(cp$NPP_sum)
# (mean(cp$NPP.max) - mean(hp$NPP.max))/mean(cp$NPP.max)
# crap1 <- subset(comb, climate=="mo_dry" & max_LAI=="high" & root_depth=="deep" & gs_sensitivity=="cons" & day < 203); nrow(crap1)
# crap2 <- subset(comb, climate=="mo_dry" & max_LAI=="low" & root_depth=="deep" & gs_sensitivity=="cons" & day < 203); nrow(crap2)
# (mean(crap1$NPP.sum) - mean(crap2$NPP.sum)) / mean(crap2$NPP.sum)
 
crap <- subset(comb, climate=="yu_dry")
crap <- summaryBy(repro_sum ~ simulation, data=crap, FUN=c(mean, length), keep.names=TRUE, 
                  id=c('climate', 'gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI', 
                       'vpmax', 'soil_water', 'soil_texture'))
crap2 <- crap[order(crap$repro_sum.mean, decreasing=TRUE),]
crap2[1:30,]


# weird irrigated simulation... find out what is going on here...
crap3 <- subset(crap2, gs_sensitivity=="cons" & eff=="high" & saf=="high" & root_depth=="shallow" &
                  max_LAI=="low" & vpmax=="high" & soil_water=="not_full" & soil_texture=="fine")

crap4 <- subset(crap2, gs_sensitivity=="risky" & eff=="high" & saf=="high" & root_depth=="shallow" &
                  max_LAI=="low" & vpmax=="high" & soil_water=="not_full" & soil_texture=="fine")
crap3;crap4
#



###_________________ SEASONAL TRAIT PLOTS _____________________### 
###_________________ SEASONAL TRAIT PLOTSS _____________________### 

#_______ plotting NPP and Reproductive output____###

plot_npp_repro <- function(dataframe_used, treatment, level1_string, level2_string, title_string) { 
  pa <- substitute(treatment)
  # goofing with different plots
  # contrast  
  # modify simulation column to remove number corresponding to the selected "treatment"
  crap <- dataframe_used
  # crap <- subset(comb, climate=="mo_wet")                                    # TURN OFF WHEN RUNNING LOOP
  # remove all "_" from simulations
  crap2 <- gsub("_", "", crap$simulation) 
  # remove number in crap2 that corresponds to the selected treatment  
  # create test vectors that can be fed to "ifelse" (need to be same length as desired ifelse output)
  crap3 <-  c(1:length(crap2)); crap3[] <- "hold"
  treat2 <- c(1:length(crap2)); treat2[] <- deparse(substitute(treatment))    
  # treat2 <- c(1:length(crap2)); treat2[] <- "eff"       # TURN OFF WHEN RUNNING LOOP
  
  # use ifelse to create a treatment vector without the desired treatment  
  crap3 <- ifelse(crap3=="hold" & treat2=="gs_sensitivity", paste(substr(crap2, 1, 1), substr(crap2, 3, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="eff", paste(substr(crap2, 1, 2), substr(crap2, 4, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="saf", paste(substr(crap2, 1, 3), substr(crap2, 5, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="refill", paste(substr(crap2, 1, 4), substr(crap2, 6, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="root_depth", paste(substr(crap2, 1, 4), substr(crap2, 6, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="max_LAI", paste(substr(crap2, 1, 5), substr(crap2, 7, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="vpmax", paste(substr(crap2, 1, 6), substr(crap2, 8, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="up_soil_water", paste(substr(crap2, 1, 8), substr(crap2, 10, 13), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="mature_rate", paste(substr(crap2, 1, 9), substr(crap2, 11, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="soil_water", paste(substr(crap2, 1, 7), substr(crap2, 9, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="weeds", paste(substr(crap2, 1, 11), substr(crap2, 13, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="soil_texture", substr(crap2, 1, 8), crap3)
  crap$compare_treat <- crap3
  # split crap into contrast1 and contast2
  contrast1 <- subset(crap, eval(pa)==level1_string)
  contrast2 <- subset(crap, eval(pa)==level2_string)
  #  contrast1 <- subset(crap, eff=="low")
  #  contrast2 <- subset(crap, eff=="high")
  
  # merge contrast1 and contrast2
  merged <- merge(contrast1, contrast2, by=c("compare_treat", "day"))
  
  ### USE THIS TO SEE IF A PARTICULAR TREATMENT FAILED WHEN IT SHOULDN'T HAVE
  # crap <- summaryBy(NPP_sum.x + NPP_sum.y ~ compare_treat, data=merged, 
  #                  id=c("gs_sensitivity.x", "eff.x", "saf.x", "root_depth.x", "max_LAI.x",
  #                       "vpmax.x", "soil_water.x", "soil_texture.x"))
  # crap[order(crap$NPP_sum.x.mean),]
  
  # CHANGE DESIRED MAX NPP VALUE HERE
  merged <- subset(merged, NPP.max.x > 0 & NPP.max.y > 0)
  
  # NPP units.  Raw data are in umol/m2/s.  
  # but in the cleaned data, NPP.sum is the summed umol/m2/s for each 30-min time interval 
  # for each day for each simulation... and then takes the mean of this value across each 5-day chunk  
  # Thus, to get the total mean umol/m2 for each day, we need to first divide by 48, which is the
  # number of 30-min timer intervals in each day.  This gives the mean umol/m2/s for each day. 
  # we then need to multiply this by the total number of seconds in each day (86400).
  # to convert to mol (rather than umol), divide by 1000000.
  # to convert to g (rather than mol), multiply by 12.011 (1 mol C = 12.011 g)
  # remember that this gives the mean g/m2 for each 5-day chunk (which is what we want to plot)
  merged$NPP.sum.x <- merged$NPP.sum.x / 48 * 86400 / 1000000 * 12.011  # gives mean g/m2 for each 5-day chunk
  merged$NPP.sum.y <- merged$NPP.sum.y / 48 * 86400 / 1000000 * 12.011
  
  # units for reproduction are in Kg C / ha, but are cummulative, thus we don't
  # want to sum them, but rather just put in units of g/m2 and plot it as is.
  # to convert Kg / ha to Kg / m2, we need to divide by 10,000 (1ha = 10,000m2)
  # then to convert kg to g, we need to muliply by 1000
  merged$reproduction.max.x <- merged$reproduction.max.x / 10000 * 1000 # gives **cumulative** g/m2
  merged$reproduction.max.y <- merged$reproduction.max.y / 10000 * 1000
  
  max(merged$NPP.sum.x, merged$NPP.sum.y)
  max(merged$reproduction.max.x, merged$reproduction.max.y)
  
  # plot 
  # TURN OFF "TYPE="n"" TO PLOT ALL POINTS
  plot(merged$NPP.sum.x ~ merged$day, col="lightgreen", cex=0.5,
       xlim=c(150,313), ylim=c(0,40), 
       ylab=expression("NPP (" * g * " " * m^-2 * " " * day^-1 * ")"), 
       xlab="Day", type="n")
  points(merged$NPP.sum.y ~ merged$day, col="darkgreen", cex=0.2, type="n")
  
  # create day summary
  # contrast1
  sum1 <- summaryBy(NPP.sum.x + reproduction.max.x ~ day, data=merged, FUN=c(mean,sd), na.rm=TRUE)
  sum1$scale_sd <- scale(sum1$NPP.sum.x.sd, center=FALSE)
  points(sum1[,1], sum1[,2], pch=21, bg=rgb(red = 0, green = 1, blue = 0, alpha = 0.5), 
         cex=(sum1$scale_sd*2), type='b') # lightgreen
  
  # contrast2
  sum2 <- summaryBy(NPP.sum.y + reproduction.max.y ~ day, data=merged, FUN=c(mean,sd), na.rm=TRUE)
  sum2$scale_sd <- scale(sum2$NPP.sum.y.sd, center=FALSE)
  points(sum2[,1], sum2[,2], pch=21, bg=rgb(red = 0, green = 0.3, blue = 0, alpha = 0.5), 
         cex=(sum2$scale_sd*2), type='b') # darkgreen
  
  # add soilwater
  # water <- summaryBy(theta_all.x ~ day, data=merged, FUN=mean, na.rm=TRUE)
  # points((water$theta_all.x.mean*4000) ~ water$day, col="cyan", type="l")
   
  # water <- summaryBy(theta_all.y ~ day, data=merged, FUN=mean, na.rm=TRUE)
  # points((water$theta_all.y.mean*4000) ~ water$day, col="blue", type="l")
   
  # add another trait/variable 
  # other <- summaryBy(ar2.sum.x ~ day, data=merged, FUN=mean, na.rm=TRUE)
  # points((other$ar2.sum.x.mean*200) ~ other$day, col="chartreuse", type="l", lwd=3)
  
  # plotting reproduction, contrast_1
  par(new=TRUE)
  plot(sum1$reproduction.max.x.mean ~ sum1$day, col="goldenrod1", lwd=2, type="l",
       axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0, 3500))  
  axis(side=4) #, at=c(0,200,400,600,800))
  mtext(expression("Cumulative reproduction (" * g * " " * m^-2 * ")"), side=4, line=2, cex=0.7)

  # plotting reproduction, contrast_2
  par(new=TRUE)
  plot(sum2$reproduction.max.y.mean ~ sum2$day, col="goldenrod3", lwd=2, type="l",
       axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0, 3500))  

  # we want total seasonal integrated NPP for the WUE calculation.  To get this, 
  # use the NPP_sum (not NPP.sum) column.  This is the NPP summed across all time steps
  # and all days for each simulation. 
  # because the original units were umol/m2/s (no conversion were done before summing),
  # we need to multiply the sum by 1800 (number of seconds in each time step) 
  # to get umol/m2 for entire season (see journal notes 12/8/2020)
  # to convert to mol (rather than umol), divide by 1000000
  # to convert to g (rather than mol), multiply by 12.011 (1 mol C = 12.011 g)
  crap <- summaryBy(NPP_sum.x + NPP_sum.y ~ merged$simulation.x, data=merged, FUN=mean, na.rm=TRUE)
  npp_sum1 <- mean(crap$NPP_sum.x.mean * 1800 / 1000000 * 12.011)
  npp_sum2 <- mean(crap$NPP_sum.y.mean * 1800 / 1000000 * 12.011)
  npp_sum1 <- round(npp_sum1,0); npp_sum1
  npp_sum2 <- round(npp_sum2,0); npp_sum2
  
  # calculate the fraction of precipitation to pass through stomata
  # note that cum_trans_season and precip are both in units of mm per unit ground area
  # also note that cum_trans_season has already been summed across all time steps
  # and all days for each iteration (it is not a 5-day chunk mean)
  level1_TF <- round(mean(merged$cum_trans_season.x)/mean(merged$precip_sum.x), 2)
  level2_TF <- round(mean(merged$cum_trans_season.y)/mean(merged$precip_sum.y), 2)
  
  # gives WUE in units of g/m2 NPP per mm/m2 of precipitation 
  level1_WUE <- round(npp_sum1/mean(merged$cum_trans_season.x), 2); level1_WUE
  level2_WUE <- round(npp_sum2/mean(merged$cum_trans_season.y), 2); level2_WUE
  
  # for total reproduction, use repro_max.  This is the absolute maximum reproduction value
  # for each entire simulation.  This is what we want for the legend.  It is in units of
  # kg C / ha.  All we need to do is covert it to units of g/m2.  
  # To convert Kg / ha to Kg / m2, we need to divide by 10,000 (1ha = 10,000m2)
  # then to convert kg to g, we need to muliply by 1000
  crap <- summaryBy(repro_max.x + repro_max.y ~ merged$simulation.x, data=merged, FUN=mean, na.rm=TRUE)
  level1_repro <- mean(crap$repro_max.x.mean, na.rm=TRUE) / 10000 * 1000; level1_repro
  level2_repro <- mean(crap$repro_max.y.mean, na.rm=TRUE) / 10000 * 1000; level2_repro
  level1_repro <- round(level1_repro, 0); level1_repro
  level2_repro <- round(level2_repro, 0); level2_repro
  
  # legend
  note1 <- paste0(substitute(level1_string), "*", "' '", "*", substitute(treatment), "*", "': NPP = '",
                  "*", npp_sum1, "*", "' g '", "*", "m^-2")
  note2 <- paste0(substitute(level2_string), "*", "' '", "*", substitute(treatment), "*", "': NPP = '",
                  "*", npp_sum2, "*", "' g '", "*", "m^-2")
  note3 <- paste0(substitute(level1_string), "*", "' '", "*", substitute(treatment), "*", "': repro = '",
                  "*", level1_repro, "*", "' g '", "*", "m^-2")
  note4 <- paste0(substitute(level2_string), "*", "' '", "*", substitute(treatment), "*", "': repro = '",
                  "*", level2_repro, "*", "' g '", "*", "m^-2")
  
  legend("topright", legend=str2lang(note1), pch=21, pt.bg="lightgreen", 
         col="black", cex=0.8, bty="n", inset=c(0.01, 0), xjust=0, text.width=70)
  legend("topright", legend=str2lang(note2), pch=21, pt.bg="darkgreen", 
         col="black", cex=0.8, bty="n", inset=c(0.01, 0.05), xjust=0, text.width=70)
  legend("topright", legend=str2lang(note3), lty=1, 
         col="goldenrod1", cex=0.8, bty="n", inset=c(0.01, 0.1), xjust=0, text.width=70)
  legend("topright", legend=str2lang(note4), lty=1, 
         col="goldenrod3", cex=0.8, bty="n", inset=c(0.01, 0.15), xjust=0, text.width=70)
  
 # legend("topright", legend=str2lang(note1), lty=NA, pch=21, pt.bg="lightgreen", col="black", 
#         cex=0.8, )
  
  
  
#  legend("topright", legend=str2lang(note1, note2), # str2lang(note2))), 
#                     lty=c(NA, NA, 1, 1),
#                     pch=c(21, 21, NA, NA),
#                     pt.bg = c("lightgreen", "darkgreen", NA, NA),
#                     col = c("black", "black", "goldenrod1", "goldenrod3"),
#                     cex=0.8)

  title(title_string, line=0.5)
}



#_______ plotting NPP and Reproductive output **for ms figure**____###

plot_npp_repro_fig <- function(dataframe_used, treatment, level1_string, level2_string, title_string) { 
  pa <- substitute(treatment)
  # goofing with different plots
  # contrast  
  # modify simulation column to remove number corresponding to the selected "treatment"
  crap <- dataframe_used
  # crap <- subset(comb, climate=="mo_wet")                                    # TURN OFF WHEN RUNNING LOOP
  # remove all "_" from simulations
  crap2 <- gsub("_", "", crap$simulation) 
  # remove number in crap2 that corresponds to the selected treatment  
  # create test vectors that can be fed to "ifelse" (need to be same length as desired ifelse output)
  crap3 <-  c(1:length(crap2)); crap3[] <- "hold"
  treat2 <- c(1:length(crap2)); treat2[] <- deparse(substitute(treatment))    
  # treat2 <- c(1:length(crap2)); treat2[] <- "eff"       # TURN OFF WHEN RUNNING LOOP
  
  # use ifelse to create a treatment vector without the desired treatment  
  crap3 <- ifelse(crap3=="hold" & treat2=="gs_sensitivity", paste(substr(crap2, 1, 1), substr(crap2, 3, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="eff", paste(substr(crap2, 1, 2), substr(crap2, 4, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="saf", paste(substr(crap2, 1, 3), substr(crap2, 5, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="refill", paste(substr(crap2, 1, 4), substr(crap2, 6, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="root_depth", paste(substr(crap2, 1, 4), substr(crap2, 6, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="max_LAI", paste(substr(crap2, 1, 5), substr(crap2, 7, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="vpmax", paste(substr(crap2, 1, 6), substr(crap2, 8, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="up_soil_water", paste(substr(crap2, 1, 8), substr(crap2, 10, 13), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="mature_rate", paste(substr(crap2, 1, 9), substr(crap2, 11, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="soil_water", paste(substr(crap2, 1, 7), substr(crap2, 9, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="weeds", paste(substr(crap2, 1, 11), substr(crap2, 13, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="soil_texture", substr(crap2, 1, 8), crap3)
  crap$compare_treat <- crap3
  # split crap into contrast1 and contast2
  contrast1 <- subset(crap, eval(pa)==level1_string)
  contrast2 <- subset(crap, eval(pa)==level2_string)
  #  contrast1 <- subset(crap, eff=="low")
  #  contrast2 <- subset(crap, eff=="high")
  
  # merge contrast1 and contrast2
  merged <- merge(contrast1, contrast2, by=c("compare_treat", "day"))
  
  # CHANGE DESIRED MAX NPP VALUE HERE
  merged <- subset(merged, NPP.max.x > 0 & NPP.max.y > 0)
  
  merged$NPP.sum.x <- merged$NPP.sum.x / 48 * 86400 / 1000000 * 12.011  # gives mean g/m2 for each 5-day chunk
  merged$NPP.sum.y <- merged$NPP.sum.y / 48 * 86400 / 1000000 * 12.011
  merged$reproduction.max.x <- merged$reproduction.max.x / 10000 * 1000 # gives **cumulative** g/m2
  merged$reproduction.max.y <- merged$reproduction.max.y / 10000 * 1000
  max(merged$NPP.sum.x, merged$NPP.sum.y)
  max(merged$reproduction.max.x, merged$reproduction.max.y)
  
  # plot 
  # TURN OFF "TYPE="n"" TO PLOT ALL POINTS
  plot(merged$NPP.sum.x ~ merged$day, col="lightgreen", cex=0.5,
       xlim=c(150,313), ylim=c(0,40), 
       ylab=expression("NPP (" * g * " " * m^-2 * " " * day^-1 * ")"), 
       xlab="Day", type="n", cex.lab=1.4)
  points(merged$NPP.sum.y ~ merged$day, col="darkgreen", cex=0.2, type="n")
  
  # create day summary
  # contrast1
  sum1 <- summaryBy(NPP.sum.x + reproduction.max.x ~ day, data=merged, FUN=c(mean,sd), na.rm=TRUE)
  sum1$scale_sd <- scale(sum1$NPP.sum.x.sd, center=FALSE)
  points(sum1[,1], sum1[,2], pch=21, bg=rgb(red = 0, green = 1, blue = 0, alpha = 0.5), 
         cex=(sum1$scale_sd*2), type='b') # lightgreen
  
  # contrast2
  sum2 <- summaryBy(NPP.sum.y + reproduction.max.y ~ day, data=merged, FUN=c(mean,sd), na.rm=TRUE)
  sum2$scale_sd <- scale(sum2$NPP.sum.y.sd, center=FALSE)
  points(sum2[,1], sum2[,2], pch=21, bg=rgb(red = 0, green = 0.3, blue = 0, alpha = 0.5), 
         cex=(sum2$scale_sd*2), type='b') # darkgreen
  
  # plotting reproduction, contrast_1
  par(new=TRUE)
  plot(sum1$reproduction.max.x.mean ~ sum1$day, col="goldenrod1", lwd=2, type="l",
       axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0, 3500))  
  axis(side=4) #, at=c(0,200,400,600,800))
  # mtext(expression("Cumulative reproduction (" * g * " " * m^-2 * ")"), side=4, line=2, cex=0.7)
  
  # plotting reproduction, contrast_2
  par(new=TRUE)
  plot(sum2$reproduction.max.y.mean ~ sum2$day, col="goldenrod3", lwd=2, type="l",
       axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0, 3500))  
  
  crap <- summaryBy(NPP_sum.x + NPP_sum.y ~ merged$simulation.x, data=merged, FUN=mean, na.rm=TRUE)
  npp_sum1 <- mean(crap$NPP_sum.x.mean * 1800 / 1000000 * 12.011)
  npp_sum2 <- mean(crap$NPP_sum.y.mean * 1800 / 1000000 * 12.011)
  npp_sum1 <- round(npp_sum1,0); npp_sum1
  npp_sum2 <- round(npp_sum2,0); npp_sum2
  
  level1_TF <- round(mean(merged$cum_trans_season.x)/mean(merged$precip_sum.x), 2)
  level2_TF <- round(mean(merged$cum_trans_season.y)/mean(merged$precip_sum.y), 2)
  level1_WUE <- round(npp_sum1/mean(merged$cum_trans_season.x), 2); level1_WUE
  level2_WUE <- round(npp_sum2/mean(merged$cum_trans_season.y), 2); level2_WUE
  
  crap <- summaryBy(repro_max.x + repro_max.y ~ merged$simulation.x, data=merged, FUN=mean, na.rm=TRUE)
  level1_repro <- mean(crap$repro_max.x.mean, na.rm=TRUE) / 10000 * 1000; level1_repro
  level2_repro <- mean(crap$repro_max.y.mean, na.rm=TRUE) / 10000 * 1000; level2_repro
  level1_repro <- round(level1_repro, 0); level1_repro
  level2_repro <- round(level2_repro, 0); level2_repro
  
  # legend
  note1 <- paste0(substitute(level1_string), "*", "' '", "*", substitute(treatment), "*", "': NPP = '",
                  "*", npp_sum1, "*", "' g '", "*", "m^-2")
  note2 <- paste0(substitute(level2_string), "*", "' '", "*", substitute(treatment), "*", "': NPP = '",
                  "*", npp_sum2, "*", "' g '", "*", "m^-2")
  note3 <- paste0(substitute(level1_string), "*", "' '", "*", substitute(treatment), "*", "': repro = '",
                  "*", level1_repro, "*", "' g '", "*", "m^-2")
  note4 <- paste0(substitute(level2_string), "*", "' '", "*", substitute(treatment), "*", "': repro = '",
                  "*", level2_repro, "*", "' g '", "*", "m^-2")
  
  legend("topright", legend=str2lang(note1), pch=21, pt.bg="lightgreen", 
         col="black", cex=1, bty="n", inset=c(0.1, 0), xjust=0, text.width=70)
  legend("topright", legend=str2lang(note2), pch=21, pt.bg="darkgreen", 
         col="black", cex=1, bty="n", inset=c(0.1, 0.08), xjust=0, text.width=70)
  legend("topright", legend=str2lang(note3), lty=1, 
         col="goldenrod1", cex=1, bty="n", inset=c(0.1, 0.16), xjust=0, text.width=70)
  legend("topright", legend=str2lang(note4), lty=1, 
         col="goldenrod3", cex=1, bty="n", inset=c(0.1, 0.24), xjust=0, text.width=70)
  
  crap <- unique(merged$climate.x)
  
  if(crap[1] == "mo_dry") {
    title("Stomatal sensitivity, Central Plains Dry", line=1, cex.main=1.5)
  } else {
    title("Stomatal sensitivity, High Plains Dry", line=1, cex.main=1.5)
  }

}



###__________ plotting NPP and repro for mo_dry and yu_dry in 2-panel plot_______###
pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/both_dry_NPP_repro.pdf", height=8, width=8)  # 10, 14  
par(mfrow=c(2,1),  oma=c(0, 0, 0, 0), mai=c(0.7, 0.9, 0.7, 0.9), mgp=c(1.5, 0.5, 0))  
# par(mfrow=c(2,1),  oma=c(1, 0, 1, 0), mai=c(0.6, 0.7, 0.4, 0.7), mgp=c(1.5, 0.5, 0))

# mo_dry
df <- subset(comb, climate=="mo_dry"); nrow(df)
plot_npp_repro_fig(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity")
mtext("(a)", side=3, line=1, adj=0, cex=1.5)

# add second y axis title
mtext(expression("Cumulative reproduction (" * g * " " * m^-2 * ")"), side=4, line=2.5, cex=1.4)

# yu_dry
df <- subset(comb, climate=="yu_dry"); nrow(df)
plot_npp_repro_fig(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity")
mtext("(b)", side=3, line=1, adj=0, cex=1.5)

# add second y axis title
mtext(expression("Cumulative reproduction (" * g * " " * m^-2 * ")"), side=4, line=2.5, cex=1.4)

dev.off()



###__________ PLOTTING NPP and repro for single traits for each climate_________###

# mo_wet
df <- subset(comb, climate=="mo_wet"); nrow(df)

pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/mo_wet_npp_repro.pdf", height=8, width=14)  # 10, 14  
# jpeg(filename = "/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/mo_wet_npp_repro.jpeg", height=8, width=14, units="in", res = 200) 

par(mfrow=c(3,3),  oma=c(0, 0, 0, 0), mai=c(0.6, 0.4, 0.2, 0.6), mgp=c(1.5, 0.5, 0))  

plot_npp_repro(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity")
plot_npp_repro(df, eff, "low", "high", "hydraulic efficiency")
plot_npp_repro(df, saf, "low", "high", "hydraulic safety")
plot_npp_repro(df, root_depth, "shallow", "deep", "root depth")
plot_npp_repro(df, max_LAI, "low", "high", "maximum LAI")
plot_npp_repro(df, vpmax, "low", "high", "Vpmax")
plot_npp_repro(df, soil_water, "not_full", "full", "starting soil water profile")
plot_npp_repro(df, soil_texture, "fine", "coarse", "soil texture")

dev.off()


# mo_dry
df <- subset(comb, climate=="mo_dry"); nrow(df)

pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/mo_dry_npp_repro.pdf", height=8, width=14)  # 10, 14  
par(mfrow=c(3,3),  oma=c(0, 0, 0, 0), mai=c(0.6, 0.4, 0.2, 0.6), mgp=c(1.5, 0.5, 0))  

plot_npp_repro(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity")
plot_npp_repro(df, eff, "low", "high", "hydraulic efficiency")
plot_npp_repro(df, saf, "low", "high", "hydraulic safety")
plot_npp_repro(df, root_depth, "shallow", "deep", "root depth")
plot_npp_repro(df, max_LAI, "low", "high", "maximum LAI")
plot_npp_repro(df, vpmax, "low", "high", "Vpmax")
plot_npp_repro(df, soil_water, "not_full", "full", "starting soil water profile")
plot_npp_repro(df, soil_texture, "fine", "coarse", "soil texture")

dev.off()


# yu_wet
df <- subset(comb, climate=="yu_wet"); nrow(df)

pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/yu_wet_npp_repro.pdf", 
    height=8, width=14)  # 10, 14  
par(mfrow=c(3,3),  oma=c(0, 0, 0, 0), mai=c(0.6, 0.4, 0.2, 0.6), mgp=c(1.5, 0.5, 0))  

plot_npp_repro(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity")
plot_npp_repro(df, eff, "low", "high", "hydraulic efficiency")
plot_npp_repro(df, saf, "low", "high", "hydraulic safety")
plot_npp_repro(df, root_depth, "shallow", "deep", "root depth")
plot_npp_repro(df, max_LAI, "low", "high", "maximum LAI")
plot_npp_repro(df, vpmax, "low", "high", "Vpmax")
plot_npp_repro(df, soil_water, "not_full", "full", "starting soil water profile")
plot_npp_repro(df, soil_texture, "fine", "coarse", "soil texture")

dev.off()


# yu_dry
df <- subset(comb, climate=="yu_dry"); nrow(df)

pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/yu_dry_npp_repro.pdf", 
    height=8, width=14)  # 10, 14  
par(mfrow=c(3,3),  oma=c(0, 0, 0, 0), mai=c(0.6, 0.4, 0.2, 0.6), mgp=c(1.5, 0.5, 0))  

plot_npp_repro(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity")
plot_npp_repro(df, eff, "low", "high", "hydraulic efficiency")
plot_npp_repro(df, saf, "low", "high", "hydraulic safety")
plot_npp_repro(df, root_depth, "shallow", "deep", "root depth")
plot_npp_repro(df, max_LAI, "low", "high", "maximum LAI")
plot_npp_repro(df, vpmax, "low", "high", "Vpmax")
plot_npp_repro(df, soil_water, "not_full", "full", "starting soil water profile")
plot_npp_repro(df, soil_texture, "fine", "coarse", "soil texture")

dev.off()


# irrigated
df <- subset(comb, climate=="irrigated"); nrow(df)

pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/irrigated_npp_repro.pdf", 
    height=8, width=14)  # 10, 14  
par(mfrow=c(3,3),  oma=c(0, 0, 0, 0), mai=c(0.6, 0.4, 0.2, 0.6), mgp=c(1.5, 0.5, 0))  

plot_npp_repro(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity")
plot_npp_repro(df, eff, "low", "high", "hydraulic efficiency")
plot_npp_repro(df, saf, "low", "high", "hydraulic safety")
plot_npp_repro(df, root_depth, "shallow", "deep", "root depth")
plot_npp_repro(df, max_LAI, "low", "high", "maximum LAI")
plot_npp_repro(df, vpmax, "low", "high", "Vpmax")
plot_npp_repro(df, soil_water, "not_full", "full", "starting soil water profile")
plot_npp_repro(df, soil_texture, "fine", "coarse", "soil texture")

dev.off()





###_________________ SEASONAL TRAIT PLOTS _____________________### 
###_________________ SEASONAL TRAIT PLOTSS _____________________### 

#_______ plotting soil resistance, xylem resistance, transpiration, soil water potential____###

plot_resistance_trans <- function(dataframe_used, treatment, level1_string, level2_string, title_string) { 
  pa <- substitute(treatment)
  # goofing with different plots
  # contrast  
  # modify simulation column to remove number corresponding to the selected "treatment"
  crap <- dataframe_used
  # crap <- subset(comb, climate=="yu_dry")                                    # TURN OFF WHEN RUNNING LOOP
  # remove all "_" from simulations
  crap2 <- gsub("_", "", crap$simulation) 
  # remove number in crap2 that corresponds to the selected treatment  
  # create test vectors that can be fed to "ifelse" (need to be same length as desired ifelse output)
  crap3 <-  c(1:length(crap2)); crap3[] <- "hold"
  treat2 <- c(1:length(crap2)); treat2[] <- deparse(substitute(treatment))    
  # treat2 <- c(1:length(crap2)); treat2[] <- "gs_sensitivity"       # TURN OFF WHEN RUNNING LOOP
  
  # use ifelse to create a treatment vector without the desired treatment  
  crap3 <- ifelse(crap3=="hold" & treat2=="gs_sensitivity", paste(substr(crap2, 1, 1), substr(crap2, 3, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="eff", paste(substr(crap2, 1, 2), substr(crap2, 4, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="saf", paste(substr(crap2, 1, 3), substr(crap2, 5, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="refill", paste(substr(crap2, 1, 4), substr(crap2, 6, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="root_depth", paste(substr(crap2, 1, 4), substr(crap2, 6, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="max_LAI", paste(substr(crap2, 1, 5), substr(crap2, 7, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="vpmax", paste(substr(crap2, 1, 6), substr(crap2, 8, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="up_soil_water", paste(substr(crap2, 1, 8), substr(crap2, 10, 13), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="mature_rate", paste(substr(crap2, 1, 9), substr(crap2, 11, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="soil_water", paste(substr(crap2, 1, 7), substr(crap2, 9, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="weeds", paste(substr(crap2, 1, 11), substr(crap2, 13, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="soil_texture", substr(crap2, 1, 8), crap3)
  crap$compare_treat <- crap3
  # split crap into contrast1 and contast2
  contrast1 <- subset(crap, eval(pa)==level1_string)
  contrast2 <- subset(crap, eval(pa)==level2_string)
  #  contrast1 <- subset(crap, gs_sensitivity == "cons")
  #  contrast2 <- subset(crap, gs_sensitivity == "risky")
  
  # merge contrast1 and contrast2
  merged <- merge(contrast1, contrast2, by=c("compare_treat", "day"))
  # CHANGE DESIRED MAX NPP VALUE HERE
  merged <- subset(merged, NPP.max.x > 0 & NPP.max.y > 0)
  
  # we want total seasonal integrated NPP for the WUE calculation.  To get this, 
  # use the NPP_sum (not NPP.sum) column.  This is the NPP summed across all time steps
  # and all days for each simulation. 
  # because the original units were umol/m2/s (no conversion were done before summing),
  # we need to multiply the sum by 1800 (number of seconds in each time step) 
  # to get umol/m2 for entire season (see journal notes 12/8/2020)
  # to convert to mol (rather than umol), divide by 1000000
  # to convert to g (rather than mol), multiply by 12.011 (1 mol C = 12.011 g)
  crap <- summaryBy(NPP_sum.x + NPP_sum.y ~ merged$simulation.x, data=merged, FUN=mean, na.rm=TRUE)
  npp_sum1 <- mean(crap$NPP_sum.x.mean * 1800 / 1000000 * 12.011)
  npp_sum2 <- mean(crap$NPP_sum.y.mean * 1800 / 1000000 * 12.011)
  npp_sum1 <- round(npp_sum1,0); npp_sum1
  npp_sum2 <- round(npp_sum2,0); npp_sum2
  
  # calculate the fraction of precipitation to pass through stomata
  # note that cum_trans_season and precip are both in units of mm per unit ground area
  # also note that cum_trans_season has already been summed across all time steps
  # and all days for each iteration (it is not a 5-day chunk mean)
  level1_TF <- round(mean(merged$cum_trans_season.x)/mean(merged$precip_sum.x), 2)
  level2_TF <- round(mean(merged$cum_trans_season.y)/mean(merged$precip_sum.y), 2)
  
  # gives WUE in units of g/m2 NPP per mm/m2 of precipitation 
  level1_WUE <- round(npp_sum1/mean(merged$cum_trans_season.x), 2); level1_WUE
  level2_WUE <- round(npp_sum2/mean(merged$cum_trans_season.y), 2); level2_WUE
  
  
  # PLOT
  # start with a large empty plot to frame the subplots
  library(Hmisc)
  
  par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.6, 0.7, 0.4, 0.2), mgp=c(1.5, 0.3, 0))  
  x<-c(1:100); y=c(1:100)
  plot(y ~ x, xlab="", ylab="", axes=FALSE, col="white")
  # box()
  
  
  # CONTRAST 1
  sum1 <- summaryBy(soil_k.x + tot_plant_xylem_k.x + Gs.max.x + Leaf_Psi.max.x + 
                    theta_all.x + Trans_Ec.sum.x ~ day, 
                    data=merged, FUN=c(mean,sd), na.rm=TRUE)
  crap <- subset(sum1, select=c(soil_k.x.mean, tot_plant_xylem_k.x.mean))
  crap <- t(crap); colnames(crap) <- sum1$day
  head(crap)
  
  # resistance, stacked bargraph plot
  myboxplot <- function() {
    # par(mgp=c(0, 0.3, 0), new = TRUE)
    barplot(crap, #resistance, 
            main = "", ylim=c(0, 100), xaxt='n', yaxt='n',
            axes=FALSE, col = c("darkgrey", "darkblue"),
            # legend.text = c("soil_k", "xylem_k"),
            beside = FALSE); box()
            axis(side=2, cex.axis=0.5, tck=-0.05, las=2,
                 at=c(10,30,50,70,90))

            mtext(expression("Soil and xylem conductance"), side=2, line=1.7, cex=0.9, adj=1.3)
            mtext(expression("(mmol" * " " * m^-2 * " " * s^-1 * " " * MPa^-1 * ")"), 
                  side=2, line=0.75, cex=0.7, adj=-11)
            # axis(side=1, cex.axis=0.4, tck=-0.05, las=1)
            
            # add T-fraction and WUE legend
            legend("topright", c(paste(level1_string, ": T_fraction = ", level1_TF, sep=""), 
                                 paste(level1_string, ": PrUE = ", level1_WUE, " g/mm/m2", sep="")),
                                 cex=0.8)
            
            # add y axis reference lines
            abline(h=10, lty=3, col='gray70')
            abline(h=30, lty=3, col='gray70')
            abline(h=50, lty=3, col='gray70')
            
            legend("topleft", c("soil_K", "xylem_K"), pch=c(22,22),
                   pt.bg=c("darkgrey", "darkblue"),cex=0.8)
  }  
  
  subplot(myboxplot, x=c(par("usr")[1], par("usr")[2]), 
         y=c(par("usr")[2]*0.333, par("usr")[4]*0.667))
 

  # plot transpiration above previous bargraph
  # y_gs <- sum1$Gs.max.x.mean
  y <- sum1$Trans_Ec.sum.x.mean; min(y); max(y)
  mylineplot <- function() {
    # par(mgp=c(0, 0.3, 0), new = TRUE) 
    plot(y ~ sum1$day, col="goldenrod1", lwd=2, type="l",
         axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0, 6)) 
    axis(side=2, cex.axis=0.4, tck=-0.05, las=2, 
         at=c(0.0, 1, 2, 3, 4, 5, 6))
    box()

      mtext(expression("Transpiration"), side=2, line=1.7, cex=0.91)
      mtext(expression("(mmol" * " " * m^2 * " " * s^-1 * ")"), side=2, line=0.75, cex=0.7)
  }

  subplot(mylineplot, x=c(par("usr")[1], par("usr")[2]), 
          y=c(par("usr")[2]*0.667, par("usr")[4]))
  
 
  # CONTRAST 2
  sum1 <- summaryBy(soil_k.y + tot_plant_xylem_k.y + Gs.max.y + Leaf_Psi.max.y + 
                    theta_all.y + Trans_Ec.sum.y ~ day, 
                    data=merged, FUN=c(mean,sd), na.rm=TRUE)
  crap <- subset(sum1, select=c(soil_k.y.mean, tot_plant_xylem_k.y.mean))
  crap <- t(crap); colnames(crap) <- sum1$day
  head(crap)
  
  # plot resistances
  myboxplot <- function() {
    # par(mgp=c(0, 0.3, 0), new = TRUE)
    barplot(crap, #resistance, 
            main = "", ylim=c(0, 100), yaxt='n', # xaxt='n',
            col = c("darkgrey", "darkblue"), # axes=FALSE, axis.lty=1
            beside = FALSE); box()
    axis(side=2, cex.axis=0.5, tck=-0.05, las=2,
         at=c(10,30,50,70,90))

    mtext(expression("Day"), side=1, line=1.8, cex=1.2)
    
    # add y axis reference lines
    abline(h=10, lty=3, col='gray70')
    abline(h=30, lty=3, col='gray70')
    abline(h=50, lty=3, col='gray70')
    
    legend("topright", c(paste(level2_string, ": T_fraction = ", level2_TF, sep=""), 
                         paste(level2_string, ": PrUE = ", level2_WUE, " g/mm/m2", sep="")),
                         cex=0.8)
  }  
  
  subplot(myboxplot, x=c(par("usr")[1], par("usr")[2]), 
          y=c(par("usr")[2]*0, par("usr")[4]*0.333))
  
  
  # plot stomatal conductance
  # plot stomatal conductance above previous bargraph
  y <- sum1$Trans_Ec.sum.y.mean; min(y); max(y)
  mylineplot <- function() {
    # par(mgp=c(0, 0.3, 0), new = TRUE) 
    plot(y ~ sum1$day, col="goldenrod3", lwd=2, type="l",
         axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0, 6)) 

    legend("topright", legend=c(level1_string, level2_string),
           col=c("goldenrod1", "goldenrod3"), lty=1, cex=0.8)
  }
  
  subplot(mylineplot, x=c(par("usr")[1], par("usr")[2]), 
          y=c(par("usr")[2]*0.667, par("usr")[4]))

  title(title_string)
}




# set working directory
library(multipanelfigure)
library(Hmisc)
setwd("/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures") 

# mo_wet climate
df <- subset(comb, climate=="mo_wet")  

ggsave("crap1.pdf", plot_resistance_trans(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity"), width = 5, height = 4.5)
ggsave("crap2.pdf", plot_resistance_trans(df, eff, "low", "high", "hydraulic efficiency"), width = 5, height = 4.5)
ggsave("crap3.pdf", plot_resistance_trans(df, saf, "low", "high", "hydraulic safety"), width = 5, height = 4.5)
ggsave("crap4.pdf", plot_resistance_trans(df, root_depth, "shallow", "deep", "root depth"), width = 5, height = 4.5)
ggsave("crap5.pdf", plot_resistance_trans(df, max_LAI, "low", "high", "maximum LAI"), width = 5, height = 4.5)
ggsave("crap6.pdf", plot_resistance_trans(df, vpmax, "low", "high", "Vpmax"), width = 5, height = 4.5)
ggsave("crap7.pdf", plot_resistance_trans(df, soil_water, "not_full", "full", "starting soil water profile"), width = 5, height = 4.5)
ggsave("crap8.pdf", plot_resistance_trans(df, soil_texture, "fine", "coarse", "soil texture"), width = 5, height = 4.5)

# using "multipanelfigure" package
figure2 <- multi_panel_figure(width = c(5, 127, 5, 127, 5), 
                              height = c(5, 127, 5, 127, 5, 127, 5, 127, 5), 
                              row_spacing = 0, column_spacing = c(5, 10))
figure_width(figure2)
figure_height(figure2)
# change panel to correct dimensions
figure2 <- multi_panel_figure(width = 290, height = 480, rows=4, columns=2, panel_label_type = "none") #, row_spacing = 20)
# figure2 <- multi_panel_figure(width = 299, height = 533, rows=4, columns=2, panel_label_type = "none")

# figure3 <- multi_panel_figure(width = 200, height = 71.8, rows = 1, columns = 2)
fig_save <- fill_panel(figure2, c("crap1.pdf"))
fig_save <- fill_panel(fig_save, c("crap2.pdf"))
fig_save <- fill_panel(fig_save, c("crap3.pdf"))
fig_save <- fill_panel(fig_save, c("crap4.pdf"))
fig_save <- fill_panel(fig_save, c("crap5.pdf"))
fig_save <- fill_panel(fig_save, c("crap6.pdf"))
fig_save <- fill_panel(fig_save, c("crap7.pdf"))
fig_save <- fill_panel(fig_save, c("crap8.pdf"))

fig_save %>% save_multi_panel_figure(filename = "mo_wet_resistance_trans.pdf")



# mo_dry climate
df <- subset(comb, climate=="mo_dry")  

ggsave("crap1.pdf", plot_resistance_trans(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity"), width = 5, height = 4.5)
ggsave("crap2.pdf", plot_resistance_trans(df, eff, "low", "high", "hydraulic efficiency"), width = 5, height = 4.5)
ggsave("crap3.pdf", plot_resistance_trans(df, saf, "low", "high", "hydraulic safety"), width = 5, height = 4.5)
ggsave("crap4.pdf", plot_resistance_trans(df, root_depth, "shallow", "deep", "root depth"), width = 5, height = 4.5)
ggsave("crap5.pdf", plot_resistance_trans(df, max_LAI, "low", "high", "maximum LAI"), width = 5, height = 4.5)
ggsave("crap6.pdf", plot_resistance_trans(df, vpmax, "low", "high", "Vpmax"), width = 5, height = 4.5)
ggsave("crap7.pdf", plot_resistance_trans(df, soil_water, "not_full", "full", "starting soil water profile"), width = 5, height = 4.5)
ggsave("crap8.pdf", plot_resistance_trans(df, soil_texture, "fine", "coarse", "soil texture"), width = 5, height = 4.5)

# using "multipanelfigure" package
figure2 <- multi_panel_figure(width = c(5, 127, 5, 127, 5), 
                              height = c(5, 127, 5, 127, 5, 127, 5, 127, 5), 
                              row_spacing = 0, column_spacing = c(5, 10))
figure_width(figure2)
figure_height(figure2)
# change panel to correct dimensions
figure2 <- multi_panel_figure(width = 290, height = 480, rows=4, columns=2, panel_label_type = "none")

# figure3 <- multi_panel_figure(width = 200, height = 71.8, rows = 1, columns = 2)
fig_save <- fill_panel(figure2, c("crap1.pdf"))
fig_save <- fill_panel(fig_save, c("crap2.pdf"))
fig_save <- fill_panel(fig_save, c("crap3.pdf"))
fig_save <- fill_panel(fig_save, c("crap4.pdf"))
fig_save <- fill_panel(fig_save, c("crap5.pdf"))
fig_save <- fill_panel(fig_save, c("crap6.pdf"))
fig_save <- fill_panel(fig_save, c("crap7.pdf"))
fig_save <- fill_panel(fig_save, c("crap8.pdf"))

fig_save %>% save_multi_panel_figure(filename = "mo_dry_resistance_trans.pdf")



# yu_wet climate
df <- subset(comb, climate=="yu_wet")  

ggsave("crap1.pdf", plot_resistance_trans(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity"), width = 5, height = 4.5)
ggsave("crap2.pdf", plot_resistance_trans(df, eff, "low", "high", "hydraulic efficiency"), width = 5, height = 4.5)
ggsave("crap3.pdf", plot_resistance_trans(df, saf, "low", "high", "hydraulic safety"), width = 5, height = 4.5)
ggsave("crap4.pdf", plot_resistance_trans(df, root_depth, "shallow", "deep", "root depth"), width = 5, height = 4.5)
ggsave("crap5.pdf", plot_resistance_trans(df, max_LAI, "low", "high", "maximum LAI"), width = 5, height = 4.5)
ggsave("crap6.pdf", plot_resistance_trans(df, vpmax, "low", "high", "Vpmax"), width = 5, height = 4.5)
ggsave("crap7.pdf", plot_resistance_trans(df, soil_water, "not_full", "full", "starting soil water profile"), width = 5, height = 4.5)
ggsave("crap8.pdf", plot_resistance_trans(df, soil_texture, "fine", "coarse", "soil texture"), width = 5, height = 4.5)

# using "multipanelfigure" package
figure2 <- multi_panel_figure(width = c(5, 127, 5, 127, 5), 
                              height = c(5, 127, 5, 127, 5, 127, 5, 127, 5), 
                              row_spacing = 0, column_spacing = c(5, 10))
figure_width(figure2)
figure_height(figure2)
# change panel to correct dimensions
figure2 <- multi_panel_figure(width = 290, height = 480, rows=4, columns=2, panel_label_type = "none")

# figure3 <- multi_panel_figure(width = 200, height = 71.8, rows = 1, columns = 2)
fig_save <- fill_panel(figure2, c("crap1.pdf"))
fig_save <- fill_panel(fig_save, c("crap2.pdf"))
fig_save <- fill_panel(fig_save, c("crap3.pdf"))
fig_save <- fill_panel(fig_save, c("crap4.pdf"))
fig_save <- fill_panel(fig_save, c("crap5.pdf"))
fig_save <- fill_panel(fig_save, c("crap6.pdf"))
fig_save <- fill_panel(fig_save, c("crap7.pdf"))
fig_save <- fill_panel(fig_save, c("crap8.pdf"))

fig_save %>% save_multi_panel_figure(filename = "yu_wet_resistance_trans.pdf")



# yu_dry climate
df <- subset(comb, climate=="yu_dry")  

ggsave("crap1.pdf", plot_resistance_trans(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity"), width = 5, height = 4.5)
ggsave("crap2.pdf", plot_resistance_trans(df, eff, "low", "high", "hydraulic efficiency"), width = 5, height = 4.5)
ggsave("crap3.pdf", plot_resistance_trans(df, saf, "low", "high", "hydraulic safety"), width = 5, height = 4.5)
ggsave("crap4.pdf", plot_resistance_trans(df, root_depth, "shallow", "deep", "root depth"), width = 5, height = 4.5)
ggsave("crap5.pdf", plot_resistance_trans(df, max_LAI, "low", "high", "maximum LAI"), width = 5, height = 4.5)
ggsave("crap6.pdf", plot_resistance_trans(df, vpmax, "low", "high", "Vpmax"), width = 5, height = 4.5)
ggsave("crap7.pdf", plot_resistance_trans(df, soil_water, "not_full", "full", "starting soil water profile"), width = 5, height = 4.5)
ggsave("crap8.pdf", plot_resistance_trans(df, soil_texture, "fine", "coarse", "soil texture"), width = 5, height = 4.5)

# using "multipanelfigure" package
figure2 <- multi_panel_figure(width = c(5, 127, 5, 127, 5), 
                              height = c(5, 127, 5, 127, 5, 127, 5, 127, 5), 
                              row_spacing = 0, column_spacing = c(5, 10))
figure_width(figure2)
figure_height(figure2)
# change panel to correct dimensions
figure2 <- multi_panel_figure(width = 290, height = 480, rows=4, columns=2, panel_label_type = "none")

# figure3 <- multi_panel_figure(width = 200, height = 71.8, rows = 1, columns = 2)
fig_save <- fill_panel(figure2, c("crap1.pdf"))
fig_save <- fill_panel(fig_save, c("crap2.pdf"))
fig_save <- fill_panel(fig_save, c("crap3.pdf"))
fig_save <- fill_panel(fig_save, c("crap4.pdf"))
fig_save <- fill_panel(fig_save, c("crap5.pdf"))
fig_save <- fill_panel(fig_save, c("crap6.pdf"))
fig_save <- fill_panel(fig_save, c("crap7.pdf"))
fig_save <- fill_panel(fig_save, c("crap8.pdf"))

fig_save %>% save_multi_panel_figure(filename = "yu_dry_resistance_trans.pdf")



# irrigated
df <- subset(comb, climate=="irrigated")  

ggsave("crap1.pdf", plot_resistance_trans(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity"), width = 5, height = 4.5)
ggsave("crap2.pdf", plot_resistance_trans(df, eff, "low", "high", "hydraulic efficiency"), width = 5, height = 4.5)
ggsave("crap3.pdf", plot_resistance_trans(df, saf, "low", "high", "hydraulic safety"), width = 5, height = 4.5)
ggsave("crap4.pdf", plot_resistance_trans(df, root_depth, "shallow", "deep", "root depth"), width = 5, height = 4.5)
ggsave("crap5.pdf", plot_resistance_trans(df, max_LAI, "low", "high", "maximum LAI"), width = 5, height = 4.5)
ggsave("crap6.pdf", plot_resistance_trans(df, vpmax, "low", "high", "Vpmax"), width = 5, height = 4.5)
ggsave("crap7.pdf", plot_resistance_trans(df, soil_water, "not_full", "full", "starting soil water profile"), width = 5, height = 4.5)
ggsave("crap8.pdf", plot_resistance_trans(df, soil_texture, "fine", "coarse", "soil texture"), width = 5, height = 4.5)

# using "multipanelfigure" package
figure2 <- multi_panel_figure(width = c(5, 127, 5, 127, 5), 
                              height = c(5, 127, 5, 127, 5, 127, 5, 127, 5), 
                              row_spacing = 0, column_spacing = c(5, 10))
figure_width(figure2)
figure_height(figure2)
# change panel to correct dimensions
figure2 <- multi_panel_figure(width = 290, height = 480, rows=4, columns=2, panel_label_type = "none")

# figure3 <- multi_panel_figure(width = 200, height = 71.8, rows = 1, columns = 2)
fig_save <- fill_panel(figure2, c("crap1.pdf"))
fig_save <- fill_panel(fig_save, c("crap2.pdf"))
fig_save <- fill_panel(fig_save, c("crap3.pdf"))
fig_save <- fill_panel(fig_save, c("crap4.pdf"))
fig_save <- fill_panel(fig_save, c("crap5.pdf"))
fig_save <- fill_panel(fig_save, c("crap6.pdf"))
fig_save <- fill_panel(fig_save, c("crap7.pdf"))
fig_save <- fill_panel(fig_save, c("crap8.pdf"))

fig_save %>% save_multi_panel_figure(filename = "irrigated_resistance_trans.pdf")





###_________________ SEASONAL TRAIT PLOTS _____________________### 
###_________________ SEASONAL TRAIT PLOTSS _____________________### 

#_______ plotting root area, soil water ____###

plot_roots_soil_water <- function(dataframe_used, treatment, level1_string, level2_string, title_string) { 
  pa <- substitute(treatment)
  # goofing with different plots
  # contrast  
  # modify simulation column to remove number corresponding to the selected "treatment"
  crap <- dataframe_used
  # crap <- subset(comb, climate=="yu_dry")                                    # TURN OFF WHEN RUNNING LOOP
  # remove all "_" from simulations
  crap2 <- gsub("_", "", crap$simulation) 
  # remove number in crap2 that corresponds to the selected treatment  
  # create test vectors that can be fed to "ifelse" (need to be same length as desired ifelse output)
  crap3 <-  c(1:length(crap2)); crap3[] <- "hold"
  treat2 <- c(1:length(crap2)); treat2[] <- deparse(substitute(treatment))    
  # treat2 <- c(1:length(crap2)); treat2[] <- "gs_sensitivity"       # TURN OFF WHEN RUNNING LOOP
  
  # use ifelse to create a treatment vector without the desired treatment  
  crap3 <- ifelse(crap3=="hold" & treat2=="gs_sensitivity", paste(substr(crap2, 1, 1), substr(crap2, 3, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="eff", paste(substr(crap2, 1, 2), substr(crap2, 4, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="saf", paste(substr(crap2, 1, 3), substr(crap2, 5, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="refill", paste(substr(crap2, 1, 4), substr(crap2, 6, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="root_depth", paste(substr(crap2, 1, 4), substr(crap2, 6, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="max_LAI", paste(substr(crap2, 1, 5), substr(crap2, 7, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="vpmax", paste(substr(crap2, 1, 6), substr(crap2, 8, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="up_soil_water", paste(substr(crap2, 1, 8), substr(crap2, 10, 13), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="mature_rate", paste(substr(crap2, 1, 9), substr(crap2, 11, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="soil_water", paste(substr(crap2, 1, 7), substr(crap2, 9, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="weeds", paste(substr(crap2, 1, 11), substr(crap2, 13, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="soil_texture", substr(crap2, 1, 8), crap3)
  crap$compare_treat <- crap3
  # split crap into contrast1 and contast2
  contrast1 <- subset(crap, eval(pa)==level1_string)
  contrast2 <- subset(crap, eval(pa)==level2_string)
  #  contrast1 <- subset(crap, gs_sensitivity == "cons")
  #  contrast2 <- subset(crap, gs_sensitivity == "risky")
  
  # merge contrast1 and contrast2
  merged <- merge(contrast1, contrast2, by=c("compare_treat", "day"))
  # CHANGE DESIRED MAX NPP VALUE HERE
  merged <- subset(merged, NPP.max.x > 0 & NPP.max.y > 0)
  
  # PLOT
  # start with a large empty plot to frame the subplots
  library(Hmisc)
  
  par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.6, 0.7, 0.4, 0.2), mgp=c(1.5, 0.5, 0))  
  x<-c(1:100); y=c(1:100)
  plot(y ~ x, xlab="", ylab="", axes=FALSE, col="white")
  # box()
  
  
  # CONTRAST 1
  
  sum1 <- summaryBy(ar0.max.x + ar1.max.x + ar2.max.x + ar3.max.x + ar4.max.x +
                    theta0.max.x + theta1.max.x + theta2.max.x + theta3.max.x + theta4.max.x ~
                    day, data=merged, FUN=c(mean,sd), na.rm=TRUE)
  
  # plot root area
  y0 <- sum1$ar0.max.x.mean
  y1 <- sum1$ar1.max.x.mean
  y2 <- sum1$ar2.max.x.mean
  y3 <- sum1$ar3.max.x.mean
  y4 <- sum1$ar4.max.x.mean; min(y0, y1, y2, y3, y4); max(y0, y1, y2, y3, y4)
  
  mylineplot1 <- function() {
    plot(y0 ~ sum1$day, col="darkgoldenrod1", lwd=2, type="l",
         axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0, 0.6))
    points(y1 ~ sum1$day, col="darkgoldenrod2", lwd=2, type="l")
    points(y2 ~ sum1$day, col="darkgoldenrod3", lwd=2, type="l")
    points(y3 ~ sum1$day, col="darkgoldenrod4", lwd=2, type="l")
    points(y4 ~ sum1$day, col="chocolate4", lwd=2, type="l")
    abline(h=0.1, lty=3, col='gray70'); abline(h=0.2, lty=3, col='gray70') 
    abline(h=0.3, lty=3, col='gray70'); abline(h=0.4, lty=3, col='gray70')

    axis(side=2, cex.axis=0.4, tck=-0.05, las=2, 
         at=c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
    box()
    mtext(expression("Root area (" * m^2 * " " * m^-2 * ")"), side=2, line=1.2, cex=0.5)
    
    legend("topright", c("roots 0-5 cm","roots 5-15 cm","roots 15-35 cm","roots 35-75 cm",
           "roots 75-105 cm"), lty=c(1, 1, 1, 1, 1), 
           col = c("darkgoldenrod1", "darkgoldenrod2", "darkgoldenrod3", "darkgoldenrod4", "chocolate4"),
           cex=0.4)
  }
  
  subplot(mylineplot1, x=c(par("usr")[1], par("usr")[2]), 
          y=c(par("usr")[2]*0.5, par("usr")[4]*0.75))
  
  
  # plot water content
  y0 <- sum1$theta0.max.x.mean
  y1 <- sum1$theta1.max.x.mean
  y2 <- sum1$theta2.max.x.mean
  y3 <- sum1$theta3.max.x.mean
  y4 <- sum1$theta4.max.x.mean; min(y0, y1, y2, y3, y4); max(y0, y1, y2, y3, y4)

  mylineplot2 <- function() {
    plot(y0 ~ sum1$day, col="deepskyblue", lwd=2, type="l",
         axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0.05, 0.40))
    points(y1 ~ sum1$day, col="deepskyblue2", lwd=2, type="l")
    points(y2 ~ sum1$day, col="deepskyblue3", lwd=2, type="l")
    points(y3 ~ sum1$day, col="deepskyblue4", lwd=2, type="l")
    points(y4 ~ sum1$day, col="dodgerblue4", lwd=2, type="l")
    abline(h=0.1, lty=3, col='gray70'); abline(h=0.15, lty=3, col='gray70')
    abline(h=0.2, lty=3, col='gray70'); abline(h=0.25, lty=3, col='gray70')
    
    axis(side=2, cex.axis=0.4, tck=-0.05, las=2, 
         at=c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35))
    box()
    mtext(expression("Soil water content"), side=2, line=1.2, cex=0.5)
    
    legend("topright", c("soil water 0-5 cm","soil water 5-15 cm","soil water 15-35 cm",
           "soil water 35-75 cm", "soil water 75-105 cm"), lty=c(1, 1, 1, 1, 1), 
           col = c("deepskyblue", "deepskyblue2", "deepskyblue3", "deepskyblue4", "dodgerblue4"),
           cex=0.4)  
  }
  
  subplot(mylineplot2, x=c(par("usr")[1], par("usr")[2]), 
          y=c(par("usr")[2]*0.75, par("usr")[4]))


  # CONTRAST 2
  
  sum1 <- summaryBy(ar0.max.y + ar1.max.y + ar2.max.y + ar3.max.y + ar4.max.y +
                      theta0.max.y + theta1.max.y + theta2.max.y + theta3.max.y + theta4.max.y ~
                      day, data=merged, FUN=c(mean,sd), na.rm=TRUE)
  
  # plot root area
  y0 <- sum1$ar0.max.y.mean
  y1 <- sum1$ar1.max.y.mean
  y2 <- sum1$ar2.max.y.mean
  y3 <- sum1$ar3.max.y.mean
  y4 <- sum1$ar4.max.y.mean; min(y0, y1, y2, y3, y4); max(y0, y1, y2, y3, y4)
  
  mylineplot1 <- function() {
    plot(y0 ~ sum1$day, col="darkgoldenrod1", lwd=2, type="l", yaxt='n',
         xlab="Days", ylab="", cex=0.5, ylim=c(0, 0.6))
    points(y1 ~ sum1$day, col="darkgoldenrod2", lwd=2, type="l")
    points(y2 ~ sum1$day, col="darkgoldenrod3", lwd=2, type="l")
    points(y3 ~ sum1$day, col="darkgoldenrod4", lwd=2, type="l")
    points(y4 ~ sum1$day, col="chocolate4", lwd=2, type="l")
    abline(h=0.1, lty=3, col='gray70'); abline(h=0.2, lty=3, col='gray70')
    abline(h=0.3, lty=3, col='gray70'); abline(h=0.4, lty=3, col='gray70')
    
    axis(side=2, cex.axis=0.4, tck=-0.05, las=2, 
         at=c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
    box()
    mtext(expression("Root area (" * m^2 * " " * m^-2 * ")"), side=2, line=1.2, cex=0.5)
  }
  
  subplot(mylineplot1, x=c(par("usr")[1], par("usr")[2]), 
          y=c(par("usr")[1], par("usr")[4]*0.25))
  
  
  # plot water content
  y0 <- sum1$theta0.max.y.mean
  y1 <- sum1$theta1.max.y.mean
  y2 <- sum1$theta2.max.y.mean
  y3 <- sum1$theta3.max.y.mean
  y4 <- sum1$theta4.max.y.mean; min(y0, y1, y2, y3, y4); max(y0, y1, y2, y3, y4)

  mylineplot2 <- function() {
    plot(y0 ~ sum1$day, col="deepskyblue", lwd=2, type="l",
         axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0.05, 0.40))
    points(y1 ~ sum1$day, col="deepskyblue2", lwd=2, type="l")
    points(y2 ~ sum1$day, col="deepskyblue3", lwd=2, type="l")
    points(y3 ~ sum1$day, col="deepskyblue4", lwd=2, type="l")
    points(y4 ~ sum1$day, col="dodgerblue4", lwd=2, type="l")
    abline(h=0.1, lty=3, col='gray70'); abline(h=0.15, lty=3, col='gray70')
    abline(h=0.2, lty=3, col='gray70'); abline(h=0.25, lty=3, col='gray70')
    
    axis(side=2, cex.axis=0.4, tck=-0.05, las=2, 
         at=c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35))
    box()
    mtext(expression("Soil water content"), side=2, line=1.2, cex=0.5)
  }
  
  subplot(mylineplot2, x=c(par("usr")[1], par("usr")[2]), 
          y=c(par("usr")[2]*0.25, par("usr")[4]*0.50))

  # add contrast labels
  mtext(level2_string, side=2, line=2.1, cex=1.2, adj=0.25)
  mtext(level1_string, side=2, line=2.1, cex=1.2, adj=0.80)
  
  title(title_string)
}



###_________________ SEASONAL TRAIT PLOTS _____________________### 
###_________________ SEASONAL TRAIT PLOTSS _____________________### 

#_______ **NEW** (8-14-2021) plotting root area, soil water ____###

plot_roots_soil_water_new <- function(dataframe_used, treatment, level1_string, level2_string, title_string) { 
  pa <- substitute(treatment)
  # goofing with different plots
  # contrast  
  # modify simulation column to remove number corresponding to the selected "treatment"
  crap <- dataframe_used
  # crap <- subset(comb, climate=="yu_dry")                                    # TURN OFF WHEN RUNNING LOOP
  # remove all "_" from simulations
  crap2 <- gsub("_", "", crap$simulation) 
  # remove number in crap2 that corresponds to the selected treatment  
  # create test vectors that can be fed to "ifelse" (need to be same length as desired ifelse output)
  crap3 <-  c(1:length(crap2)); crap3[] <- "hold"
  treat2 <- c(1:length(crap2)); treat2[] <- deparse(substitute(treatment))    
  # treat2 <- c(1:length(crap2)); treat2[] <- "gs_sensitivity"       # TURN OFF WHEN RUNNING LOOP
  
  # use ifelse to create a treatment vector without the desired treatment  
  crap3 <- ifelse(crap3=="hold" & treat2=="gs_sensitivity", paste(substr(crap2, 1, 1), substr(crap2, 3, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="eff", paste(substr(crap2, 1, 2), substr(crap2, 4, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="saf", paste(substr(crap2, 1, 3), substr(crap2, 5, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="refill", paste(substr(crap2, 1, 4), substr(crap2, 6, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="root_depth", paste(substr(crap2, 1, 4), substr(crap2, 6, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="max_LAI", paste(substr(crap2, 1, 5), substr(crap2, 7, 9), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="vpmax", paste(substr(crap2, 1, 6), substr(crap2, 8, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="up_soil_water", paste(substr(crap2, 1, 8), substr(crap2, 10, 13), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="mature_rate", paste(substr(crap2, 1, 9), substr(crap2, 11, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="soil_water", paste(substr(crap2, 1, 7), substr(crap2, 9, 9), sep=""), crap3)
  # crap3 <- ifelse(crap3=="hold" & treat2=="weeds", paste(substr(crap2, 1, 11), substr(crap2, 13, 13), sep=""), crap3)
  crap3 <- ifelse(crap3=="hold" & treat2=="soil_texture", substr(crap2, 1, 8), crap3)
  crap$compare_treat <- crap3
  # split crap into contrast1 and contast2
  contrast1 <- subset(crap, eval(pa)==level1_string)
  contrast2 <- subset(crap, eval(pa)==level2_string)
  #  contrast1 <- subset(crap, gs_sensitivity == "cons")
  #  contrast2 <- subset(crap, gs_sensitivity == "risky")
  
  # merge contrast1 and contrast2
  merged <- merge(contrast1, contrast2, by=c("compare_treat", "day"))
  # CHANGE DESIRED MAX NPP VALUE HERE
  merged <- subset(merged, NPP.max.x > 0 & NPP.max.y > 0)
  
  # PLOT
  # start with a large empty plot to frame the subplots
  library(Hmisc)
  
  par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.6, 0.6, 0.6), mgp=c(1.5, 0.5, 0))  
  x<-c(1:100); y=c(1:100)
  plot(y ~ x, xlab="", ylab="", axes=FALSE, col="white")
  # box()
  
  # treatment and contrast legend (**needs** to be in main plot, not subplot)
  # note1 <- paste0(substitute(level2_string), "*", "' '", "*", substitute(treatment), "*", "': repro = '",
  #                "*", "' g '", "*", "m^-2")
  
  note1 <- paste0(substitute(level1_string))
  note2 <- paste0(substitute(level2_string))
  
  legend("topleft", legend=str2lang(note1), lty=1, col="lightgreen", cex=0.8, bty="n", 
         lwd=2, inset=c(0.0, 0.0), xjust=1)
  legend("topleft", legend=str2lang(note2), lty=1, col="darkgreen", cex=0.8, bty="n", 
         lwd=2, inset=c(0.0, 0.035), xjust=1)
  
  
  # CONTRASTS 1 & 2
  
  # calculate means and sums for contrast 1
  sum1 <- summaryBy(ar0.max.x + ar1.max.x + ar2.max.x + ar3.max.x + ar4.max.x +
                    theta0.max.x + theta1.max.x + theta2.max.x + theta3.max.x + theta4.max.x ~
                    day, data=merged, FUN=c(mean,sd), na.rm=TRUE)
  
  # calculate means and sums for contrast 2
  sum2 <- summaryBy(ar0.max.y + ar1.max.y + ar2.max.y + ar3.max.y + ar4.max.y +
                      theta0.max.y + theta1.max.y + theta2.max.y + theta3.max.y + theta4.max.y ~
                      day, data=merged, FUN=c(mean,sd), na.rm=TRUE)
  
  # calculate root areas for contrast 1
  y0_1 <- sum1$ar0.max.x.mean
  y1_1 <- sum1$ar1.max.x.mean
  y2_1 <- sum1$ar2.max.x.mean
  y3_1 <- sum1$ar3.max.x.mean
  y4_1 <- sum1$ar4.max.x.mean; min(y0_1, y1_1, y2_1, y3_1, y4_1); max(y0_1, y1_1, y2_1, y3_1, y4_1)
  shallow_1 <- y0_1
  medium_1 <- y2_1
  deep_1 <- y4_1
  
  # calculate root areas for contrast 2
  y0_2 <- sum2$ar0.max.y.mean
  y1_2 <- sum2$ar1.max.y.mean
  y2_2 <- sum2$ar2.max.y.mean
  y3_2 <- sum2$ar3.max.y.mean
  y4_2 <- sum2$ar4.max.y.mean; min(y0_2, y1_2, y2_2, y3_2, y4_2); max(y0_2, y1_2, y2_2, y3_2, y4_2)
  shallow_2 <- y0_2
  medium_2 <- y2_2
  deep_2 <- y4_2
  
  # plot shallow roots (both treatments/contrasts)
  mylineplot1 <- function() {
    # plot contrast 1 shallow roots
    plot(shallow_1 ~ sum1$day, col="lightgreen", lwd=2, type="l",
         axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0.2, 0.6))
    # plot contrast 2 shallow roots 
    points(shallow_2 ~ sum2$day, col="darkgreen", lwd=2, type="l")
    # dotted depth index lines
    abline(h=0.1, lty=3, col='gray70'); abline(h=0.2, lty=3, col='gray70') 
    abline(h=0.3, lty=3, col='gray70'); abline(h=0.4, lty=3, col='gray70')
    # add axes and legend
    axis(side=2, cex.axis=0.5, tck=-0.05, las=2, 
         at=c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
    box()
    mtext(expression("Root area (" * m^2 * " " * m^-2 * ")"), side=2, line=1.2, cex=0.7)
    
    legend("topright", c("roots 0-5 cm"), cex=0.6)
  }
  # add subplot to main plot
  subplot(mylineplot1, x=c(par("usr")[1], par("usr")[2]*0.5), 
          y=c(par("usr")[4]*0.667, par("usr")[4]))

  
  # plot medium roots (both treatments/contrasts)
  mylineplot1 <- function() {
    # plot contrast 1 medium roots
    plot(medium_1 ~ sum1$day, col="lightgreen", lwd=2, type="l",
         axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0, 0.2))
    # plot contrast 2 medium roots 
    points(medium_2 ~ sum2$day, col="darkgreen", lwd=2, type="l")
    # dotted depth index lines
    abline(h=0.0, lty=3, col='gray70'); abline(h=0.2, lty=3, col='gray70') 
    abline(h=0.05, lty=3, col='gray70'); abline(h=0.2, lty=3, col='gray70') 
    abline(h=0.10, lty=3, col='gray70'); abline(h=0.2, lty=3, col='gray70')
    abline(h=0.15, lty=3, col='gray70'); abline(h=0.2, lty=3, col='gray70')
    # add axes and legend
    axis(side=2, cex.axis=0.5, tck=-0.05, las=2, 
         at=c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6))
    box()
    mtext(expression("Root area (" * m^2 * " " * m^-2 * ")"), side=2, line=1.2, cex=0.7)
    
    legend("topright", c("roots 15-35 cm"), cex=0.6)
  }
  # add subplot to main plot
  subplot(mylineplot1, x=c(par("usr")[1], par("usr")[2]*0.5), 
          y=c(par("usr")[4]*0.333, par("usr")[4]*0.667))
  
  
  # plot deep roots (both treatments/contrasts)
  mylineplot1 <- function() {
    # plot contrast 1 deep roots
    plot(deep_1 ~ sum1$day, col="lightgreen", lwd=2, type="l",
         axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0, 0.2))
    # plot contrast 2 deep roots 
    points(deep_2 ~ sum2$day, col="darkgreen", lwd=2, type="l")
    # dotted depth index lines
    abline(h=0.0, lty=3, col='gray70'); abline(h=0.2, lty=3, col='gray70') 
    abline(h=0.05, lty=3, col='gray70'); abline(h=0.2, lty=3, col='gray70') 
    abline(h=0.10, lty=3, col='gray70'); abline(h=0.2, lty=3, col='gray70')
    abline(h=0.15, lty=3, col='gray70'); abline(h=0.2, lty=3, col='gray70')
    # add axes and legend
    axis(side=2, cex.axis=0.5, tck=-0.05, las=2, 
         at=c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6))
    axis(side=1, cex.axis=0.5, tck=-0.05, las=1, 
         at=c(150, 200, 250, 300), padj=-1.5)
    box()
    mtext(expression("Root area (" * m^2 * " " * m^-2 * ")"), side=2, line=1.2, cex=0.7)
    mtext(expression("Day"), side=1, line=1, cex=0.7)
    
    legend("topright", c("roots 75-105 cm"), cex=0.6)
  }
  # add subplot to main plot
  subplot(mylineplot1, x=c(par("usr")[1], par("usr")[2]*0.5), 
          y=c(par("usr")[1], par("usr")[4]*0.333))
  
  
  # calculate soil water contents for both contrasts
  # contrast_1
  y0_1 <- sum1$theta0.max.x.mean
  y1_1 <- sum1$theta1.max.x.mean
  y2_1 <- sum1$theta2.max.x.mean
  y3_1 <- sum1$theta3.max.x.mean
  y4_1 <- sum1$theta4.max.x.mean; min(y0_1, y1_1, y2_1, y3_1, y4_1); max(y0_1, y1_1, y2_1, y3_1, y4_1)
  shallow_1 <- y0_1
  medium_1 <- y2_1
  deep_1 <- y4_1
 
  # contrast_2 
  y0_2 <- sum2$theta0.max.y.mean
  y1_2 <- sum2$theta1.max.y.mean
  y2_2 <- sum2$theta2.max.y.mean
  y3_2 <- sum2$theta3.max.y.mean
  y4_2 <- sum2$theta4.max.y.mean; min(y0_2, y1_2, y2_2, y3_2, y4_2); max(y0_2, y1_2, y2_2, y3_2, y4_2)
  shallow_2 <- y0_2
  medium_2 <- y2_2
  deep_2 <- y4_2
  
  # plot shallow soil water (both treatments/contrasts)
  mylineplot2 <- function() {
    # contrast_1
    plot(shallow_1 ~ sum1$day, col="lightgreen", lwd=2, type="l",
         axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0.05, 0.40))
    # contrast_2
    points(shallow_2 ~ sum2$day, col="darkgreen", lwd=2, type="l")
    # add index depth lines
    abline(h=0.1, lty=3, col='gray70'); abline(h=0.15, lty=3, col='gray70')
    abline(h=0.2, lty=3, col='gray70'); abline(h=0.25, lty=3, col='gray70')
    # add axes and legend
    axis(side=4, cex.axis=0.5, tck=-0.05, las=2, 
         at=c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35))
    box()
    mtext(expression("Soil water content"), side=4, line=1.2, cex=0.7)
    
    legend("topright", c("soil water 0-5 cm"), cex=0.6)  
  }
  # add subplot to mainplot
  subplot(mylineplot2, x=c(par("usr")[2]*0.5, par("usr")[2]), 
          y=c(par("usr")[4]*0.667, par("usr")[4]))

  
  # plot medium soil water (both treatments/contrasts)
  mylineplot2 <- function() {
    # contrast_1
    plot(medium_1 ~ sum1$day, col="lightgreen", lwd=2, type="l",
         axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0.05, 0.40))
    # contrast_2
    points(medium_2 ~ sum2$day, col="darkgreen", lwd=2, type="l")
    # add index depth lines
    abline(h=0.1, lty=3, col='gray70'); abline(h=0.15, lty=3, col='gray70')
    abline(h=0.2, lty=3, col='gray70'); abline(h=0.25, lty=3, col='gray70')
    # add axes and legend
    axis(side=4, cex.axis=0.5, tck=-0.05, las=2, 
         at=c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35))
    box()
    mtext(expression("Soil water content"), side=4, line=1.2, cex=0.7)
    
    legend("topright", c("soil water 15-35 cm"), cex=0.6)  
  }
  # add subplot to mainplot
  subplot(mylineplot2, x=c(par("usr")[2]*0.5, par("usr")[2]), 
          y=c(par("usr")[4]*0.333, par("usr")[4]*0.667))
  
  
  # plot deep soil water (both treatments/contrasts)
  mylineplot2 <- function() {
    # contrast_1
    plot(deep_1 ~ sum1$day, col="lightgreen", lwd=2, type="l",
         axes=FALSE, xlab="", ylab="", cex=0.5, ylim=c(0.05, 0.40))
    # contrast_2
    points(deep_2 ~ sum2$day, col="darkgreen", lwd=2, type="l")
    # add index depth lines
    abline(h=0.1, lty=3, col='gray70'); abline(h=0.15, lty=3, col='gray70')
    abline(h=0.2, lty=3, col='gray70'); abline(h=0.25, lty=3, col='gray70')
    # add axes and legend
    axis(side=4, cex.axis=0.5, tck=-0.05, las=2, 
         at=c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35))
    axis(side=1, cex.axis=0.5, tck=-0.05, las=1, 
         at=c(150, 200, 250, 300), padj=-1.5)
    box()
    mtext(expression("Soil water content"), side=4, line=1.2, cex=0.7)
    mtext(expression("Day"), side=1, line=1, cex=0.7)
    
    legend("topright", c("soil water 75-105 cm"), cex=0.6)  
  }
  # add subplot to mainplot
  subplot(mylineplot2, x=c(par("usr")[2]*0.5, par("usr")[2]), 
          y=c(par("usr")[1], par("usr")[4]*0.333))
  
  # add contrast labels
  mtext("root area", side=3, line=0.1, cex=1.2, adj=0.18)
  mtext("soil water", side=3, line=0.1, cex=1.2, adj=0.84)

  title(title_string, line=1.5)
}

df <- subset(comb, climate=="mo_wet")  
plot_roots_soil_water_new(df, gs_sensitivity, "cons", "risky", "stomatal response")





# set working directory
library(multipanelfigure)
setwd("/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures") 

# mo_wet climate
df <- subset(comb, climate=="mo_wet")  

ggsave("crap1.pdf", plot_roots_soil_water_new(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity"), width = 5, height = 4.5)
ggsave("crap2.pdf", plot_roots_soil_water_new(df, eff, "low", "high", "hydraulic efficiency"), width = 5, height = 4.5)
ggsave("crap3.pdf", plot_roots_soil_water_new(df, saf, "low", "high", "hydraulic safety"), width = 5, height = 4.5)
ggsave("crap4.pdf", plot_roots_soil_water_new(df, root_depth, "shallow", "deep", "root depth"), width = 5, height = 4.5)
ggsave("crap5.pdf", plot_roots_soil_water_new(df, max_LAI, "low", "high", "maximum LAI"), width = 5, height = 4.5)
ggsave("crap6.pdf", plot_roots_soil_water_new(df, vpmax, "low", "high", "Vpmax"), width = 5, height = 4.5)
ggsave("crap7.pdf", plot_roots_soil_water_new(df, soil_water, "not_full", "full", "starting soil water profile"), width = 5, height = 4.5)
ggsave("crap8.pdf", plot_roots_soil_water_new(df, soil_texture, "fine", "coarse", "soil texture"), width = 5, height = 4.5)

# using "multipanelfigure" package
figure2 <- multi_panel_figure(width = c(5, 127, 5, 127, 5), 
                              height = c(5, 127, 5, 127, 5, 127, 5, 127, 5), 
                              row_spacing = 0, column_spacing = c(5, 10))
figure_width(figure2)
figure_height(figure2)
# change panel to correct dimensions
figure2 <- multi_panel_figure(width = 299, height = 510, rows=4, columns=2, panel_label_type = "none")

# figure3 <- multi_panel_figure(width = 200, height = 71.8, rows = 1, columns = 2)
fig_save <- fill_panel(figure2, c("crap1.pdf"))
fig_save <- fill_panel(fig_save, c("crap2.pdf"))
fig_save <- fill_panel(fig_save, c("crap3.pdf"))
fig_save <- fill_panel(fig_save, c("crap4.pdf"))
fig_save <- fill_panel(fig_save, c("crap5.pdf"))
fig_save <- fill_panel(fig_save, c("crap6.pdf"))
fig_save <- fill_panel(fig_save, c("crap7.pdf"))
fig_save <- fill_panel(fig_save, c("crap8.pdf"))

fig_save %>% save_multi_panel_figure(filename = "mo_wet_roots_soil_water.pdf")



# mo_dry climate
df <- subset(comb, climate=="mo_dry")  

ggsave("crap1.pdf", plot_roots_soil_water_new(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity"), width = 5, height = 4.5)
ggsave("crap2.pdf", plot_roots_soil_water_new(df, eff, "low", "high", "hydraulic efficiency"), width = 5, height = 4.5)
ggsave("crap3.pdf", plot_roots_soil_water_new(df, saf, "low", "high", "hydraulic safety"), width = 5, height = 4.5)
ggsave("crap4.pdf", plot_roots_soil_water_new(df, root_depth, "shallow", "deep", "root depth"), width = 5, height = 4.5)
ggsave("crap5.pdf", plot_roots_soil_water_new(df, max_LAI, "low", "high", "maximum LAI"), width = 5, height = 4.5)
ggsave("crap6.pdf", plot_roots_soil_water_new(df, vpmax, "low", "high", "Vpmax"), width = 5, height = 4.5)
ggsave("crap7.pdf", plot_roots_soil_water_new(df, soil_water, "not_full", "full", "starting soil water profile"), width = 5, height = 4.5)
ggsave("crap8.pdf", plot_roots_soil_water_new(df, soil_texture, "fine", "coarse", "soil texture"), width = 5, height = 4.5)

# using "multipanelfigure" package
figure2 <- multi_panel_figure(width = c(5, 127, 5, 127, 5), 
                              height = c(5, 127, 5, 127, 5, 127, 5, 127, 5), 
                              row_spacing = 0, column_spacing = c(5, 10))
figure_width(figure2)
figure_height(figure2)
# change panel to correct dimensions
figure2 <- multi_panel_figure(width = 299, height = 510, rows=4, columns=2, panel_label_type = "none")

# figure3 <- multi_panel_figure(width = 200, height = 71.8, rows = 1, columns = 2)
fig_save <- fill_panel(figure2, c("crap1.pdf"))
fig_save <- fill_panel(fig_save, c("crap2.pdf"))
fig_save <- fill_panel(fig_save, c("crap3.pdf"))
fig_save <- fill_panel(fig_save, c("crap4.pdf"))
fig_save <- fill_panel(fig_save, c("crap5.pdf"))
fig_save <- fill_panel(fig_save, c("crap6.pdf"))
fig_save <- fill_panel(fig_save, c("crap7.pdf"))
fig_save <- fill_panel(fig_save, c("crap8.pdf"))

fig_save %>% save_multi_panel_figure(filename = "mo_dry_roots_soil_water.pdf")



# yu_wet climate
df <- subset(comb, climate=="yu_wet")  

ggsave("crap1.pdf", plot_roots_soil_water_new(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity"), width = 5, height = 4.5)
ggsave("crap2.pdf", plot_roots_soil_water_new(df, eff, "low", "high", "hydraulic efficiency"), width = 5, height = 4.5)
ggsave("crap3.pdf", plot_roots_soil_water_new(df, saf, "low", "high", "hydraulic safety"), width = 5, height = 4.5)
ggsave("crap4.pdf", plot_roots_soil_water_new(df, root_depth, "shallow", "deep", "root depth"), width = 5, height = 4.5)
ggsave("crap5.pdf", plot_roots_soil_water_new(df, max_LAI, "low", "high", "maximum LAI"), width = 5, height = 4.5)
ggsave("crap6.pdf", plot_roots_soil_water_new(df, vpmax, "low", "high", "Vpmax"), width = 5, height = 4.5)
ggsave("crap7.pdf", plot_roots_soil_water_new(df, soil_water, "not_full", "full", "starting soil water profile"), width = 5, height = 4.5)
ggsave("crap8.pdf", plot_roots_soil_water_new(df, soil_texture, "fine", "coarse", "soil texture"), width = 5, height = 4.5)

# using "multipanelfigure" package
figure2 <- multi_panel_figure(width = c(5, 127, 5, 127, 5), 
                              height = c(5, 127, 5, 127, 5, 127, 5, 127, 5), 
                              row_spacing = 0, column_spacing = c(5, 10))
figure_width(figure2)
figure_height(figure2)
# change panel to correct dimensions
figure2 <- multi_panel_figure(width = 299, height = 510, rows=4, columns=2, panel_label_type = "none")

# figure3 <- multi_panel_figure(width = 200, height = 71.8, rows = 1, columns = 2)
fig_save <- fill_panel(figure2, c("crap1.pdf"))
fig_save <- fill_panel(fig_save, c("crap2.pdf"))
fig_save <- fill_panel(fig_save, c("crap3.pdf"))
fig_save <- fill_panel(fig_save, c("crap4.pdf"))
fig_save <- fill_panel(fig_save, c("crap5.pdf"))
fig_save <- fill_panel(fig_save, c("crap6.pdf"))
fig_save <- fill_panel(fig_save, c("crap7.pdf"))
fig_save <- fill_panel(fig_save, c("crap8.pdf"))

fig_save %>% save_multi_panel_figure(filename = "yu_wet_roots_soil_water.pdf")



# yu_dry climate
df <- subset(comb, climate=="yu_dry")  

ggsave("crap1.pdf", plot_roots_soil_water_new(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity"), width = 5, height = 4.5)
ggsave("crap2.pdf", plot_roots_soil_water_new(df, eff, "low", "high", "hydraulic efficiency"), width = 5, height = 4.5)
ggsave("crap3.pdf", plot_roots_soil_water_new(df, saf, "low", "high", "hydraulic safety"), width = 5, height = 4.5)
ggsave("crap4.pdf", plot_roots_soil_water_new(df, root_depth, "shallow", "deep", "root depth"), width = 5, height = 4.5)
ggsave("crap5.pdf", plot_roots_soil_water_new(df, max_LAI, "low", "high", "maximum LAI"), width = 5, height = 4.5)
ggsave("crap6.pdf", plot_roots_soil_water_new(df, vpmax, "low", "high", "Vpmax"), width = 5, height = 4.5)
ggsave("crap7.pdf", plot_roots_soil_water_new(df, soil_water, "not_full", "full", "starting soil water profile"), width = 5, height = 4.5)
ggsave("crap8.pdf", plot_roots_soil_water_new(df, soil_texture, "fine", "coarse", "soil texture"), width = 5, height = 4.5)

# using "multipanelfigure" package
figure2 <- multi_panel_figure(width = c(5, 127, 5, 127, 5), 
                              height = c(5, 127, 5, 127, 5, 127, 5, 127, 5), 
                              row_spacing = 0, column_spacing = c(5, 10))
figure_width(figure2)
figure_height(figure2)
# change panel to correct dimensions
figure2 <- multi_panel_figure(width = 299, height = 510, rows=4, columns=2, panel_label_type = "none")

# figure3 <- multi_panel_figure(width = 200, height = 71.8, rows = 1, columns = 2)
fig_save <- fill_panel(figure2, c("crap1.pdf"))
fig_save <- fill_panel(fig_save, c("crap2.pdf"))
fig_save <- fill_panel(fig_save, c("crap3.pdf"))
fig_save <- fill_panel(fig_save, c("crap4.pdf"))
fig_save <- fill_panel(fig_save, c("crap5.pdf"))
fig_save <- fill_panel(fig_save, c("crap6.pdf"))
fig_save <- fill_panel(fig_save, c("crap7.pdf"))
fig_save <- fill_panel(fig_save, c("crap8.pdf"))

fig_save %>% save_multi_panel_figure(filename = "yu_dry_roots_soil_water.pdf")



# irrigated
df <- subset(comb, climate=="irrigated")  

ggsave("crap1.pdf", plot_roots_soil_water_new(df, gs_sensitivity, "cons", "risky", "stomatal sensitivity"), width = 5, height = 4.5)
ggsave("crap2.pdf", plot_roots_soil_water_new(df, eff, "low", "high", "hydraulic efficiency"), width = 5, height = 4.5)
ggsave("crap3.pdf", plot_roots_soil_water_new(df, saf, "low", "high", "hydraulic safety"), width = 5, height = 4.5)
ggsave("crap4.pdf", plot_roots_soil_water_new(df, root_depth, "shallow", "deep", "root depth"), width = 5, height = 4.5)
ggsave("crap5.pdf", plot_roots_soil_water_new(df, max_LAI, "low", "high", "maximum LAI"), width = 5, height = 4.5)
ggsave("crap6.pdf", plot_roots_soil_water_new(df, vpmax, "low", "high", "Vpmax"), width = 5, height = 4.5)
ggsave("crap7.pdf", plot_roots_soil_water_new(df, soil_water, "not_full", "full", "starting soil water profile"), width = 5, height = 4.5)
ggsave("crap8.pdf", plot_roots_soil_water_new(df, soil_texture, "fine", "coarse", "soil texture"), width = 5, height = 4.5)

# using "multipanelfigure" package
figure2 <- multi_panel_figure(width = c(5, 127, 5, 127, 5), 
                              height = c(5, 127, 5, 127, 5, 127, 5, 127, 5), 
                              row_spacing = 0, column_spacing = c(5, 10))
figure_width(figure2)
figure_height(figure2)
# change panel to correct dimensions
figure2 <- multi_panel_figure(width = 299, height = 510, rows=4, columns=2, panel_label_type = "none")

# figure3 <- multi_panel_figure(width = 200, height = 71.8, rows = 1, columns = 2)
fig_save <- fill_panel(figure2, c("crap1.pdf"))
fig_save <- fill_panel(fig_save, c("crap2.pdf"))
fig_save <- fill_panel(fig_save, c("crap3.pdf"))
fig_save <- fill_panel(fig_save, c("crap4.pdf"))
fig_save <- fill_panel(fig_save, c("crap5.pdf"))
fig_save <- fill_panel(fig_save, c("crap6.pdf"))
fig_save <- fill_panel(fig_save, c("crap7.pdf"))
fig_save <- fill_panel(fig_save, c("crap8.pdf"))

fig_save %>% save_multi_panel_figure(filename = "irrigated_roots_soil_water.pdf")

dev.off() # need to turn off graphics device before starting NPP heatmaps




###__________ NPP HEATMAPS __________###
###________ NPP heatmap _____________###
# bulid function to make NPP matrix to give pheatmap

# mo_wet
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='mo_wet'); nrow(res) 
res <- summaryBy(NPP_sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI', 'vpmax',
                      'soil_water', 'soil_texture'))
# convert NPP to g/m2 (see above explanation)... gives g/m2
res$NPP_sum <- res$NPP_sum * 1800 / 1000000 * 12.011

# loop through res and create trait x trait matrix of NPP values
df <- res
treat_list <- colnames(df)[c(3:ncol(df))]; treat_list
woo <- c()
woo2 <- c()
name1 <- c()
for(i in 1:length(treat_list)) {
  for(j in 1:length(treat_list)) {    # change number here
    # i = 1; j = 2
    if(i != j) { # need this so i and j are never the same trait
      crap <- subset(df, select=c('NPP_sum', treat_list[i], treat_list[j])); crap
      sum1 <- summaryBy(NPP_sum ~ ., data=crap, FUN=mean, keep.names = TRUE); sum1
      sum2 <- c(sum1[1,3]); sum2
      sum2 <- c(sum2, sum1[2,3]); sum2
      sum3 <- c(sum1[3,3]); sum3
      sum3 <- c(sum3, sum1[4,3]); sum3
      all <- rbind(sum2, sum3); all
      colnames(all) <- c(paste(treat_list[j], sum1[1,2], sep="_"),
                         paste(treat_list[j], sum1[2,2], sep="_")); all
      all <- as.data.frame(all, row.names = FALSE); all
      name1 <- c(paste(treat_list[i], sum1[1,1,], sep="_"),
                 paste(treat_list[i], sum1[3,1,], sep="_")); name1
      woo <- as.data.frame(c(woo, all)); woo
    }
  }
  rownames(woo) <- name1; woo
  woo$crap1<-NA; woo$crap2<-NA; woo
  colnames(woo)[colnames(woo)=="crap1"] <- name1[1]; woo
  colnames(woo)[colnames(woo)=="crap2"] <- name1[2]; woo
  
  woo[,order(names(woo))]
  woo2 <- rbind(woo2, woo); woo2
  woo <- c()
}  

library("pheatmap")
#df <- as.data.frame(scale(woo2)); df
df <- as.data.frame(woo2); head(df)

pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/mo_wet_npp_heatmap.pdf", height=5, width=6)  # 10, 14  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
# plot heatmap
pheatmap(df, color = colorRampPalette(c("white", "firebrick3"))(50),
         cuttree_rows=4, main=expression("Central Plains (Wet) NPP (g " * m^-2 * ")")) 
dev.off()



# mo_dry
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='mo_dry'); nrow(res) 
res <- summaryBy(NPP_sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI', 'vpmax',
                      'soil_water', 'soil_texture'))
# convert NPP to g/m2 (see above explanation)... gives g/m2
res$NPP_sum <- res$NPP_sum * 1800 / 1000000 * 12.011

# loop through res and create trait x trait matrix of NPP values
df <- res
treat_list <- colnames(df)[c(3:ncol(df))]; treat_list
woo <- c()
woo2 <- c()
name1 <- c()
for(i in 1:length(treat_list)) {
  for(j in 1:length(treat_list)) {    # change number here
    # i = 1; j = 2
    if(i != j) { # need this so i and j are never the same trait
      crap <- subset(df, select=c('NPP_sum', treat_list[i], treat_list[j])); crap
      sum1 <- summaryBy(NPP_sum ~ ., data=crap, FUN=mean, keep.names = TRUE); sum1
      sum2 <- c(sum1[1,3]); sum2
      sum2 <- c(sum2, sum1[2,3]); sum2
      sum3 <- c(sum1[3,3]); sum3
      sum3 <- c(sum3, sum1[4,3]); sum3
      all <- rbind(sum2, sum3); all
      colnames(all) <- c(paste(treat_list[j], sum1[1,2], sep="_"),
                         paste(treat_list[j], sum1[2,2], sep="_")); all
      all <- as.data.frame(all, row.names = FALSE); all
      name1 <- c(paste(treat_list[i], sum1[1,1,], sep="_"),
                 paste(treat_list[i], sum1[3,1,], sep="_")); name1
      woo <- as.data.frame(c(woo, all)); woo
    }
  }
  rownames(woo) <- name1; woo
  woo$crap1<-NA; woo$crap2<-NA; woo
  colnames(woo)[colnames(woo)=="crap1"] <- name1[1]; woo
  colnames(woo)[colnames(woo)=="crap2"] <- name1[2]; woo
  
  woo[,order(names(woo))]
  woo2 <- rbind(woo2, woo); woo2
  woo <- c()
}  

library("pheatmap")
#df <- as.data.frame(scale(woo2)); df
df <- as.data.frame(woo2); head(df)

pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/mo_dry_npp_heatmap.pdf", height=5, width=6)  # 10, 14  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
# plot heatmap
pheatmap(df, color = colorRampPalette(c("white", "firebrick3"))(50),
         cuttree_rows=4, main=expression("Central Plains (Dry) NPP (g " * m^-2 * ")")) 
dev.off()



# yu_wet
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='yu_wet'); nrow(res) 
res <- summaryBy(NPP_sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI', 'vpmax',
                      'soil_water', 'soil_texture'))
# convert NPP to g/m2 (see above explanation)... gives g/m2
res$NPP_sum <- res$NPP_sum * 1800 / 1000000 * 12.011

# loop through res and create trait x trait matrix of NPP values
df <- res
treat_list <- colnames(df)[c(3:ncol(df))]; treat_list
woo <- c()
woo2 <- c()
name1 <- c()
for(i in 1:length(treat_list)) {
  for(j in 1:length(treat_list)) {    # change number here
    # i = 1; j = 2
    if(i != j) { # need this so i and j are never the same trait
      crap <- subset(df, select=c('NPP_sum', treat_list[i], treat_list[j])); crap
      sum1 <- summaryBy(NPP_sum ~ ., data=crap, FUN=mean, keep.names = TRUE); sum1
      sum2 <- c(sum1[1,3]); sum2
      sum2 <- c(sum2, sum1[2,3]); sum2
      sum3 <- c(sum1[3,3]); sum3
      sum3 <- c(sum3, sum1[4,3]); sum3
      all <- rbind(sum2, sum3); all
      colnames(all) <- c(paste(treat_list[j], sum1[1,2], sep="_"),
                         paste(treat_list[j], sum1[2,2], sep="_")); all
      all <- as.data.frame(all, row.names = FALSE); all
      name1 <- c(paste(treat_list[i], sum1[1,1,], sep="_"),
                 paste(treat_list[i], sum1[3,1,], sep="_")); name1
      woo <- as.data.frame(c(woo, all)); woo
    }
  }
  rownames(woo) <- name1; woo
  woo$crap1<-NA; woo$crap2<-NA; woo
  colnames(woo)[colnames(woo)=="crap1"] <- name1[1]; woo
  colnames(woo)[colnames(woo)=="crap2"] <- name1[2]; woo
  
  woo[,order(names(woo))]
  woo2 <- rbind(woo2, woo); woo2
  woo <- c()
}  

library("pheatmap")
#df <- as.data.frame(scale(woo2)); df
df <- as.data.frame(woo2); head(df)

pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/yu_wet_npp_heatmap.pdf", height=5, width=6)  # 10, 14  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
# plot heatmap
pheatmap(df, color = colorRampPalette(c("white", "firebrick3"))(50),
         cuttree_rows=4, main=expression("High Plains (Wet) NPP (g " * m^-2 * ")")) 
dev.off()



# yu_dry
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='yu_dry'); nrow(res) 
res <- summaryBy(NPP_sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI', 'vpmax',
                      'soil_water', 'soil_texture'))
# convert NPP to g/m2 (see above explanation)... gives g/m2
res$NPP_sum <- res$NPP_sum * 1800 / 1000000 * 12.011

# loop through res and create trait x trait matrix of NPP values
df <- res
treat_list <- colnames(df)[c(3:ncol(df))]; treat_list
woo <- c()
woo2 <- c()
name1 <- c()
for(i in 1:length(treat_list)) {
  for(j in 1:length(treat_list)) {    # change number here
    # i = 1; j = 2
    if(i != j) { # need this so i and j are never the same trait
      crap <- subset(df, select=c('NPP_sum', treat_list[i], treat_list[j])); crap
      sum1 <- summaryBy(NPP_sum ~ ., data=crap, FUN=mean, keep.names = TRUE); sum1
      sum2 <- c(sum1[1,3]); sum2
      sum2 <- c(sum2, sum1[2,3]); sum2
      sum3 <- c(sum1[3,3]); sum3
      sum3 <- c(sum3, sum1[4,3]); sum3
      all <- rbind(sum2, sum3); all
      colnames(all) <- c(paste(treat_list[j], sum1[1,2], sep="_"),
                         paste(treat_list[j], sum1[2,2], sep="_")); all
      all <- as.data.frame(all, row.names = FALSE); all
      name1 <- c(paste(treat_list[i], sum1[1,1,], sep="_"),
                 paste(treat_list[i], sum1[3,1,], sep="_")); name1
      woo <- as.data.frame(c(woo, all)); woo
    }
  }
  rownames(woo) <- name1; woo
  woo$crap1<-NA; woo$crap2<-NA; woo
  colnames(woo)[colnames(woo)=="crap1"] <- name1[1]; woo
  colnames(woo)[colnames(woo)=="crap2"] <- name1[2]; woo
  
  woo[,order(names(woo))]
  woo2 <- rbind(woo2, woo); woo2
  woo <- c()
}  

library("pheatmap")
#df <- as.data.frame(scale(woo2)); df
df <- as.data.frame(woo2); head(df)

pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/yu_dry_npp_heatmap.pdf", height=5, width=6)  # 10, 14  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
# plot heatmap
pheatmap(df, color = colorRampPalette(c("white", "firebrick3"))(50),
         cuttree_rows=4, main=expression("High Plains (Dry) NPP (g " * m^-2 * ")"))
dev.off()



# irrigated
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='irrigated'); nrow(res) 
res <- summaryBy(NPP_sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI', 'vpmax',
                      'soil_water', 'soil_texture'))
# convert NPP to g/m2 (see above explanation)... gives g/m2
res$NPP_sum <- res$NPP_sum * 1800 / 1000000 * 12.011

# loop through res and create trait x trait matrix of NPP values
df <- res
treat_list <- colnames(df)[c(3:ncol(df))]; treat_list
woo <- c()
woo2 <- c()
name1 <- c()
for(i in 1:length(treat_list)) {
  for(j in 1:length(treat_list)) {    # change number here
    # i = 1; j = 2
    if(i != j) { # need this so i and j are never the same trait
      crap <- subset(df, select=c('NPP_sum', treat_list[i], treat_list[j])); crap
      sum1 <- summaryBy(NPP_sum ~ ., data=crap, FUN=mean, keep.names = TRUE); sum1
      sum2 <- c(sum1[1,3]); sum2
      sum2 <- c(sum2, sum1[2,3]); sum2
      sum3 <- c(sum1[3,3]); sum3
      sum3 <- c(sum3, sum1[4,3]); sum3
      all <- rbind(sum2, sum3); all
      colnames(all) <- c(paste(treat_list[j], sum1[1,2], sep="_"),
                         paste(treat_list[j], sum1[2,2], sep="_")); all
      all <- as.data.frame(all, row.names = FALSE); all
      name1 <- c(paste(treat_list[i], sum1[1,1,], sep="_"),
                 paste(treat_list[i], sum1[3,1,], sep="_")); name1
      woo <- as.data.frame(c(woo, all)); woo
    }
  }
  rownames(woo) <- name1; woo
  woo$crap1<-NA; woo$crap2<-NA; woo
  colnames(woo)[colnames(woo)=="crap1"] <- name1[1]; woo
  colnames(woo)[colnames(woo)=="crap2"] <- name1[2]; woo
  
  woo[,order(names(woo))]
  woo2 <- rbind(woo2, woo); woo2
  woo <- c()
}  

library("pheatmap")
#df <- as.data.frame(scale(woo2)); df
df <- as.data.frame(woo2); head(df)

pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/irrigated_npp_heatmap.pdf", height=5, width=6)  # 10, 14  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
# plot heatmap
pheatmap(df, color = colorRampPalette(c("white", "firebrick3"))(50),
         cuttree_rows=4, main=expression("Irrigated NPP (g " * m^-2 * ")"))
dev.off()





###__________ reproduction HEATMAPS __________###
###________ Reproduction heatmaps _____________###
# bulid function to make NPP matrix to give pheatmap

# mo_wet
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='mo_wet'); nrow(res) 
treat <- paste(unique(res$climate))
res <- summaryBy(repro_max ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI', 'vpmax',
                      'soil_water', 'soil_texture'))
# convert reproduction to g/m2 (see above for explanation)
res$repro_max <- res$repro_max / 10000 * 1000

# loop through res and create trait x trait matrix of NPP values
df <- res
treat_list <- colnames(df)[c(3:ncol(df))]; treat_list
woo <- c()
woo2 <- c()
name1 <- c()
for(i in 1:length(treat_list)) {
  for(j in 1:length(treat_list)) {    # change number here
    # i = 1; j = 2
    if(i != j) { # need this so i and j are never the same trait
      crap <- subset(df, select=c('repro_max', treat_list[i], treat_list[j])); crap
      sum1 <- summaryBy(repro_max ~ ., data=crap, FUN=mean, keep.names = TRUE); sum1
      sum2 <- c(sum1[1,3]); sum2
      sum2 <- c(sum2, sum1[2,3]); sum2
      sum3 <- c(sum1[3,3]); sum3
      sum3 <- c(sum3, sum1[4,3]); sum3
      all <- rbind(sum2, sum3); all
      colnames(all) <- c(paste(treat_list[j], sum1[1,2], sep="_"),
                         paste(treat_list[j], sum1[2,2], sep="_")); all
      all <- as.data.frame(all, row.names = FALSE); all
      name1 <- c(paste(treat_list[i], sum1[1,1,], sep="_"),
                 paste(treat_list[i], sum1[3,1,], sep="_")); name1
      woo <- as.data.frame(c(woo, all)); woo
    }
  }
  rownames(woo) <- name1; woo
  woo$crap1<-NA; woo$crap2<-NA; woo
  colnames(woo)[colnames(woo)=="crap1"] <- name1[1]; woo
  colnames(woo)[colnames(woo)=="crap2"] <- name1[2]; woo
  
  woo[,order(names(woo))]
  woo2 <- rbind(woo2, woo); woo2
  woo <- c()
}  

library("pheatmap")
#df <- as.data.frame(scale(woo2)); df
df <- as.data.frame(woo2); head(df)
pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/mo_wet_repro_heatmap.pdf", height=5, width=6)  # 10, 14  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
# plot heatmap
pheatmap(df, color = colorRampPalette(c("white", "firebrick3"))(50),
         cuttree_rows=4, main=expression("Central Plains (Wet) Reproduction (g " * m^-2 * ")")) 
dev.off()



# mo_dry
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='mo_dry'); nrow(res) 
treat <- paste(unique(res$climate))
res <- summaryBy(repro_max ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI', 'vpmax',
                      'soil_water', 'soil_texture'))
# convert reproduction to g/m2 (see above for explanation)
res$repro_max <- res$repro_max / 10000 * 1000

# loop through res and create trait x trait matrix of NPP values
df <- res
treat_list <- colnames(df)[c(3:ncol(df))]; treat_list
woo <- c()
woo2 <- c()
name1 <- c()
for(i in 1:length(treat_list)) {
  for(j in 1:length(treat_list)) {    # change number here
    # i = 1; j = 2
    if(i != j) { # need this so i and j are never the same trait
      crap <- subset(df, select=c('repro_max', treat_list[i], treat_list[j])); crap
      sum1 <- summaryBy(repro_max ~ ., data=crap, FUN=mean, keep.names = TRUE); sum1
      sum2 <- c(sum1[1,3]); sum2
      sum2 <- c(sum2, sum1[2,3]); sum2
      sum3 <- c(sum1[3,3]); sum3
      sum3 <- c(sum3, sum1[4,3]); sum3
      all <- rbind(sum2, sum3); all
      colnames(all) <- c(paste(treat_list[j], sum1[1,2], sep="_"),
                         paste(treat_list[j], sum1[2,2], sep="_")); all
      all <- as.data.frame(all, row.names = FALSE); all
      name1 <- c(paste(treat_list[i], sum1[1,1,], sep="_"),
                 paste(treat_list[i], sum1[3,1,], sep="_")); name1
      woo <- as.data.frame(c(woo, all)); woo
    }
  }
  rownames(woo) <- name1; woo
  woo$crap1<-NA; woo$crap2<-NA; woo
  colnames(woo)[colnames(woo)=="crap1"] <- name1[1]; woo
  colnames(woo)[colnames(woo)=="crap2"] <- name1[2]; woo
  
  woo[,order(names(woo))]
  woo2 <- rbind(woo2, woo); woo2
  woo <- c()
}  

library("pheatmap")
#df <- as.data.frame(scale(woo2)); df
df <- as.data.frame(woo2); head(df)
pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/mo_dry_repro_heatmap.pdf", height=5, width=6)  # 10, 14  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
# plot heatmap
pheatmap(df, color = colorRampPalette(c("white", "firebrick3"))(50),
         cuttree_rows=4, main=expression("Central Plains (Dry) Reproduction (g " * m^-2 * ")")) 
dev.off()



# yu_wet
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='yu_wet'); nrow(res) 
treat <- paste(unique(res$climate))
res <- summaryBy(repro_max ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI', 'vpmax',
                      'soil_water', 'soil_texture'))
# convert reproduction to g/m2 (see above for explanation)
res$repro_max <- res$repro_max / 10000 * 1000

# loop through res and create trait x trait matrix of NPP values
df <- res
treat_list <- colnames(df)[c(3:ncol(df))]; treat_list
woo <- c()
woo2 <- c()
name1 <- c()
for(i in 1:length(treat_list)) {
  for(j in 1:length(treat_list)) {    # change number here
    # i = 1; j = 2
    if(i != j) { # need this so i and j are never the same trait
      crap <- subset(df, select=c('repro_max', treat_list[i], treat_list[j])); crap
      sum1 <- summaryBy(repro_max ~ ., data=crap, FUN=mean, keep.names = TRUE); sum1
      sum2 <- c(sum1[1,3]); sum2
      sum2 <- c(sum2, sum1[2,3]); sum2
      sum3 <- c(sum1[3,3]); sum3
      sum3 <- c(sum3, sum1[4,3]); sum3
      all <- rbind(sum2, sum3); all
      colnames(all) <- c(paste(treat_list[j], sum1[1,2], sep="_"),
                         paste(treat_list[j], sum1[2,2], sep="_")); all
      all <- as.data.frame(all, row.names = FALSE); all
      name1 <- c(paste(treat_list[i], sum1[1,1,], sep="_"),
                 paste(treat_list[i], sum1[3,1,], sep="_")); name1
      woo <- as.data.frame(c(woo, all)); woo
    }
  }
  rownames(woo) <- name1; woo
  woo$crap1<-NA; woo$crap2<-NA; woo
  colnames(woo)[colnames(woo)=="crap1"] <- name1[1]; woo
  colnames(woo)[colnames(woo)=="crap2"] <- name1[2]; woo
  
  woo[,order(names(woo))]
  woo2 <- rbind(woo2, woo); woo2
  woo <- c()
}  

library("pheatmap")
#df <- as.data.frame(scale(woo2)); df
df <- as.data.frame(woo2); head(df)
pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/yu_wet_repro_heatmap.pdf", height=5, width=6)  # 10, 14  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
# plot heatmap
pheatmap(df, color = colorRampPalette(c("white", "firebrick3"))(50),
         cuttree_rows=4, main=expression("High Plains (Wet) Reproduction (g " * m^-2 * ")")) 
dev.off()



# yu_dry
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='yu_dry'); nrow(res) 
treat <- paste(unique(res$climate))
res <- summaryBy(repro_max ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI', 'vpmax',
                      'soil_water', 'soil_texture'))
# convert reproduction to g/m2 (see above for explanation)
res$repro_max <- res$repro_max / 10000 * 1000

# loop through res and create trait x trait matrix of NPP values
df <- res
treat_list <- colnames(df)[c(3:ncol(df))]; treat_list
woo <- c()
woo2 <- c()
name1 <- c()
for(i in 1:length(treat_list)) {
  for(j in 1:length(treat_list)) {    # change number here
    # i = 1; j = 2
    if(i != j) { # need this so i and j are never the same trait
      crap <- subset(df, select=c('repro_max', treat_list[i], treat_list[j])); crap
      sum1 <- summaryBy(repro_max ~ ., data=crap, FUN=mean, keep.names = TRUE); sum1
      sum2 <- c(sum1[1,3]); sum2
      sum2 <- c(sum2, sum1[2,3]); sum2
      sum3 <- c(sum1[3,3]); sum3
      sum3 <- c(sum3, sum1[4,3]); sum3
      all <- rbind(sum2, sum3); all
      colnames(all) <- c(paste(treat_list[j], sum1[1,2], sep="_"),
                         paste(treat_list[j], sum1[2,2], sep="_")); all
      all <- as.data.frame(all, row.names = FALSE); all
      name1 <- c(paste(treat_list[i], sum1[1,1,], sep="_"),
                 paste(treat_list[i], sum1[3,1,], sep="_")); name1
      woo <- as.data.frame(c(woo, all)); woo
    }
  }
  rownames(woo) <- name1; woo
  woo$crap1<-NA; woo$crap2<-NA; woo
  colnames(woo)[colnames(woo)=="crap1"] <- name1[1]; woo
  colnames(woo)[colnames(woo)=="crap2"] <- name1[2]; woo
  
  woo[,order(names(woo))]
  woo2 <- rbind(woo2, woo); woo2
  woo <- c()
}  

library("pheatmap")
#df <- as.data.frame(scale(woo2)); df
df <- as.data.frame(woo2); head(df)
pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/yu_dry_repro_heatmap.pdf", height=5, width=6)  # 10, 14  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
# plot heatmap
pheatmap(df, color = colorRampPalette(c("white", "firebrick3"))(50),
         cuttree_rows=4, main=expression("High Plains (Dry) Reproduction (g " * m^-2 * ")")) 
dev.off()



# irrigated
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='irrigated'); nrow(res) 
treat <- paste(unique(res$climate))
res <- summaryBy(repro_max ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI', 'vpmax',
                      'soil_water', 'soil_texture'))
# convert reproduction to g/m2 (see above for explanation)
res$repro_max <- res$repro_max / 10000 * 1000

# loop through res and create trait x trait matrix of NPP values
df <- res
treat_list <- colnames(df)[c(3:ncol(df))]; treat_list
woo <- c()
woo2 <- c()
name1 <- c()
for(i in 1:length(treat_list)) {
  for(j in 1:length(treat_list)) {    # change number here
    # i = 1; j = 2
    if(i != j) { # need this so i and j are never the same trait
      crap <- subset(df, select=c('repro_max', treat_list[i], treat_list[j])); crap
      sum1 <- summaryBy(repro_max ~ ., data=crap, FUN=mean, keep.names = TRUE); sum1
      sum2 <- c(sum1[1,3]); sum2
      sum2 <- c(sum2, sum1[2,3]); sum2
      sum3 <- c(sum1[3,3]); sum3
      sum3 <- c(sum3, sum1[4,3]); sum3
      all <- rbind(sum2, sum3); all
      colnames(all) <- c(paste(treat_list[j], sum1[1,2], sep="_"),
                         paste(treat_list[j], sum1[2,2], sep="_")); all
      all <- as.data.frame(all, row.names = FALSE); all
      name1 <- c(paste(treat_list[i], sum1[1,1,], sep="_"),
                 paste(treat_list[i], sum1[3,1,], sep="_")); name1
      woo <- as.data.frame(c(woo, all)); woo
    }
  }
  rownames(woo) <- name1; woo
  woo$crap1<-NA; woo$crap2<-NA; woo
  colnames(woo)[colnames(woo)=="crap1"] <- name1[1]; woo
  colnames(woo)[colnames(woo)=="crap2"] <- name1[2]; woo
  
  woo[,order(names(woo))]
  woo2 <- rbind(woo2, woo); woo2
  woo <- c()
}  

library("pheatmap")
#df <- as.data.frame(scale(woo2)); df
df <- as.data.frame(woo2); head(df)
pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/irrigated_repro_heatmap.pdf", height=5, width=6)  # 10, 14  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
# plot heatmap
pheatmap(df, color = colorRampPalette(c("white", "firebrick3"))(50),
         cuttree_rows=4, main=expression("Irrigated Reproduction (g " * m^-2 * ")")) 
dev.off()






###___ start here to plot the random forest importance figure
###___ fit random forest model to each climate scenario and create importance table___###
# subset dataset as wanted...
library(randomForest)

# mo_wet random forest
res <- subset(comb, climate=="mo_wet"); nrow(res) 
res$climate <- factor(res$climate) # removes unused levels
res <- summaryBy(NPP.sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI',
                      'vpmax', 'soil_water', 'soil_texture'))
# remove "simulation column
res <- subset(res, select=-c(simulation)); nrow(res)
set.seed(101)  # turn this off if you want the results to vary bewteen runs
train = sample(1:nrow(res), 200) # default = 130
rf <- randomForest(NPP.sum~., data = res, subset = train, importance=TRUE, mtry=4, ntree=3)
print(rf)
# importance(rf)
# varImpPlot(rf)

# loop through randomForest, calculate one tree each time and save importance values (only way to get entire importance distribution)
crap <- c()
for(i in 1:350) {
  # i=2
  rf_res = randomForest(NPP.sum~., data = res, subset = train, importance=TRUE,
                        mtry=4, ntree=20)
  imp <- importance(rf_res); imp <- as.data.frame( t(imp[,1])); imp  
  # imp <- as.data.frame( t(subset(rf_res$importance, select=-c(IncNodePurity))) ); imp
  imp$iteration <- i
  crap <- rbind.data.frame(crap, imp); crap
}
imp_dist_mo_wet <- crap
min(imp_dist_mo_wet$gs_sensitivity)
max(imp_dist_mo_wet$gs_sensitivity)
mean(imp_dist_mo_wet$gs_sensitivity)


# mo_dry random forest
res <- subset(comb, climate=="mo_dry"); nrow(res) 
res$climate <- factor(res$climate) # removes unused levels
res <- summaryBy(NPP.sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI',
                      'vpmax', 'soil_water', 'soil_texture'))
# remove "simulation column
res <- subset(res, select=-c(simulation)); nrow(res)
set.seed(101)  # turn this off if you want the results to vary bewteen runs
train = sample(1:nrow(res), 200)
# loop through randomForest, calculate one tree each time and save importance values (only way to get entire importance distribution)
crap <- c()
for(i in 1:350) {
  rf_res = randomForest(NPP.sum~., data = res, subset = train, importance=TRUE,
                        mtry=4, ntree=20)
  imp <- importance(rf_res); imp <- as.data.frame( t(imp[,1])); imp  
  # imp <- as.data.frame( t(subset(rf_res$importance, select=-c(IncNodePurity))) )
  imp$iteration <- i
  crap <- rbind.data.frame(crap, imp)
}
imp_dist_mo_dry <- crap


# yu_wet random forest
res <- subset(comb, climate=="yu_wet"); nrow(res) 
res$climate <- factor(res$climate) # removes unused levels
res <- summaryBy(NPP.sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI',
                      'vpmax', 'soil_water', 'soil_texture'))
# remove "simulation column
res <- subset(res, select=-c(simulation)); nrow(res)
set.seed(101)  # turn this off if you want the results to vary bewteen runs
train = sample(1:nrow(res), 200)
# loop through randomForest, calculate one tree each time and save importance values (only way to get entire importance distribution)
crap <- c()
for(i in 1:350) {
  rf_res = randomForest(NPP.sum~., data = res, subset = train, importance=TRUE,
                        mtry=4, ntree=20)
  imp <- importance(rf_res); imp <- as.data.frame( t(imp[,1])); imp  
  # imp <- as.data.frame( t(subset(rf_res$importance, select=-c(IncNodePurity))) )
  imp$iteration <- i
  crap <- rbind.data.frame(crap, imp)
}
imp_dist_yu_wet <- crap


# yu_dry random forest
res <- subset(comb, climate=="yu_dry"); nrow(res) 
res$climate <- factor(res$climate) # removes unused levels
res <- summaryBy(NPP.sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI',
                      'vpmax', 'soil_water', 'soil_texture'))
# remove "simulation column
res <- subset(res, select=-c(simulation)); nrow(res)
set.seed(101)  # turn this off if you want the results to vary bewteen runs
train = sample(1:nrow(res), 200)
# loop through randomForest, calculate one tree each time and save importance values (only way to get entire importance distribution)
crap <- c()
for(i in 1:350) {
  rf_res = randomForest(NPP.sum~., data = res, subset = train, importance=TRUE,
                        mtry=4, ntree=20)
  imp <- importance(rf_res); imp <- as.data.frame( t(imp[,1])); imp  
  # imp <- as.data.frame( t(subset(rf_res$importance, select=-c(IncNodePurity))) )
  imp$iteration <- i
  crap <- rbind.data.frame(crap, imp)
}
imp_dist_yu_dry <- crap


# irrigated random forest
res <- subset(comb, climate=="irrigated"); nrow(res) 
res$climate <- factor(res$climate) # removes unused levels
res <- summaryBy(NPP.sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'max_LAI',
                      'vpmax', 'soil_water', 'soil_texture'))
# remove "simulation column
res <- subset(res, select=-c(simulation)); nrow(res)
set.seed(101)  # turn this off if you want the results to vary bewteen runs
train = sample(1:nrow(res), 200)
# loop through randomForest, calculate one tree each time and save importance values (only way to get entire importance distribution)
crap <- c()
for(i in 1:350) {
  rf_res = randomForest(NPP.sum~., data = res, subset = train, importance=TRUE,
                        mtry=4, ntree=20)
  imp <- importance(rf_res); imp <- as.data.frame( t(imp[,1])); imp  
  # imp <- as.data.frame( t(subset(rf_res$importance, select=-c(IncNodePurity))) )
  imp$iteration <- i
  crap <- rbind.data.frame(crap, imp)
}
imp_dist_irrigated <- crap


# checking data
nrow(imp_dist_mo_wet)
nrow(imp_dist_mo_dry)
nrow(imp_dist_yu_wet)
nrow(imp_dist_yu_dry)
nrow(imp_dist_irrigated)



###________ plot importance figure ___________###
library(Hmisc)

# plot pdf
pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/importance.pdf", 
    height=3.9, width=7.6)  # 3.5, 7.6  

# create blank plot
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.1, 0.1, 0.1, 0.1), mgp=c(0, 0, 0)) 
plot(c(1:8), c(1:8), col="white", axes=FALSE, xlab="", ylab="")

# make dataframe list
df_list <- list(imp_dist_mo_wet, imp_dist_mo_dry, imp_dist_yu_wet,
                imp_dist_yu_dry, imp_dist_irrigated)

# plot loop
for(i in 1:length(df_list)) {
  # i = 1
  df <- as.data.frame(df_list[[i]]); head(df); class(df)
  df <- subset(df, select=-c(iteration)) # get rid of last "iteration" col
  min_val <- min(df); max_val <- max(df)
  
  for(j in 1:ncol(df)) {
    # j = 1
    df2 <- df[,j]; head(df2)
    
    myfunction <- function(){
      par(mgp=c(1, -0.1, 0), new = TRUE) 
      boxplot(df2, notch=TRUE, 
              col=c('red'), horizontal=TRUE, outline=FALSE,
              axes=FALSE, ylim=c(min_val, max_val)); box()
      
      # add verticle background lines
      # crud <- max_val - min_val
      # abline(v=(min_val + crud*0.5), col="gray70", lty=3)
      # abline(v=(min_val + crud*0.335), col="gray70", lty=3)
      # abline(v=(min_val + crud*0.665), col="gray70", lty=3)
      # abline(v=(min_val + crud*0.165), col="gray70", lty=3)
      # abline(v=(min_val + crud*0.830), col="gray70", lty=3)
      abline(v=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), col="gray70", lty=3)

      # plot again over the background lines
      boxplot(df2, notch=TRUE, 
              col=c('red'), horizontal=TRUE, outline=FALSE,
              axes=FALSE, ylim=c(min_val, max_val), add=TRUE); box()
      
      # add x axis label to bottom most row of plots (soil texture)
      if(j==8) {
        boxplot(df2, notch=TRUE, 
                col=c('red'), horizontal=TRUE, outline=FALSE,
                axes=FALSE, ylim=c(min_val, max_val), add=TRUE); box()
        if(max_val < 25) {
          axis(1, at = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), cex.axis=0.5, tck=-0.15)
        } else {
          axis(1, at = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), cex.axis=0.5, tck=-0.15)
        }
        mtext("% increase MSE", side=1, cex=0.7, padj=1.3)
      }

    }

    subplot(myfunction, par("usr")[2], par("usr")[4], vadj=1.5+j, hadj=6-i, size=c(1.20, 0.35))
  }  
}  

# add treatment and soil trait labels
crap <- colnames(imp_dist_irrigated); crap
crap <- c("Stomatal sensitivity", "Hydraulic efficiency",
          "Hydraulic safety", "Root depth", "Maximum LAI",
          "Vpmax", "Initial soil water", "Soil Texture")

# make the "from" smaller to squish all text downward
seq1 <- seq(from=6.85, to=1.9, length=8); seq1 # 6.7  1
text_cex <- 0.9

text(x=0.75, y=seq1[1], crap[1], adj=0, cex=text_cex)
text(x=0.75, y=seq1[2], crap[2], adj=0, cex=text_cex)
text(x=0.75, y=seq1[3], crap[3], adj=0, cex=text_cex)
text(x=0.75, y=seq1[4], crap[4], adj=0, cex=text_cex)
text(x=0.75, y=seq1[5], crap[5], adj=0, cex=text_cex)
text(x=0.75, y=seq1[6], crap[6], adj=0, cex=text_cex)
text(x=0.75, y=seq1[7], crap[7], adj=0, cex=text_cex)
text(x=0.75, y=seq1[8], crap[8], adj=0, cex=text_cex)

# add climate scenario labels across the top of the plot
text_cex <- 0.9; text_angle=0

# make "to" smaller to squish towards the y axis
x_seq <- seq(from=2.26, to=7.35, length=5); x_seq
y_pos_1 <- 7.9; y_pos_2 <- 7.5  

text(x=x_seq[1], y=y_pos_1, "Central Plains", adj=0, cex=text_cex, srt=text_angle)
  text(x=x_seq[1]+0.3, y=y_pos_2, "Wet", adj=0, cex=text_cex, srt=text_angle)
  
text(x=x_seq[2], y=y_pos_1, "Central Plains", adj=0, cex=text_cex, srt=text_angle)
  text(x=x_seq[2]+0.3, y=y_pos_2, "Dry", adj=0, cex=text_cex, srt=text_angle)  
  
text(x=x_seq[3], y=y_pos_1, "High Plains", adj=0, cex=text_cex, srt=text_angle)
  text(x=x_seq[3]+0.3, y=y_pos_2, "Wet", adj=0, cex=text_cex, srt=text_angle)  
  
text(x=x_seq[4], y=y_pos_1, "High Plains", adj=0, cex=text_cex, srt=text_angle)
  text(x=x_seq[4]+0.3, y=y_pos_2, "Dry", adj=0, cex=text_cex, srt=text_angle) 
  
text(x=x_seq[5], y=y_pos_1, "Irrigated", adj=0, cex=text_cex, srt=text_angle)
  
dev.off()




###________ plot climate figure ___________###
library(Hmisc)
library(doBy)

# import climate data
mo_wet_clim <- read.table("/home/sean/sean_stuff/c_stuff/trees/TREES311_Jan_2020/sean_files/sean_v4/mo_wet_clim.txt", header=TRUE)
mo_dry_clim <- read.table("/home/sean/sean_stuff/c_stuff/trees/TREES311_Jan_2020/sean_files/sean_v4/mo_dry_clim.txt", header=TRUE)
yu_wet_clim <- read.table("/home/sean/sean_stuff/c_stuff/trees/TREES311_Jan_2020/sean_files/sean_v4/yu_wet_clim.txt", header=TRUE)
yu_dry_clim <- read.table("/home/sean/sean_stuff/c_stuff/trees/TREES311_Jan_2020/sean_files/sean_v4/yu_dry_clim.txt", header=TRUE)
irrigated_clim <- read.table("/home/sean/sean_stuff/c_stuff/trees/TREES311_Jan_2020/sean_files/sean_v4/fully_irrigated.txt", header=TRUE)

# make dataframe list
df_list <- list(mo_wet_clim, mo_dry_clim, yu_wet_clim, yu_dry_clim, irrigated_clim)

# determin max and min values for all x and y axes
max_precip <- 160
min_precip <- 0

max_temp <- max(mo_wet_clim$t_ref, mo_dry_clim$t_ref, yu_wet_clim$t_ref, 
                yu_dry_clim$t_ref, irrigated_clim$t_ref); max_temp
min_temp <- min(mo_wet_clim$t_ref, mo_dry_clim$t_ref, yu_wet_clim$t_ref, 
                yu_dry_clim$t_ref, irrigated_clim$t_ref); min_temp

max_vpd <- max(mo_wet_clim$d_canopy, mo_dry_clim$d_canopy, yu_wet_clim$d_canopy, 
               yu_dry_clim$d_canopy, irrigated_clim$d_canopy); max_vpd
min_vpd <- 0

max_par <- max(mo_wet_clim$Qpar, mo_dry_clim$Qpar, yu_wet_clim$Qpar, 
               yu_dry_clim$Qpar, irrigated_clim$Qpar); max_par
min_par <- 0

max_val <- c(max_precip, max_temp, max_vpd, max_par)
min_val <- c(min_precip, min_temp, min_vpd, min_par)


# plot pdf
pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/climate.pdf", 
    height=4, width=7.6)  # 4, 8  

# create blank plot
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.1, 0.1, 0.1, 0.1), mgp=c(0, 0, 0)) 
plot(c(1:8), c(1:8), col="white", axes=FALSE, xlab="", ylab="")

# plot loop
for(i in 1:length(df_list)) {
  # i = 1
  df <- as.data.frame(df_list[[i]]); head(df); class(df)
  df <- subset(df, select=c(Date, precip, t_ref, d_canopy, Qpar))
  df$day <- substr(df$Date, 5, 7)
  df_max <- summaryBy(t_ref + d_canopy + Qpar ~ day, data=df, FUN=max, na.rm=TRUE)
  df_sum <- summaryBy(precip ~ day, data=df, FUN=sum, na.rm=TRUE)
  df_comb <- cbind.data.frame(df_sum, df_max[,2:4])
  colnames(df_comb) <- c("day", "precip", "temp", "vpd", "par")
  df_comb$precip <- ifelse(df_comb$precip==0, NA, df_comb$precip)
  # change "day" to POSIXct time
  crap <- df_comb$day
  crap <- "1/1/2018"
  crap <- as.POSIXct(crap, format = "%m/%e/%Y")
  crap <- crap + as.numeric(df_comb$day) * 24 * 60 * 60
  df_comb$day <- crap
  
  # loop within each climate df
  for(j in 2:ncol(df_comb)) {
    # j = 2
    df2 <- df_comb[,c(1,j)]; head(df2)
    
    myfunction <- function(){
      if(i==1) {
        par(mgp=c(0, 0.3, 0), new = TRUE) 
        plot(df2[,1], df2[,2], axes=FALSE, pch=21, bg="red", xlab="", ylab="",
             ylim=c(min_val[j-1], max_val[j-1]),
             cex=0.5); box()
             axis(side=2, cex.axis=0.4, tck=-0.05, las=2)
             
        # add cumulative precip column if J==2 (precip)
        if(j==2) {
          df2$cum_precip <- ifelse(is.na(df2$precip)==TRUE, 0, df2$precip)  # NA = 0
          df2$cum_precip <- cumsum(df2$cum_precip)     
          # add cumulative precip to plot
          points(df2[,1], df2[,3]/13, pch=21, bg="red", xlab="", ylab="",
               ylim=c(0, 1500), cex=0.5, type="l"); box()
          mtext(paste("total =", round(max(df2$cum_precip, 0)), "mm"), side=1, adj=0.95, padj=-10, cex=0.4)
        }

        if(j==2) { 
          abline(h=c(0,50,100,150), col="gray70", lty=3) 
        } else if(j==3) {
          abline(h=c(0,10,20,30), col="gray70", lty=3)
        } else if(j==4) {
          abline(h=c(0,1,2,3,4,5), col="gray70", lty=3)
        } else if(j==5) {
          abline(h=c(0,500,1000,1500,2000), col="gray70", lty=3)
        }  
      }
      
      if(j==5) {
        par(mgp=c(0, -0.2, 0), new = TRUE) 
        plot(df2[,1], df2[,2], axes=FALSE, pch=21, bg="red", xlab="", ylab="",
             ylim=c(min_val[j-1], max_val[j-1]),
             cex=0.5); box()
        axis.POSIXct(1, df2[,1], cex.axis=0.4, tck=-0.05)
        abline(h=c(0,500,1000,1500,2000), col="gray70", lty=3)
      }
      
      if(j==2) {
        par(mgp=c(0, 0, 0), new = TRUE) 
        plot(df2[,1], df2[,2], axes=FALSE, pch=21, bg="red", xlab="", ylab="",
             ylim=c(min_val[j-1], max_val[j-1]),
             cex=0.5); box()
          abline(h=c(0,50,100,150), col="gray70", lty=3) 
        # calculate cumulative precip
        df2$cum_precip <- ifelse(is.na(df2$precip)==TRUE, 0, df2$precip)  # NA = 0
        df2$cum_precip <- cumsum(df2$cum_precip)     
        # add cumulative precip to plot
        points(df2[,1], df2[,3]/13, axes=FALSE, pch=21, bg="red", xlab="", ylab="",
               ylim=c(0, 1500), cex=0.5, type="l"); box()
        # add cumulative precip labels (if not central plains wet)
        if(i==2) { mtext(paste("total =", round(max(df2$cum_precip, 0)), "mm"), side=1, adj=0.95, padj=-7.5, cex=0.4) }
        if(i==3) { mtext(paste("total =", round(max(df2$cum_precip, 0)), "mm"), side=1, adj=0.95, padj=-9.0, cex=0.4) }
        if(i==4) { mtext(paste("total =", round(max(df2$cum_precip, 0)), "mm"), side=1, adj=0.95, padj=-7.0, cex=0.4) }
        if(i==5) { mtext(paste("total =", round(max(df2$cum_precip, 0)), "mm"), side=1, adj=0.95, padj=-10.0, cex=0.4) }
      }
      
      if(i!=1 & j!=5 & j!=2) {
        par(mgp=c(0, 0, 0), new = TRUE) 
        plot(df2[,1], df2[,2], axes=FALSE, pch=21, bg="red", xlab="", ylab="",
             ylim=c(min_val[j-1], max_val[j-1]),
             cex=0.5); box()
        if(j==2) { 
          abline(h=c(0,50,100,150), col="gray70", lty=3) 
        } else if(j==3) {
          abline(h=c(0,10,20,30), col="gray70", lty=3)
        } else if(j==4) {
          abline(h=c(0,1,2,3,4,5), col="gray70", lty=3)
        } 
      }
      
    }

    subplot(myfunction, par("usr")[2], par("usr")[4], vadj=-0.4+j, hadj=6-i, size=c(1.20, 0.80))
  }
}


# add climate trait labels
# make the "from" smaller to squish all text downward
seq1 <- seq(from=6.6, to=1.8, length=4); seq1
text_cex <- 0.70

text(x=0.75, y=seq1[1], "Precipitation (mm)", adj=0, cex=text_cex)
text(x=0.75, y=seq1[2], expression("Temperature (" * C * ~degree * ")"), 
     adj=0, cex=text_cex)
text(x=0.75, y=seq1[3], "VPD (kPa)", adj=0, cex=text_cex)
text(x=0.75, y=seq1[4], expression("PAR (mmol " * m^-2 * " " * s-2 * ")"), 
     adj=0, cex=text_cex)

# add climate scenario labels across the top of the plot
text_cex <- 0.8; text_angle=0

# make "to" smaller to squish towards the y axis
x_seq <- seq(from=2.30, to=7.35, length=5); x_seq
y_pos_1 <- 8.0; y_pos_2 <- 7.6  

text(x=x_seq[1], y=y_pos_1, "Central Plains", adj=0, cex=text_cex, srt=text_angle)
text(x=x_seq[1]+0.3, y=y_pos_2, "Wet", adj=0, cex=text_cex, srt=text_angle)

text(x=x_seq[2], y=y_pos_1, "Central Plains", adj=0, cex=text_cex, srt=text_angle)
text(x=x_seq[2]+0.3, y=y_pos_2, "Dry", adj=0, cex=text_cex, srt=text_angle)  

text(x=x_seq[3], y=y_pos_1, "High Plains", adj=0, cex=text_cex, srt=text_angle)
text(x=x_seq[3]+0.3, y=y_pos_2, "Wet", adj=0, cex=text_cex, srt=text_angle)  

text(x=x_seq[4], y=y_pos_1, "High Plains", adj=0, cex=text_cex, srt=text_angle)
text(x=x_seq[4]+0.3, y=y_pos_2, "Dry", adj=0, cex=text_cex, srt=text_angle) 

text(x=x_seq[5], y=y_pos_1, "Irrigated", adj=0, cex=text_cex, srt=text_angle)

dev.off()





#____ FANCY DECISION TREE PLOTS USING ggCtreeReg
# fancy graphics for ctree object
# using the ggCtreeReg code from github
library(partykit)
library(tidyverse)
library(data.tree)
library(igraph)
library(caret)
library(ggthemes)
# download zipped package from github
# in terminal, navigate to Download directory and type "sudo unzip ./ggCtree-master.zip"
# then run the below lines of code...
# I've also manipulated the source code of ggCtreeReg (at the path below) to fix the poor 
# alignment between the decision tree and wisker plots


# CLIMATE == MO_WET
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='mo_wet'); nrow(res) 
res$climate <- factor(res$climate) # removes unused levels

res <- summaryBy(repro_max + NPP_sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'vpmax', 'max_LAI'))
# convert reproduction to g/m2 (see above for explanation)
res$repro_max <- res$repro_max / 10000 * 1000
colnames(res)[colnames(res)=="repro_max"] <- "grain_yield"

# build tree
fit <- ctree(grain_yield ~ gs_sensitivity + eff + saf + root_depth + vpmax + max_LAI, 
             data=res, alpha = 1)

source("/home/sean/sean_stuff/r_stuff/R_functions_help_other_materials/ggCtree-master/ggCtreeReg_v2_mo_wet.R")
# plot as pdf
pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/fancy_tree_mo_wet.pdf", height=11, width=10)  # 5, 7  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
ggCtreeReg(fit, title_woo=expression("     Central Plains (Wet) Reproduction (g " * m^-2 * ")"))
dev.off()



# CLIMATE == MO_DRY
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='mo_dry'); nrow(res) 
res$climate <- factor(res$climate) # removes unused levels

res <- summaryBy(repro_max + NPP_sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'vpmax', 'max_LAI'))
# convert reproduction to g/m2 (see above for explanation)
res$repro_max <- res$repro_max / 10000 * 1000
colnames(res)[colnames(res)=="repro_max"] <- "grain_yield"

# build tree
fit <- ctree(grain_yield ~ gs_sensitivity + eff + saf + root_depth + vpmax + max_LAI, 
             data=res, alpha = 1)

source("/home/sean/sean_stuff/r_stuff/R_functions_help_other_materials/ggCtree-master/ggCtreeReg_v2_mo_dry.R")
# plot as pdf
pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/fancy_tree_mo_dry.pdf", height=11, width=10)  # 5, 7  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
ggCtreeReg(fit, title_woo=expression("     Central Plains (Dry) Reproduction (g " * m^-2 * ")"))
dev.off()



# CLIMATE == YU_WET
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='yu_wet'); nrow(res) 
res$climate <- factor(res$climate) # removes unused levels

res <- summaryBy(repro_max + NPP_sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'vpmax', 'max_LAI'))
# convert reproduction to g/m2 (see above for explanation)
res$repro_max <- res$repro_max / 10000 * 1000
colnames(res)[colnames(res)=="repro_max"] <- "grain_yield"

# build tree
fit <- ctree(grain_yield ~ gs_sensitivity + eff + saf + root_depth + vpmax + max_LAI, 
             data=res, alpha = 1)

source("/home/sean/sean_stuff/r_stuff/R_functions_help_other_materials/ggCtree-master/ggCtreeReg_v2_yu_wet.R")
# plot as pdf
pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/fancy_tree_yu_wet.pdf", height=11, width=10)  # 5, 7  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
ggCtreeReg(fit, title_woo=expression("     High Plains (Wet) Reproduction (g " * m^-2 * ")"))
dev.off()



# CLIMATE == YU_DRY
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='yu_dry'); nrow(res) 
res$climate <- factor(res$climate) # removes unused levels

res <- summaryBy(repro_max + NPP_sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'vpmax', 'max_LAI'))
# convert reproduction to g/m2 (see above for explanation)
res$repro_max <- res$repro_max / 10000 * 1000
colnames(res)[colnames(res)=="repro_max"] <- "grain_yield"

# build tree
fit <- ctree(grain_yield ~ gs_sensitivity + eff + saf + root_depth + vpmax + max_LAI, 
             data=res, alpha = 1)

source("/home/sean/sean_stuff/r_stuff/R_functions_help_other_materials/ggCtree-master/ggCtreeReg_v2_yu_dry.R")
# plot as pdf
pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/fancy_tree_yu_dry.pdf", height=11, width=10)  # 5, 7  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
ggCtreeReg(fit, title_woo=expression("     High Plains (Dry) Reproduction (g " * m^-2 * ")"))
dev.off()



# CLIMATE == IRRIGATED  
res <- subset(comb, NPP_sum > 0); nrow(res)
res <- subset(res, climate=='irrigated'); nrow(res) 
res$climate <- factor(res$climate) # removes unused levels

res <- summaryBy(repro_max + NPP_sum ~ simulation, data=res, FUN=mean, keep.names=TRUE,
                 id=c('gs_sensitivity', 'eff', 'saf', 'root_depth', 'vpmax', 'max_LAI'))
# convert reproduction to g/m2 (see above for explanation)
res$repro_max <- res$repro_max / 10000 * 1000
colnames(res)[colnames(res)=="repro_max"] <- "grain_yield"

# build tree
fit <- ctree(grain_yield ~ gs_sensitivity + eff + saf + root_depth + vpmax + max_LAI, 
             data=res, alpha = 1)

source("/home/sean/sean_stuff/r_stuff/R_functions_help_other_materials/ggCtree-master/ggCtreeReg_v2_irrigated.R")
# plot as pdf
pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/fancy_tree_irrigated.pdf", height=11, width=10)  # 5, 7  
par(mfrow=c(1,1),  oma=c(0, 0, 0, 0), mai=c(0.4, 0.4, 0.2, 0), mgp=c(1.5, 0.5, 0))  
ggCtreeReg(fit, title_woo=expression("     Irrigated Reproduction (g " * m^-2 * ")"))
dev.off()



#_________ sapflow validation figures ___________##
#_________ sapflow validation figures ___________##
# rm(list=ls())
setwd("/home/sean/sean_stuff/c_stuff/trees/TREES311_Jan_2020/sean_files/sean_v4") 
library(smatr)
library(doBy)

# import limited water data
clim_40 <- read.table("sap_clim_40.txt", header=TRUE)
sim_40 <- read.table("40_et_val.sim", header=TRUE)
clim_40 <- clim_40[1:nrow(sim_40),] # run if clim has more rows than sim and hyd
hyd_40 <- read.table("40_et_val.hyd", header=TRUE)
comb_40 <- cbind.data.frame(clim_40, sim_40, hyd_40)

# import fully watered 
clim_100 <- read.table("sap_clim_100.txt", header=TRUE)
sim_100 <- read.table("100_et_val.sim", header=TRUE)
clim_100 <- clim_100[1:nrow(sim_100),] # run if clim has more rows than sim and hyd
hyd_100 <- read.table("100_et_val.hyd", header=TRUE)
comb_100 <- cbind.data.frame(clim_100, sim_100, hyd_100)
# add year, day, row columns to both combined dataframes
comb_40$year <- substring(comb_40$Date, 1, 4)
comb_40$day <- substring(comb_40$Date, 5, 7)
comb_40$row <- c(1:nrow(comb_40))
comb_100$year <- substring(comb_100$Date, 1, 4)
comb_100$day <- substring(comb_100$Date, 5, 7)
comb_100$row <- c(1:nrow(comb_100))

# function to force to numeric
numeric <- function(df) {
  as.numeric(as.character(df))
}
# add a day + time column
comb_40$day_time <- numeric(comb_40$day) + comb_40$Time/24
comb_100$day_time <- numeric(comb_100$day) + comb_100$Time/24

# compare field sapflow data with simulated sapflow (scaled transpiration) _____####
# first we need to convert transpiration output to g/m2/30min
colnames(comb_40)[26] <- "Trans_Ec"
colnames(comb_100)[26] <- "Trans_Ec"
# convert E per leaf area to E per ground area
comb_40$Trans_Ec <- comb_40$Trans_Ec * comb_40$liveLAI  # give mmol per m2 ground area s-1
comb_100$Trans_Ec <- comb_100$Trans_Ec * comb_100$liveLAI
# convert Trans_Ec to total mmol for each given timestep (30min) (1800 sec per timestep)
comb_40$Trans_Ec <- 1800 * comb_40$Trans_Ec
comb_100$Trans_Ec <- 1800 * comb_100$Trans_Ec
# convert mmol to mm of water.  Assuming molecular wt of H2O is 18g mol-1
comb_40$Trans_Ec <- (comb_40$Trans_Ec * 18)/1000  # give g H2O m-2 30min-1
comb_40$Trans_Ec[1:100]
comb_100$Trans_Ec <- (comb_100$Trans_Ec * 18)/1000  # give g H2O m-2 30min-1
comb_100$Trans_Ec[1:100]

# import sapflow data
sap <- read.csv("/home/sean/sean_stuff/c_stuff/trees/TREES311_Jan_2020/sean_files/sap_final.csv")

# sapflow data were collected all the way up until October 9.  
# The guages appear to stop working well ca Sep 7... 
# maybe even before this date.  ...subsetting from July 26 to Sep 7
# sap[2000:2400, c("date_time_new", "trees_day")]
crap <- subset(sap, day_time>240 & day_time <281); head(crap)

# plot all 30-min data
# 40% ET
plot(comb_40$Trans_Ec ~ comb_40$day_time, col="blue", xlim=c(240, 300), 
     ylim=c(0, 475), type="p")
points(crap$sap_H_40 ~ crap$day_time, col="red", type='l')

# 100% ET
plot(comb_100$Trans_Ec ~ comb_100$day_time, col="blue", xlim=c(240, 300), 
     ylim=c(0, 475), type="p")
points(crap$sap_H_100 ~ crap$day_time, col="red", type='l')

# plot 1:1
# add day column and make numeric
sap$day <- substr(sap$day_time, 1, 3)
comb_40$day <- as.numeric(as.character(comb_40$day)); is.numeric(comb_40$day)
comb_100$day <- as.numeric(as.character(comb_100$day)); is.numeric(comb_100$day)
sap$day <- as.numeric(sap$day); is.numeric(sap$day)
# subset to desired days
crap1_40 <- subset(comb_40, day>240 & day<281)
crap1_100 <- subset(comb_100, day>240 & day<281)
crap2 <- subset(sap, day>240 & day<281)
# sum Transpiraiton for each day
sum_40  <- summaryBy(Trans_Ec ~ day, data=crap1_40, FUN=(sum),
                          keep.names=TRUE)
sum_100 <- summaryBy(Trans_Ec ~ day, data=crap1_100, FUN=(sum),
                          keep.names=TRUE)
sum_sap <- summaryBy(sap_A_100 + sap_H_100 + sap_A_40 + sap_H_40 ~ 
                     day, data=crap2, FUN=(sum),
                     keep.names=TRUE, id=c("date_time_new"))

# create posix time column
sum_sap$time_stamp <- substr(sum_sap$date_time_new, 1,6)
sum_sap$time_stamp <- paste(sum_sap$time_stamp, "2017", sep=" ")
sum_sap$time_stamp <- as.POSIXct(sum_sap$time_stamp, format = "%b %d %Y")

# merge sapflow and TREES output into a single df
merged_40 <-  merge(sum_40,  sum_sap, by="day")
merged_100 <- merge(sum_100, sum_sap, by="day")


##_________ plotting ________________##
# plot all daily sums 
# units = g/m2/30min.  Therefore daily sums... one day = 48 30-min intervals
# units = g/m2/day.  Divide by 1000 to give kg/m2/day

pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/validation_fig.pdf", 
    height=4.5, width=11)
# jpeg(filename = "/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/validation_fig.jpeg", 
#     height=4.5, width=9, units="in", res = 800)
par(mfrow=c(1,2),  oma=c(1, 1, 1, 1), mai=c(.7, .7, .3, .3), mgp=c(1.8,0.5,0))

# panel 1 (full water)
plot(merged_100$Trans_Ec/1000 ~ merged_100$time_stamp, 
     ylim=c(0, 6.0), type='l',   
     col="blue", cex.axis=1, cex.lab=1.2, las=1,
     ylab=expression("Transpiration (kg " * m^-2 *" " * day^-1 * ")"),
     xlab=expression("Date"))
points(merged_100$sap_H_100/1000 ~ merged_100$time_stamp, pch=21, bg="orange", cex=0.5)
mtext("Full water", side = 3, line = 0.1, outer = FALSE, adj = 0.0)

legend("topright", c("TREES sim", "Sapflow"), 
       pch=c(NA,21), lty=c(1,NA), pt.bg=c(NA, "orange"), 
       col=c("blue", "black"), cex=0.9, bty="o")

txt1 <- expression(italic(r^2) * " = 0.58; " * italic("p") * " < 0.001")
txt2 <- expression("RSE = 0.655; bias = -0.381")
legend("bottomright", c(txt1, txt2), bty="n")

# panel 2 (limited water)
plot(merged_40$Trans_Ec/1000 ~ merged_40$time_stamp, 
     ylim=c(0, 6.0), type='l', 
     col="blue", cex.axis=1, cex.lab=1.2, las=1,
     ylab=expression("Transpiration (kg " * m^-2 *" " * day^-1 * ")"),
     xlab=expression("Date"))
points(merged_40$sap_H_40/1000 ~ merged_40$time_stamp, pch=21, bg="orange", cex=0.5)
mtext("Limited water", side = 3, line = 0.1, outer = FALSE, adj = 0.0)

legend("topright", c("TREES sim", "Sapflow"), 
       pch=c(NA,21), lty=c(1,NA), pt.bg=c(NA, "orange"), 
       col=c("blue", "black"), cex=0.9, bty="o")

txt1 <- expression(italic(r^2) * " = 0.63; " * italic("p") * " < 0.001")
txt2 <- expression("RSE = 0.522; bias = 0.111")
legend("bottomright", c(txt1, txt2), bty="n")

dev.off()


# linear models for above plots
# full water
y <- merged_100$sap_H_100/1000 # measured
x <- merged_100$Trans_Ec /1000 # modeled 
SMA_100 <- sma(y ~ x); plot(SMA_100); summary(SMA_100)
LM_100 <- lm(y ~ x); summary(LM_100)  
# mse_100 <- mean(LM_100$residuals^2); mse_100 # use RSE given in summary(lm)
bias_100 <- mean(x - y); bias_100
# 40% ET
y <- merged_40$sap_H_40/1000 # measured
x <- merged_40$Trans_Ec /1000 # modeled 
SMA_40 <- sma(y ~ x); plot(SMA_40); summary(SMA_40)
LM_40 <- lm(y ~ x); summary(LM_40) 
# mse_40 <- mean(LM_40$residuals^2); mse_40 # use RSE given in summary(lm)
bias_40 <- mean(x - y); bias_40




#_________ weibull model comparison figure - high vs low embolism resistance ___________##
#_________ weibull model comparison figure - high vs low embolism resistance ___________##
# rm(list=ls())
setwd("/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures") 

pdf(file="Pxx_comparison_figure.pdf", height=4.3, width=5)
par(mfrow=c(1,1),  oma=c(1, 1, 1, 1), mai=c(.7, .7, .3, .3), mgp=c(1.8,0.5,0))

# drought susceptible weibull coefficients
x <- seq(from=-6, to=5, length=400)
b <- 1.9  # related to the slope #10
c <- 2.128  # is 30% of            #10
k_sat = 100                      #10

y <- k_sat*(exp(-(-x/b)^c))  # this gives proper values of psi (-) on x axis
#y <- k_sat*(exp(-(x/b)^c)); y  # this gives psi as positive values on x axis
plot(y~x, cex=0.2, xlim=c(-6, 2), ylim=c(0, 100),
     xlab="Xylem water potential (MPa)",
     ylab= "Xylem conductivity (Percent of maximal)",
     col='black', type='l', lty=1,
     cex.axis=0.8, cex.lab=1, las=1, tck=-0.03)

# solve for P50 (math in journal 8/11/2020)
solve_50_sus <- -0.7^(1/c) * b; solve_50_sus
# solve for P88 (in journal 5/5/2021)
solve_88_sus <- -2.120^(1/c) * b; solve_88_sus

# draw P50 segments (susceptible geno)
segments(solve_50_sus, 0, solve_50_sus, 50, col='red', lty=1)
# segments(solve_50_sus, 50, -6, 50, col='red', lty=3)
points(solve_50_sus, 50, col='blue')

# draw P88 segments (susceptible geno)
segments(solve_88_sus, 0, solve_88_sus, 12, col='red', lty=1)
# segments(solve_88_sus, 12, -6, 12, col='red', lty=3)
points(solve_88_sus, 12, col='blue')

# drought resistant weibull coefficients
x <- seq(from=-6, to=5, length=400)
b <- 2.7  # related to the slope #10
c <- 2.128  # is 30% of            #10
k_sat = 100                      #10
y <- k_sat*(exp(-(-x/b)^c))  # this gives proper values of psi (-) on x axis
points(y~x, cex=0.2, col="black", type='l', lty=3)

# solve for P50 (math in journal 8/11/2020)
solve_50_res <- -0.7^(1/c) * b; solve_50_res
# solve for P88 (in journal 5/5/2021)
solve_88_res <- -2.120^(1/c) * b; solve_88_res

# draw P50 segments (resistant geno)
segments(solve_50_res, 0, solve_50_res, 50, col='red', lty=3)
# segments(solve_50, 50, -6, 50, col='red', lty=3)
points(solve_50_res, 50, col='blue')

# draw P88 segments (resistant geno)
segments(solve_88_res, 0, solve_88_res, 12, col='red', lty=3)
# segments(solve_88, 12, -6, 12, col='red', lty=3)
points(solve_88_res, 12, col='blue')

# add P50 and P88 values and labels to figure
# p50
text(-1.5, 50, paste("P50 susceptible = ", round(solve_50_sus, 2)), cex=0.7, pos=4)
text(-1.5, 44, paste("P50 resistant = ", round(solve_50_res, 2)), cex=0.7, pos=4)
# p88
text(-1.5, 12, paste("P88 susceptible = ", round(solve_88_sus, 2)), cex=0.7, pos=4)
text(-1.5, 6, paste("P88 resistant = ", round(solve_88_res, 2)), cex=0.7, pos=4)

# legend
legend("topleft", c("Embolism susceptible", "Embolism resistant"), 
       lty=c(1,3), col=c("black", "black"), cex=0.6, bty="o")

dev.off()




###_______________ conceptual network and climate diagram _______________________________###
###_______________ conceptual network and climate diagram _______________________________###
###_______________ conceptual network and climate diagram _______________________________###
# display.brewer.all()

library(RColorBrewer)
pal <- brewer.pal(9,"Greens")

pdf(file="/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/network_climate_diagram.pdf", height=4, width=9)    
# jpeg(filename = "/home/sean/sean_stuff/r_stuff/2020/crop_water_strategies/figures/network_climate_diagram.jpeg", height=4, width=10, units="in", res = 600)
par(mfrow=c(1,1),  oma=c(1, 1, 1, 1), mai=c(0.2, 0.1, 0.45, 0.15), mgp=c(2.2,0.5,0), xpd=TRUE) # need "xpd=TRUE" to plot in the margins (climate box)
# par(mfrow=c(1,1),  oma=c(1, 1, 1, 1), mai=c(.5, .5, .3, .3), mgp=c(2.2,0.5,0), xpd=TRUE) # need "xpd=TRUE" to plot in the margins (climate box)


# plotting psi ~ time
x <- c(0:100); y <- c(0:100)  
plot(y ~ x, pch=21, col='white', axes=FALSE, xlab=NA, ylab=NA)

# dimensions of arrow boxes
rec_x <- c(0,5,0,17,22,17)
rec_y <- c(30,50,70,70,50,30)
# dimensions of last box (yield box)
end_x <- rec_x[c(1:4,6)]
end_x <- end_x+(end_x[4]*1.15)*4
end_x[c(4,5)] <- end_x[c(4,5)]*1.05
end_y <- rec_y[c(1:4,6)]
crap <- end_y[1]*0.2
end_y[1]=end_y[1] - crap 
end_y[3]=end_y[3] + crap 
end_y[4]=end_y[4] + crap
end_y[5]=end_y[5] - crap
# offset factor for separating arrow boxes...
scale <- 1.15  # default=1.15
# draw polygons...
polygon(x=rec_x, y=rec_y, col=pal[1])
polygon(x=rec_x+(rec_x[4]*scale), y=rec_y, col=pal[2])
polygon(x=rec_x+(rec_x[4]*scale)*2, y=rec_y, col=pal[3])
polygon(x=rec_x+(rec_x[4]*scale)*3, y=rec_y, col=pal[4])
polygon(x=end_x, y=end_y, col=pal[5])

# determine placement of label on x axis for the first box
mean_shift_x <- mean(c(rec_x[5],rec_x[2]))-1
# add text in first box
text(mean_shift_x-0.5, rec_y[5]+4, expression("soil"), cex=1.2) 
text(mean_shift_x-0.5, rec_y[5]-4, expression("water"), cex=1.2) 
# add text to second box
text(mean_shift_x + rec_x[4]*scale-1, rec_y[5]+8, expression("soil"), cex=1.2)
text(mean_shift_x + rec_x[4]*scale-1, rec_y[5], expression("water"), cex=1.2)
text(mean_shift_x + rec_x[4]*scale-1, rec_y[5]-8, expression("uptake"), cex=1.2)
# add text to third box
text(mean_shift_x + rec_x[4]*scale*2-1, rec_y[5]+8, expression("xylem"), cex=1.2)
text(mean_shift_x + rec_x[4]*scale*2-1, rec_y[5], expression("water"), cex=1.2)
text(mean_shift_x + rec_x[4]*scale*2-1, rec_y[5]-8, expression("transport"), cex=1.2)
# add text to fourth box
text(mean_shift_x + rec_x[4]*scale*3, rec_y[5], expression("transpiration"), cex=1.2)
# add text to fifth box
text(mean_shift_x + rec_x[4]*scale*4, rec_y[5]+4, expression(CO[2]), cex=1.2)
text(mean_shift_x + rec_x[4]*scale*4, rec_y[5]-4, expression("assimilation"), cex=1.2)

## add trait boxes
## coordinates for upper row of boxes
rec_x2 <- c(10,5,7,7,2,2,18,18,13,13,15,10)
rec_y2 <- c(70,80,80,85,85,100,100,85,85,80,80,70)
# draw upper row of boxes
polygon(x=rec_x2, y=rec_y2, col="mistyrose2") # box1
# polygon(x=rec_x2+(rec_x[4]*scale)*1, y=rec_y2, col="mistyrose2") # box2
polygon(x=rec_x2+(rec_x[4]*scale)*2, y=rec_y2, col="mistyrose2") # box3

# coordinates for upper **double** arrow trait box
rec_x3 <- c(10,5,7,7,2,2,37,37,33,33,35,30,25,27,27,13,13,15)
rec_y3 <- c(70,80,80,85,85,100,100,85,85,80,80,70,80,80,85,85,80,80)
polygon(x=rec_x3+59, y=rec_y3, col="mistyrose2")
# polygon(x=rec_x2+(rec_x[4]*scale)*3, y=rec_y2, col="mistyrose2") # box4
# polygon(x=rec_x2+(rec_x[4]*scale)*4, y=rec_y2, col="mistyrose2") # box5

# coordinates for lower row of boxes
rec_x4 <- rec_x2
rec_y4 <- c(30,20,20,15,15,0,0,15,15,20,20,30)
# draw boxes
polygon(x=rec_x4, y=rec_y4, col="mistyrose2") # box6
polygon(x=rec_x4+(rec_x[4]*scale)*1, y=rec_y4, col="mistyrose2") # box7
polygon(x=rec_x4+(rec_x[4]*scale)*2, y=rec_y4, col="mistyrose2") # box8
polygon(x=rec_x4+(rec_x[4]*scale)*3, y=rec_y4, col="mistyrose2") # box9
polygon(x=rec_x4+(rec_x[4]*scale)*4, y=rec_y4, col="mistyrose2") # box10

## add text to **upper** trait boxes
# box1
text(rec_x2[1], rec_y2[6]-4, expression("soil"), cex=1) 
text(rec_x2[1], rec_y2[6]-10, expression("texture"), cex=1) 
# box2
# text(rec_x2[1]+(rec_x[4]*scale)*1, rec_y2[6]-4, expression("crap"), cex=0.8) 
# text(rec_x2[1]+(rec_x[4]*scale)*1, rec_y2[6]-10, expression("crap"), cex=0.8) 
# box3
text(rec_x2[1]+(rec_x[4]*scale)*2, rec_y2[6]-4, expression("hydraulic"), cex=1) 
text(rec_x2[1]+(rec_x[4]*scale)*2, rec_y2[6]-10, expression("efficiency"), cex=1) 
# box4
text(rec_x2[1]+(rec_x[4]*scale)*3.5, rec_y2[6]-7, expression("stomatal sensitivity"), cex=1) 
# box5
# text(rec_x2[1]+(rec_x[4]*scale)*4, rec_y2[6]-4, expression("crap"), cex=0.8) 
# text(rec_x2[1]+(rec_x[4]*scale)*4, rec_y2[6]-10, expression("crap"), cex=0.8) 

## add text to **lower** trait boxes
# box1
text(rec_x4[1], rec_y4[6]+11, expression("initial"), cex=1) 
text(rec_x4[1], rec_y4[6]+5, expression("soil water"), cex=1) 
# box2
text(rec_x4[1]+(rec_x[4]*scale)*1, rec_y4[6]+11, expression("root"), cex=1) 
text(rec_x4[1]+(rec_x[4]*scale)*1, rec_y4[6]+5, expression("depth"), cex=1) 
# box3
text(rec_x4[1]+(rec_x[4]*scale)*2, rec_y4[6]+11, expression("hydraulic"), cex=1) 
text(rec_x4[1]+(rec_x[4]*scale)*2, rec_y4[6]+5, expression("safety"), cex=1) 
# box4
text(rec_x4[1]+(rec_x[4]*scale)*3, rec_y4[6]+11, expression("maximum"), cex=1) 
text(rec_x4[1]+(rec_x[4]*scale)*3, rec_y4[6]+5, expression("LAI"), cex=1) 
# box5
text(rec_x4[1]+(rec_x[4]*scale)*4, rec_y4[6]+7, expression("Vpmax"), cex=1) 

# add climate box
arrows(x0=-2, y0=-5, x1=103, y1=-5, angle=90, length=0, col="blue", lty=2, xpd=TRUE)
arrows(x0=-2, y0=-5, x1=-2, y1=115, angle=90, length=0, col="blue", lty=2, xpd=TRUE)
arrows(x0=-2, y0=115, x1=103, y1=115, angle=90, length=0, col="blue", lty=2, xpd=TRUE)
arrows(x0=103, y0=115, x1=103, y1=-5, angle=90, length=0, col="blue", lty=2, xpd=TRUE)

mtext("     Climate", side=3, line=0.2, cex=1.2, adj=0, col="blue")

dev.off()



# make figure report using knitr
# SEE CODE "markdown_figures_v2.Rmd" IN THE **FIGURES** DIRECTORY



