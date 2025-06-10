				######################################
				#######	PREANALYSIS		########
				######################################

library(reshape)
library(lattice)
library(latticeExtra)
library(car)
library(heplots)
library(lsr)
library(mgcv)
library(lmerTest)

######################################
#######	  READ DATA		########
######################################

	###### CLEAR WORKSPACE #####

rm(list=setdiff(ls(), c("data.rt.exp1.raw","data.rt.exp2.raw","data.rt.exp3.raw","data.rt.exp4.raw","data.rt.exp5.raw","data.rt.exp6.raw","data.rt.exp7.raw",
			      "data.corr.exp1.raw","data.corr.exp2.raw","data.corr.exp3.raw","data.corr.exp4.raw","data.corr.exp5.raw","data.corr.exp6.raw","data.corr.exp7.raw",
				"spat.exp1.raw","spat.exp2.raw","spat.exp3.raw","spat.exp4.raw","spat.exp5.raw","spat.exp6.raw")))

	###### TEST: YES (= 1) / NO (= 0) #####

test <- 0
	
	###### READ DATA ######

for(i in c(1:49)){

	if(i < 10){
		file <- sprintf("C:\\Users\\emiel\\Desktop\\Multi-Actor MNS\\Experimenten\\Experimenten\\4H\\Exp7_4H con imi search\\Data\\data0%d.txt", i)
	}else{
		file <- sprintf("C:\\Users\\emiel\\Desktop\\Multi-Actor MNS\\Experimenten\\Experimenten\\4H\\Exp7_4H con imi search\\Data\\data%d.txt", i)
	}

	data <- read.table(file, header = F)

	###### NAME VARIABLES ######

	names <- c("ppn", "lftd", "gesl", "blok", "vinger", "aantal", "pos", "xr", "r", "corr",
		"rt", "re")

	names(data) <- names


	###### TEST: implement random RTs and random errors ######

	if(test == 1){	
		mean.test <- sample(c(400,500,600), 1, replace = T)
		rt <- rnorm(nrow(data), mean = mean.test, sd = 100)
		data$rt <- rt

		corr <- sample(c(0,1,1,1,1,1,1), nrow(data), replace = T)
		data$corr <- corr
	}

	###### < 100 MS CRITERION ######

		###### create additional column ######

	data["plus100"] <- NA

		###### compute ######

	for(j in 1:nrow(data)){
		if(data$rt[j] < 100){
			data$plus100[j] = 0
		}else{
			data$plus100[j] = 1
		}
	}

	###### > 2000 MS CRITERION ######	

		###### create additional column ######

	data["min2000"] <- NA

		###### compute ######

	for(j in 1:nrow(data)){
		if(data$rt[j] >= 2000){
			data$min2000[j] = 0
		}else{
			data$min2000[j] = 1
		}
	}

	###### MAX 3 SD CRITERION ######

		###### create additional column ######	

	data["max3SD"] <- NA

		###### create dataset with only correct trials and only trials above 100 ms ######

	data.3SD <- data[data$corr == 1, ]
	data.3SD <- data.3SD[data.3SD$min2000 == 1, ]
	data.3SD <- data.3SD[data.3SD$plus100 == 1, ]

		###### create container variable to store results ######

	if(exists("data.full") == 0){
		subject <- c(1:50)
		mean.3SD <- NA
		sd.3SD <- NA
		min.3SD <- NA
		max.3SD <- NA
		container.3SD <- data.frame(subject, mean.3SD, sd.3SD, min.3SD, max.3SD)
	}

	container.3SD$mean.3SD[i] <- mean(data.3SD$rt)
	container.3SD$sd.3SD[i] <- sd(data.3SD$rt)

	container.3SD$min.3SD[i] <- container.3SD$mean.3SD[i] - 3*container.3SD$sd.3SD[i]
	container.3SD$max.3SD[i] <- container.3SD$mean.3SD[i] + 3*container.3SD$sd.3SD[i]

		###### compute ######

	for(j in 1:nrow(data)){
		if(data$rt[j] <= container.3SD$max.3SD[i] && data$rt[j] >= container.3SD$min.3SD[i]){
			data$max3SD[j] = 1
		}else{
			data$max3SD[j] = 0
		}
	}

	###### MERGE WITH COMPLETE DATASET ######

	if(exists("data.full") == 0){
		data.full <- data
	}else{
		data.full <- rbind(data.full, data)
	}	
}

head(container.3SD)

########################################################
#######	  DELETE UNIMPORTANT VARIABLES	########
########################################################

data.full <- data.full[ , -c(5,8,9,12) ]
head(data.full)
tail(data.full)

##########################################################################
#######	 	 SPECIFY TYPE OF VARIABLES				########
##########################################################################

str(data.full)

data.full$lftd <- as.numeric(data.full$lftd)
	#age
data.full$blok <- factor(data.full$blok + 1)
	#block number
data.full$aantal <- factor(data.full$aantal, levels = c(1,2,3,4), labels = c("One", "Two", "Three", "Four"))
	#number of hands making a movement coded as factor (i.e., one, two, three, or four)
data.full$aantal_continu <- as.numeric(data.full$aantal)
	#number of hands making a movement coded as continuous variable
data.full$pos <- factor(data.full$pos)
	#configuration of the hands
data.full$corr <- factor(data.full$corr)
	#accuracy: 0 = error, 1 = correct
data.full$plus100 <- factor(data.full$plus100)
	#is RT slower than 100 ms?: 0 = no, 1 = yes
data.full$min2000 <- factor(data.full$min2000)
	#is RT faster than 2000 ms?: 0 = no, 1 = yes
data.full$max3SD <- factor(data.full$max3SD)
	#is RT within the 3 SD bounds?: 0 = no, 1 = yes

data.full <- data.full[, c(1:5,12,13,14,6:11)]
str(data.full)

########################################################
#######	  	DESCRIPTIVE DATA			########
########################################################

data.descriptive <- data.full
data.descriptive <- data.descriptive[data.descriptive$ppn != 44, ]
data.descriptive <- data.descriptive[data.descriptive$ppn != 47, ]
data.descriptive <- cast(ppn + lftd + gesl ~ ., data = data.descriptive, value = "rt", length)
data.descriptive

print(age_m <- mean(data.descriptive$lftd, na.rm = T))
print(age_sd <- sd(data.descriptive$lftd, na.rm = T))
print(gender <- ftable(data.descriptive$gesl))

########################################################
#######	 	 OUTLIER DATA			########
########################################################

nrow(data.full)

	###### full ######

data.outlier <- data.full
data.outlier <- data.full[data.full$ppn != 44, ]
data.outlier <- data.outlier[data.outlier$ppn != 47, ]
ftable(data.outlier$plus100, data.outlier$min2000, data.outlier$corr, data.outlier$max3SD, col.vars = c(1,2), dnn = c("plus100", "min2000", "corr", "max3SD"))

	###### no resp ######

data.outlier.r <- data.outlier[data.outlier$min2000 == 1, ]

(1 - (nrow(data.outlier.r) / nrow(data.outlier)))*100

	###### 100 ms ######

data.outlier.100 <- data.outlier.r[data.outlier.r$plus100 == 1, ]

(1 - (nrow(data.outlier.100) / nrow(data.outlier.r)))*100

	###### corr ######

data.outlier.corr <- data.outlier.100[data.outlier.100$corr == 1, ]

(1 - (nrow(data.outlier.corr) / nrow(data.outlier.100)))*100

	###### 3 SD ######

data.outlier.sd <- data.outlier.corr[data.outlier.corr$max3SD == 1, ]

(1 - (nrow(data.outlier.sd) / nrow(data.outlier.corr)))*100
	
########################################################
#######	 	 RT DATA				########
########################################################

data.rt <- data.full[data.full$plus100 == 1, ]
data.rt <- data.rt[data.rt$min2000 == 1, ]
data.rt <- data.rt[data.rt$max3SD == 1, ]
data.rt <- data.rt[data.rt$corr == 1, ]
data.rt <- data.rt[, -c(2,3,7,9:11)]
head(data.rt)

	####### RT PER SUBJ ########

data.rt.test <- cast(ppn ~ ., data = data.rt, value = "rt", mean)
names(data.rt.test)[2] <- "rt"
boxplot(data.rt.test$rt)
hist(data.rt.test$rt)
barchart(rt ~ factor(ppn), data = data.rt.test)

mean(data.rt.test$rt) - 3*sd(data.rt.test$rt)
which(data.rt.test$rt < mean(data.rt.test$rt) - 3*sd(data.rt.test$rt))
mean(data.rt.test$rt) + 3*sd(data.rt.test$rt)
which(data.rt.test$rt > mean(data.rt.test$rt) + 3*sd(data.rt.test$rt))

########################################################
#######	 	 ERROR DATA				########
########################################################

data.corr <- data.full[data.full$plus100 == 1, ]
data.corr <- data.corr[data.corr$min2000 == 1, ]
data.corr <- data.corr[, -c(2,3,9:11)]
head(data.corr)

	####### ACCURACY PER SUBJ ########

###### raw ######	

data.acc <- cast(ppn ~ corr, data = data.corr, value = "rt", length)
names(data.acc)[c(2,3)] <- c("err","corr")

data.acc

###### perc ######

data.acc.perc <- data.acc
data.acc.perc$err <- data.acc$err / (data.acc$err + data.acc$corr)
data.acc.perc$corr <- data.acc$corr / (data.acc$err + data.acc$corr)
data.acc.perc$err <-  data.acc.perc$err * 100
data.acc.perc$corr <- data.acc.perc$corr * 100

data.acc.perc$err
mean(data.acc.perc$err) + 3*sd(data.acc.perc$err)
which(data.acc.perc$err > mean(data.acc.perc$err) + 3*sd(data.acc.perc$err))

########################################################
#######	 	 	REMOVE			########
########################################################

data.rt <- data.rt[data.rt$ppn != 44, ]
data.rt <- data.rt[data.rt$ppn != 47, ]

data.corr <- data.corr[data.corr$ppn != 44, ]
data.corr <- data.corr[data.corr$ppn != 47, ]

data.rt.exp7.raw <- data.rt
data.corr.exp7.raw <- data.corr

########################################################
#######	 	CORRELATION RT & ERR		########
########################################################

data.cor.rt <- cast(ppn ~ aantal, data = data.rt, value = "rt", mean)
data.cor.rt["one_four"] <- data.cor.rt$One - data.cor.rt$Four

data.cor.err <- cast(ppn ~ aantal + corr, data = data.corr, value = "rt", length)
data.cor.err["One_perc_err"] <- data.cor.err$One_0 / (data.cor.err$One_0 + data.cor.err$One_1)
data.cor.err["Four_perc_err"] <- data.cor.err$Four_0 / (data.cor.err$Four_0 + data.cor.err$Four_1)
data.cor.err["one_four"] <- data.cor.err$One_perc_err - data.cor.err$Four_perc_err

cor.test(data.cor.rt$one_four, data.cor.err$one_four)
plot(data.cor.rt$one_four, data.cor.err$one_four)

				######################################
				#######	    SCRIPTS  	########
				######################################

############################################
#######	     SE function		########
############################################

SE_Within <- function(data, colnames, ref = F){

	###### create data frame with relevant data ######

	data_norm <- data.frame(matrix(nrow = nrow(data), ncol = length(colnames) + 2))
	names(data_norm) <- c("subj_mean", "grand_mean", colnames)

	###### complete data set ######

		###### subject mean ######

	data_norm$subj_mean <- rowMeans(data[,c(colnames)])

		###### grand mean ######

	data_norm$grand_mean <- mean(as.matrix(data[,c(colnames)]))

		###### normalized data ######

	for(i in 1:length(colnames)){
		for(j in 1:nrow(data_norm)){
			data_norm[j, colnames[i]] <- data[j, colnames[i]] - data_norm$subj_mean[j] +  data_norm$grand_mean[j]
		}
	}

	###### calculate standard errors according to different methods ######

		###### between subject SE and CI ######

	sd.between <- as.vector(sapply(data[,c(colnames)], sd))
	se.between <- sd.between / sqrt(nrow(data_norm))
	ci95.between <- se.between * qt(0.975, nrow(data_norm) - 1)

		###### Cousineau SE and CI ######

	var.within <- as.vector(sapply(data_norm[,c(colnames)], var))
	se.within <- sqrt(var.within) / sqrt(nrow(data_norm))
	ci95.within <- se.within * qt(0.975, nrow(data_norm) - 1)

		###### Morey SE and CI ######

	var.within.corr <- var.within * (length(colnames) / (length(colnames) - 1))	
	sd.within.corr <- sqrt(var.within.corr)	
	se.within.corr <- sd.within.corr / sqrt(nrow(data_norm))	
	ci95.within.corr <- se.within.corr * qt(0.975, nrow(data_norm) - 1)
	ci95.baguley <- ci95.within.corr * (sqrt(2)/2)

	###### return standard error information ######

	output <- data.frame(row.names = colnames, se.between, ci95.between, se.within, ci95.within,
				   se.within.corr, ci95.within.corr, ci95.baguley)
	
	if(ref){
		cat("references:","\n")
		within.ref <- "within: 'Cousineau, D. (2005). Confidence intervals in within-subject designs: A simpler solution to Loftus and Masson's method. Tutorials in Quantitative Methods for Psychology, 2005, 1, 42-45'"
		cat("  ",within.ref,"\n")
		within.corr.ref <- "within.corr: 'Morey, R.D. (2008). Confidence intervals from normalized data: A correction to Cousineau (2005). Tutorials in Quantitative Methods for Psychology, 4, 61-64'"
		cat("  ",within.corr.ref,"\n")
		baguley.ref <- "baguley: 'Baguley T. (2012). Calculating and graphing within-subject confidence intervals for ANOVA. Behav Res, 44, 158-175'"
		cat("  ",baguley.ref,"\n","\n")
	}

	return(output)	
}

				######################################
				#######	RT ANALYSIS		########
				######################################

########################################################
#######	 	 VISUALISATION			########
########################################################

	###### PANEL ######

panel.err <- function(x, y, subscripts, stderr, box.ratio, ...)
{
  d <- 1/(nlevels(y)+nlevels(y)/box.ratio)

  panel.arrows(as.numeric(x),y-stderr[subscripts], as.numeric(x), y+stderr[subscripts],
               code=3,angle=90, length=0.3, unit = "cm")
}

	###### SE ######

data.plot.rt.se <- cast(ppn ~ aantal, data = data.rt, value = "rt", mean)
SE <- SE_Within(data = data.plot.rt.se, colnames = c("One","Two","Three","Four"))$ci95.within.corr

	###### PLOT ######

data.plot.rt.a <- cast(ppn + aantal ~ ., data = data.rt, value = "rt", mean)
data.plot.rt <- cast(aantal ~ ., data = data.plot.rt.a, mean)
names(data.plot.rt)[2] <- "RT"

mytheme = trellis.par.get()
mytheme$plot.polygon$col = "grey"
mytheme$fontsize$text = 14
mytheme$axis.text$cex = 1

barchart(RT ~ aantal, data = data.plot.rt, ylab = "RT", ylim = c(337, 473), scale = list(tick.number = 10),
	   par.settings = mytheme,
	   panel = function(x, y, subscripts, stderr, box.ratio, ...)
	   {
             panel.barchart(x, y, subscripts = subscripts, box.ratio = box.ratio, origin = -1.5,...)
             panel.err(x, y, subscripts = subscripts, box.ratio = box.ratio, stderr = SE)
         })

########################################################
#######	 	  	 ANALYSIS		  	########
########################################################

	####### LME ######

##### random structure ######

#lme.rt <- lmer(rt ~ aantal_continu + (1 + aantal_continu | ppn), data = data.rt, REML = F)
#lme.rt2 <- lmer(rt ~ aantal_continu + (1 + aantal_continu || ppn), data = data.rt, REML = F)
#lme.rt3 <- lmer(rt ~ aantal_continu + (1 | ppn), data = data.rt, REML = F)

#anova(lme.rt, lme.rt3)

##### fixed effects ######

lme.rt <- lmer(rt ~ aantal_continu + (1 + aantal_continu | ppn), data = data.rt, REML = T)
summary(lme.rt)
anova(lme.rt, type = 3)
confint(lme.rt, parm = names(fixef(lme.rt))[-1], method = "Wald")

	####### GAM ######

##### models ######

lme.rt.gamm.lin <- gam(rt ~ aantal_continu + s(factor(data.rt$ppn), bs = "re"), data = data.rt)
summary(lme.rt.gamm.lin)

lme.rt.gamm <- gam(rt ~ s(aantal_continu, k = 4, bs = "tp") + s(factor(data.rt$ppn), bs = "re"), data = data.rt)
summary(lme.rt.gamm)

####### statistics ######

AIC(lme.rt.gamm.lin, lme.rt.gamm)
round(AIC(lme.rt.gamm.lin) - AIC(lme.rt.gamm),2)
round(lme.rt.gamm.lin$gcv.ubre - lme.rt.gamm$gcv.ubre,2)

##################################################
#######	 	   PLOTS		  	########
##################################################

data.predict <- cbind(data.rt, predict(lme.rt.gamm.lin), predict(lme.rt.gamm))
names(data.predict)[7:8] <- c("predict.lin", "predict.gam")
head(data.predict)

data.plot.fit.lin.a <- cast(ppn + aantal ~ ., data = data.predict, value = "predict.lin", mean)
data.plot.fit.lin <- cast(aantal ~ ., data = data.plot.fit.lin.a, mean)
head(data.plot.fit.lin)
names(data.plot.fit.lin)[2] <- "predict.lin"

data.plot.fit.gam.a <- cast(ppn + aantal ~ ., data = data.predict, value = "predict.gam", mean)
data.plot.fit.gam <- cast(aantal ~ ., data = data.plot.fit.gam.a, mean)
head(data.plot.fit.gam)
names(data.plot.fit.gam)[2] <- "predict.gam"

mytheme = trellis.par.get()
mytheme$fontsize$text = 14
mytheme$axis.text$cex = 1

#ppi <- 1200
#tiff(filename = "C:\\Users\\emiel\\Desktop\\Fig8_hres.tiff", compression = "zip", width = 7*ppi, height = 7*ppi, 
#     pointsize = 7*(9/5), res = ppi)

barchart(RT ~ aantal, data = data.plot.rt, col = "grey",
	xlab = list("Number of Observed Movements", cex = 1.1), ylab = list("RT", cex = 1.1),
	ylim = c(350,460), scales = list(tck = c(1,0), y = list(at = round(seq(355, 455, 10)))),
	par.settings = mytheme,
	panel=function(x, y, subscripts, stderr, box.ratio, ...)
	{
             panel.barchart(x, y, subscripts = subscripts, box.ratio = box.ratio, origin = 0,...)
             panel.err(x, y, subscripts = subscripts, box.ratio = box.ratio, stderr = SE)
      }
) +
#xyplot(predict.lin ~ aantal, type = c("l", "p"), data = data.plot.fit.lin, lwd = 2, par.settings = simpleTheme(col = "black", lty = 2, pch = 19)) +
xyplot(predict.gam ~ aantal, type = c("spline", "p"), data = data.plot.fit.gam, lwd = 2,par.settings = simpleTheme(col = "black", lty = 1, pch = 19))

#dev.off()

				########################################################
				#######			EXP 3 VS 7			########
				########################################################

########################################################
#######	 	  PREPROCESSING			########
########################################################

data.rt.exp3 <- data.rt.exp3.raw
data.rt.exp3$exp <- 3
data.rt.exp3$exp <- factor(data.rt.exp3$exp)
data.rt.exp3 <- data.rt.exp3[,-6]
head(data.rt.exp3)
tail(data.rt.exp3)

data.rt.exp7 <- data.rt.exp7.raw
data.rt.exp7$exp <- 7
data.rt.exp7$exp <- factor(data.rt.exp7$exp)
data.rt.exp7$ppn <- data.rt.exp7$ppn + max(data.rt.exp3$ppn)
head(data.rt.exp7)
tail(data.rt.exp7)

data.rt.combo37 <- rbind(data.rt.exp3, data.rt.exp7)
str(data.rt.combo37)
length(table(data.rt.combo37$ppn))

########################################################
#######	 	   ANALYSIS				########
########################################################

	###### LME ######

##### random structure ######

#lme.combo37 <- lmer(rt ~ exp*aantal_continu + (1 + aantal_continu | ppn), data = data.rt.combo37, REML = F)
#lme2.combo37 <- lmer(rt ~ exp*aantal_continu + (1 + aantal_continu || ppn), data = data.rt.combo37, REML = F)
#lme3.combo37 <- lmer(rt ~ exp*aantal_continu + (1 | ppn), data = data.rt.combo37, REML = F)

#anova(lme.combo37, lme3.combo37)

##### fixed structure ######

lme.combo37 <- lmer(rt ~ exp*aantal_continu + (1 + aantal_continu | ppn), data = data.rt.combo37, REML = T)
lme.combo37 <- lmer(rt ~ exp + exp:aantal_continu + (1 + aantal_continu | ppn), data = data.rt.combo37, REML = T)
summary(lme.combo37)
confint(lme.combo37, parm = names(fixef(lme.combo37))[-1], method = "Wald")

	###### GAM ######

###### models ######

lme.rt.lin.combo37 <- gam(rt ~ exp + aantal_continu + s(factor(data.rt.combo37$ppn), bs = "re"), data = data.rt.combo37)
summary(lme.rt.lin.combo37)

lme.rt.lin.inter.combo37 <- gam(rt ~ exp*aantal_continu + s(factor(data.rt.combo37$ppn), bs = "re"), data = data.rt.combo37)
summary(lme.rt.lin.inter.combo37)

lme.rt.gamm.combo37 <- gam(rt ~ exp + s(aantal_continu, k = 4, bs = "tp") + s(factor(data.rt.combo37$ppn), bs = "re"), data = data.rt.combo37)
summary(lme.rt.gamm.combo37)

lme.rt.gamm.inter.combo37 <- gam(rt ~ exp + s(aantal_continu, k = 4, bs = "tp") + s(aantal_continu, k = 4, bs = "tp", by = exp) + s(factor(data.rt.combo37$ppn), bs = "re"), data = data.rt.combo37)
summary(lme.rt.gamm.inter.combo37)

###### statistics ######

AIC(lme.rt.lin.combo37, lme.rt.lin.inter.combo37, lme.rt.gamm.combo37, lme.rt.gamm.inter.combo37)
round(AIC(lme.rt.gamm.combo37) - AIC(lme.rt.gamm.inter.combo37),2)
round(AIC(lme.rt.lin.inter.combo37) - AIC(lme.rt.gamm.inter.combo37),2)
round(AIC(lme.rt.lin.combo37) - AIC(lme.rt.gamm.inter.combo37),2)

round(lme.rt.gamm.combo37$gcv.ubre - lme.rt.gamm.inter.combo37$gcv.ubre,2)
round(lme.rt.lin.inter.combo37$gcv.ubre - lme.rt.gamm.inter.combo37$gcv.ubre,2)
round(lme.rt.lin.combo37$gcv.ubre - lme.rt.gamm.inter.combo37$gcv.ubre,2)

anova(lme.rt.lin.combo37, lme.rt.gamm.inter.combo37, test = "Chisq")
anova(lme.rt.lin.inter.combo37, lme.rt.gamm.inter.combo37, test = "Chisq")
anova(lme.rt.gamm.combo37, lme.rt.gamm.inter.combo37, test = "Chisq")

###### plot ######

data.predict.combo37 <- cbind(data.rt.combo37, predict(lme.rt.gamm.inter.combo37))
names(data.predict.combo37)[8] <- c("predict.gam")
head(data.predict.combo37)

data.plot.fit.gam.combo37.a <- cast(ppn + exp + aantal ~ ., data = data.predict.combo37, value = "predict.gam", mean)
data.plot.fit.combo37.gam <- cast(exp + aantal ~ ., data = data.plot.fit.gam.combo37.a, mean)
names(data.plot.fit.combo37.gam)[3] <- "predict.gam"

xyplot(predict.gam ~ aantal | exp, type = c("spline", "p"), data = data.plot.fit.combo37.gam, lwd = 2, par.settings = simpleTheme(col = "black", lty = 1, pch = 19))

				##################################################
				#######	Error Rate ANALYSIS		########
				##################################################

########################################################
#######	 	CALCULATE ERROR RATES		########
########################################################

data.err <- cast(ppn + aantal + aantal_continu ~ corr, data = data.corr, value = "rt", length)
data.err["err"] <- data.err[,4] / (data.err[,4] + data.err[,5])
data.err <- data.err[,-c(4,5)]
head(data.err)

########################################################
#######	 	 VISUALISATION			########
########################################################

	###### SE ######

data.plot.err.se <- cast(ppn ~ aantal, data = data.err, value = "err", mean) 
SE <- SE_Within(data = data.plot.err.se, colnames = c("One","Two","Three","Four"))$ci95.within.corr*100

	###### PLOT ######

data.plot.err.a <- cast(ppn + aantal ~ ., data = data.err, value = "err", mean)
data.plot.err <- cast(aantal ~ ., data = data.plot.err.a, mean)
names(data.plot.err)[2] <- "ErrorRate"

mytheme = trellis.par.get()
mytheme$fontsize$text = 14
mytheme$axis.text$cex = 1

ppi <- 1200
tiff(filename = "C:\\Users\\emiel\\Desktop\\FigS3_hres.tiff", compression = "zip", width = 7*ppi, height = 7*ppi, 
     pointsize = 7*(9/5), res = ppi)

barchart(ErrorRate*100 ~ aantal, data = data.plot.err, , auto.key = list(corner = c(.95,.99)), col = "grey",
	xlab = list("Number of Observed Movements", cex = 1.1), ylab = list("Error Rate", cex = 1.1),
	ylim = c(0.2,8.7), scales = list(tck = c(1,0), y = list(at = round(seq(0.5, 8.5, 1),1))),
	par.settings = mytheme,
	panel=function(x, y, subscripts, stderr, box.ratio, ...)
	{
             panel.barchart(x, y, subscripts = subscripts, box.ratio = box.ratio, origin = 0,...)
             panel.err(x, y, subscripts = subscripts, box.ratio = box.ratio, stderr = SE)
      })

dev.off()


