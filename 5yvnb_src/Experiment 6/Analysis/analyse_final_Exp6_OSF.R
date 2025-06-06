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
library(gamm4)
library(lmerTest)
library(BayesFactor)
library(TOSTER)

options(contrasts = c("contr.sum", "contr.poly"))

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


for(i in c(1:50)){

	if(i < 10){
		file <- sprintf("C:\\Users\\emiel\\Desktop\\Multi-Actor MNS\\Experimenten\\Experimenten\\4H\\Exp6_4H con neutral close\\Data\\data0%d.txt", i)
	}else{
		file <- sprintf("C:\\Users\\emiel\\Desktop\\Multi-Actor MNS\\Experimenten\\Experimenten\\4H\\Exp6_4H con neutral close\\Data\\data%d.txt", i)
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

tail(container.3SD)

########################################################
#######	  		SPAT				########
########################################################

data.full$spat <- NA
data.full$con_spat <- NA

for(i in 1:nrow(data.full)){

	###### left or right? ######

	if(data.full$aantal[i] == 1){
		if(data.full$pos[i] == 1 || data.full$pos[i] == 3){
			data.full$spat[i] = 0
		}else if(data.full$pos[i] == 2 || data.full$pos[i] == 4){
			data.full$spat[i] = 1
		}else{
			data.full$spat[i] = 2
		}
	}else if(data.full$aantal[i] == 2){
		if(data.full$pos[i] == 3){
			data.full$spat[i] = 0
		}else if(data.full$pos[i] == 4){
			data.full$spat[i] = 1
		}else{
			data.full$spat[i] = 2
		}
	}else{
		data.full$spat[i] = 2
	}

	###### congruent or incongruent? ######

	if(data.full$aantal[i] == 1 || data.full$aantal[i] == 2){
		if(data.full$vinger[i] == 1){
			if(data.full$spat[i] == 0){
				data.full$con_spat[i] = 1
			}else if(data.full$spat[i] == 1){
				data.full$con_spat[i] = 0
			}else{
				data.full$con_spat[i] = 2
			}
		}else{
			if(data.full$spat[i] == 0){
				data.full$con_spat[i] = 0
			}else if(data.full$spat[i] == 1){
				data.full$con_spat[i] = 1
			}else{
				data.full$con_spat[i] = 2
			}
		}
	}else{
		data.full$con_spat[i] = 2
	}	

}

head(data.full[,c("vinger","aantal","pos","spat","con_spat")], n = 20)
ftable(data.full$aantal, data.full$con_spat)

########################################################
#######	  DELETE UNIMPORTANT VARIABLES	########
########################################################

data.full <- data.full[ , -c(5,8,9,12,16) ]
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
data.full$aantal <- factor(data.full$aantal, levels = c(0,1,2,3,4), labels = c("Neutral", "One", "Two", "Three", "Four"))
	#number of hands making a movement coded as factor (i.e., neutral, one, two, three, or four)
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
data.full$con_spat <- factor(data.full$con_spat, levels = c(1,0,2), labels = c("sC", "sIC", "sN"))
	#spatial compatibility: congruent (sC), incongruent (sIC), or neutral (sN)

data.full <- data.full[, c(1:5,13,6,12,7:11)]
str(data.full)
summary(data.full)

########################################################
#######	  	DESCRIPTIVE DATA			########
########################################################

data.descriptive <- data.full
data.descriptive <- data.descriptive[data.descriptive$ppn != 20, ]
data.descriptive <- data.descriptive[data.descriptive$ppn != 48, ]
data.descriptive <- cast(ppn + lftd + gesl ~ ., data = data.descriptive, value = "rt", length)

print(age_m <- mean(data.descriptive$lftd, na.rm = T))
print(age_sd <- sd(data.descriptive$lftd, na.rm = T))
print(gender <- ftable(data.descriptive$gesl))

########################################################
#######	 	 OUTLIER DATA			########
########################################################

nrow(data.full)

	###### full ######

data.outlier <- data.full
data.outlier <- data.full[data.full$ppn != 20, ]
data.outlier <- data.outlier[data.outlier$ppn != 48, ]
#data.outlier <- data.outlier[data.outlier$aantal == "Neutral", ]
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
data.rt <- data.rt[, -c(2,3,9,11:13)]
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

data.corr <- data.full
#data.corr <- data.corr[data.corr$aantal == "Neutral", ]
data.corr <- data.corr[data.corr$plus100 == 1, ]
data.corr <- data.corr[data.corr$min2000 == 1, ]
data.corr <- data.corr[, -c(2,3,11:13)]
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

barchart(err ~ factor(ppn), data = data.acc.perc)
hist(data.acc.perc$err)
boxplot(data.acc.perc$err)

mean(data.acc.perc$err) + 3*sd(data.acc.perc$err)
which(data.acc.perc$err > mean(data.acc.perc$err) + 3*sd(data.acc.perc$err))

########################################################
#######	 	 	REMOVE			########
########################################################

data.rt <- data.rt[data.rt$ppn != 20, ]
data.rt <- data.rt[data.rt$ppn != 48, ]

data.corr <- data.corr[data.corr$ppn != 20, ]
data.corr <- data.corr[data.corr$ppn != 48, ]

data.rt.exp6.raw <- data.rt
data.corr.exp6.raw <- data.corr

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

data.rt.noN <- data.rt[data.rt$aantal_continu != 1, ]
data.plot.rt.noN.se <- cast(ppn ~ aantal, data = data.rt.noN, value = "rt", mean)
SE <- SE_Within(data = data.plot.rt.noN.se, colnames = c("One","Two","Three","Four"))$ci95.within.corr

	###### PLOT ######

data.plot.rt.noN.a <- cast(ppn + aantal ~ ., data = data.rt.noN, value = "rt", mean)
data.plot.rt.noN <- cast(aantal ~ ., data = data.plot.rt.noN.a, mean)
names(data.plot.rt.noN)[2] <- "RT"

mytheme = trellis.par.get()
mytheme$plot.polygon$col = "grey"
mytheme$fontsize$text = 14
mytheme$axis.text$cex = 1

barchart(RT ~ aantal, data = data.plot.rt.noN, ylab = "RT", ylim = c(393, 442), scale = list(tick.number = 10),
	   par.settings = mytheme,
	   panel = function(x, y, subscripts, stderr, box.ratio, ...)
	   {
             panel.barchart(x, y, subscripts = subscripts, box.ratio = box.ratio, origin = -1.5,...)
             panel.err(x, y, subscripts = subscripts, box.ratio = box.ratio, stderr = SE)
         })

##################################################
#######	 	   ANALYSIS		  	########
##################################################

	####### LME ######

##### random structure ######

#lme.rt.noN <- lmer(rt ~ aantal_continu + (1 + aantal_continu | ppn), data = data.rt.noN, REML = F)
#lme.rt2.noN <- lmer(rt ~ aantal_continu + (1 + aantal_continu || ppn), data = data.rt.noN, REML = F)
#lme.rt3.noN <- lmer(rt ~ aantal_continu + (1 | ppn), data = data.rt.noN, REML = F)

#anova(lme.rt.noN, lme.rt3.noN)

##### fixed effects ######

lme.rt.noN <- lmer(rt ~ aantal_continu + (1 | ppn), data = data.rt.noN, REML = T)
summary(lme.rt.noN)
anova(lme.rt.noN, type = 3)
confint(lme.rt.noN, parm = names(fixef(lme.rt.noN))[-1], method = "Wald")

	###### ANALYSIS GAM #####

###### models ######

lme.rt.gamm.lin.noN <- gam(rt ~ aantal_continu + s(ordered(data.rt.noN$ppn), bs = "re"), data = data.rt.noN, gamma = 1)
summary(lme.rt.gamm.lin.noN)

lme.rt.gamm.noN <- gam(rt ~ s(aantal_continu, k = 4, bs = "tp") + s(ordered(data.rt.noN$ppn), bs = "re"), data = data.rt.noN, gamma = 1)
summary(lme.rt.gamm.noN)

###### statistics ######

AIC(lme.rt.gamm.lin.noN, lme.rt.gamm.noN)
round(AIC(lme.rt.gamm.lin.noN) - AIC(lme.rt.gamm.noN),2) 
round(lme.rt.gamm.lin.noN$gcv.ubre - lme.rt.gamm.noN$gcv.ubre,2) 

##############################################################
#######	 	        PLOT 				########
##############################################################

data.predict.noN <- cbind(data.rt.noN , predict(lme.rt.gamm.lin.noN), predict(lme.rt.gamm.noN))
names(data.predict.noN)[8:9] <- c("predict.lin", "predict.gam")
head(data.predict.noN)

data.plot.fit.lin.noN.a <- cast(ppn + aantal ~ ., data = data.predict.noN, value = "predict.lin", mean)
data.plot.fit.lin.noN <- cast(aantal ~ ., data = data.plot.fit.lin.noN.a, mean)
head(data.plot.fit.lin.noN)
names(data.plot.fit.lin.noN)[2] <- "predict.lin"

data.plot.fit.gam.noN.a <- cast(ppn + aantal ~ ., data = data.predict.noN, value = "predict.gam", mean)
data.plot.fit.gam.noN <- cast(aantal ~ ., data = data.plot.fit.gam.noN.a, mean)
head(data.plot.fit.gam.noN)
names(data.plot.fit.gam.noN)[2] <- "predict.gam"

mytheme = trellis.par.get()
mytheme$fontsize$text = 14
mytheme$axis.text$cex = 1

#ppi <- 1200
#tiff(filename = "C:\\Users\\emiel\\Desktop\\Fig7_hres.tiff", compression = "zip", width = 7*ppi, height = 7*ppi, 
#     pointsize = 7*(9/5), res = ppi)

barchart(RT ~ aantal, data = data.plot.rt.noN, type = c("l", "p"), col = "grey",
	xlab = list("Number of Observed Movements", cex = 1.1), ylab = list("RT", cex = 1.1),
	ylim = c(403,437), scales = list(tck = c(1,0), y = list(at = round(seq(405, 435, 5)))),
	par.settings = mytheme,
	panel=function(x, y, subscripts, stderr, box.ratio, ...)
	{
             panel.barchart(x, y, subscripts = subscripts, box.ratio = box.ratio, origin = 0,...)
             panel.err(x, y, subscripts = subscripts, box.ratio = box.ratio, stderr = SE)
      }
)+
#xyplot(predict.lin ~ aantal, type = c("l", "p"), data = data.plot.fit.lin.noN, lwd = 2, par.settings = simpleTheme(col = "black", lty = 2, pch = 19)) +
xyplot(predict.gam ~ aantal, type = c("spline", "p"), data = data.plot.fit.gam.noN, lwd = 2, par.settings = simpleTheme(col = "black", lty = 1, pch = 19))

#dev.off()

				##################################################
				#######	SPATIAL RT ANALYSIS		########
				##################################################

ftable(data.full$aantal, data.full$con_spat)
ftable(data.rt$aantal, data.rt$con_spat)

########################################################
#######	 	 VISUALISATION			########
########################################################

data.plot.spatial.a <- cast(ppn + aantal + con_spat ~ ., data = data.rt, value = "rt", mean)
data.plot.spatial <- cast(aantal + con_spat ~ ., data = data.plot.spatial.a, mean)
names(data.plot.spatial)[3] <- "RT"
data.plot.spatial <- data.plot.spatial[2:6,]

barchart(RT ~ aantal, groups = con_spat, data = data.plot.spatial, auto.key = T)

########################################################
#######	 	 	ANALYSIS			########
########################################################

data.analysis.spatial <- cast(ppn ~ aantal + con_spat, data = data.rt, value = "rt", mean)
data.analysis.spatial <- data.analysis.spatial[,-c(2,7:9)]
spat.exp6.raw <- data.analysis.spatial
head(data.analysis.spatial)
	
aantal <- factor(rep(1:2, each = 2))
con <- factor(rep(1:2, times = 2))
idata <- data.frame(aantal, con)

fit.rt <- lm(cbind(One_sC, One_sIC, Two_sC, Two_sIC) ~ 1, data = data.analysis.spatial)
etasq(Anova(fit.rt, type = "III", test = "Wilks", idata = idata, idesign = ~aantal*con), anova = T, partial = T)
etasq(Anova(fit.rt, type = "III", test = "Wilks", idata = idata, idesign = ~aantal*con), anova = T, partial = T)$"approx F"

##############################################################
#######	 	 	ANALYSIS ALL			########
##############################################################

	##### PREPROCESSING #####

spat.exp1 <- spat.exp1.raw
spat.exp1 <- spat.exp1[,1:5]
spat.exp1$exp <- factor(1)
head(spat.exp1)
tail(spat.exp1)

spat.exp2 <- spat.exp2.raw
spat.exp2 <- spat.exp2[,1:5]
spat.exp2$exp <- factor(2)
spat.exp2$ppn <- spat.exp2$ppn + max(spat.exp1$ppn)
head(spat.exp2)
tail(spat.exp2)

spat.exp3 <- spat.exp3.raw
spat.exp3 <- spat.exp3[,1:5]
spat.exp3$exp <- factor(3)
spat.exp3$ppn <- spat.exp3$ppn + max(spat.exp2$ppn)
head(spat.exp3)
tail(spat.exp3)

spat.exp4 <- spat.exp4.raw
spat.exp4 <- spat.exp4[,c(1,3:6)]
spat.exp4$exp <- factor(4)
spat.exp4$ppn <- spat.exp4$ppn + max(spat.exp3$ppn)
head(spat.exp4)
tail(spat.exp4)

spat.exp5 <- spat.exp5.raw
spat.exp5 <- spat.exp5[,c(1,3:6)]
spat.exp5$exp <- factor(5)
spat.exp5$ppn <- spat.exp5$ppn + max(spat.exp4$ppn)
head(spat.exp5)
tail(spat.exp5)

spat.exp6 <- spat.exp6.raw
spat.exp6 <- spat.exp6[,1:5]
spat.exp6$exp <- factor(6)
spat.exp6$ppn <- spat.exp6$ppn + max(spat.exp5$ppn)
head(spat.exp6)
tail(spat.exp6)

data.spatial.combo <- rbind(spat.exp1,spat.exp2,spat.exp3,spat.exp4,spat.exp5,spat.exp6)
#data.spatial.combo <- rbind(spat.exp1,spat.exp2,spat.exp3,spat.exp5,spat.exp6)
data.spatial.combo <- as.data.frame(data.spatial.combo)
summary(data.spatial.combo)
head(data.spatial.combo)
tail(data.spatial.combo)
nrow(data.spatial.combo)

	###### PLOT ######

##### Panel #####

panel.err.spat <- function(x, y, subscripts, groups, stderr, box.ratio, ...)
{
  d <- 1/(nlevels(groups)+nlevels(groups)/box.ratio)

  g <- (as.numeric(groups[subscripts])-1); g <- (g-median(g))*d

  panel.arrows(as.numeric(x)+g,y-stderr[subscripts], as.numeric(x)+g, y+stderr[subscripts],
               code=3,angle=90, length=0.3, unit = "cm")
}

##### SE #####

head(data.spatial.combo)
SE.spat <- SE_Within(data = data.spatial.combo, colnames = names(data.spatial.combo)[-c(1,6)], ref = F)$ci95.within.corr

##### Dataset #####

data.plot.spatial.combo.a <- melt(id.vars = c("ppn","exp"), variable_name = "condition", data = data.spatial.combo)
data.plot.spatial.combo.a <- cbind(data.plot.spatial.combo.a, colsplit(data.plot.spatial.combo.a$condition, split = "_", names = c("number", "con")))
data.plot.spatial.combo.b <- cast(exp + number + con ~ ., data = data.plot.spatial.combo.a, value = "value", mean)
data.plot.spatial.combo <- cast(number + con ~ ., data = data.plot.spatial.combo.b, mean)
data.plot.spatial.combo$con <- factor(c("C","IC","C","IC"))
names(data.plot.spatial.combo)[3] <- "RT"
head(data.plot.spatial.combo)

##### Plot #####

mytheme = trellis.par.get()
mytheme$superpose.polygon$col = c("white", "grey")
mytheme$fontsize$text = 14
mytheme$axis.text$cex = 1

#ppi <- 1200
#tiff(filename = "C:\\Users\\emiel\\Desktop\\Fig11_hres.tiff", compression = "zip", width = 7*ppi, height = 7*ppi, 
#     pointsize = 7*(9/5), res = ppi)

barchart(RT ~ number, groups = con, data = data.plot.spatial.combo,
	ylim = c(382,458), scales = list(tck = c(1,0), y = list(at = round(seq(385, 455, 10)))), box.ratio = 3,
	xlab = list("Number of Observed Movements", cex = 1.1), ylab = list("RT", cex = 1.1),
      auto.key = list(corner = c(0.025,.975), points = F, rectangles = T, height = 0.85, size = 3.5, padding.text = 2.5),
	par.settings = mytheme,
	panel=function(x, y, subscripts, groups, stderr, box.ratio, ...)
	{
             panel.barchart(x, y, subscripts = subscripts, groups = groups, box.ratio = box.ratio, origin = 0,...)
             panel.err.spat(x, y, subscripts = subscripts, groups = groups, box.ratio = box.ratio, stderr = SE.spat)
      }	
)

#dev.off()

	###### ANOVA ######

cbind(One_sC, One_sIC, Two_sC, Two_sIC)

aantal <- factor(rep(1:2, each = 2))
con <- factor(rep(1:2, times = 2))
idata <- data.frame(aantal, con)

fit.rt <- lm(cbind(One_sC, One_sIC, Two_sC, Two_sIC) ~ exp, data = data.spatial.combo)
etasq(Anova(fit.rt, type = "III", test = "Wilks", idata = idata, idesign = ~aantal*con), anova = T, partial = T)
etasq(Anova(fit.rt, type = "III", test = "Wilks", idata = idata, idesign = ~aantal*con), anova = T, partial = T)$"approx F"

	###### Effect? ######

##### t-test #####

head(data.spatial.combo)

t.test( (data.spatial.combo$One_sC + data.spatial.combo$Two_sC) / 2, (data.spatial.combo$One_sIC + data.spatial.combo$Two_sIC) / 2, paired = T)
cohensD( (data.spatial.combo$One_sC + data.spatial.combo$Two_sC) / 2, (data.spatial.combo$One_sIC + data.spatial.combo$Two_sIC) / 2, method = "paired")

t.test(data.spatial.combo$Two_sIC - data.spatial.combo$Two_sC, data.spatial.combo$One_sIC - data.spatial.combo$One_sC, paired = T)
cohensD(data.spatial.combo$Two_sIC - data.spatial.combo$Two_sC, data.spatial.combo$One_sIC - data.spatial.combo$One_sC, method = "paired")

ttest.tstat(1.42193, n1 = 281, simple = T)^-1
TOSTone(m = 0.08482525, mu = 0, sd = 1, n = 281, low_eqbound_d = -0.2, high_eqbound_d = 0.2)

##### BF ANOVA #####

data.spatial.combo.melt <- data.frame(data.spatial.combo)
data.spatial.combo.melt <- melt(data = data.spatial.combo.melt, id.var = c("ppn","exp"), measure.vars = c("One_sC","One_sIC","Two_sC","Two_sIC"))
data.spatial.combo.melt <- cbind(data.spatial.combo.melt, colsplit(data.spatial.combo.melt$variable, split = "_", names = c("number", "con")))
data.spatial.combo.melt$ppn <- factor(data.spatial.combo.melt$ppn)
data.spatial.combo.melt$exp <- factor(data.spatial.combo.melt$exp)
names(data.spatial.combo.melt)[4] <- "RT"
head(data.spatial.combo.melt)
str(data.spatial.combo.melt)

bf_num <- lmBF(RT ~ exp*number*con - number:con + ppn, data = data.spatial.combo.melt, whichRandom = "ppn")
bf_denom <- lmBF(RT ~ exp*number*con + ppn, data = data.spatial.combo.melt, whichRandom = "ppn")
bf <- print(bf_num / bf_denom)

#newbf <- recompute(bf, iterations = 500000)
#newbf

				########################################################
				#######			EXP 1-6			########
				########################################################

########################################################
#######	 	  PREPROCESSING			########
########################################################

data.rt.exp1 <- data.rt.exp1.raw
data.rt.exp1$exp <- 1
data.rt.exp1$exp <- factor(data.rt.exp1$exp)
data.rt.exp1 <- data.rt.exp1[data.rt.exp1$con %in% "C", ]
data.rt.exp1 <- data.rt.exp1[, -6]
head(data.rt.exp1)
tail(data.rt.exp1)

data.rt.exp2 <- data.rt.exp2.raw
data.rt.exp2$exp <- 2
data.rt.exp2$exp <- factor(data.rt.exp2$exp)
data.rt.exp2$ppn <- data.rt.exp2$ppn + max(data.rt.exp1$ppn)
data.rt.exp2 <- data.rt.exp2[data.rt.exp2$con %in% "C", ]
data.rt.exp2 <- data.rt.exp2[, -6]
head(data.rt.exp2)
tail(data.rt.exp2)

data.rt.exp3 <- data.rt.exp3.raw
data.rt.exp3$exp <- 3
data.rt.exp3$exp <- factor(data.rt.exp3$exp)
data.rt.exp3$ppn <- data.rt.exp3$ppn + max(data.rt.exp2$ppn)
head(data.rt.exp3)
tail(data.rt.exp3)

data.rt.exp4 <- data.rt.exp4.raw
data.rt.exp4$exp <- 4
data.rt.exp4$exp <- factor(data.rt.exp4$exp)
data.rt.exp4$ppn <- data.rt.exp4$ppn + max(data.rt.exp3$ppn)
data.rt.exp4 <- data.rt.exp4[data.rt.exp4$aantal %in% c("One","Two","Three","Four"), ]
data.rt.exp4$aantal <- factor(data.rt.exp4$aantal)
head(data.rt.exp4)
tail(data.rt.exp4)

data.rt.exp5 <- data.rt.exp5.raw
data.rt.exp5$exp <- 5
data.rt.exp5$exp <- factor(data.rt.exp5$exp)
data.rt.exp5$ppn <- data.rt.exp5$ppn + max(data.rt.exp4$ppn)
data.rt.exp5 <- data.rt.exp5[data.rt.exp5$aantal %in% c("One","Two","Three","Four"), ]
data.rt.exp5$aantal <- factor(data.rt.exp5$aantal)
head(data.rt.exp5)
tail(data.rt.exp5)

data.rt.exp6 <- data.rt.exp6.raw
data.rt.exp6$exp <- 5
data.rt.exp6$exp <- factor(data.rt.exp6$exp)
data.rt.exp6$ppn <- data.rt.exp6$ppn + max(data.rt.exp5$ppn)
data.rt.exp6 <- data.rt.exp6[data.rt.exp6$aantal %in% c("One","Two","Three","Four"), ]
data.rt.exp6$aantal <- factor(data.rt.exp6$aantal)
head(data.rt.exp6)
tail(data.rt.exp6)

data.rt.combo26 <- rbind(data.rt.exp2, data.rt.exp6)
length(table(data.rt.combo26$ppn))

data.rt.combo3456 <- rbind(data.rt.exp3, data.rt.exp4, data.rt.exp5, data.rt.exp6)
str(data.rt.combo3456)

########################################################
#######	    ANALYSIS EXP 2 VS EXP 6		########
########################################################

	###### GAM ######

###### models ######

lme.rt.lin.combo26 <- gam(rt ~ exp + aantal_continu + s(factor(data.rt.combo26$ppn), bs = "re"), data = data.rt.combo26)
summary(lme.rt.lin.combo26)

lme.rt.lin.inter.combo26 <- gam(rt ~ exp*aantal_continu + s(factor(data.rt.combo26$ppn), bs = "re"), data = data.rt.combo26)
summary(lme.rt.lin.inter.combo26)

lme.rt.gamm.combo26 <- gam(rt ~ exp + s(aantal_continu, k = 4, bs = "tp") + s(factor(data.rt.combo26$ppn), bs = "re"), data = data.rt.combo26)
summary(lme.rt.gamm.combo26)

lme.rt.gamm.inter.combo26 <- gam(rt ~ exp + s(aantal_continu, k = 4, bs = "tp") + s(aantal_continu, k = 4, bs = "tp", by = exp) + s(factor(data.rt.combo26$ppn), bs = "re"), data = data.rt.combo26)
summary(lme.rt.gamm.inter.combo26)

###### statistics ######

AIC(lme.rt.lin.combo26, lme.rt.lin.inter.combo26, lme.rt.gamm.combo26, lme.rt.gamm.inter.combo26)
round(AIC(lme.rt.gamm.combo26) - AIC(lme.rt.gamm.inter.combo26),2)
round(AIC(lme.rt.lin.inter.combo26) - AIC(lme.rt.gamm.inter.combo26),2)
round(AIC(lme.rt.lin.combo26) - AIC(lme.rt.gamm.inter.combo26),2)

round(lme.rt.gamm.combo26$gcv.ubre - lme.rt.gamm.inter.combo26$gcv.ubre,2)
round(lme.rt.lin.inter.combo26$gcv.ubre - lme.rt.gamm.inter.combo26$gcv.ubre,2)
round(lme.rt.lin.combo26$gcv.ubre - lme.rt.gamm.inter.combo26$gcv.ubre,2)

anova(lme.rt.lin.combo26, lme.rt.gamm.inter.combo26, test = "Chisq")
anova(lme.rt.lin.inter.combo26, lme.rt.gamm.inter.combo26, test = "Chisq")
anova(lme.rt.gamm.combo26, lme.rt.gamm.inter.combo26, test = "Chisq")

###### plot ######

data.predict.combo26 <- cbind(data.rt.combo26, predict(lme.rt.gamm.inter.combo26))
names(data.predict.combo26)[9] <- c("predict.gam")
head(data.predict.combo26)

data.plot.fit.gam.combo26.a <- cast(ppn + exp + aantal ~ ., data = data.predict.combo26, value = "predict.gam", mean)
data.plot.fit.combo26.gam <- cast(exp + aantal ~ ., data = data.plot.fit.gam.combo26.a, mean)
names(data.plot.fit.combo26.gam)[3] <- "predict.gam"

xyplot(predict.gam ~ aantal | exp, type = c("spline", "p"), data = data.plot.fit.combo26.gam, lwd = 2, par.settings = simpleTheme(col = "black", lty = 1, pch = 19))

	###### RT COMPARISON ######

data.rt.comp2 <- cast(ppn ~ ., data = data.rt.exp2, value = "rt", mean)
names(data.rt.comp2)[2] <- "RT"
data.rt.comp6 <- cast(ppn ~ ., data = data.rt.exp6, value = "rt", mean)
names(data.rt.comp6)[2] <- "RT"

t.test(data.rt.comp2$RT, data.rt.comp6$RT, var.equal = F)
cohensD(data.rt.comp2$RT, data.rt.comp6$RT, method = "unequal")

########################################################
#######	       ANALYSIS EXP 3-6			########
########################################################

nrow(cast(ppn ~ ., data = data.rt.combo3456, value = "rt", mean))

	####### LME ######

##### random structure ######

#lme.rt.combo3456 <- lmer(rt ~ exp + aantal_continu + (1 + aantal_continu | ppn), data = data.rt.combo3456, REML = F)
#lme.rt2.combo3456 <- lmer(rt ~ exp + aantal_continu + (1 + aantal_continu || ppn), data = data.rt.combo3456, REML = F)
#lme.rt3.combo3456 <- lmer(rt ~ exp + aantal_continu + (1 | ppn), data = data.rt.combo3456, REML = F)

#anova(lme.rt.combo3456, lme.rt3.combo3456)

##### fixed effects ######

lme.rt.combo3456 <- lmer(rt ~ exp + aantal_continu + (1 + aantal_continu | ppn), data = data.rt.combo3456, REML = T)
summary(lme.rt.combo3456)
anova(lme.rt.combo3456, type = 3)
confint(lme.rt.combo3456, parm = names(fixef(lme.rt.combo3456))[-1], method = "Wald")

	####### GAM ######

###### models ######

lme.rt.null.combo3456 <- gam(rt ~ exp + aantal_continu + s(factor(data.rt.combo3456$ppn), bs = "re"), data = data.rt.combo3456)
summary(lme.rt.null.combo3456)

lme.rt.lin.combo3456 <- gam(rt ~ exp + aantal_continu + s(factor(data.rt.combo3456$ppn), bs = "re"), data = data.rt.combo3456)
summary(lme.rt.lin.combo3456)

lme.rt.gamm.combo3456 <- gam(rt ~ exp + s(aantal_continu, k = 4, bs = "tp") + s(factor(data.rt.combo3456$ppn), bs = "re"), data = data.rt.combo3456)
summary(lme.rt.gamm.combo3456)

###### statistics ######

AIC(lme.rt.null.combo3456,lme.rt.lin.combo3456, lme.rt.gamm.combo3456)
round(AIC(lme.rt.null.combo3456) - AIC(lme.rt.lin.combo3456),2)
round(AIC(lme.rt.null.combo3456) - AIC(lme.rt.gamm.combo3456),2)
round(AIC(lme.rt.lin.combo3456) - AIC(lme.rt.gamm.combo3456),2)

round(lme.rt.null.combo3456$gcv.ubre,1) - round(lme.rt.lin.combo3456$gcv.ubre,1)
round(lme.rt.null.combo3456$gcv.ubre,1) - round(lme.rt.gamm.combo3456$gcv.ubre,1)
round(lme.rt.lin.combo3456$gcv.ubre,1) - round(lme.rt.gamm.combo3456$gcv.ubre,1)

anova(lme.rt.lin.combo3456, lme.rt.gamm.combo3456, test = "Chisq")

	###### GAMM4 ######

lme.rt.null.combo3456 <- gamm4(rt ~ exp + aantal_continu, random = ~(1 | ppn), data = data.rt.combo3456, REML = T)
summary(lme.rt.null.combo3456$gam)
logLik(lme.rt.null.combo3456$mer)

lme.rt.lin.combo3456 <- gamm4(rt ~ exp + aantal_continu, random = ~(1 | ppn), data = data.rt.combo3456, REML = T)
summary(lme.rt.lin.combo3456$gam)
logLik(lme.rt.lin.combo3456$mer)

lme.rt.gamm.combo3456 <- gamm4(rt ~ exp + s(aantal_continu, k = 4, bs = "tp"), random = ~(1 | ppn), data = data.rt.combo3456, REML = T) #gamm4 gives same results
summary(lme.rt.gamm.combo3456$gam)
logLik(lme.rt.gamm.combo3456$mer)

########################################################
#######	  ANALYSIS EXP 1-2 vs. EXP 3-6	########
########################################################

	###### PREPROCESSING ######

data.rt.combo12 <- rbind(data.rt.exp1, data.rt.exp2)
data.rt.combo12$type <- factor(1)
str(data.rt.combo12)

data.rt.combo3456 <- rbind(data.rt.exp3, data.rt.exp4, data.rt.exp5, data.rt.exp6)
data.rt.combo3456$type <- factor(2)
str(data.rt.combo3456)

data.rt.combo.all <- rbind(data.rt.combo12, data.rt.combo3456)
str(data.rt.combo.all)

summary(data.rt.combo.all$ppn)
length(table(data.rt.combo.all$ppn))
length(table(data.rt.combo.all[data.rt.combo.all$type == 1, ]$ppn))
length(table(data.rt.combo.all[data.rt.combo.all$type == 2, ]$ppn))

	###### GAM ######

###### models ######

lme.rt.lin.combo.all <- gam(rt ~ type + aantal_continu + s(factor(data.rt.combo.all$ppn), bs = "re"), data = data.rt.combo.all)
summary(lme.rt.lin.combo.all)

lme.rt.lin.inter.combo.all <- gam(rt ~ type*aantal_continu + s(factor(data.rt.combo.all$ppn), bs = "re"), data = data.rt.combo.all)
summary(lme.rt.lin.inter.combo.all)

lme.rt.gamm.combo.all <- gam(rt ~ type + s(aantal_continu, k = 4, bs = "tp") + s(factor(data.rt.combo.all$ppn), bs = "re"), data = data.rt.combo.all)
summary(lme.rt.gamm.combo.all)

lme.rt.gamm.inter.combo.all <- gam(rt ~ type + s(aantal_continu, k = 4, bs = "tp") + s(aantal_continu, k = 4, bs = "tp", by = type) + s(factor(data.rt.combo.all$ppn), bs = "re"), data = data.rt.combo.all)
summary(lme.rt.gamm.inter.combo.all)

###### statistics ######

AIC(lme.rt.lin.combo.all, lme.rt.lin.inter.combo.all, lme.rt.gamm.combo.all, lme.rt.gamm.inter.combo.all)
round(AIC(lme.rt.gamm.combo.all) - AIC(lme.rt.gamm.inter.combo.all),2)
round(AIC(lme.rt.lin.inter.combo.all) - AIC(lme.rt.gamm.inter.combo.all),2)
round(AIC(lme.rt.lin.combo.all) - AIC(lme.rt.gamm.inter.combo.all),2)

round(lme.rt.gamm.combo.all$gcv.ubre - lme.rt.gamm.inter.combo.all$gcv.ubre,2)
round(lme.rt.lin.inter.combo.all$gcv.ubre - lme.rt.gamm.inter.combo.all$gcv.ubre,2)
round(lme.rt.lin.combo.all$gcv.ubre - lme.rt.gamm.inter.combo.all$gcv.ubre,2)

anova(lme.rt.gamm.combo.all, lme.rt.gamm.inter.combo.all, test = "F")
anova(lme.rt.lin.inter.combo.all, lme.rt.gamm.inter.combo.all, test = "F")
anova(lme.rt.lin.combo.all, lme.rt.gamm.inter.combo.all, test = "F")

###### plot ######

data.predict.combo.all <- cbind(data.rt.combo.all, predict(lme.rt.gamm.inter.combo.all))
names(data.predict.combo.all)[10] <- c("predict.gam")
head(data.predict.combo.all)

data.plot.fit.gam.combo.all.a <- cast(ppn + type + aantal ~ ., data = data.predict.combo.all, value = "predict.gam", mean)
data.plot.fit.combo.all.gam <- cast(type + aantal ~ ., data = data.plot.fit.gam.combo.all.a, mean)
names(data.plot.fit.combo.all.gam)[3] <- "predict.gam"

xyplot(predict.gam ~ aantal | type, type = c("spline", "p"), data = data.plot.fit.combo.all.gam, lwd = 2, par.settings = simpleTheme(col = "black", lty = 1, pch = 19))

	###### RT COMPARISON ######

data.rt.comp12 <- cast(ppn ~ ., data = data.rt.combo12, value = "rt", mean)
names(data.rt.comp12)[2] <- "RT"

data.rt.comp3456 <- cast(ppn ~ ., data = data.rt.combo3456, value = "rt", mean)
names(data.rt.comp3456)[2] <- "RT"

t.test(data.rt.comp12$RT, data.rt.comp3456$RT, var.equal = F)
cohensD(data.rt.comp12$RT, data.rt.comp3456$RT, method = "unequal")

########################################################
#######	 	   PLOT EXP 3-6			########
########################################################

	###### SE ######

data.plot.combo3456.se <- cast(ppn + exp ~ aantal, data = data.rt.combo3456, value = "rt", mean)
SE.combo3456 <- SE_Within(data = data.plot.combo3456.se, colnames = c("One","Two","Three","Four"))$ci95.within.corr

	###### PLOT ######

data.plot.rt.a.combo3456 <- cast(ppn + exp + aantal  ~ ., data = data.rt.combo3456, value = "rt", mean)
data.plot.rt.b.combo3456 <- cast(exp + aantal  ~ ., data = data.plot.rt.a.combo3456, mean)
data.plot.rt.combo3456 <- cast(aantal ~ ., data = data.plot.rt.b.combo3456, mean)
names(data.plot.rt.combo3456)[2] <- "RT"

data.predict <- cbind(data.rt.combo3456 , predict(lme.rt.lin.combo3456$gam), predict(lme.rt.gamm.combo3456$gam))
names(data.predict)[9:10] <- c("predict.lin", "predict.gam")
head(data.predict)

data.plot.fit.lin.a <- cast(ppn + exp + aantal ~ ., data = data.predict, value = "predict.lin", mean)
data.plot.fit.lin.b <- cast(exp + aantal ~ ., data = data.plot.fit.lin.a, mean)
data.plot.fit.lin <- cast(aantal ~ ., data = data.plot.fit.lin.b, mean)
head(data.plot.fit.lin)
names(data.plot.fit.lin)[2] <- "predict.lin"

data.plot.fit.gam.a <- cast(ppn + exp + aantal ~ ., data = data.predict, value = "predict.gam", mean)
data.plot.fit.gam.b <- cast(exp + aantal ~ ., data = data.plot.fit.gam.a, mean)
data.plot.fit.gam <- cast(aantal ~ ., data = data.plot.fit.gam.b, mean)
head(data.plot.fit.gam)
names(data.plot.fit.gam)[2] <- "predict.gam"

mytheme = trellis.par.get()
mytheme$fontsize$text = 14
mytheme$axis.text$cex = 1

#ppi <- 1200
#tiff(filename = "C:\\Users\\emiel\\Desktop\\Fig10_hres.tiff", compression = "zip", width = 7*ppi, height = 7*ppi, 
#     pointsize = 7*(9/5), res = ppi)

barchart(RT ~ aantal, data = data.plot.rt.combo3456, col = "grey",
	xlab = list("Number of Observed Movements", cex = 1.1), ylab = list("RT", cex = 1.1),
	ylim = c(388,417), scales = list(tck = c(1,0), y = list(at = round(seq(390, 415, 5)))),
	par.settings = mytheme,
	panel=function(x, y, subscripts, stderr, box.ratio, ...)
	{
             panel.barchart(x, y, subscripts = subscripts, box.ratio = box.ratio, origin = 0,...)
             panel.err(x, y, subscripts = subscripts, box.ratio = box.ratio, stderr = SE.combo3456)
      }
)+
#xyplot(predict.lin ~ aantal, type = c("l", "p"), data = data.plot.fit.lin, lwd = 2, par.settings = simpleTheme(col = "black", lty = 2, pch = 19)) +
xyplot(predict.gam ~ aantal, type = c("spline", "p"), data = data.plot.fit.gam, lwd = 2,par.settings = simpleTheme(col = "black", lty = 1, pch = 19))

#dev.off()
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

data.err.noN <- data.err[data.err$aantal_continu != 1, ]
data.plot.err.noN.a <- cast(ppn + aantal ~ ., data = data.err.noN, value = "err", mean)
data.plot.err.noN <- cast(aantal ~ ., data = data.plot.err.noN.a, mean)
names(data.plot.err.noN)[2] <- "ErrorRate"

barchart(ErrorRate*100 ~ aantal, data = data.plot.err.noN, ylab = "Error Rate", ylim = c(-0.4, 6.4), scale = list(tick.number = 20))

				########################################################
				#######			EXP 3 - 6 ER		########
				########################################################

########################################################
#######	 	  PREPROCESSING			########
########################################################

data.corr.exp3 <- data.corr.exp3.raw
data.corr.exp3$exp <- 3
data.corr.exp3$exp <- factor(data.corr.exp3$exp)
head(data.corr.exp3)

data.corr.exp4 <- data.corr.exp4.raw
data.corr.exp4$exp <- 4
data.corr.exp4$exp <- factor(data.corr.exp4$exp)
data.corr.exp4$ppn <- data.corr.exp4$ppn + max(data.corr.exp3$ppn)
head(data.corr.exp4)

data.corr.exp5 <- data.corr.exp5.raw
data.corr.exp5$exp <- 5
data.corr.exp5$exp <- factor(data.corr.exp5$exp)
data.corr.exp5$ppn <- data.corr.exp5$ppn + max(data.corr.exp3$ppn)
data.corr.exp5 <- data.corr.exp5[data.corr.exp5$aantal %in% c("One","Two","Three","Four"), ]
data.corr.exp5$aantal <- factor(data.corr.exp5$aantal)
head(data.corr.exp5)

data.corr.exp6 <- data.corr.exp6.raw
data.corr.exp6$exp <- 6
data.corr.exp6$exp <- factor(data.corr.exp6$exp)
data.corr.exp6$ppn <- data.corr.exp6$ppn + max(data.corr.exp3$ppn)
data.corr.exp6 <- data.corr.exp6[data.corr.exp6$aantal %in% c("One","Two","Three","Four"), ]
data.corr.exp6$aantal <- factor(data.corr.exp6$aantal)
head(data.corr.exp6)

data.corr.combo3456 <- rbind(data.corr.exp3, data.corr.exp4, data.corr.exp5, data.corr.exp6)
str(data.corr.combo3456)

########################################################
#######	 	   ANALYSIS				########
########################################################

	###### LME #####

glme.err.combo3456 <- glmer(corr ~ exp + aantal_continu + (1 | ppn), family = "binomial", data = data.corr.combo3456)
summary(glme.err.combo3456)

	###### GAM #####

lme.corr.lin.combo3456 <- gam(corr ~ exp + aantal_continu + s(factor(data.corr.combo3456$ppn), bs = "re"), data = data.corr.combo3456, family = "binomial")
summary(lme.corr.lin.combo3456)

lme.corr.gamm.combo3456 <- gam(corr ~ exp + s(aantal_continu, k = 4, bs = "tp") + s(factor(data.corr.combo3456$ppn), bs = "re"), data = data.corr.combo3456, family = "binomial")
summary(lme.corr.gamm.combo3456)

AIC(lme.corr.lin.combo3456, lme.corr.gamm.combo3456)


########################################################
#######	 	CALCULATE ERROR RATES		########
########################################################

data.err.combo3456 <- cast(ppn + exp + aantal + aantal_continu ~ corr, data = data.corr.combo3456, value = "rt", length)
data.err.combo3456["err"] <- data.err.combo3456[,5] / (data.err.combo3456[,5] + data.err.combo3456[,6])
data.err.combo3456 <- data.err.combo3456[,-c(5,6)]
head(data.err.combo3456)

########################################################
#######	 	 VISUALISATION			########
########################################################

	###### SE ######

data.plot.err.combo3456.se <- cast(ppn + exp ~ aantal, data = data.err.combo3456, value = "err", mean)
SE.combo3456.err <- SE_Within(data = data.plot.err.combo3456.se, colnames = names(data.plot.err.combo3456.se)[-c(1,2)], ref = T)$ci95.within.corr*100

	###### PLOT ######

data.plot.err.combo3456.a <- cast(ppn + exp + aantal ~ ., data = data.err.combo3456, value = "err", mean)
data.plot.err.combo3456.b <- cast(exp + aantal  ~ ., data = data.plot.err.combo3456.a, mean)
data.plot.err.combo3456 <- cast(aantal  ~ ., data = data.plot.err.combo3456.b, mean)
names(data.plot.err.combo3456)[2] <- "ErrorRate"

mytheme = trellis.par.get()
mytheme$fontsize$text = 14
mytheme$axis.text$cex = 1

ppi <- 1200
tiff(filename = "C:\\Users\\emiel\\Desktop\\FigS2_hres.tiff", compression = "zip", width = 7*ppi, height = 7*ppi, 
     pointsize = 7*(9/5), res = ppi)

barchart(ErrorRate*100 ~ aantal, data = data.plot.err.combo3456, col = "grey",
	xlab = list("Number of Observed Movements", cex = 1.1), ylab = list("Error Rate", cex = 1.1),
	ylim = c(1.2,5.8), scales = list(tck = c(1,0), y = list(at = round(seq(1.5, 5.5, 1),1))),
      auto.key = list(corner = c(0.025,.975), points = F, rectangles = T, height = 0.85, size = 3.5, padding.text = 2.5),
	par.settings = mytheme,
	panel=function(x, y, subscripts, stderr, box.ratio, ...)
	{
             panel.barchart(x, y, subscripts = subscripts, box.ratio = box.ratio, origin = 0,...)
             panel.err(x, y, subscripts = subscripts, box.ratio = box.ratio, stderr = SE.combo3456.err)
      }	
)

dev.off()


