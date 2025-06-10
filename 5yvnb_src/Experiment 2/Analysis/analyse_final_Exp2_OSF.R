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
		file <- sprintf("C:\\Users\\emiel\\Desktop\\Multi-Actor MNS\\Experimenten\\Experimenten\\4H\\Exp2_4H close\\Data\\data0%d.txt", i)
	}else{
		file <- sprintf("C:\\Users\\emiel\\Desktop\\Multi-Actor MNS\\Experimenten\\Experimenten\\4H\\Exp2_4H close\\Data\\data%d.txt", i)
	}

	data <- read.table(file, header = F)

	###### NAME VARIABLES ######

	names <- c("ppn", "lftd", "gesl", "blok", "cue", "vinger", "aantal", "pos", "con", "xr", "r", "corr",
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

##############################################################
#######	  		SPATIAL				########
##############################################################

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
		if(data.full$cue[i] == 1){
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

head(data.full[,c("cue","aantal","pos","spat","con_spat")], n = 20)
ftable(data.full$aantal, data.full$con, data.full$con_spat)

########################################################
#######	  DELETE UNIMPORTANT VARIABLES	########
########################################################

data.full <- data.full[ , -c(5,6,10,11,14,18) ]
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
data.full$con <- factor(data.full$con, levels = c(1,0), labels = c("C", "IC"))
	#congruency (i.e., congruent or incongruent)
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

data.full <- data.full[, c(1:5,14,6:7,13,8:12)]
str(data.full)

########################################################
#######	  	DESCRIPTIVE DATA			########
########################################################

data.descriptive <- data.full
data.descriptive <- data.descriptive[data.descriptive$ppn != 48, ] #excluded from sample (see below)
data.descriptive <- data.descriptive[data.descriptive$ppn != 22, ] #excluded from sample (see below)
data.descriptive <- cast(ppn + lftd + gesl ~ ., data = data.descriptive , value = "rt", length)
data.descriptive

print(age_m <- mean(data.descriptive$lftd, na.rm = T))
print(age_sd <- sd(data.descriptive$lftd, na.rm = T))
print(gender <- ftable(data.descriptive$gesl))

########################################################
#######	 	 OUTLIER DATA			########
########################################################

nrow(data.full)
head(data.full)

	###### full ######

data.outlier <- data.full[data.full$ppn != 48, ]
data.outlier <- data.full[data.full$ppn != 22, ]
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

data.rt <- data.full
data.rt <- data.full[data.full$plus100 == 1, ]
data.rt <- data.rt[data.rt$min2000 == 1, ]
data.rt <- data.rt[data.rt$max3SD == 1, ]
data.rt <- data.rt[data.rt$corr == 1, ]
data.rt <- data.rt[, -c(2,3,10,12:14)]
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
data.corr <- data.full[data.full$plus100 == 1, ]
data.corr <- data.corr[data.corr$min2000 == 1, ]
data.corr <- data.corr[, -c(2,3,12:14)]
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
#######	 	 REMOVE PPN				########
########################################################

data.rt <- data.rt[data.rt$ppn != 48, ]
data.rt <- data.rt[data.rt$ppn != 22, ]

data.corr <- data.corr[data.corr$ppn != 48, ]
data.corr <- data.corr[data.corr$ppn != 22, ]

data.rt.exp2.raw <- data.rt
data.corr.exp2.raw <- data.corr

########################################################
#######	 	CORRELATION RT & ERR		########
########################################################

data.cor.rt <- cast(ppn ~ con, data = data.rt, value = "rt", mean)
data.cor.rt["con"] <- data.cor.rt$IC - data.cor.rt$C

data.cor.err <- cast(ppn ~ con + corr, data = data.corr, value = "rt", length)
data.cor.err["C_perc_err"] <- data.cor.err$C_0 / (data.cor.err$C_0 + data.cor.err$C_1)
data.cor.err["IC_perc_err"] <- data.cor.err$IC_0 / (data.cor.err$IC_0 + data.cor.err$IC_1)
data.cor.err["con"] <- data.cor.err$IC_perc_err - data.cor.err$C_perc_err

cor.test(data.cor.rt$con, data.cor.err$con)
plot(data.cor.rt$con, data.cor.err$con)

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

	###### RAW ######

###### panel ######

panel.err <- function(x, y, subscripts, groups, stderr, box.ratio, ...)
{
  d <- 1/(nlevels(groups)+nlevels(groups)/box.ratio)

  g <- (as.numeric(groups[subscripts])-1); g <- (g-median(g))*d

  panel.arrows(as.numeric(x)+g,y-stderr[subscripts], as.numeric(x)+g, y+stderr[subscripts],
               code=3,angle=90, length=0.3, unit = "cm")
}

###### SE ######

data.plot.rt.se <-  cast(ppn ~ aantal + con, data = data.rt, value = "rt", mean)
SE <- SE_Within(data = data.plot.rt.se, colnames = names(data.plot.rt.se)[-1], ref = T)$ci95.within.corr

###### plot ######

data.plot.rt.a <- cast(ppn + aantal + con ~ ., data = data.rt, value = "rt", mean)
data.plot.rt <- cast(aantal + con ~ ., data = data.plot.rt.a, mean)
names(data.plot.rt)[3] <- "RT"

mytheme = trellis.par.get()
mytheme$superpose.polygon$col = c("white", "grey")
#mytheme$fontsize$text = 19

barchart(RT ~ aantal, groups = con, data = data.plot.rt,
	ylim = c(412,488), scales = list(tck = c(1,0), y = list(at = round(seq(410, 485, 5)))),
	xlab = list("Number of Observed Movements", cex = 1.1), ylab = list("RT", cex = 1.1),
	auto.key = list(corner = c(.95,.99), title = "Congruency", cex.title = 1, lines.title = 2.5, points = F, rectangles = T), 
	par.settings = mytheme,
	panel=function(x, y, subscripts, groups, stderr, box.ratio, ...)
	{
             panel.barchart(x, y, subscripts = subscripts, groups = groups, box.ratio = box.ratio, origin = 0,...)
             panel.abline(h = 0, col = "black",...)
             panel.err(x, y, subscripts = subscripts, groups = groups, box.ratio = box.ratio, stderr = SE)
      }	
)

	###### DIFF ######

###### panel ######

panel.err.diff <- function(x, y, subscripts, stderr, box.ratio, ...)
{
  d <- 1/(nlevels(y)+nlevels(y)/box.ratio)

  panel.arrows(as.numeric(x),y-stderr[subscripts], as.numeric(x), y+stderr[subscripts],
               code=3,angle=90, length=0.3, unit = "cm")
}


###### SE ######

data.rt.diff.a <- cast(ppn ~ aantal + con, data = data.rt, value = "rt", mean)
data.rt.diff <- data.frame(ppn = data.rt.diff.a$ppn, One = rep(999, times = nrow(data.rt.diff.a)), Two = rep(999, times = nrow(data.rt.diff.a)), Three = rep(999, times = nrow(data.rt.diff.a)), Four = rep(999, times = nrow(data.rt.diff.a)))
for(i in 1:nrow(data.rt.diff.a)){
	data.rt.diff$One[i] = data.rt.diff.a$One_IC[i] - data.rt.diff.a$One_C[i]
	data.rt.diff$Two[i] = data.rt.diff.a$Two_IC[i] - data.rt.diff.a$Two_C[i]
	data.rt.diff$Three[i] = data.rt.diff.a$Three_IC[i] - data.rt.diff.a$Three_C[i]
	data.rt.diff$Four[i] = data.rt.diff.a$Four_IC[i] - data.rt.diff.a$Four_C[i]
}

SE_diff <- SE_Within(data = data.rt.diff, colnames = c("One","Two","Three","Four"), ref = T)$se.between

###### data plot ######

data.plot.rt.diff <- data.frame(aantal = c(1:4), Congruency = 1:4)
data.plot.rt.diff$Congruency[1] <- mean(data.rt.diff$One)
data.plot.rt.diff$Congruency[2] <- mean(data.rt.diff$Two)
data.plot.rt.diff$Congruency[3] <- mean(data.rt.diff$Three)
data.plot.rt.diff$Congruency[4] <- mean(data.rt.diff$Four)
data.plot.rt.diff$aantal <- factor(data.plot.rt.diff$aantal, levels = c(1,2,3,4), labels = c("One", "Two", "Three", "Four"))

###### plot ######

mytheme = trellis.par.get()
mytheme$plot.polygon$col = "grey"
mytheme$fontsize$text = 14
mytheme$axis.text$cex = 1

barchart(Congruency ~ aantal, data = data.plot.rt.diff, ylab = "Congruency Effect", ylim = c(-1.5, 43.5), scale = list(tick.number = 10),
	   par.settings = mytheme,
	   panel = function(x, y, subscripts, stderr, box.ratio, ...)
	   {
             panel.barchart(x, y, subscripts = subscripts, box.ratio = box.ratio, origin = -1.5,...)
             panel.err.diff(x, y, subscripts = subscripts, box.ratio = box.ratio, stderr = SE_diff)
         })

########################################################
#######	 	   	ANALYSIS			########
########################################################

	####### LME ######

##### random structure ######

#lme.rt <- lmer(rt ~ con*aantal_continu + (1 + con + aantal_continu + con:aantal_continu | ppn), data = data.rt, REML = F)
	#no convergence with or withour correlations
#lme.rt2 <- lmer(rt ~ con*aantal_continu + (1 + con + aantal_continu | ppn), data = data.rt, REML = F)
#lme.rt3 <- lmer(rt ~ con*aantal_continu + (1 + con + aantal_continu || ppn), data = data.rt, REML = F)
#lme.rt4 <- lmer(rt ~ con*aantal_continu + (1 + con | ppn), data = data.rt, REML = F)
#lme.rt5 <- lmer(rt ~ con*aantal_continu + (1 + aantal_continu | ppn), data = data.rt, REML = F)

#anova(lme.rt2, lme.rt4)

##### fixed effects ######

lme.rt4 <- lmer(rt ~ con*aantal_continu + (1 + con | ppn), data = data.rt, REML = T)
lme.rt4 <- lmer(rt ~ con + con:aantal_continu + (1 + con | ppn), data = data.rt, REML = T)
summary(lme.rt4)
anova(lme.rt4, type = 3)
confint(lme.rt4, parm = names(fixef(lme.rt4))[-1], method = "Wald")

	###### ANALYSIS GAM #####

###### models ######

lme.rt.gamm.null <- gam(rt ~ con + aantal_continu + s(factor(data.rt$ppn), bs = "re"), data = data.rt)
	#adding random slope for congruency changes little
		#lme.rt.gamm.null <- gam(rt ~ con + aantal_continu + s(factor(data.rt$ppn), bs = "re") + s(con, factor(data.rt$ppn), bs = "re"), data = data.rt)
summary(lme.rt.gamm.null)

lme.rt.gamm.lin <- gam(rt ~ con + aantal_continu:con + s(factor(data.rt$ppn), bs = "re"), data = data.rt)
	#model fit does not change if main effect aantal_continu is added
		#lme.rt.gamm.lin <- gam(rt ~ aantal_continu*con + s(factor(data.rt$ppn), bs = "re"), data = data.rt, select = F, gamma = 1)
	#adding random slope for congruency changes little
		#lme.rt.gamm.lin <- gam(rt ~ con + aantal_continu:con + s(factor(data.rt$ppn), bs = "re") + s(con, factor(data.rt$ppn), bs = "re"), data = data.rt)
summary(lme.rt.gamm.lin)

lme.rt.gamm <- gam(rt ~ con + s(aantal_continu, bs = "tp", k = 4, by = con) + s(factor(data.rt$ppn), bs = "re"), data = data.rt)
	#adding random slope for congruency changes little
		#lme.rt.gamm <- gam(rt ~ con + s(aantal_continu, bs = "tp", k = 4, by = con) + s(factor(data.rt$ppn), bs = "re") + s(con, factor(data.rt$ppn), bs = "re"), data = data.rt)
summary(lme.rt.gamm)

###### statistics ######

AIC(lme.rt.gamm.null, lme.rt.gamm.lin, lme.rt.gamm)
round(AIC(lme.rt.gamm.null) - AIC(lme.rt.gamm.lin), 2)
round(AIC(lme.rt.gamm.null) - AIC(lme.rt.gamm), 2)
round(AIC(lme.rt.gamm.lin) - AIC(lme.rt.gamm), 2)

round(lme.rt.gamm.null$gcv.ubre - lme.rt.gamm.lin$gcv.ubre, 2)
round(lme.rt.gamm.null$gcv.ubre - lme.rt.gamm$gcv.ubre, 2)
round(lme.rt.gamm.lin$gcv.ubre - lme.rt.gamm$gcv.ubre, 2)

anova.gam(lme.rt.gamm.lin, lme.rt.gamm, test = "Chisq")

########################################################
#######	 	   PLOTS LME		  	########
########################################################

data.predict <- cbind(data.rt, predict(lme.rt.gamm.lin), predict(lme.rt.gamm))
names(data.predict)[9:10] <- c("predict.lin","predict.gam")
head(data.predict)

data.plot.fit.lin.a <- cast(ppn + aantal + con ~ ., data = data.predict, value = "predict.lin", mean)
data.plot.fit.lin <- cast(aantal + con ~ ., data = data.plot.fit.lin.a, mean)
head(data.plot.fit.lin)
names(data.plot.fit.lin)[3] <- "predict.lin"

data.plot.fit.gam.a <- cast(ppn + aantal + con ~ ., data = data.predict, value = "predict.gam", mean)
data.plot.fit.gam <- cast(aantal + con ~ ., data = data.plot.fit.gam.a, mean)
head(data.plot.fit.gam)
names(data.plot.fit.gam)[3] <- "predict.gam"

mytheme = trellis.par.get()
mytheme$superpose.polygon$col = c("white", "grey")
mytheme$fontsize$text = 14
mytheme$axis.text$cex = 1

#ppi <- 1200
#tiff(filename = "C:\\Users\\emiel\\Desktop\\Fig3_hres.tiff", compression = "zip", width = 7*ppi, height = 7*ppi, 
#     pointsize = 7*(9/5), res = ppi)

barchart(RT ~ aantal, groups = con, data = data.plot.rt,
	ylim = c(423,477), scales = list(tck = c(1,0), y = list(at = round(seq(425, 475, 5)))), box.ratio = 3,
	xlab = list("Number of Observed Movements", cex = 1.1), ylab = list("RT", cex = 1.1),
      auto.key = list(corner = c(0.025,.975), points = F, rectangles = T, height = 0.85, size = 3.5, padding.text = 2.5),
	par.settings = mytheme,
	panel=function(x, y, subscripts, groups, stderr, box.ratio, ...)
	{
             panel.barchart(x, y, subscripts = subscripts, groups = groups, box.ratio = box.ratio, origin = 0,...)
             panel.err(x, y, subscripts = subscripts, groups = groups, box.ratio = box.ratio, stderr = SE)
      }	
)+
xyplot(predict.gam ~ aantal, groups = con, type = c("spline", "p"), data = data.plot.fit.gam, lwd = 2, alpha = 1, par.settings = simpleTheme(col = "black", lty = 1, pch = 19),
		panel = panel.superpose, 
	 	panel.groups = function(x, y, ... , group.number){
       	panel.xyplot(x + (group.number-1.5)/(8/3), y, ...) #x: 1-4, group.number: 1 or 2
	 }) #+
#xyplot(predict.lin ~ aantal, groups = con, type = c("spline", "p"), data = data.plot.fit.lin, lwd = 2, alpha = 1, par.settings = simpleTheme(col = "black", lty = 2, pch = 19),
#		panel = panel.superpose,
#	 	panel.groups = function(x, y, ... , group.number){
#       	panel.xyplot(x + (group.number-1.5)/(8/3), y, ...) #x: 1-4, group.number: 1 or 2
#	 })

#dev.off()


				##################################################
				#######	SPATIAL RT ANALYSIS		########
				##################################################

ftable(data.full$aantal, data.full$con, data.full$con_spat)
ftable(data.rt$aantal, data.rt$con, data.rt$con_spat)

########################################################
#######	 	 VISUALISATION			########
########################################################

data.plot.spatial.a <- cast(ppn + aantal + con_spat ~ ., data = data.rt, value = "rt", mean)
data.plot.spatial <- cast(aantal + con_spat ~ ., data = data.plot.spatial.a, mean)
names(data.plot.spatial)[3] <- "RT"
data.plot.spatial <- data.plot.spatial[1:5,]

barchart(RT ~ aantal, groups = con_spat, data = data.plot.spatial, auto.key = T)

########################################################
#######	 	 	ANALYSIS			########
########################################################

data.analysis.spatial <- cast(ppn ~ aantal + con_spat, data = data.rt, value = "rt", mean)
data.analysis.spatial <- data.analysis.spatial[,-c(6:8)]
spat.exp2.raw <- data.analysis.spatial
head(data.analysis.spatial)
	
aantal <- factor(rep(1:2, each = 2))
con <- factor(rep(1:2, times = 2))
idata <- data.frame(aantal, con)

fit.rt <- lm(cbind(One_sC, One_sIC, Two_sC, Two_sIC) ~ 1, data = data.analysis.spatial)
etasq(Anova(fit.rt, type = "III", test = "Wilks", idata = idata, idesign = ~aantal*con), anova = T, partial = T)
etasq(Anova(fit.rt, type = "III", test = "Wilks", idata = idata, idesign = ~aantal*con), anova = T, partial = T)$"approx F"

				##################################################
				#######		EXP 1 + EXP 2		########
				##################################################

########################################################
#######	 	  PREPROCESSING			########
########################################################

data.rt.exp1 <- data.rt.exp1.raw
data.rt.exp1$exp <- 1
data.rt.exp1$exp <- factor(data.rt.exp1$exp)
str(data.rt.exp1)

data.rt.exp2 <- data.rt.exp2.raw
data.rt.exp2$exp <- 2
data.rt.exp2$exp <- factor(data.rt.exp2$exp)
data.rt.exp2$ppn <- data.rt.exp2$ppn + max(data.rt.exp1$ppn)
str(data.rt.exp2)

data.rt.combo <- rbind(data.rt.exp1, data.rt.exp2)
str(data.rt.combo)

summary(data.rt.combo$ppn)
length(table(data.rt.combo$ppn))

	###### LME ######

##### random structure ######

#lme.rt.combo <- lmer(rt ~ exp + con*aantal_continu + (1 + con + aantal_continu + con:aantal_continu | ppn), data = data.rt.combo, REML = F)
#lme.rt2.combo <- lmer(rt ~ exp + con*aantal_continu + (1 + con + aantal_continu + con:aantal_continu || ppn), data = data.rt.combo, REML = F)
#lme.rt3.combo <- lmer(rt ~ exp + con*aantal_continu + (1 + con + aantal_continu | ppn), data = data.rt.combo, REML = F)
#lme.rt4.combo <- lmer(rt ~ exp + con*aantal_continu + (1 + aantal_continu | ppn), data = data.rt.combo, REML = F)
#lme.rt5.combo <- lmer(rt ~ exp + con*aantal_continu + (1 + con | ppn), data = data.rt.combo, REML = F)

#anova(lme.rt.combo, lme.rt3.combo)

##### fixed effects ######

lme.rt.combo <- lmer(rt ~ exp + aantal_continu*con + (1 + con + aantal_continu + con:aantal_continu | ppn), data = data.rt.combo, REML = T)
summary(lme.rt.combo)
confint(lme.rt.combo, parm = names(fixef(lme.rt.combo))[-1], method = "Wald")

lme.rt.combo.sep <- lmer(rt ~ exp + aantal_continu:con + (1 + con + aantal_continu + con:aantal_continu | ppn), data = data.rt.combo, REML = T)
summary(lme.rt.combo.sep)
confint(lme.rt.combo.sep, parm = names(fixef(lme.rt.combo.sep))[-1], method = "Wald")

########################################################
#######	 	   ANALYSIS				########
########################################################

	###### gam ######

###### models ######

lme.rt.null.combo <- gam(rt ~ exp + con + aantal_continu + s(factor(data.rt.combo$ppn), bs = "re"), data = data.rt.combo)
summary(lme.rt.null.combo)

lme.rt.lin.combo <- gam(rt ~ exp + con + aantal_continu:con + s(factor(data.rt.combo$ppn), bs = "re"), data = data.rt.combo)
summary(lme.rt.lin.combo)

lme.rt.gamm.combo <- gam(rt ~ exp + con + s(aantal_continu, k = 4, bs = "tp", by = con) + s(factor(data.rt.combo$ppn), bs = "re"), data = data.rt.combo)
summary(lme.rt.gamm.combo)

###### statistics ######

AIC(lme.rt.null.combo,lme.rt.lin.combo, lme.rt.gamm.combo)
round(AIC(lme.rt.null.combo) - AIC(lme.rt.lin.combo),2)
round(AIC(lme.rt.null.combo) - AIC(lme.rt.gamm.combo),2)
round(AIC(lme.rt.lin.combo) - AIC(lme.rt.gamm.combo),2)

round(lme.rt.null.combo$gcv.ubre,1) - round(lme.rt.lin.combo$gcv.ubre,1)
round(lme.rt.null.combo$gcv.ubre,1) - round(lme.rt.gamm.combo$gcv.ubre,1)
round(lme.rt.lin.combo$gcv.ubre,1) - round(lme.rt.gamm.combo$gcv.ubre,1)

anova.gam(lme.rt.lin.combo, lme.rt.gamm.combo, test = "Chisq")

	###### GAMM4 ######

lme.rt.null.combo <- gamm4(rt ~ exp + con + aantal_continu, random = ~(1 | ppn), data = data.rt.combo, REML = T)
summary(lme.rt.null.combo$gam)
logLik(lme.rt.null.combo$mer)

lme.rt.lin.combo <- gamm4(rt ~ exp + con + aantal_continu:con, random = ~(1 | ppn), data = data.rt.combo, REML = T)
summary(lme.rt.lin.combo$gam)
logLik(lme.rt.lin.combo$mer)

lme.rt.gamm.combo <- gamm4(rt ~ exp + con + s(aantal_continu, k = 4, bs = "tp", by = con), random = ~(1 | ppn), data = data.rt.combo, REML = T) #gamm4 gives same results
summary(lme.rt.gamm.combo$gam)
logLik(lme.rt.gamm.combo$mer)

########################################################
#######	 	   PLOT				########
########################################################

	###### SE ######

data.plot.combo.se <- cast(ppn + exp ~ aantal + con, data = data.rt.combo, value = "rt", mean)
SE.combo <- SE_Within(data = data.plot.combo.se, colnames = names(data.plot.combo.se)[-c(1,2)], ref = T)$ci95.within.corr

	###### PLOT ######

data.plot.rt.a.combo <- cast(ppn + exp + aantal + con ~ ., data = data.rt.combo, value = "rt", mean)
data.plot.rt.b.combo <- cast(exp + aantal + con ~ ., data = data.plot.rt.a.combo, mean)
data.plot.rt.combo <- cast(aantal + con ~ ., data = data.plot.rt.b.combo, mean)
names(data.plot.rt.combo)[3] <- "RT"

data.predict <- cbind(data.rt.combo, predict(lme.rt.lin.combo), predict(lme.rt.gamm.combo))
names(data.predict)[10:11] <- c("predict.lin", "predict.gam")
head(data.predict)

data.plot.fit.lin.a <- cast(ppn + exp + aantal + con ~ ., data = data.predict, value = "predict.lin", mean)
data.plot.fit.lin.b <- cast(exp + aantal + con ~ ., data = data.plot.fit.lin.a, mean)
data.plot.fit.lin <- cast(aantal + con ~ ., data = data.plot.fit.lin.b, mean)
head(data.plot.fit.lin)
names(data.plot.fit.lin)[3] <- "predict.lin"

data.plot.fit.gam.a <- cast(ppn + exp + aantal + con ~ ., data = data.predict, value = "predict.gam", mean)
data.plot.fit.gam.b <- cast(exp + aantal + con ~ ., data = data.plot.fit.gam.a, mean)
data.plot.fit.gam <- cast(aantal + con ~ ., data = data.plot.fit.gam.b, mean)
head(data.plot.fit.gam)
names(data.plot.fit.gam)[3] <- "predict.gam"

mytheme = trellis.par.get()
mytheme$superpose.polygon$col = c("white", "grey")
mytheme$fontsize$text = 14
mytheme$axis.text$cex = 1

#ppi <- 1200
#tiff(filename = "C:\\Users\\emiel\\Desktop\\Fig9_hres.tiff", compression = "zip", width = 7*ppi, height = 7*ppi, 
#     pointsize = 7*(9/5), res = ppi)

barchart(RT ~ aantal, groups = con, data = data.plot.rt.combo,
	ylim = c(413,462), scales = list(tck = c(1,0), y = list(at = round(seq(415, 462, 5)))), box.ratio = 3,
	xlab = list("Number of Observed Movements", cex = 1.1), ylab = list("RT", cex = 1.1),
      auto.key = list(corner = c(0.025,.975), points = F, rectangles = T, height = 0.85, size = 3.5, padding.text = 2.5),
	par.settings = mytheme,
	panel=function(x, y, subscripts, groups, stderr, box.ratio, ...)
	{
             panel.barchart(x, y, subscripts = subscripts, groups = groups, box.ratio = box.ratio, origin = 0,...)
             panel.err(x, y, subscripts = subscripts, groups = groups, box.ratio = box.ratio, stderr = SE.combo)
      }	
)+
xyplot(predict.gam ~ aantal, groups = con, type = c("spline", "p"), data = data.plot.fit.gam, lwd = 2, alpha = 1, par.settings = simpleTheme(col = "black", lty = 1, pch = 19),
		panel = panel.superpose, 
	 	panel.groups = function(x, y, ... , group.number){
       	panel.xyplot(x + (group.number-1.5)/(8/3), y, ...) #x: 1-4, group.number: 1 or 2
	 }) #+
#xyplot(predict.lin ~ aantal, groups = con, type = c("spline", "p"), data = data.plot.fit.lin, lwd = 2, alpha = 1, par.settings = simpleTheme(col = "black", lty = 2, pch = 19),
#		panel = panel.superpose,
#	 	panel.groups = function(x, y, ... , group.number){
#       	panel.xyplot(x + (group.number-1.5)/(8/3), y, ...) #x: 1-4, group.number: 1 or 2
#	 })

#dev.off()

				##################################################
				#######	   Error Rate ANALYSIS		########
				##################################################

########################################################
#######	 	CALCULATE ERROR RATES		########
########################################################

data.err <- cast(ppn + aantal + aantal_continu + con ~ corr, data = data.corr, value = "rt", length)
data.err["err"] <- data.err[,5] / (data.err[,5] + data.err[,6])
data.err <- data.err[,-c(5,6)]
head(data.err)

########################################################
#######	 	 VISUALISATION			########
########################################################

	######  RAW	 ######

data.plot.err.a <- cast(ppn + aantal + con ~ ., data = data.err, value = "err", mean)
data.plot.err <- cast(aantal + con ~ ., data = data.plot.err.a, mean)
names(data.plot.err)[3] <- "ErrorRate"

barchart(ErrorRate ~ aantal, groups = con, data = data.plot.err, type = c("l", "p"), auto.key = list(corner = c(.95,.99)))

	######  DIFF  ######

###### data plot ######

data.err.diff.a <- cast(ppn ~ aantal + con, data = data.err, value = "err", mean)
data.err.diff <- data.frame(ppn = data.err.diff.a$ppn, One = rep(999, times = nrow(data.err.diff.a)), Two = rep(999, times = nrow(data.err.diff.a)), Three = rep(999, times = nrow(data.err.diff.a)), Four = rep(999, times = nrow(data.err.diff.a)))
for(i in 1:nrow(data.err.diff.a)){
	data.err.diff$One[i] = data.err.diff.a$One_IC[i] - data.err.diff.a$One_C[i]
	data.err.diff$Two[i] = data.err.diff.a$Two_IC[i] - data.err.diff.a$Two_C[i]
	data.err.diff$Three[i] = data.err.diff.a$Three_IC[i] - data.err.diff.a$Three_C[i]
	data.err.diff$Four[i] = data.err.diff.a$Four_IC[i] - data.err.diff.a$Four_C[i]
}

data.plot.err.diff <- data.frame(aantal = c(1:4), Congruency = 1:4)
data.plot.err.diff$Congruency[1] <- mean(data.err.diff$One)
data.plot.err.diff$Congruency[2] <- mean(data.err.diff$Two)
data.plot.err.diff$Congruency[3] <- mean(data.err.diff$Three)
data.plot.err.diff$Congruency[4] <- mean(data.err.diff$Four)
data.plot.err.diff$aantal <- factor(data.plot.err.diff$aantal, levels = c(1,2,3,4), labels = c("One", "Two", "Three", "Four"))

###### plot ######

barchart(Congruency*100 ~ aantal, data = data.plot.err.diff, ylab = "Congruency Effect", ylim = c(-0.4,6.4), scale = list(tick.number = 20))

				##################################################
				#######	    EXP 1 + EXP 2 ER		########
				##################################################

########################################################
#######	 	  PREPROCESSING			########
########################################################

data.corr.exp1 <- data.corr.exp1.raw
data.corr.exp1$exp <- 1
data.corr.exp1$exp <- factor(data.corr.exp1$exp)
str(data.corr.exp1)

data.corr.exp2 <- data.corr.exp2.raw
data.corr.exp2$exp <- 2
data.corr.exp2$exp <- factor(data.corr.exp2$exp)
data.corr.exp2$ppn <- data.corr.exp2$ppn + max(data.corr.exp1$ppn)
str(data.corr.exp2)

data.corr.combo <- rbind(data.corr.exp1, data.corr.exp2)
str(data.corr.combo)
summary(data.corr.combo)

########################################################
#######	 	   ANALYSIS				########
########################################################

	###### LME #####

glme.err.combo <- glmer(corr ~ exp + aantal_continu*con + (1 | ppn), family = "binomial", data = data.corr.combo)
summary(glme.err.combo)

	###### GAM #####

lme.corr.lin.combo <- gam(corr ~ exp + con + aantal_continu:con + s(factor(data.corr.combo$ppn), bs = "re"), data = data.corr.combo, family = "binomial")
summary(lme.corr.lin.combo)

lme.corr.gamm.combo <- gam(corr ~ exp + con + s(aantal_continu, k = 4, bs = "tp", by = con) + s(factor(data.corr.combo$ppn), bs = "re"), data = data.corr.combo, family = "binomial")
summary(lme.corr.gamm.combo)

AIC(lme.corr.lin.combo, lme.corr.gamm.combo)

########################################################
#######	 	CALCULATE ERROR RATES		########
########################################################

data.err.combo <- cast(ppn + exp + aantal + aantal_continu + con ~ corr, data = data.corr.combo, value = "rt", length)
data.err.combo["err"] <- data.err.combo[,6] / (data.err.combo[,6] + data.err.combo[,7])
data.err.combo <- data.err.combo[,-c(6,7)]
head(data.err.combo)

########################################################
#######	 	 VISUALISATION			########
########################################################

	###### SE ######

data.plot.err.combo.se <- cast(ppn + exp ~ aantal + con, data = data.err.combo, value = "err", mean)
SE.combo.err <- SE_Within(data = data.plot.err.combo.se, colnames = names(data.plot.err.combo.se)[-c(1,2)], ref = T)$ci95.within.corr*100

	###### PLOT ######

data.plot.err.combo.a <- cast(ppn + exp + aantal + con ~ ., data = data.err.combo, value = "err", mean)
data.plot.err.combo.b <- cast(exp + aantal + con ~ ., data = data.plot.err.combo.a, mean)
data.plot.err.combo <- cast(aantal + con ~ ., data = data.plot.err.combo.b, mean)
names(data.plot.err.combo)[3] <- "ErrorRate"

mytheme = trellis.par.get()
mytheme$superpose.polygon$col = c("white", "grey")
mytheme$fontsize$text = 14
mytheme$axis.text$cex = 1

ppi <- 1200
tiff(filename = "C:\\Users\\emiel\\Desktop\\FigS1_hres.tiff", compression = "zip", width = 7*ppi, height = 7*ppi, 
     pointsize = 7*(9/5), res = ppi)

barchart(ErrorRate*100 ~ aantal, groups = con, data = data.plot.err.combo,
	ylim = c(1.2,8.8), scales = list(tck = c(1,0), y = list(at = round(seq(1.5, 8.5, 1),1))), box.ratio = 3,
	xlab = list("Number of Observed Movements", cex = 1.1), ylab = list("Error Rate", cex = 1.1),
      auto.key = list(corner = c(0.025,.975), points = F, rectangles = T, height = 0.85, size = 3.5, padding.text = 2.5),
	par.settings = mytheme,
	panel=function(x, y, subscripts, groups, stderr, box.ratio, ...)
	{
             panel.barchart(x, y, subscripts = subscripts, groups = groups, box.ratio = box.ratio, origin = 0,...)
             panel.err(x, y, subscripts = subscripts, groups = groups, box.ratio = box.ratio, stderr = SE.combo.err)
      }	
)

dev.off()

########################################################
#######	 	   PLOTS LME		  	########
########################################################

prob.predict.glme.lin <- exp(predict(lme.corr.lin.combo)) / (1 + exp(predict(lme.corr.lin.combo)))
prob.predict.glme.gam <- exp(predict(lme.corr.gamm.combo)) / (1 + exp(predict(lme.corr.gamm.combo)))

data.predict <- cbind(data.corr.combo, 1 - prob.predict.glme.lin , 1 - prob.predict.glme.gam)
names(data.predict)[10:11] <- c("lin", "gam")

data.predict.lin <- cast(ppn + aantal + aantal_continu + con ~ ., data = data.predict, value = "lin", mean)
head(data.predict.lin)
names(data.predict.lin)[5] <- "lin"

data.predict.gam <- cast(ppn + aantal + aantal_continu + con ~ ., data = data.predict, value = "gam", mean)
head(data.predict.gam)
names(data.predict.gam)[5] <- "gam"

data.predict <- cbind(data.err.combo, data.predict.lin$lin, data.predict.gam$gam)
names(data.predict)[7:8] <- c("predict.lin", "predict.gam")
head(data.predict)

data.plot.fit.lin.a <- cast(ppn + aantal + con ~ ., data = data.predict, value = "predict.lin", mean)
data.plot.fit.lin <- cast(aantal + con ~ ., data = data.plot.fit.lin.a, mean)
data.plot.fit.lin
names(data.plot.fit.lin)[3] <- "predict.lin"

data.plot.fit.gam.a <- cast(ppn + aantal + con ~ ., data = data.predict, value = "predict.gam", mean)
data.plot.fit.gam <- cast(aantal + con ~ ., data = data.plot.fit.gam.a, mean)
head(data.plot.fit.gam)
names(data.plot.fit.gam)[3] <- "predict.gam"

barchart(ErrorRate ~ aantal, groups = con, data = data.plot.err.combo, type = c("l", "p"), auto.key = list(corner = c(.95,.99))) +
xyplot(predict.lin ~ aantal, groups = con, type = c("spline"), data = data.plot.fit.lin, col = "black") +
xyplot(predict.gam ~ aantal, groups = con, type = c("spline"), data = data.plot.fit.gam, col = "red")



