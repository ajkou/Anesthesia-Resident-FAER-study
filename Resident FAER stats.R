setwd("C:\\Users\\KOU\\Documents\\GitHub\\Anesthesia-Resident-FAER-study")
demog <- read.table("DEMOG.txt", header=T, sep="\t")
video <- read.table("VIDEO.txt", header=T, sep="\t")
survey <- read.table("SURVEY.txt", header=T, sep="\t")

#data type transforms
	demog <- replace(apply(demog, 2, as.character), demog=="Y",1)
	demog <- replace(apply(demog, 2, as.character), demog=="N",0)
	demog <- replace(apply(demog, 2, as.character), demog=="M",1)
	demog <- replace(apply(demog, 2, as.character), demog=="F",0)
	demog <- data.frame(demog)
	survey <- replace(apply(survey , 2, as.character), survey =="SD",1)
	survey <- replace(apply(survey , 2, as.character), survey =="D",2)
	survey <- replace(apply(survey , 2, as.character), survey =="A",3)
	survey <- replace(apply(survey , 2, as.character), survey =="SA",4)
	survey <- data.frame(survey)
	
#Estimated power required for the study based on Udani 2014 Simulation based mastery learning; pooled variance used for effect size
	pwr.t.test(d=(4.4-1.5)/mean(c(3.2, 1.9)), sig.level=0.05, power=0.8, type="paired", alternative="greater")
	#Test that was used in paper
	pwr.t.test(d=(4.4-1.5)/mean(c(3.2, 1.9)), sig.level=0.05, power=0.8, type="two.sample", alternative="two.sided")
	
#Pseudo descriptive statistics; searching for differences in subpops
	group.c <- demog[demog$Trial=="Pre" & demog$Group=="c",]
	group.i <- demog[demog$Trial=="Pre" & demog$Group=="i",]
	mytable <- aggregate(apply(demog[demog$Trial=="Pre",], 2, as.numeric), by=list(demog[demog$Trial=="Pre",]$Group), mean)
	mytable.1 <- data.frame(t(mytable))
	mytable.1 <- apply(mytable.1, 2, as.character)
	mytable.1 <- apply(mytable.1, 2, as.numeric)
	mytable.1 <- apply(mytable.1, 2, round, digits=2)
	#input ranges
	mytable.range <- aggregate(apply(demog[demog$Trial=="Pre",], 2, as.numeric), by=list(demog[demog$Trial=="Pre",]$Group), range)
	mytable.range1 <- paste("[", t(mytable.range[1,])[seq(8,33,2),], "-", t(mytable.range[1,])[seq(9,33,2),], "]", sep="")
	mytable.range2 <- paste("[", t(mytable.range[2,])[seq(8,33,2),], "-", t(mytable.range[2,])[seq(9,33,2),], "]",  sep="")
	#input tests
	mytable.ttests <- NULL
	for (i in 4:ncol(demog)) {
	    myx <- try(t.test(as.numeric(as.character(demog[demog$Trial=="Pre" & demog$Group=="c",i])), as.numeric(as.character(demog[demog$Trial=="Pre" & demog$Group=="i",i]))), silent=T)
	    if (class(myx)!="try-error") {
	      if (names(demog)[i]=="Gender" | names(demog)[i]=="PreviousUSLines" | names(demog)[i]=="PreviousPNB" | names(demog)[i]=="PreviousObsPNB" | names(demog)[i]=="PreviousSimPNB") {
	        mytable.ttests <- c(mytable.ttests, prop.test(cbind(table(group.c[,i]), table(group.i[,i])), correct=T)[[3]])
	      } else {
	        mytable.ttests <- c(mytable.ttests, t.test(as.numeric(as.character(group.c[,i])), as.numeric(as.character(group.i[,i])))[[3]])
	      }	    
	    } else {
	      mytable.ttests <- c(mytable.ttests, 1)
	    }
	}
	mytable.ttests <- round(mytable.ttests, 2)
	mytable.1 <- cbind(mytable.1[5:17,1], mytable.range1, mytable.1[5:17,2], mytable.range2, mytable.ttests)
	mytable.1 <- rbind(c(nrow(group.c), "-", nrow(group.i), "-", "-"), mytable.1)
	colnames(mytable.1) <- c("Control", "range", "Intervention", "range", "test")
	rownames(mytable.1) <- c("n", colnames(mytable)[5:length(mytable)])
	mytable.1
	
	#Other aggregations
	aggregate(apply(video, 2, as.numeric), by=list(video$Group), mean)
	aggregate(apply(video, 2, as.numeric), by=list(video$Trial), mean)
	aggregate(TotalChecklistScore~Trial+Group, video, mean )
	aggregate(TotalChecklistScore~Reviewer+Trial, video, mean )
	aggregate(apply(survey , 2, as.numeric), by=list(survey$Group), mean)
	aggregate(apply(survey , 2, as.numeric), by=list(survey$Trial), mean)
	
	#difference in score to compare by paired t-test; Post is used to flag i/c as the repeated measures are row assorted
	#Post-Pre
	scorediff <- video$TotalChecklistScore[video$Trial=="Post"] - video$TotalChecklistScore[video$Trial=="Pre"]
	aggregate(scorediff, by=list(video$Group[video$Trial=="Post"]), mean )
	t.test(scorediff[video$Group[video$Trial=="Post"]=="c"], scorediff[video$Group[video$Trial=="Post"]=="i"], paired=T)
  #Ret-Post
	scorediff <- video$TotalChecklistScore[video$Trial=="Ret"] - video$TotalChecklistScore[video$Trial=="Post"]
	aggregate(scorediff, by=list(video$Group[video$Trial=="Post"]), mean )
	t.test(scorediff[video$Group[video$Trial=="Post"]=="c"], scorediff[video$Group[video$Trial=="Post"]=="i"], paired=T)
	#Ret-Pre
	scorediff <- video$TotalChecklistScore[video$Trial=="Ret"] - video$TotalChecklistScore[video$Trial=="Pre"]
	aggregate(scorediff, by=list(video$Group[video$Trial=="Post"]), mean )
	t.test(scorediff[video$Group[video$Trial=="Post"]=="c"], scorediff[video$Group[video$Trial=="Post"]=="i"], paired=T)
	


	
#Histograms of score dist
	hist(video$TotalChecklistScore[video$Trial=="Ret"])
	hist(video$TotalChecklistScore[video$Trial=="Post"])
	hist(video$TotalChecklistScore[video$Trial=="Pre"])
	hist(video$Globalscore[video$Trial=="Ret"])
	hist(video$Globalscore[video$Trial=="Post"])
	hist(video$Globalscore[video$Trial=="Pre"])

#Scattermatrix bundles
	library(car)
	Ret <- video$TotalChecklistScore[video$Trial=="Ret" & video$Reviewer==1]
	Post <- video$TotalChecklistScore[video$Trial=="Post" & video$Reviewer==1]
	Pre <- video$TotalChecklistScore[video$Trial=="Pre" & video$Reviewer==1]
	a1 <- Post-Pre
	a2 <- Ret-Post
	a3 <- Ret-Pre
	c1 <- as.numeric(as.character(demog$PreviousPNBn[demog$Trial=="Pre"]))
	cc <- as.numeric(as.character(demog$PreviousUSLinesn[demog$Trial=="Pre"]))
	ccc <- as.numeric(as.character(demog$PreviousObsPNBn[demog$Trial=="Pre"]))
	d <- as.numeric(as.character(demog$TimeSpentinSimPrac.s.[demog$Trial=="Pre"]))
	e1 <- as.numeric(as.character(survey$PNBConfid[survey$Trial=="Pre"]))
	e2 <- as.numeric(as.character(survey$PNBConfid[survey$Trial=="Post"]))		
	e3 <- as.numeric(as.character(survey$PNBConfid[survey$Trial=="Ret"]))
	f <- as.numeric(as.character(demog$Age[demog$Trial=="Pre"]))
	x <- as.numeric(as.character(demog$Gender[demog$Trial=="Pre"]))
	y <- demog$Group[demog$Trial=="Pre"]
	scatterplotMatrix(~Pre+Post+Ret+e1+e2+e3+d|y, var.labels=c("Pre", "Post", "Ret", "Confid-Pre", "Confid-Post", "Confid-Ret", "Time Spent"))
	scatterplotMatrix(~a1+a2+a3+e1+e2+e3+d|y, var.labels=c("df(Post,Pre)", "df(Ret,Post)", "df(Ret,Pre)", "Confid-Pre", "Confid-Post", "Confid-Ret", "Time Spent"))
	scatterplotMatrix(~Pre+Post+Ret+c1+cc+ccc+d|y, var.labels=c("Pre", "Post", "Ret", "nPNBexp", "nUSLines", "nPNBObs", "Time Spent"))
	scatterplotMatrix(~a1+a2+a3+c1+cc+ccc+d|y, var.labels=c("df(Post,Pre)", "df(Ret,Post)", "df(Ret,Pre)", "nPNBexp", "nUSLines", "nPNBObs", "Time Spent"))

	#Model Selection (bidirectional) fit <- lm(Post~ c1+ cc + ccc + d + x + y )
		library(MASS)
		fit <- lm(Post~ c1+ cc + ccc  )
		step <- stepAIC(fit, direction="both")
		step$anova # display results
		summary(lm(Post~  d+x  ))


#Comparison Tests 
	#T-tests between Intv and Cntl groups
	t.test(video$TotalChecklistScore[video$Trial=="Ret" & video$Group=="c"],
	video$TotalChecklistScore[video$Trial=="Ret" & video$Group=="i"],paired=F)
	t.test(video$TotalChecklistScore[video$Trial=="Post" & video$Group=="c"],
	video$TotalChecklistScore[video$Trial=="Post" & video$Group=="i"],paired=F)
	t.test(video$TotalChecklistScore[video$Trial=="Pre" & video$Group=="c"],
	video$TotalChecklistScore[video$Trial=="Pre" & video$Group=="i"],paired=F)
	
	#Chi-sq tests of global score
	chisq.test(table(video$Globalscore[video$Trial=="Ret"], video$Group[video$Trial=="Ret"]))
	chisq.test(table(video$Globalscore[video$Trial=="Post"], video$Group[video$Trial=="Post"]))
	chisq.test(table(video$Globalscore[video$Trial=="Pre"], video$Group[video$Trial=="Pre"]))
		#3 time points
		chisq.test(table(video$Globalscore, video$Trial))
	#Chi-sq tests of confidence statement
	chisq.test(table(survey$PNBConfid[survey$Trial=="Ret"], survey$Group[survey$Trial=="Ret"]))
	chisq.test(table(survey$PNBConfid[survey$Trial=="Post"], survey$Group[survey$Trial=="Post"]))
	chisq.test(table(survey$PNBConfid[survey$Trial=="Pre"], survey$Group[survey$Trial=="Pre"]))
		#3 time points
		chisq.test(table(survey$PNBConfid, survey$Trial))

#Barplot of Cntl/Intv.Pre/Post/Ret
	par(mfrow=c(1,2))
	dh <- video$Trial
	levels(dh) <- c(2,1,3)
	dh <- as.numeric(as.character(dh))
	ah <- video$Group
	boxplot(video$TotalChecklistScore~ah+dh, ylab="Checklist Score [0-18]", xaxt="n", col = "grey")
	axis(1, at=1:6, sapply(c("Pre", "Post", "Ret"), paste, c("Cntl","Intv"), sep=".")[1:6], cex.axis=0.8)
	boxplot(video$Globalscore~ah+dh, ylab="Global Score [1-5]", xaxt="n", col = "grey")
	axis(1, at=1:6, sapply(c("Pre", "Post", "Ret"), paste, c("Cntl","Intv"), sep=".")[1:6], cex.axis=0.8)

#Barplot of Confidence Cntl/Intv.Pre/Post/Ret
	par(mfrow=c(1,2))
	confid.tab <- aggregate( PNBConfid~Trial+Group, survey, table)
	barplot(t(confid.tab[c(2,5,1,4,3,6),3]), legend = 1:4, beside=TRUE, xlim=c(0,35))
	dh <- confid.tab[1:6,2]
	levels(dh) <- c("Cntl", "Intv")
	mylabels <- paste(unlist(confid.tab[1]), dh, sep=".")[c(2,5,1,4,3,6)]
	axis(1, labels=mylabels, at=seq(2.0, 30, by=5.2), tick=F, cex.axis=0.75)
	confid.tab <- aggregate( PNBConfid~Trial+Group, survey, table)
	barplot(t(confid.tab[c(2,5,1,4,3,6),3]), xlim=c(0,8.5), legend = 1:4, beside=F)
	axis(1, labels=mylabels, at=seq(0.75, 6.75, by=1.2), tick=F, cex.axis=0.75)

#Video score items over test period [1-18]
	items <- aggregate(.~Trial, video, sum )[c(2,1,3),]
	plot(rep(c(1,2,3),18),unlist(items[,6:23])/56, xlim=c(1,4.5), xaxt="n", ylab="%Correct", xlab="")
	axis(1, at=1:3, c("Pre", "Post", "Ret"))
	apply(items[,6:23]/56,2, lines, x=c(1,2,3))
	mytextPos <- items[3,6:23][order(items[3,6:23], decreasing=T)]
	plainNames <- sub("^X\\d+\\.", "", colnames(mytextPos))
	text(c(rep(3.1,4), 3.6, 4.15, 3.1, 3.6, 4.15, 3.1, 3.6, 4.15,  3.1, 3.1, 3.6, 3.1, 3.1, 3.1),mytextPos/56, plainNames, pos=4, cex=0.9)

#Individual Subject Travel Plot
	pre.sc <- video$TotalChecklistScore[video$Trial=="Pre" & video$Reviewer==2]
	post.sc <- video$TotalChecklistScore[video$Trial=="Post" & video$Reviewer==2]
	ret.sc <- video$TotalChecklistScore[video$Trial=="Ret" & video$Reviewer==2]
	my.groupi <- video$Group[seq(1,length(video$Group), by=6)]=="i"
	my.groupo <- !(my.groupi)
	rbind(pre.sc, post.sc, ret.sc)
	par(mfrow=c(1,2))
	plot(rep(1:3,1, each=28), c(pre.sc, post.sc, ret.sc))
	points(rbind(pre.sc[my.groupi], post.sc[my.groupi], ret.sc[my.groupi]), x=rep(1:3,14), col="red")
	points(rbind(pre.sc[my.groupo], post.sc[my.groupo], ret.sc[my.groupo]), x=rep(1:3,14), col="blue")
	apply(rbind(pre.sc[my.groupi], post.sc[my.groupi], ret.sc[my.groupi]),2, lines, x=c(1,2,3), col="red")
	apply(rbind(pre.sc[my.groupo], post.sc[my.groupo], ret.sc[my.groupo]),2, lines, x=c(1,2,3), col="blue")
	list1 <- c(pre.sc[my.groupi], post.sc[my.groupi], ret.sc[my.groupi])
	list2 <- c(pre.sc[my.groupo], post.sc[my.groupo], ret.sc[my.groupo])
	ft <- c(pre.sc[my.groupi]%in%pre.sc[my.groupo],
	post.sc[my.groupi]%in%post.sc[my.groupo],
	ret.sc[my.groupi]%in%ret.sc[my.groupo])
	points(c(pre.sc[my.groupi], post.sc[my.groupi], ret.sc[my.groupi])[ft], x=rep(1:3,1, each=14)[ft], col="purple", pch=16)

#ICC between rater1 and rater2
	library(psych)
	#Total score
	data.ICC <- cbind(subset(video["TotalChecklistScore"], video$Reviewer==1),subset(video["TotalChecklistScore"], video$Reviewer==2))
	colnames(data.ICC) <- c("Rater1", "Rater2")
	ICC(data.ICC)
	#Global score
	data.ICC <- cbind(subset(video["Globalscore"], video$Reviewer==1),subset(video["Globalscore"], video$Reviewer==2))
	colnames(data.ICC) <- c("Rater1", "Rater2")
	ICC(data.ICC)
	#Pre
	data.ICC <- cbind(subset(video["TotalChecklistScore"], video$Reviewer==1 & video$Trial=="Pre"),subset(video["TotalChecklistScore"], video$Reviewer==2  & video$Trial=="Pre"))
	colnames(data.ICC) <- c("Rater1", "Rater2")
	ICC(data.ICC)
	#Post
	data.ICC <- cbind(subset(video["TotalChecklistScore"], video$Reviewer==1 & video$Trial=="Post"),subset(video["TotalChecklistScore"], video$Reviewer==2  & video$Trial=="Post"))
	colnames(data.ICC) <- c("Rater1", "Rater2")
	ICC(data.ICC)
	#Ret
	data.ICC <- cbind(subset(video["TotalChecklistScore"], video$Reviewer==1 & video$Trial=="Ret"),subset(video["TotalChecklistScore"], video$Reviewer==2  & video$Trial=="Ret"))
	colnames(data.ICC) <- c("Rater1", "Rater2")
	ICC(data.ICC)

#TukeyHSD for checklistscore
	dh <- as.numeric(as.character(survey$TotalChecklistScore))
	fm1 <- aov(dh ~ survey$Group + survey$Trial)
	summary(fm1)
	TukeyHSD(fm1, ordered = TRUE)

#Friedman non parametric test of confidence
	friedman.test(survey$PNBConfid,survey$Trial,survey$Participant)

#RMANOVA of confidence level against group and trial; Group is a between subject variable
	dh <- as.numeric(as.character(survey$PNBConfid))
	summary(aov(dh~ (survey$Trial+survey$Group)+Error(survey$Participant)))

#Scoring system comparison; correlation
	cor(video$TotalChecklistScore, video$Globalscore, method="pearson")
	cor(video$TotalChecklistScore, video$Globalscore, method="spearman")


