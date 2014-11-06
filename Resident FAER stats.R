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
survey <- data.frame(survey )

#Pseudo descriptive statistics; searching for differences in subpops
aggregate(apply(demog, 2, as.numeric), by=list(demog$Group), mean)
aggregate(apply(demog[demog$Trial=="Pre",], 2, as.numeric), by=list(demog[demog$Trial=="Pre",]$Group), mean)

aggregate(apply(video, 2, as.numeric), by=list(video$Group), mean)
aggregate(apply(video, 2, as.numeric), by=list(video$Trial), mean)
aggregate( TotalChecklistScore~Trial+Group, video, mean )
aggregate( TotalChecklistScore~Reviewer+Trial, video, mean )

aggregate(apply(survey , 2, as.numeric), by=list(survey$Group), mean)
aggregate(apply(survey , 2, as.numeric), by=list(survey$Trial), mean)

hist(video$TotalChecklistScore[video$Trial=="Ret"])
hist(video$TotalChecklistScore[video$Trial=="Post"])
hist(video$TotalChecklistScore[video$Trial=="Pre"])
hist(video$Globalscore[video$Trial=="Ret"])
hist(video$Globalscore[video$Trial=="Post"])
hist(video$Globalscore[video$Trial=="Pre"])

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


#T-tests between Intv and Cntl groups
t.test(video$TotalChecklistScore[video$Trial=="Ret" & video$Group=="c"],
video$TotalChecklistScore[video$Trial=="Ret" & video$Group=="i"],paired=TRUE)
t.test(video$TotalChecklistScore[video$Trial=="Post" & video$Group=="c"],
video$TotalChecklistScore[video$Trial=="Post" & video$Group=="i"],paired=TRUE)
t.test(video$TotalChecklistScore[video$Trial=="Pre" & video$Group=="c"],
video$TotalChecklistScore[video$Trial=="Pre" & video$Group=="i"],paired=TRUE)

#Barplot of Cntl/Intv.Pre/Post/Ret
dh <- video$Trial
levels(dh) <- c(2,1,3)
dh <- as.numeric(as.character(dh ))
ah <- video$Group
boxplot(video$TotalChecklistScore~ah+dh, ylab="Score", xaxt="n")
axis(1, at=1:6, c(paste("Cntl", c("Pre", "Post", "Ret"), sep="."), paste("Intv", c("Pre", "Post", "Ret"), sep=".")), col = "grey")


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
