# Years A's made it to playoffs
AY1 = c( 2000,2001,2002,2003,2012,2013 )
AY2 = c( 1992,2006 )
AY3 = c( 1990 )
# Year range of our analysis
styear = 1990
endyear = 2013


# Load column names
# (this file took a little manual editing from the website)
H = read.table('gamelog_header.txt',sep="\t")

# Load regular season data
RSdata = c()
for( y in c(styear:endyear) )
{
	rsfile = paste( "RegularSeason_1990_2013/GL",as.character(y),".TXT",sep="" )
	rsdata = read.table( rsfile,sep="," )
	RSdata = rbind( RSdata,rsdata )
}

# Load division series data
DVdata = read.table('GLDV.TXT',sep=",")

# Load league championship data
LCdata = read.table('GLLC.TXT',sep=",")

# Load world series data
WSdata = read.table('GLWS.TXT',sep=",")

# Reformat the tables a little bit so stats from home/visitors are in one column

DATA = list();
DATAw = list();
DATAl = list()

visi = c(1:21,22:49)
homi = c(1:21,50:77)

vis = RSdata[,visi]
hom = RSdata[,homi]
names(hom) = names(vis)
DATA[["RS"]] = rbind( hom,vis )
w1 = vis[which(vis$V10 > vis$V11),] # games where visiting team won
w2 = hom[which(hom$V11 > hom$V10),] # games where home team won
tmp = w2$V4
w2$V4 = w2$V7
w2$V7 = tmp
DATAw[["RS"]] = rbind(w1,w2)
l1 = vis[which(vis$V10 < vis$V11),] # games where visiting team loss
l2 = hom[which(hom$V11 < hom$V10),] # games where home team loss
tmp = l2$V4
l2$V4 = l2$V7
l2$V7 = l2$V4
DATAl[["RS"]] = rbind(l1,l2)

vis = DVdata[,visi]
hom = DVdata[,homi]
names(hom) = names(vis)
DATA[["DV"]] = rbind( hom,vis )
w1 = vis[which(vis$V10 > vis$V11),]
w2 = hom[which(hom$V11 > hom$V10),]
tmp = w2$V4
w2$V4 = w2$V7
w2$V7 = tmp
DATAw[["DV"]] = rbind(w1,w2)
l1 = vis[which(vis$V10 < vis$V11),]
l2 = hom[which(hom$V11 < hom$V10),]
tmp = l2$V4
l2$V4 = l2$V7
l2$V7 = l2$V4
DATAl[["DV"]] = rbind(l1,l2)

vis = LCdata[,visi]
hom = LCdata[,homi]
names(hom) = names(vis)
DATA[["LC"]] = rbind( hom,vis )
w1 = vis[which(vis$V10 > vis$V11),]
w2 = hom[which(hom$V11 > hom$V10),]
tmp = w2$V4
w2$V4 = w2$V7
w2$V7 = tmp
DATAw[["LC"]] = rbind(w1,w2)
l1 = vis[which(vis$V10 < vis$V11),]
l2 = hom[which(hom$V11 < hom$V10),]
tmp = l2$V4
l2$V4 = l2$V7
l2$V7 = l2$V4
DATAl[["LC"]] = rbind(l1,l2)

vis = WSdata[,visi]
hom = WSdata[,homi]
names(hom) = names(vis)
DATA[["WS"]] = rbind( hom,vis )
w1 = vis[which(vis$V10 > vis$V11),]
w2 = hom[which(hom$V11 > hom$V10),]
tmp = w2$V4
w2$V4 = w2$V7
w2$V7 = tmp
DATAw[["WS"]] = rbind(w1,w2)
l1 = vis[which(vis$V10 < vis$V11),]
l2 = hom[which(hom$V11 < hom$V10),]
tmp = l2$V4
l2$V4 = l2$V7
l2$V7 = l2$V4
DATAl[["WS"]] = rbind(l1,l2)


#####################################################

# Is there really better pitching in the post-season?
# Fewer walks 
#	with and without at bat normalization
#	split by winners and losers
# More strikeouts
# Fewer at bats

atbati = 22
walki = 31
koi = 33
hiti = 23

# Calculate walk stats and groups by year for all teams and for A's
team="OAK"
Walksbyyear=list()
Walksbyyear_team=list()
for( round in c( "RS","DV","LC","WS" ) )
{
	for( i in c(styear:endyear) )
	{
		myyear = which( grepl(as.character(i),DATA[[round]][,1])==TRUE )
		myteam = intersect( myyear, which( DATA[[round]]$V7==team) )
		myteam = union( myteam,intersect( myyear, which( DATA[[round]]$V4==team)+dim(DATA[[round]])[1]/2 ) )

		Walksbyyear[[round]][["raw"]][[i]] = DATA[[round]][myyear,walki]
		Walksbyyear[[round]][["norm"]][[i]] = DATA[[round]][myyear,walki] / DATA[[round]][myyear,atbati]
		Walksbyyear_team[[round]][["raw"]][[i]] = DATA[[round]][myteam,walki]
		Walksbyyear_team[[round]][["norm"]][[i]] = DATA[[round]][myteam,walki] / DATA[[round]][myteam,atbati]

		myyear = which( grepl(as.character(i),DATAw[[round]][,1])==TRUE )
		myteam = intersect( myyear, which( DATAw[[round]]$V4==team) )
		Walksbyyear[[round]][["winners"]][[i]] = DATAw[[round]][myyear,walki] / DATAw[[round]][myyear,atbati]
		Walksbyyear_team[[round]][["winners"]][[i]] = DATAw[[round]][myteam,walki] / DATAw[[round]][myteam,atbati]

		myyear = which( grepl(as.character(i),DATAl[[round]][,1])==TRUE )
		myteam = intersect( myyear, which( DATAl[[round]]$V4==team) )
		Walksbyyear[[round]][["losers"]][[i]] = DATAl[[round]][myyear,walki] / DATAl[[round]][myyear,atbati]
		Walksbyyear_team[[round]][["losers"]][[i]] = DATAl[[round]][myteam,walki] / DATAl[[round]][myteam,atbati]
	}
}

# Calculate mean and std for walk stats
mWalksbyyear=list()
sWalksbyyear=list()
mWalksbyyear_team=list()
sWalksbyyear_team=list()
for( r in c("RS","DV","LC",'WS') )
{
	for( t in c("raw","norm","winners","losers") )
	{
		mWalksbyyear[[r]][[t]] = unlist(lapply(Walksbyyear[[r]][[t]][styear:endyear],mean))
		sWalksbyyear[[r]][[t]] = unlist(lapply(Walksbyyear[[r]][[t]][styear:endyear],sd))
		mWalksbyyear_team[[r]][[t]] = unlist(lapply(Walksbyyear_team[[r]][[t]][styear:endyear],mean))
		sWalksbyyear_team[[r]][[t]] = unlist(lapply(Walksbyyear_team[[r]][[t]][styear:endyear],sd))
	}
}


# PLOT SOME STATS!
dev.new()

# Plot data by year
layout(1)
t = "norm" 
mycol = "gray"
plot(mWalksbyyear[["RS"]][[t]],type="l",col=mycol,lwd=3,ylim=c(0,0.2),ylab="Walks Per at Bat",xaxt="n",xlab="",main="Average Walks Per at Bat Per Game")
axis(1,at=1:length(mWalksbyyear[["RS"]][[t]]),labels=as.character(styear:endyear),las=2)
lines(mWalksbyyear[["RS"]][[t]]-sWalksbyyear[["RS"]][[t]],type="l",col=mycol,lwd=3,lty=2)
lines(mWalksbyyear[["RS"]][[t]]+sWalksbyyear[["RS"]][[t]],type="l",col=mycol,lwd=3,lty=2)
points(mWalksbyyear_team[["RS"]][[t]],pch="+",col=mycol,lwd=3,lty=2)

#text( 3.5,mWalksbyyear[["RS"]][[t]][1]-0.01,"Regular Season Average",col=mycol )
#text( 3,mWalksbyyear[["RS"]][[t]][1]+sWalksbyyear[["RS"]][[t]][1]-0.01,c("Average + 1 Std"),col=mycol )
#text( 3,mWalksbyyear[["RS"]][[t]][1]-sWalksbyyear[["RS"]][[t]][1]-0.01,c("Average - 1 Std"),col=mycol )
#legend( 1,0.2,c("A's Regular Season"),pch="+",col=mycol)

abline( v=c(1997-1989),col="gray",lty=2)
#text( 1997-1989-0.5,0,c("Billy Beane becomes manager"),col="gray",srt=90,pos=4 )
abline( v=AY1-1989,col="red")
#text( AY1-1989-0.5,0,c("A's make DV"),col="red",srt=90,pos=4 )
abline( v=AY2-1989,col="blue" )
#text( AY2-1989-0.5,0,c("A's make LC"),col="blue",srt=90,pos=4 )
abline( v=AY3-1989,col="green" )
#text( AY3-1989-0.5,0,c("A's make WS"),col="green",srt=90,pos=4 )


lines(mWalksbyyear[["DV"]][[t]],type="l",col="red",lwd=3)
#legend( 1,0.2,c("Division Series"),lty=1,col="red",bg="white",lwd=3)
#points(mWalksbyyear_team[["DV"]][[t]],pch=8,col="red",lwd=2)
lines(mWalksbyyear[["LC"]][[t]],type="l",col="blue",lwd=3)
#legend( 1,0.2,c("League Champsionships"),lty=1,col="blue",bg="white",lwd=3)
#points(mWalksbyyear_team[["LC"]][[t]],pch=8,col="blue",lwd=2)
lines(mWalksbyyear[["WS"]][[t]],type="l",col="green",lwd=3)
#legend( 1,0.2,c("World Series"),lty=1,col="green",bg="white",lwd=3)
#points(mWalksbyyear_team[["WS"]][[t]],pch=8,col="green",lwd=2)

legend(2,7,c("Regular Season","Division Series","League Championship","World Series"),
		lty=c(1,1),col=c("gray","red","blue","green"),cex=0.75,bg="white",lwd=2)



# Boxplots of walk data represented in 3 ways
par(mar=c(4,4,2,2))
layout(matrix(c(1,2,3),nrow=1,byrow=T))

# boxplot of walks per game
STATLIST = list( log(DATA$RS[,walki]),log(DATA$DV[,walki]),log(DATA$LC[,walki]),log(DATA$WS[,walki]) )
for( x in 1:length(STATLIST) ){ STATLIST[[x]][is.infinite(STATLIST[[x]])] = 1 }
S1 = boxplot( STATLIST,ylab="Log( Walks per Game )",sep="" ,
		pch="+",names=c("RS","DC","LC","WS"),notch=TRUE,las=2,boxwex=0.45,
		col=c("gray","red","blue","green"))
legend(0.75,2.85,c("Regular Season","Division Champs","League Champs","World Series"),fill=c("gray","red","blue","green"),cex=1.2,bg="white")
abline( h=S1$stats[3,1],col="red",lty=2)

# boxplot of walks norm by batter
STATLIST = list( log(DATA$RS[,walki]/DATA$RS[,atbati]),log(DATA$DV[,walki]/DATA$DV[,atbati]),log(DATA$LC[,walki]/DATA$LC[,atbati]),log(DATA$WS[,walki]/DATA$WS[,atbati]) )
minval = min(STATLIST[[1]][is.finite(STATLIST[[1]])])
for( x in 1:length(STATLIST) ){ STATLIST[[x]][is.infinite(STATLIST[[x]])] = minval }
S2 = boxplot( STATLIST,ylab="log( Walks per At Bat per Game )",
		pch="+",names=c("RS","DC","LC","WS"),notch=TRUE,las=2,boxwex=0.45,
		col=c("gray","red","blue","green"))
abline( h=S2$stats[3,1],col="red",lty=2)

# boxplot of walks norm by batter for winners
STATLIST = list( log(DATAw$RS[,walki]/DATAw$RS[,atbati]),log(DATAw$DV[,walki]/DATAw$DV[,atbati]),log(DATAw$LC[,walki]/DATAw$LC[,atbati]),log(DATAw$WS[,walki]/DATAw$WS[,atbati]) )
for( x in 1:length(STATLIST) ){ STATLIST[[x]][is.infinite(STATLIST[[x]])] = minval }
S3 = boxplot( STATLIST,ylab="log( Walks per At Bat per Game  ) for Winners",boxwex=0.45,
		pch="+",names=c("RS","DC","LC","WS"),notch=TRUE,las=2,
		col=c("gray","red","blue","green"))
abline( h=S2$stats[3,1],col="red",lty=2)


# Boxplots of Winners v Losers v A's
i0 = union( which( RS$V7==team), which( RS$V4==team)+dim(RS)[1]/2 )
i1 = union( which( DV$V7==team), which( DV$V4==team)+dim(DV)[1]/2 )
i2 = union( which( LC$V7==team), which( LC$V4==team)+dim(LC)[1]/2 )
i3 = union( which( WS$V7==team), which( WS$V4==team)+dim(WS)[1]/2 )
STATLIST = list(
		log(DATAw$RS[,walki]/DATAw$RS[,atbati]),log(DATAl$RS[,walki]/DATAl$RS[,atbati]),log(DATA$RS[i0,walki]/DATA$RS[i0,atbati]),
		log(DATAw$DV[,walki]/DATAw$DV[,atbati]),log(DATAl$DV[,walki]/DATAl$DV[,atbati]),log(DATA$DV[i1,walki]/DATA$DV[i1,atbati]),
		log(DATAw$LC[,walki]/DATAw$LC[,atbati]),log(DATAl$LC[,walki]/DATAl$LC[,atbati]),log(DATA$LC[i2,walki]/DATA$LC[i2,atbati]),
		log(DATAw$WS[,walki]/DATAw$WS[,atbati]),log(DATAl$WS[,walki]/DATAl$WS[,atbati]),log(DATA$WS[i3,walki]/DATA$WS[i3,atbati]) )
for( x in 1:length(STATLIST) ){ STATLIST[[x]][is.infinite(STATLIST[[x]])] = minval }

par(mar=c(5,5,2,2))
layout(matrix(c(1,2,3,4),nrow=1))
savg = S2$stats[3,1]
S4 = boxplot( x=STATLIST[1:3],ylab="log( Walks per At Bat per Game  )",
		pch="+",xlab="Regular Season",names=NULL,ylim=c(-4.5,-0.5),
		notch=TRUE,las=1,col=c("white","gray","forestgreen"),boxwex=0.45)
legend(0.5,-0.5,c("Winners","Losers","Athletics"),fill=c("white","gray","forestgreen"),cex=1.2,bg="white")
wavg = S4$stats[3,1]
lavg = S4$stats[3,2]
abline( h=savg,col="red" )
abline( h=c(wavg,lavg),col="red",lty=2 )
boxplot( x=STATLIST[4:6],ylab="log( Walks per At Bat per Game  )",
		pch="+",xlab="Division Championships",names=NULL,ylim=c(-4.5,-0.5),
		notch=TRUE,las=1,col=c("white","gray","forestgreen"),boxwex=0.45)
abline( h=savg,col="red" )
abline( h=c(wavg,lavg),col="red",lty=2 )
boxplot( x=STATLIST[7:9],ylab="log( Walks per At Bat per Game  )",
		pch="+",xlab="League Championships",names=NULL,ylim=c(-4.5,-0.5),
		notch=TRUE,las=1,col=c("white","gray","forestgreen"),boxwex=0.45)
abline( h=savg,col="red" )
abline( h=c(wavg,lavg),col="red",lty=2 )
boxplot( x=STATLIST[7:9],ylab="log( Walks per At Bat per Game  )",
		pch="+",xlab="World Series",names=NULL,ylim=c(-4.5,-0.5),
		notch=TRUE,las=1,col=c("white","gray","forestgreen"),boxwex=0.45)
abline( h=savg,col="red" )
abline( h=c(wavg,lavg),col="red",lty=2 )




# Boxplot of various other stats
layout(matrix(c(1,2,3),nrow=1,byrow=T))
stats = c( hiti,koi,atbati )
statsn = c( "Hits","Strikeouts","At Bats" )
for( j in 1:length(stats) )
{
	i = stats[j]
	STATLIST = list( log(DATA$RS[,i]),log(DATA$DV[,i]),log(DATA$LC[,i]),log(DATA$WS[,i]) )
	boxplot( STATLIST,ylab=paste( "Log( ",statsn[j]," per Game )",sep="" ),
			pch="+",names=c("RS","DV","LC","WS"),col=c("gray","red","blue","green"),notch=TRUE,las=2)
	abline( h=log(median(RS[,i])),col="red",lty=2)
}



## PREDICTING WINNERS VIA WALKING ABILITY

# For every year, for every team, calculate the walking rate for half the season
teams = unique(DATA$RS$V4)
walkrates = list()
for( year in styear:endyear )
{
	tmp = which( grepl(year,DATA$RS$V1)==TRUE )
	l = length(tmp)
	vi = tmp[1:(l/2)]
	hi = tmp[(l/2+1):l]
	for( team in teams )
	{
		teamvi = intersect( vi,which( DATA$RS$V4==team ) )
		teamhi = intersect( hi,which( DATA$RS$V7==team ) )
		traini = c( teamvi[1:floor(length(teamvi)/2)], teamhi[1:floor(length(teamhi)/2)] )
		walks = DATA$RS[traini,walki] / DATA$RS[traini,atbati]
		walkrates[[as.character(year)]][[team]] = mean(walks)
	}
}

# For every game in the second half of every season, predict who will win based on walk rate
# 0 = home, 1 = vis
predictions = c()
truth = c()
for( year in styear:endyear )
{
	tmp = which( grepl(year,DATA$RS$V1)==TRUE )
	l = length(tmp)
	vi = tmp[1:(l/2)]
	predi = vi[ ceiling(length(vi)/2):length(vi)]

	walk1 = walkrates[[as.character(year)]][as.character(DATA$RS[predi,4])]
	walk2 = walkrates[[as.character(year)]][as.character(DATA$RS[predi,7])]

	add.pred = matrix(0,length(predi))
	add.truth = matrix(0,length(predi))
	add.pred[which(walk1>walk2)] = 1
	add.truth[which(DATA$RS[predi,10] > DATA$RS[predi,11])] = 1

	predictions = c( predictions,add.pred )
	truth = c( truth,add.truth )
	print( paste( "Year:",year," % correct:",sum(!xor(add.pred,add.truth))/length(add.truth) ) )
}
print( paste( "Overall % correct:",sum(!xor(predictions,truth))/length(truth) ) )

mypred = prediction( predictions,truth )
myperf = performance( mypred,measure="tpr", x.measure="fpr" )
plot(myperf,main=paste( "Precision Recall Curve",sep=""),ylim=c(0,1),xlim=c(0,1) )

