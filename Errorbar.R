
##Make same plot sorted by region in a certain order and also sort by pollutant (CO comes top) ########################
df=data.frame(c(0.91,1.09,1.06,0.97,1.02,1.12,0.85,1.00),c(0.86,1.02,1.03,0.91,0.91,1.05,0.81,0.95),c(0.97,1.17,1.10,1.03,1.13,1.20,0.90,1.05))
#name column
colnames(df)=c('Point','LCI','UCI')
df$Pol=c('CO','CO','CO','CO','NO2','NO2','NO2','NO2')
df$region=c('East','North','West','South','East','North','West','South')
df$region=factor(df$region)
df$Pol=factor(df$Pol)

# Releevl factor, but put the levels in reverse order. 
# If you want to put North at the top, put North at the end of statement
df$region=factor(df$region,levels=c('East','West','South','North'))

ggplot(data=df,aes(x=Pol,y=Point,quality,fill=region,colour=region))+
	geom_errorbar(aes(ymin=LCI,ymax=UCI),width=0.35,position=position_dodge(0.5))+
	geom_point(position=position_dodge(0.5),cex=3.25)+
	coord_flip()+
	scale_x_discrete(limits=rev(levels(df$Pol)))+
	ggtitle('Estimated RR per IQR increment')+
	guides(fill=FALSE,colour=guide_legend(reverse=TRUE))
