

### ### ### WHITE SANDS 3 YEAR DATA ### ### ###

library(flextable)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(ggeasy)
library(dplyr)
library(rlist)
library(lmPerm)
library(reactable)
library(RVAideMemoire)
library(PERMANOVA)
library(huxtable)
library(tidyverse)
library(readxl)
library(readr)
library(rlist)

setwd("C:/Users/Mikaela/OneDrive/Documents/Masters Stuff/R Stuff")

AvgRep <- read.csv("JRN_metadata_Study549_CfixBiomass.csv", header = TRUE)

S1<-AvgRep %>% filter(Site=="WHS")
S1$Year<-rep("2020",length(S1$Site))
S1$Resp<-S1$Resp*-1
setwd("C:/Users/Mikaela/Documents/NonMastersStuff")

S2<- read.csv("WHSrehydrationData.csv", header = TRUE)
S2$Year<-rep("2021",length(S2$Site))

S3<- read.csv("WHS3Data.csv", header = TRUE)
S3$Year<-rep("2022",length(S3$Site))

S01<-S1 %>% dplyr::select(Site,Type,Time,PAR0,PAR25,PAR50,PAR100,PAR150,PAR300,PAR500,PAR750,PAR1000,PAR1250,PAR1600,PAR2000,Resp,Net,QY,LightSat,Shape,Amax,Year)
S02<-S2 %>% dplyr::select(Site,Type,Time,PAR0,PAR25,PAR50,PAR100,PAR150,PAR300,PAR500,PAR750,PAR1000,PAR1250,PAR1600,PAR2000,Resp,Net,QY,LightSat,Shape,Amax,Year)
S03<-S3 %>% dplyr::select(Site,Type,Time,PAR0,PAR25,PAR50,PAR100,PAR150,PAR300,PAR500,PAR750,PAR1000,PAR1250,PAR1600,PAR2000,Resp,Net,QY,LightSat,Shape,Amax,Year)
TheValues<-rbind(S01,S02,S03)

TheValues$Time[TheValues$Time == "half"| TheValues$Time == "0.5"] <- "Half"
TheValues$Time[TheValues$Time == "two"|TheValues$Time ==  "2"] <- "Two"
TheValues$Time[TheValues$Time == "six"|TheValues$Time ==  "6"] <- "Six"
TheValues$Time[TheValues$Time == "twelve"| TheValues$Time == "12"] <- "Twelve"
TheValues$Time[TheValues$Time == "twentyfour"| TheValues$Time == "24"] <- "TwentyFour"
TheValues$Time[TheValues$Time == "36"] <- "ThirtySix"

TheValues$Type[TheValues$Type == "la"|TheValues$Type == "LA"] <- "LCC"
TheValues$Type[TheValues$Type == "da"|TheValues$Type == "DA"] <- "DCC"
TheValues$Type[TheValues$Type == "pt"|TheValues$Type == "PT"] <- "CYL"
TheValues$Type[TheValues$Type == "cl"|TheValues$Type == "CL"] <- "CHL"
TheValues$Type[TheValues$Type == "mo"|TheValues$Type == "MO"] <- "MOS"

### ### ### YAY ### ### ###

PT1 = theme(plot.title = element_text(hjust = 0.5))
PT2 = theme(plot.subtitle = element_text(hjust = 0.5))

TiOrd <- function(x) {factor(x$Time, levels=c("Half","Two","Six","Twelve","TwentyFour","ThirtySix"))}
TyOrd<- function(x){factor(x$Type,levels=c("LCC","DCC","CYL","CHL","MOS"))}

TheValues$Type<-TyOrd(TheValues)
TheValues$Time<-TiOrd(TheValues)

#scale_y_continuous(limits = c(0,15))+ 
View(Check)
Check<-TheValues[TheValues$Resp < 0,]

ggplot(TheValues,aes(x=Time,y=Resp,color=Year)) +geom_boxplot() + geom_point(size=3,alpha=.8) +stat_summary(fun=mean, geom="point", shape=23, size=3)+ geom_abline(intercept=0,slope=0)+ggtitle("Respiration") +xlab("Biocrust Type") + ylab("Respiration (µmol CO2 m-2 s-1)") + theme_bw() +
  theme(legend.position="bottom") + PT1+ facet_grid(~Type) 
dev.copy(png,"WHS3resp.png",width=21,height=10,units="cm",res=300)
dev.off()

ggplot(TheValues,aes(x=Time,y=Amax,color=Year)) +geom_boxplot() + geom_point(size=3,alpha=.8) +stat_summary(fun=mean, geom="point", shape=23, size=3)+ geom_abline(intercept=0,slope=0)+ggtitle("Fixation") +scale_y_continuous(limits = c(0,15))+ xlab("Biocrust Type") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() +
  theme(legend.position="bottom") + PT1+ facet_grid(~Type) 
dev.copy(png,"WHS3fix.png",width=21,height=10,units="cm",res=300)
dev.off()

# +scale_y_continuous(limits = c(-12,7))
ggplot(TheValues,aes(x=Time,y=Net,color=Year)) +geom_boxplot() + geom_point(size=3,alpha=.8) +stat_summary(fun=mean, geom="point", shape=23, size=3)+ geom_abline(intercept=0,slope=0)+ggtitle("Net") + xlab("Biocrust Type") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() +
  theme(legend.position="bottom") + PT1+ facet_grid(~Type) 
dev.copy(png,"WHS3net.png",width=35,height=10,units="cm",res=300)
dev.off()

Mallr <- aggregate(TheValues$Resp, by=list(TheValues$Year,TheValues$Type,TheValues$Time), FUN=mean)
colnames(Mallr) = c("Year","Type","Time","Respiration")
Malla <- aggregate(TheValues$Amax, by=list(TheValues$Year,TheValues$Type,TheValues$Time), FUN=mean)
colnames(Malla) = c("Year","Type","Time","C-Fixation")
Malln <- aggregate(TheValues$Net, by=list(TheValues$Year,TheValues$Type,TheValues$Time), FUN=mean)
colnames(Malln) = c("Year","Type","Time","Net")

M2<- Malln%>% filter(Year=="2021")
Ma <- aggregate(TheValues$Net, by=list(M2$Year,M2$Type), FUN=max)
colnames(Ma) = c("Year","Type","Net")

SE <- function(x) sd(x) / sqrt(length(x)) 

Mallre <- aggregate(TheValues$Resp, by=list(TheValues$Year,TheValues$Type,TheValues$Time), FUN=SE)
colnames(Mallre) = c("Year","Type","Time","Rse")
Mallae <- aggregate(TheValues$Amax, by=list(TheValues$Year,TheValues$Type,TheValues$Time), FUN=SE)
colnames(Mallae) = c("Year","Type","Time","Cse")
Mallne <- aggregate(TheValues$Net, by=list(TheValues$Year,TheValues$Type,TheValues$Time), FUN=SE)
colnames(Mallne) = c("Year","Type","Time","Nse")

Mallr$SE<-Mallre$Rse
Malla$SE<-Mallae$Cse
Malln$SE<-Mallne$Nse

Mallr$SEp<-Mallr$Respiration+Mallr$SE
Mallr$SEm<-Mallr$Respiration-Mallr$SE
Malla$SEp<-Malla$`C-Fixation`+Malla$SE
Malla$SEm<-Malla$`C-Fixation`-Malla$SE
Malln$SEp<-Malln$Net+Malln$SE
Malln$SEm<-Malln$Net-Malln$SE

Mallr$Time<-TiOrd(Mallr)
Mallr$Type<-TyOrd(Mallr)
Malla$Time<-TiOrd(Malla)
Malla$Type<-TyOrd(Malla)
Malln$Time<-TiOrd(Malln)
Malln$Type-TyOrd(Malln)

library(flextable)
Malla$`C-Fixation`<-round(Malla$`C-Fixation`,digits=2)
Mallr$Respiration<-round(Mallr$Respiration,digits=2)
Malln$Net<-round(Malln$Net,digits=2)
Malla$SE<-round(Malla$SE,digits=2)
Mallr$SE<-round(Mallr$SE,digits=2)
Malln$SE<-round(Malln$SE,digits=2)

CRNtab<-Malla
CRNtab$`C-Fixation` <- paste(CRNtab$`C-Fixation`, CRNtab$SE, sep=" ± ")
CRNtab$Respiration <- paste(Mallr$Respiration, Mallr$SE, sep=" ± ")
CRNtab$Net <- paste(Malln$Net, Malln$SE, sep=" ± ")

CRNtable<- CRNtab %>% dplyr::select(Year,Type,Time,`C-Fixation`,Respiration,Net)
install.packages("flextable")
# MAKING TABLE #
library(tableone)
remove.packages("flextable")
View(CRNtable)
u1<-as.data.frame(CRNtable)

doc <- addFlexTable(CRNtable)
writeDoc(doc, file = "r-reporters-word-document-add-table.docx")
install.packages("flextable")
library(flextable)
library(tidyr)

CRNtable0<-as.data.frame(CRNtable)

CRNtableF<-flextable(CRNtable0) %>% 
  autofit(add_w = 0.2)%>%
  align(align = "left") %>%
  align(align = "left", part = "header")
#%>%add_header_lines("Table x: header-here") %>%
#add_footer_lines("footer-here")

print(CRNtableF, preview = "docx")

#+geom_errorbar(aes(color="#000000",ymin=(yminA),ymax=(ymaxA)), width=.2,alpha=.8) 
ResAvg <- ggplot(Mallr,aes(x=Time,y=Respiration,color=Type)) + geom_point(size=2,alpha=.8) + geom_errorbar(aes(color=Type,ymin=(SEm),ymax=(SEp)), width=.2,alpha=.8) +scale_y_continuous(limits = c(0,16))+geom_abline(intercept=0,slope=0)+ ggtitle("Respiration") + xlab("Biocrust Type") + ylab("Respiration (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044","#000044")) + PT1+facet_grid(~Year,scale="free",space='free')
FixAvg <- ggplot(Malla,aes(x=Time,y=`C-Fixation`,color=Type)) + geom_point(size=2,alpha=.8) + geom_errorbar(aes(color=Type,ymin=(SEm),ymax=(SEp)), width=.2,alpha=.8) +scale_y_continuous(limits = c(0,16))+geom_abline(intercept=0,slope=0)+ ggtitle("Fixation") + xlab("Biocrust Type") + ylab("Fixation (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044","#000044")) + PT1+facet_grid(~Year,scale="free",space='free')
NetAvg <- ggplot(Malln,aes(x=Time,y=Net,color=Type)) + geom_point(size=2,alpha=.8) +geom_errorbar(aes(color=Type,ymin=(SEm),ymax=(SEp)), width=.2,alpha=.8) + scale_y_continuous(limits = c(-13,7.5))+geom_abline(intercept=0,slope=0)+ ggtitle("Net") + xlab("Biocrust Type") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044","#000044")) + PT1+facet_grid(~Year,scale="free",space='free')

ggarrange(FixAvg,ResAvg,NetAvg, ncol=1, nrow=3)
dev.copy(png,"CRNallSE.png",width=13,height=15,units="in",res=300)
dev.off()

### STATS ###

#AL<- NEWcyan%>% filter(Genus=="Allocoleopsis")
library(emmeans)
library(multcomp)
library(AICcmodavg)
# LINEAR MODLES #
A.null <- lm(Amax ~ 1, data = TheValues)
A.Year <- lm(Amax ~ Year, data = TheValues)
A.Type <- lm(Amax ~ Type, data = TheValues)
A.Time <- lm(Amax ~ Time, data = TheValues)
A.Year.Type <- lm(Amax ~ Year*Type, data = TheValues)
A.Year.Time <- lm(Amax ~ Year*Time, data = TheValues)
A.Time.Type <- lm(Amax ~ Time*Type, data = TheValues)
A.Time.Type.Year <- lm(Amax ~ Year*Type*Time, data = TheValues)

R.null <- lm(Resp ~ 1, data = TheValues)
R.Year <- lm(Resp~ Year, data = TheValues)
R.Type <- lm(Resp~ Type, data = TheValues)
R.Time <- lm(Resp ~ Time, data = TheValues)
R.Year.Type <- lm(Resp~ Year*Type, data = TheValues)
R.Year.Time <- lm(Resp ~ Year*Time, data = TheValues)
R.Time.Type <- lm(Resp ~ Time*Type, data = TheValues)
R.Time.Type.Year <- lm(Resp ~ Year*Type*Time, data = TheValues)

N.null <- lm(Net ~ 1, data = TheValues)
N.Year <- lm(Net ~ Year, data = TheValues)
N.Type <- lm(Net ~ Type, data = TheValues)
N.Time <- lm(Net ~ Time, data = TheValues)
N.Year.Type <- lm(Net ~ Year*Type, data = TheValues)
N.Year.Time <- lm(Net ~ Year*Time, data = TheValues)
N.Time.Type <- lm(Net ~ Time*Type, data = TheValues)
N.Time.Type.Year <- lm(Net ~ Year*Type*Time, data = TheValues)

# TESTING LINEAR MODLES #
A.cand.list <- list(
  "null" = A.null,
  "Year" = A.Year,
  "Time"=A.Time,
  "Type" = A.Type,
  "Year*Type" = A.Year.Type,
  "Year*Time" =A.Year.Time,
  "Time*Type"=A.Time.Type,
  "Year*Type*Time"=A.Time.Type.Year) 

R.cand.list <- list(
  "null" = R.null,
  "Year" = R.Year,
  "Time"=R.Time,
  "Type" = R.Type,
  "Year*Type" = R.Year.Type,
  "Year*Time" =R.Year.Time,
  "Time*Type"=R.Time.Type,
  "Year*Type*Time"=R.Time.Type.Year) 

N.cand.list <- list(
  "null" = N.null,
  "Year" = N.Year,
  "Time"=N.Time,
  "Type" = N.Type,
  "Year*Type" = N.Year.Type,
  "Year*Time" =N.Year.Time,
  "Time*Type"=N.Time.Type,
  "Year*Type*Time"=N.Time.Type.Year) 


aictab(A.cand.list)
aictab(R.cand.list)
aictab(N.cand.list)

# ANOVA TESTING #

S1<-TheValues %>% filter(Year=="2020")
S2<-TheValues %>% filter(Year=="2021")
S3<-TheValues %>% filter(Year=="2022")
N36<-TheValues %>% filter(Time !="ThirtySix")

A.lm.ANOVA <- lm(Amax ~ Year*Type*Time, data = N36)
car::Anova(A.lm.ANOVA, type = "III", test.statistic = "F")
R.lm.ANOVA <- lm(Resp ~ Year*Type*Time, data = N36)
car::Anova(R.lm.ANOVA, type = "III", test.statistic = "F")
N.lm.ANOVA <- lm(Net ~ Year*Type*Time, data = N36)
car::Anova(N.lm.ANOVA, type = "III", test.statistic = "F")

A1.lm.ANOVA <- lm(Amax ~ Type*Time, data = S1)
car::Anova(A1.lm.ANOVA, type = "III", test.statistic = "F")
A2.lm.ANOVA <- lm(Amax ~ Type*Time, data = S2)
car::Anova(A2.lm.ANOVA, type = "III", test.statistic = "F")
A3.lm.ANOVA <- lm(Amax ~ Type*Time, data = S3)
car::Anova(A3.lm.ANOVA, type = "III", test.statistic = "F")
R1.lm.ANOVA <- lm(Resp ~ Type*Time, data = S1)
car::Anova(R1.lm.ANOVA, type = "III", test.statistic = "F")
R2.lm.ANOVA <- lm(Resp ~ Type*Time, data = S2)
car::Anova(R2.lm.ANOVA, type = "III", test.statistic = "F")
R3.lm.ANOVA <- lm(Resp ~ Type*Time, data = S3)
car::Anova(R3.lm.ANOVA, type = "III", test.statistic = "F")
N1.lm.ANOVA <- lm(Net ~ Type*Time, data = S1)
car::Anova(N1.lm.ANOVA, type = "III", test.statistic = "F")
N2.lm.ANOVA <- lm(Net ~ Type*Time, data = S2)
car::Anova(N2.lm.ANOVA, type = "III", test.statistic = "F")
N3.lm.ANOVA <- lm(Net ~ Type*Time, data = S3)
car::Anova(N3.lm.ANOVA, type = "III", test.statistic = "F")

qqnorm(resid(A.lm.ANOVA))
qqline(resid(A.lm.ANOVA))
qqnorm(resid(R.lm.ANOVA))
qqline(resid(R.lm.ANOVA))
qqnorm(resid(N.lm.ANOVA))
qqline(resid(N.lm.ANOVA))
qqnorm(resid(A1.lm.ANOVA))
qqline(resid(A1.lm.ANOVA))
qqnorm(resid(A2.lm.ANOVA))
qqline(resid(A2.lm.ANOVA))
qqnorm(resid(A3.lm.ANOVA))
qqline(resid(A3.lm.ANOVA))
qqnorm(resid(R1.lm.ANOVA))
qqline(resid(R1.lm.ANOVA))
qqnorm(resid(R2.lm.ANOVA))
qqline(resid(R2.lm.ANOVA))
qqnorm(resid(R3.lm.ANOVA))
qqline(resid(R3.lm.ANOVA))
qqnorm(resid(N1.lm.ANOVA))
qqline(resid(N1.lm.ANOVA))
qqnorm(resid(N2.lm.ANOVA))
qqline(resid(N2.lm.ANOVA))
qqnorm(resid(N3.lm.ANOVA))
qqline(resid(N3.lm.ANOVA))

# PAIRWISE COMPARISON #
resid(A.lm.ANOVA) %>% shapiro.test()
emmeans(A.lm.ANOVA, ~ Year*Type*Time)
contrast(emmeans(A.lm.ANOVA, specs= ~Year*Type*Time), method = "pairwise")
resid(R.lm.ANOVA) %>% shapiro.test()
emmeans(R.lm.ANOVA, ~ Year*Type*Time)
contrast(emmeans(R.lm.ANOVA, specs= ~Year*Type*Time), method = "pairwise")
resid(N.lm.ANOVA) %>% shapiro.test()
emmeans(N.lm.ANOVA, ~ Year*Type*Time)
contrast(emmeans(N.lm.ANOVA, specs= ~Year*Type*Time), method = "pairwise")
resid(A3.lm.ANOVA) %>% shapiro.test()
emmeans(A3.lm.ANOVA, ~ Type*Time)
contrast(emmeans(A3.lm.ANOVA, specs= ~Type*Time), method = "pairwise")
resid(A1.lm.ANOVA) %>% shapiro.test()
emmeans(A1.lm.ANOVA, ~ Type*Time)
contrast(emmeans(A1.lm.ANOVA, specs= ~Type*Time), method = "pairwise")
resid(A2.lm.ANOVA) %>% shapiro.test()
emmeans(A2.lm.ANOVA, ~ Type*Time)
contrast(emmeans(A2.lm.ANOVA, specs= ~Type*Time), method = "pairwise")
resid(R3.lm.ANOVA) %>% shapiro.test()
emmeans(R3.lm.ANOVA, ~ Type*Time)
contrast(emmeans(R3.lm.ANOVA, specs= ~Type*Time), method = "pairwise")
resid(R1.lm.ANOVA) %>% shapiro.test()
emmeans(R1.lm.ANOVA, ~ Type*Time)
contrast(emmeans(R1.lm.ANOVA, specs= ~Type*Time), method = "pairwise")
resid(R2.lm.ANOVA) %>% shapiro.test()
emmeans(R2.lm.ANOVA, ~ Type*Time)
contrast(emmeans(R2.lm.ANOVA, specs= ~Type*Time), method = "pairwise")
resid(N3.lm.ANOVA) %>% shapiro.test()
emmeans(N3.lm.ANOVA, ~ Type*Time)
contrast(emmeans(N3.lm.ANOVA, specs= ~Type*Time), method = "pairwise")
resid(N1.lm.ANOVA) %>% shapiro.test()
emmeans(N1.lm.ANOVA, ~ Type*Time)
contrast(emmeans(N1.lm.ANOVA, specs= ~Type*Time), method = "pairwise")
resid(N2.lm.ANOVA) %>% shapiro.test()
emmeans(N2.lm.ANOVA, ~ Type*Time)
contrast(emmeans(A2.lm.ANOVA, specs= ~Type*Time), method = "pairwise")

A.emmeans.Tukey <- (emmeans(A.lm.ANOVA, ~ Year*Type*Time)) %>%
  multcomp::cld(Letters = LETTERS)
R.emmeans.Tukey <- (emmeans(R.lm.ANOVA, ~ Year*Type*Time)) %>%
  multcomp::cld(Letters = LETTERS)
N.emmeans.Tukey <- (emmeans(N.lm.ANOVA, ~ Year*Type*Time)) %>%
  multcomp::cld(Letters = LETTERS)
A3.emmeans.Tukey <- (emmeans(A3.lm.ANOVA, ~ Type*Time)) %>%
  multcomp::cld(Letters = LETTERS)
A1.emmeans.Tukey <- (emmeans(A1.lm.ANOVA, ~ Type*Time)) %>%
  multcomp::cld(Letters = LETTERS)
A2.emmeans.Tukey <- (emmeans(A2.lm.ANOVA, ~ Type*Time)) %>%
  multcomp::cld(Letters = LETTERS)
R3.emmeans.Tukey <- (emmeans(R3.lm.ANOVA, ~ Type*Time)) %>%
  multcomp::cld(Letters = LETTERS)
R1.emmeans.Tukey <- (emmeans(R1.lm.ANOVA, ~ Type*Time)) %>%
  multcomp::cld(Letters = LETTERS)
R2.emmeans.Tukey <- (emmeans(R2.lm.ANOVA, ~ Type*Time)) %>%
  multcomp::cld(Letters = LETTERS)
N3.emmeans.Tukey <- (emmeans(N3.lm.ANOVA, ~ Type*Time)) %>%
  multcomp::cld(Letters = LETTERS)
N1.emmeans.Tukey <- (emmeans(N1.lm.ANOVA, ~ Type*Time)) %>%
  multcomp::cld(Letters = LETTERS)
N2.emmeans.Tukey <- (emmeans(N2.lm.ANOVA, ~ Type*Time)) %>%
  multcomp::cld(Letters = LETTERS)


# PLOTTING LETTERED ANOVA RESULTS #

A0<-A.emmeans.Tukey %>%
  mutate(.group = trimws(.group)) %>% # Trim off whitespace around letters
  ggplot(aes(x = Type, y = emmean, col = Year)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +
  geom_text(aes(x = Type, y = emmean, label = .group),
            vjust = -0.5,angle=90, show.legend = FALSE) +
  ggtitle("C Fixation", 
          subtitle = "Same-letter means are not different at alpha = 0.05; Tukey method") +
  ylab("Percent Abundance +/- 95% CI")+scale_color_manual(values=c(  "#48BFE3","#5E60CE","#151A7B","#000044"))+ theme_bw() + theme (legend.position = "none")+facet_grid(~Time)+PT1+PT2

R0<-R.emmeans.Tukey %>%
  mutate(.group = trimws(.group)) %>% # Trim off whitespace around letters
  ggplot(aes(x = Type, y = emmean, col = Year)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +
  geom_text(aes(x = Type, y = emmean, label = .group),
            vjust = -0.5,angle=90, show.legend = FALSE) +
  ggtitle("Respiration", 
          subtitle = "Same-letter means are not different at alpha = 0.05; Tukey method") +
  ylab("Percent Abundance +/- 95% CI")+scale_color_manual(values=c(  "#48BFE3","#5E60CE","#151A7B","#000044"))+ theme_bw() + theme (legend.position = "none")+facet_grid(~Time)+PT1+PT2
N0<-N.emmeans.Tukey %>%
  mutate(.group = trimws(.group)) %>% # Trim off whitespace around letters
  ggplot(aes(x = Type, y = emmean, col = Year)) +
  geom_point() +geom_abline(intercept=0,slope=0)+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +
  geom_text(aes(x = Type, y = emmean, label = .group),
            vjust = -0.5,angle=90, show.legend = FALSE) +
  ggtitle("Net", 
          subtitle = "Same-letter means are not different at alpha = 0.05; Tukey method") +
  ylab("Percent Abundance +/- 95% CI")+scale_color_manual(values=c(  "#48BFE3","#5E60CE","#151A7B","#000044"))+ theme_bw() + theme (legend.position = "none")+facet_grid(~Time)+PT1+PT2

A10<-A1.emmeans.Tukey %>%
  mutate(.group = trimws(.group)) %>% # Trim off whitespace around letters
  ggplot(aes(x = Type, y = emmean, col = Type)) +
  geom_point()+scale_y_continuous(limits = c(-0,11)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +
  geom_text(aes(x = Type, y = emmean, label = .group),
            vjust = -0.5,angle=90, show.legend = FALSE) +
  ggtitle("2020 C Fixation", 
          subtitle = "Same-letter means are not different at alpha = 0.05; Tukey method") +
  ylab("Percent Abundance +/- 95% CI")+scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() + theme (legend.position = "none")+facet_grid(~Time)+PT1+PT2

A20<-A2.emmeans.Tukey %>%
  mutate(.group = trimws(.group)) %>% # Trim off whitespace around letters
  ggplot(aes(x = Type, y = emmean, col = Type)) +
  geom_point()+scale_y_continuous(limits = c(0,11)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +
  geom_text(aes(x = Type, y = emmean, label = .group),
            vjust = -0.5,angle=90, show.legend = FALSE) +
  ggtitle("2021 C Fixation", 
          subtitle = "Same-letter means are not different at alpha = 0.05; Tukey method") +
  ylab("Percent Abundance +/- 95% CI")+scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() + theme (legend.position = "none")+facet_grid(~Time)+PT1+PT2

A30<-A3.emmeans.Tukey %>%
  mutate(.group = trimws(.group)) %>% # Trim off whitespace around letters
  ggplot(aes(x = Type, y = emmean, col = Type)) +
  geom_point() +scale_y_continuous(limits = c(0,11))+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +
  geom_text(aes(x = Type, y = emmean, label = .group),
            vjust = -0.5,angle=90, show.legend = FALSE) +
  ggtitle("2022 C Fixation", 
          subtitle = "Same-letter means are not different at alpha = 0.05; Tukey method") +
  ylab("Percent Abundance +/- 95% CI")+scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() + theme (legend.position = "none")+facet_grid(~Time)+PT1+PT2


R10<-R1.emmeans.Tukey %>%
  mutate(.group = trimws(.group)) %>% # Trim off whitespace around letters
  ggplot(aes(x = Type, y = emmean, col = Type)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +
  geom_text(aes(x = Type, y = emmean, label = .group),
            vjust = -0.5,angle=90, show.legend = FALSE) +
  ggtitle("2020 Respiration", 
          subtitle = "Same-letter means are not different at alpha = 0.05; Tukey method") +
  ylab("Percent Abundance +/- 95% CI")+scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() + theme (legend.position = "none")+facet_grid(~Time)+PT1+PT2

R20<-R2.emmeans.Tukey %>%
  mutate(.group = trimws(.group)) %>% # Trim off whitespace around letters
  ggplot(aes(x = Type, y = emmean, col = Type)) +
  geom_point()+scale_y_continuous(limits = c(0,9)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +
  geom_text(aes(x = Type, y = emmean, label = .group),
            vjust = -0.5,angle=90, show.legend = FALSE) +
  ggtitle("2021 Respiration", 
          subtitle = "Same-letter means are not different at alpha = 0.05; Tukey method") +
  ylab("Percent Abundance +/- 95% CI")+scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() + theme (legend.position = "none")+facet_grid(~Time)+PT1+PT2

R30<-R3.emmeans.Tukey %>%
  mutate(.group = trimws(.group)) %>% # Trim off whitespace around letters
  ggplot(aes(x = Type, y = emmean, col = Type)) +
  geom_point() +scale_y_continuous(limits = c(-0,9))+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +
  geom_text(aes(x = Type, y = emmean, label = .group),
            vjust = -0.5,angle=90, show.legend = FALSE) +
  ggtitle("2022 Respiration", 
          subtitle = "Same-letter means are not different at alpha = 0.05; Tukey method") +
  ylab("Percent Abundance +/- 95% CI")+scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() + theme (legend.position = "none")+facet_grid(~Time)+PT1+PT2

N10<-N1.emmeans.Tukey %>%
  mutate(.group = trimws(.group)) %>% # Trim off whitespace around letters
  ggplot(aes(x = Type, y = emmean, col = Type)) +
  geom_point() +scale_y_continuous(limits = c(-11,8))+geom_abline(intercept=0,slope=0)+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +
  geom_text(aes(x = Type, y = emmean, label = .group),
            vjust = -0.5,angle=90, show.legend = FALSE) +
  ggtitle("2020 Net C Fixation", 
          subtitle = "Same-letter means are not different at alpha = 0.05; Tukey method") +
  ylab("Percent Abundance +/- 95% CI")+scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() + theme (legend.position = "none")+facet_grid(~Time)+PT1+PT2

N20<-N2.emmeans.Tukey %>%
  mutate(.group = trimws(.group)) %>% # Trim off whitespace around letters
  ggplot(aes(x = Type, y = emmean, col = Type)) +
  geom_point() +geom_abline(intercept=0,slope=0)+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +scale_y_continuous(limits = c(-11,8))+
  geom_text(aes(x = Type, y = emmean, label = .group),
            vjust = -0.5,angle=90, show.legend = FALSE) +
  ggtitle("2021 Net C Fixation", 
          subtitle = "Same-letter means are not different at alpha = 0.05; Tukey method") +
  ylab("Percent Abundance +/- 95% CI")+scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() + theme (legend.position = "none")+facet_grid(~Time)+PT1+PT2

N30<-N3.emmeans.Tukey %>%
  mutate(.group = trimws(.group)) %>% # Trim off whitespace around letters
  ggplot(aes(x = Type, y = emmean, col = Type)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +
  geom_text(aes(x = Type, y = emmean, label = .group),
            vjust = -0.5,angle=90, show.legend = FALSE) +geom_abline(intercept=0,slope=0)+
  scale_y_continuous(limits = c(-11,8))+
  ggtitle("2022 Net C Fixation", 
          subtitle = "Same-letter means are not different at alpha = 0.05; Tukey method") +
  ylab("Percent Abundance +/- 95% CI")+scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() + theme (legend.position = "none")+facet_grid(~Time)+PT1+PT2

ggarrange(N10,N20,N30, ncol=1, nrow=3,labels="AUTO")
dev.copy(png,"Net3anova.png",width=17,height=12,units="in",res=300)
dev.off()

ggarrange(A10,A20,A30, ncol=1, nrow=3,labels="AUTO")
dev.copy(png,"Cfix3anova.png",width=17,height=10,units="in",res=300)
dev.off()

ggarrange(R10,R20,R30, ncol=1, nrow=3,labels="AUTO")
dev.copy(png,"Resp3anova.png",width=17,height=10,units="in",res=300)
dev.off()

ggarrange(A0,R0,N0, ncol=1, nrow=3,labels="AUTO")
dev.copy(png,"CRN3anova.png",width=18,height=18,units="in",res=300)
dev.off()


