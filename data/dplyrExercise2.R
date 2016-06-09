
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(corrplot)

dataGeneral<-read.csv("/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/TODO_JUNTO.csv", header = T)
dataDesigualdad<-read.csv("/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/datos_desigualdad2014.csv", header = T)
dataDesempleo<-read.csv("/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/unemployment2015.csv", header = T)
dataReligion<-read.csv("/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/religions.csv", header = T)

joinFileTotalConDesigualdad<- full_join(dataGeneral, dataDesigualdad, by="Id2")
joinFileTotalConDesempleo<- full_join(dataDesempleo, joinFileTotalConDesigualdad, by="Id2")
joinFileTotalConReligion <- full_join(dataReligion, joinFileTotalConDesempleo, by="Id2")
write.csv(joinFileTotalConReligion, "/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/TODO_JUNTO_CON_RELIGION.csv")


joinFileTotalConReligion<- read.csv("/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/TODO_JUNTO_CON_RELIGION.csv", header = T)

SouthernBaptistvKasich <- ggplot(data=joinFileTotalConReligion, aes(x=SBCRATE, y=Kasich))+geom_point()+geom_smooth()
SouthernBaptistvTrump <- ggplot(data=joinFileTotalConReligion, aes(x=SBCRATE, y=Trump))+geom_point()+geom_smooth()
SouthernBaptistvCruz <- ggplot(data=joinFileTotalConReligion, aes(x=SBCRATE, y=Cruz))+geom_point()+geom_smooth()
SouthernBaptistvClinton <- ggplot(data=joinFileTotalConReligion, aes(x=SBCRATE, y=Clinton))+geom_point()+geom_smooth()
SouthernBaptistvSanders <- ggplot(data=joinFileTotalConReligion, aes(x=SBCRATE, y=Sanders))+geom_point()+geom_smooth()

GridSouthernBaptist<- grid.arrange(SouthernBaptistvSanders, SouthernBaptistvClinton, SouthernBaptistvCruz, SouthernBaptistvTrump, SouthernBaptistvKasich, ncol = 2)
GridSouthernBaptist


EvangelicalvKasich <- ggplot(data=joinFileTotalConReligion, aes(x=EVANADH, y=Kasich))+geom_point()+geom_smooth()
EvangelicalvTrump <- ggplot(data=joinFileTotalConReligion, aes(x=EVANADH, y=Trump))+geom_point()+geom_smooth()
EvangelicalvCruz <- ggplot(data=joinFileTotalConReligion, aes(x=EVANADH, y=Cruz))+geom_point()+geom_smooth()
EvangelicalvClinton <- ggplot(data=joinFileTotalConReligion, aes(x=EVANADH, y=Clinton))+geom_point()+geom_smooth()
EvangelicalvSanders <- ggplot(data=joinFileTotalConReligion, aes(x=EVANADH, y=Sanders))+geom_point()+geom_smooth()

GridEvangelical<- grid.arrange(EvangelicalvSanders, EvangelicalvClinton, EvangelicalvCruz, EvangelicalvTrump, EvangelicalvKasich, ncol = 2)
GridEvangelical

cor(joinFileTotalConReligion$Trump, joinFileTotalConReligion$EVANADH, method="pearson", use="complete")
cor(joinFileTotalConReligion$Cruz, joinFileTotalConReligion$EVANADH, method="pearson", use="complete")



BlackProtestantvKasich <- ggplot(data=joinFileTotalConReligion, aes(x=BPRTRATE, y=Kasich))+geom_point()+geom_smooth()
BlackProtestantvTrump <- ggplot(data=joinFileTotalConReligion, aes(x=BPRTRATE, y=Trump))+geom_point()+geom_smooth()
BlackProtestantvCruz <- ggplot(data=joinFileTotalConReligion, aes(x=BPRTRATE, y=Cruz))+geom_point()+geom_smooth()
BlackProtestantvClinton <- ggplot(data=joinFileTotalConReligion, aes(x=BPRTRATE, y=Clinton))+geom_point()+geom_smooth()
BlackProtestantvSanders <- ggplot(data=joinFileTotalConReligion, aes(x=BPRTRATE, y=Sanders))+geom_point()+geom_smooth()

GridBlackProtestant<- grid.arrange(BlackProtestantvSanders, BlackProtestantvClinton, BlackProtestantvCruz, BlackProtestantvTrump, BlackProtestantvKasich, ncol = 2)
GridBlackProtestant


GinivKasich <- ggplot(data=joinFileTotalConReligion, aes(x=Gini, y=Kasich))+geom_point()+geom_smooth()
GinivTrump <- ggplot(data=joinFileTotalConReligion, aes(x=Gini, y=Trump))+geom_point()+geom_smooth()
GinivCruz <- ggplot(data=joinFileTotalConReligion, aes(x=Gini, y=Cruz))+geom_point()+geom_smooth()
GinivClinton <- ggplot(data=joinFileTotalConReligion, aes(x=Gini, y=Clinton))+geom_point()+geom_smooth()
GinivSanders <- ggplot(data=joinFileTotalConReligion, aes(x=Gini, y=Sanders))+geom_point()+geom_smooth()

GridGini<- grid.arrange(GinivSanders, GinivClinton, GinivCruz, GinivTrump, GinivKasich, ncol = 2)
GridGini

TeenageBirthvKasich <- ggplot(data=joinFileTotalConReligion, aes(x=TeenageBirthRate, y=Kasich))+geom_point()+geom_smooth()
TeenageBirthvTrump <- ggplot(data=joinFileTotalConReligion, aes(x=TeenageBirthRate, y=Trump))+geom_point()+geom_smooth()
TeenageBirthvCruz <- ggplot(data=joinFileTotalConReligion, aes(x=TeenageBirthRate, y=Cruz))+geom_point()+geom_smooth()
TeenageBirthvClinton <- ggplot(data=joinFileTotalConReligion, aes(x=TeenageBirthRate, y=Clinton))+geom_point()+geom_smooth()
TeenageBirthvSanders <- ggplot(data=joinFileTotalConReligion, aes(x=TeenageBirthRate, y=Sanders))+geom_point()+geom_smooth()

GridTeenageBirth<- grid.arrange(TeenageBirthvSanders, TeenageBirthvClinton, TeenageBirthvCruz, TeenageBirthvTrump, TeenageBirthvKasich, ncol = 2)
GridTeenageBirth

Difference25And75vKasich <- ggplot(data=joinFileTotalConReligion, aes(x=ShareBetweenp25andp75, y=Kasich))+geom_point()+geom_smooth()
Difference25And75vTrump <- ggplot(data=joinFileTotalConReligion, aes(x=ShareBetweenp25andp75, y=Trump))+geom_point()+geom_smooth()
Difference25And75vCruz <- ggplot(data=joinFileTotalConReligion, aes(x=ShareBetweenp25andp75, y=Cruz))+geom_point()+geom_smooth()
Difference25And75vClinton <- ggplot(data=joinFileTotalConReligion, aes(x=ShareBetweenp25andp75, y=Clinton))+geom_point()+geom_smooth()
Difference25And75vSanders <- ggplot(data=joinFileTotalConReligion, aes(x=ShareBetweenp25andp75, y=Sanders))+geom_point()+geom_smooth()

GridDifference25And75<- grid.arrange(Difference25And75vSanders, Difference25And75vClinton, Difference25And75vCruz, Difference25And75vTrump, Difference25And75vKasich, ncol = 2)
GridDifference25And75


UpwardMobilityvKasich <- ggplot(data=joinFileTotalConReligion, aes(x=AbsoluteUpWardMobility, y=Kasich))+geom_point()+geom_smooth()
UpwardMobilityvTrump <- ggplot(data=joinFileTotalConReligion, aes(x=AbsoluteUpWardMobility, y=Trump))+geom_point()+geom_smooth()
UpwardMobilityvCruz <- ggplot(data=joinFileTotalConReligion, aes(x=AbsoluteUpWardMobility, y=Cruz))+geom_point()+geom_smooth()
UpwardMobilityvClinton <- ggplot(data=joinFileTotalConReligion, aes(x=AbsoluteUpWardMobility, y=Clinton))+geom_point()+geom_smooth()
UpwardMobilityvSanders <- ggplot(data=joinFileTotalConReligion, aes(x=AbsoluteUpWardMobility, y=Sanders))+geom_point()+geom_smooth()

GridUpwardMobility<- grid.arrange(UpwardMobilityvSanders, UpwardMobilityvClinton, UpwardMobilityvCruz, UpwardMobilityvTrump, UpwardMobilityvKasich, ncol = 2)
GridUpwardMobility




UnemploymentvKasich <- ggplot(data=joinFileTotalConReligion, aes(x=Unemployment2015, y=Kasich))+geom_point()+geom_smooth()
UnemploymentvTrump <- ggplot(data=joinFileTotalConReligion, aes(x=Unemployment2015, y=Trump))+geom_point()+geom_smooth()
UnemploymentvCruz <- ggplot(data=joinFileTotalConReligion, aes(x=Unemployment2015, y=Cruz))+geom_point()+geom_smooth()
UnemploymentvClinton <- ggplot(data=joinFileTotalConReligion, aes(x=Unemployment2015, y=Clinton))+geom_point()+geom_smooth()
UnemploymentvSanders <- ggplot(data=joinFileTotalConReligion, aes(x=Unemployment2015, y=Sanders))+geom_point()+geom_smooth()

GridUnemployment<- grid.arrange(UnemploymentvSanders, UnemploymentvClinton, UnemploymentvCruz, UnemploymentvTrump, UnemploymentvKasich, ncol = 2)
GridUnemployment


UnemploymentvKasichLog <- ggplot(data=joinFileTotalConReligion, aes(x=Unemployment2015, y=Kasich))+geom_point()+geom_smooth()+scale_x_log10()
UnemploymentvTrumpLog <- ggplot(data=joinFileTotalConReligion, aes(x=Unemployment2015, y=Trump))+geom_point()+geom_smooth()+scale_x_log10()
UnemploymentvCruzLog <- ggplot(data=joinFileTotalConReligion, aes(x=Unemployment2015, y=Cruz))+geom_point()+geom_smooth()+scale_x_log10()
UnemploymentvClintonLog <- ggplot(data=joinFileTotalConReligion, aes(x=Unemployment2015, y=Clinton))+geom_point()+geom_smooth()+scale_x_log10()
UnemploymentvSandersLog <- ggplot(data=joinFileTotalConReligion, aes(x=Unemployment2015, y=Sanders))+geom_point()+geom_smooth()+scale_x_log10()

GridUnemploymentLog<- grid.arrange(UnemploymentvSandersLog, UnemploymentvClintonLog, UnemploymentvCruzLog, UnemploymentvTrumpLog, UnemploymentvKasichLog, ncol = 2)
GridUnemploymentLog

cor(joinFileTotalConReligion$Unemployment2015, joinFileTotalConReligion$Trump, method="pearson", use="complete")
cor(joinFileTotalConReligion$Unemployment2015, joinFileTotalConReligion$Sanders, method="pearson", use="complete")
cor(joinFileTotalConReligion$Unemployment2015, joinFileTotalConReligion$Clinton, method="pearson", use="complete")



ParentIncomevKasichLog <- ggplot(data=joinFileTotalConReligion, aes(x=Median.Parent.Income, y=Kasich))+geom_point()+geom_smooth()
ParentIncomevTrumpLog <- ggplot(data=joinFileTotalConReligion, aes(x=Median.Parent.Income, y=Trump))+geom_point()+geom_smooth()
ParentIncomevCruzLog <- ggplot(data=joinFileTotalConReligion, aes(x=Median.Parent.Income, y=Cruz))+geom_point()+geom_smooth()
ParentIncomevClintonLog <- ggplot(data=joinFileTotalConReligion, aes(x=Median.Parent.Income, y=Clinton))+geom_point()+geom_smooth()
ParentIncomevSandersLog <- ggplot(data=joinFileTotalConReligion, aes(x=Median.Parent.Income, y=Sanders))+geom_point()+geom_smooth()

ParentIncome<- grid.arrange(ParentIncomevSandersLog, ParentIncomevClintonLog, ParentIncomevCruzLog, ParentIncomevTrumpLog, ParentIncomevKasichLog, ncol = 2)
ParentIncome