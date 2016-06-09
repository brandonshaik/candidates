
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(corrplot)

#JOINING ALL FILES IN A DIRECTORY
ListOfStatesGOP <- list.files(path="/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/resultados", pattern="*GOP.csv")
JoiningGOP <- lapply(ListOfStatesGOP, fread, sep=",")
JoinStateFilesGOP <- rbindlist(JoiningGOP)
write.csv(JoinStateFilesGOP, "/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/resultadosTodosEstadosGOP.csv")
#JoinDemocrats
ListOfStatesDEM <- list.files(path="/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/resultados", pattern="*Dem.csv")
JoiningDEM <- lapply(ListOfStatesDEM, fread, sep=",")
JoinStateFilesDEM <- rbindlist(JoiningDEM)
write.csv(JoinStateFilesDEM, "/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/resultadosTodosEstadosDEM.csv")
#JoinBothFiles
joinFileResults <- full_join(JoinStateFilesGOP, JoinStateFilesDEM, by="Id2")
write.csv(joinFileResults, "/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/TodosLosResultadosPrimarias.csv")

#Now, the socio-demographic data
dataDesempleo<-read.csv("/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/desempleo.csv", header = T)
dataPoblacion<-read.csv("/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/poblacion.csv", header = T)

#JoiningAllFiles
joinFileInitial <- full_join(dataDesempleo, dataPoblacion, by="Id2")
joinFileFinal<-full_join(joinFileInitial, joinFileResults, by="Id2")
write.csv(joinFileFinal, "/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/TODO_JUNTO.csv")

joinFile2 <- read.csv("/Users/albertocairo/Desktop/1R_HTML_CSS_JS/R/dplyrJOINExercise/TODO_JUNTO.csv", header=T)

cor(joinFile2[sapply(joinFile2, function(x) !is.factor(x))], use="complete", method="pearson")

cor(joinFile2$Trump, joinFile2$Total.bachelor.s.degree.or.higher_., method="pearson", use="complete")
cor(joinFile2$Trump, joinFile2$Median.age..years., method="pearson", use="complete")

BAvKasich <- ggplot(data=joinFile2, aes(x=Total.bachelor.s.degree.or.higher_., y=Kasich))+geom_point()+geom_smooth()
BAvTrump <- ggplot(data=joinFile2, aes(x=Total.bachelor.s.degree.or.higher_., y=Trump))+geom_point()+geom_smooth()
BAvCruz <- ggplot(data=joinFile2, aes(x=Total.bachelor.s.degree.or.higher_., y=Cruz))+geom_point()+geom_smooth()
BAvClinton <- ggplot(data=joinFile2, aes(x=Total.bachelor.s.degree.or.higher_., y=Clinton))+geom_point()+geom_smooth()
BAvSanders <- ggplot(data=joinFile2, aes(x=Total.bachelor.s.degree.or.higher_., y=Sanders))+geom_point()+geom_smooth()

GridEducation<- grid.arrange(BAvSanders, BAvClinton, BAvCruz, BAvTrump, BAvKasich, ncol = 2)
GridEducation

BlacksvKasich <- ggplot(data=joinFile2, aes(x=Black.or.African.American_., y=Kasich))+geom_point()+geom_smooth()
BlacksvTrump <- ggplot(data=joinFile2, aes(x=Black.or.African.American_., y=Trump))+geom_point()+geom_smooth()
BlacksvCruz <- ggplot(data=joinFile2, aes(x=Black.or.African.American_., y=Cruz))+geom_point()+geom_smooth()
BlacksvClinton <- ggplot(data=joinFile2, aes(x=Black.or.African.American_., y=Clinton))+geom_point()+geom_smooth()
BlacksvSanders <- ggplot(data=joinFile2, aes(x=Black.or.African.American_., y=Sanders))+geom_point()+geom_smooth()

GridBlacks<- grid.arrange(BlacksvSanders, BlacksvClinton, BlacksvCruz, BlacksvTrump, BlacksvKasich, ncol = 2)
GridBlacks


HispanicvKasich <- ggplot(data=joinFile2, aes(x=Hispanic_., y=Kasich))+geom_point()+geom_smooth()
HispanicvTrump <- ggplot(data=joinFile2, aes(x=Hispanic_., y=Trump))+geom_point()+geom_smooth()
HispanicvCruz <- ggplot(data=joinFile2, aes(x=Hispanic_., y=Cruz))+geom_point()+geom_smooth()
HispanicvClinton <- ggplot(data=joinFile2, aes(x=Hispanic_., y=Clinton))+geom_point()+geom_smooth()
HispanicvSanders <- ggplot(data=joinFile2, aes(x=Hispanic_., y=Sanders))+geom_point()+geom_smooth()

GridHispanic<- grid.arrange(HispanicvSanders, HispanicvClinton, HispanicvCruz, HispanicvTrump, HispanicvKasich, ncol = 2)
GridHispanic


AgevKasich <- ggplot(data=joinFile2, aes(x=Median.age..years., y=Kasich))+geom_point()+geom_smooth()
AgevTrump <- ggplot(data=joinFile2, aes(x=Median.age..years., y=Trump))+geom_point()+geom_smooth()
AgevCruz <- ggplot(data=joinFile2, aes(x=Median.age..years., y=Cruz))+geom_point()+geom_smooth()
AgevClinton <- ggplot(data=joinFile2, aes(x=Median.age..years., y=Clinton))+geom_point()+geom_smooth()
AgevSanders <- ggplot(data=joinFile2, aes(x=Median.age..years., y=Sanders))+geom_point()+geom_smooth()

GridAge<- grid.arrange(AgevSanders, AgevClinton, AgevCruz, AgevTrump, AgevKasich, ncol = 2)
GridAge


SixtyTwovKasich <- ggplot(data=joinFile2, aes(x=X62.years.and.over, y=Kasich))+geom_point()+geom_smooth()
SixtyTwoyvTrump <- ggplot(data=joinFile2, aes(x=X62.years.and.over, y=Trump))+geom_point()+geom_smooth()
SixtyTwovCruz <- ggplot(data=joinFile2, aes(x=X62.years.and.over, y=Cruz))+geom_point()+geom_smooth()
SixtyTwovClinton <- ggplot(data=joinFile2, aes(x=X62.years.and.over, y=Clinton))+geom_point()+geom_smooth()
SixtyTwovSanders <- ggplot(data=joinFile2, aes(x=X62.years.and.over, y=Sanders))+geom_point()+geom_smooth()

GridSixtyTwo<- grid.arrange(SixtyTwovSanders, SixtyTwovClinton, SixtyTwovCruz, SixtyTwoyvTrump, SixtyTwovKasich, ncol = 2)
GridSixtyTwo


