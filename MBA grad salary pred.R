mbaa.df <- read.csv(paste("Deans Dilemma.csv", sep=""))

summary(mbaa.df)

barplot(table(mbaa.df$Gender),main="Frequency distribution of gender",xlab = "Gender",ylab = "Frequency")

barplot(table(mbaa.df$Board_SSC),main="Frequency distribution of Different Boards",xlab = "Boards",ylab = "Frequency")

barplot(table(mbaa.df$Entrance_Test),main="Frequency distribution of Different Entrance test",xlab = "Test",ylab = "Frequency")

barplot(table(mbaa.df$Stream_HSC),main="Frequency distribution of Different stream",xlab = "Stream",ylab = "Frequency")

barplot(table(mbaa.df$Course_Degree),main="Frequency distribution of Different Course degree",xlab = "Course",ylab = "Frequency")

barplot(table(mbaa.df$Specialization_MBA),main="Frequency distribution of Different Specialization of MBA",xlab = "Specialization",ylab = "Frequency")

barplot(table(mbaa.df$Placement),main="Frequency distribution of status of placement",xlab = "Status of placement",ylab = "Frequency")

hist(mbaa.df$Marks_Communication,main="Distribution of marks of Communication",xlab="Marks",col = "grey")

hist(mbaa.df$Marks_Projectwork,main="Distribution of marks of Project",xlab="Marks",col = "grey")

hist(mbaa.df$Marks_BOCA,main="Distribution of marks of BOCA",xlab="Marks",col = "grey")

hist(mbaa.df$Salary,main="Distribution of Salary of placed students",xlab="Salary",col = "grey")

barplot(by(mbaa.df$Percent_SSC,mbaa.df$Board_SSC,sum),main = "Distribution of sum of marks of each board",xlab = "Board marks sum",ylab = "Total" )

barplot(by(mbaa.df$Percent_HSC,mbaa.df$Board_HSC,sum),main = "Distribution of sum of marks of each board",xlab = "Board marks sum",ylab = "Total" )

barplot(by(mbaa.df$Percent_HSC,mbaa.df$Course_Degree,sum),main = "Distribution of selection of course degree on the basis of there marks ",xlab = "Course degree",ylab = "Total" )

barplot(by(mbaa.df$Percent_Degree,mbaa.df$Course_Degree,sum),main = "Distribution of sum of marks of each Degree",xlab = "Degree marks sum",ylab = "Total" )

barplot(table(mbaa.df$Stream_HSC,mbaa.df$Board_HSC),main = "Distribution of boards and corresponding stream",xlab = "Board ",ylab = "Frequency" )

library(corrgram)
corrgram(mbaa.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of MBA variables")

fit <- lm(mbaa.df$Salary ~ mbaa.df$Marks_Communication + mbaa.df$Marks_Projectwork + mbaa.df$Marks_BOCA + mbaa.df$Percent_MBA + mbaa.df$Specialization_MBA + mbaa.df$Gender)

summary(fit)

confint(fit)

coefficients(fit)

