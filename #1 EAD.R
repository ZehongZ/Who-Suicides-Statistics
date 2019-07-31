#Install packages
library(data.table)

#Import tables
myd=data.table::fread("who_suicide_statistics.csv", header = T)

#Check firts 5 rows to see if dataset imported properly
head(myd)

#Check dimension of the dataset: 43776*6
dim(myd)

#Check types of variables
str(myd)

#Summary of datasets
summary(myd)

#Numbers of countries: There are 141 countires in total 
country=as.factor(myd$country)
levels(country)

#Time range:From 1979 to 2016
range(myd$year)

#Age range: 5-14 years; 15-24 years; 25-34 years; 35-54 years; 55-74 years; 75+ years
age=as.factor(myd$age)
levels(age)

#suicides_no range: including NA
range(myd$suicides_no)
head(myd$suicides_no,100)

#Missing value plot: suicides_no and population contain missing values
library(VIM)
library(mice)
aggr(myd, prop=FALSE, numbers=TRUE)

#Aggregate total populations by countries
str(myd)
country_population=myd[,c(1,6)]
head(country_population)
population=aggregate(.~country, data=country_population, sum, na.rm=TRUE)
head(population)
library(ggplot2)
library(gcookbook)
ggplot(population, aes(x=country,y=population))+geom_bar(stat="identity")

#Top 20 most populations countries:
top20=population[order(-population$population),]
head(top20,20)
Top20_Largest_Population_Countries=top20[1:20,]
Top20_Largest_Population_Countries
ggplot(Top20_Largest_Population_Countries, aes(x=Top20_Largest_Population_Countries$country, y=format(Top20_Largest_Population_Countries$population,scientific = FALSE)))+geom_bar(stat="identity")+xlab("Countires")+ylab("Population")+geom_text(aes(label=country), vjust=-0.2)+theme(axis.title.x=element_blank(),
                                                                                                                                                                                                                                                                                         axis.text.x=element_blank(),                                                                                                                                                                                                                                                                                axis.ticks.x=element_blank())
#Top 20 least population countires
least20=population[order(population$population),]
head(least20,20)
Least20_Population_Countries=least20[1:20,]
Least20_Population_Countries
ggplot(Least20_Population_Countries, aes(x=Least20_Population_Countries$country, y=format(Least20_Population_Countries$population, scientific = FALSE)))+geom_bar(stat="identity")+xlab("Countries")+ylab("Populations")+geom_text(aes(label=country), vjust=-0.2)+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())


#Aggregate Total Suicides numbers by countries
str(myd)
country_suicides=myd[,c(1,5)]
head(country_suicides)
suicides=aggregate(.~country, data=country_suicides, sum, na.rm=TRUE)
head(suicides)

#Top20 Most Suicides Populations
Top20_suicides=suicides[order(-suicides$suicides_no),]
head(Top20_Most_Suicides_Countries,20)
Top20_Most_Suicides_Countries=Top20_suicides[1:20,]
Top20_Most_Suicides_Countries
ggplot(Top20_Most_Suicides_Countries, aes(x=Top20_Most_Suicides_Countries$country, y=format(Top20_Most_Suicides_Countries$suicides_no, scientific = FALSE)))+geom_bar(stat="identity")+xlab("Countries")+ylab("Number of Suicides Populations")+geom_text(aes(label=country),vjust=-0.2)+theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

#Least20 Most Suicides Populations
least20_suicides=suicides[order(suicides$suicides_no),]
Least20_Suicides_Countries=least20_suicides[1:20,]
Least20_Population_Countries
ggplot(Least20_Suicides_Countries, aes(x=Least20_Suicides_Countries$country, y=format(Least20_Suicides_Countries$suicides_no, scientific = FALSE)))+geom_bar(stat="identity")+xlab("Countries")+ylab("Number of Suicides Populations")+geom_text(aes(label=country), vjust=-0.2)+theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),axis.title.x = element_blank())

#Aggregate population and suicides by countries
myd2=myd[,c(1,5,6)]
str(myd2)
myd2_agg=aggregate(.~country, data=myd2, sum, na.rm=TRUE)
head(myd2_agg)
myd2_agg$suicide_rate=myd2_agg$suicides_no/myd2_agg$population*100
myd2_agg$suicide_rate

#Top 20 highest suicide rates countries
myd2_agg=myd2_agg[order(-myd2_agg$suicide_rate),]
top20_suicides_rates=myd2_agg[1:20,]
top20_suicides_rates
ggplot(top20_suicides_rates, aes(x=top20_suicides_rates$country, y=format(top20_suicides_rates$suicide_rate, scientific = FALSE)))+geom_bar(stat="identity")+xlab("Countries")+ylab("Number of Suicides Populations")+geom_text(aes(label=country),vjust=-0.2)+theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())

#Top 20 lowest suicides rates countries
myd2_agg=myd2_agg[order(myd2_agg$suicide_rate),]
low20_suicides_rates=myd2_agg[1:20,]
low20_suicides_rates
ggplot(low20_suicides_rates, aes(x=low20_suicides_rates$country, y=format(low20_suicides_rates$suicide_rate, scientific = FALSE)))+geom_bar(stat="identity")+xlab("Countries")+ylab("Suicides Rates")+geom_text(aes(label=country),vjust=-0.2)+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())

#Age group with highest suicides populations
age_suicides=myd[,c(4,5)]
str(sex_suicides)
age=aggregate(.~age, data=age_suicides, sum, na.rm=TRUE)
head(age)
ggplot(age, aes(x=age,y=format(suicides_no, scientific = FALSE)))+geom_bar(stat = "identity")+xlab("Age Group")+ylab("Suicides Population")+geom_text(aes(label=age),vjust=-0.2)+theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())

#Sex group with highest suicides populations
sex_suicides=myd[,c(3,5)]
str(sex_suicides)
sex=aggregate(.~sex, data=sex_suicides, sum, na.rm=TRUE)
head(sex)
str(sex)
ggplot(sex, aes(x=sex, y=format(suicides_no, scientific = FALSE)))+geom_bar(stat="identity")+xlab("Sex")+ylab("Suicides_no")+geom_text(aes(label=sex),vjust=-0.2)+theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

#Suicides populations compare to total populations over the years
myd3=myd[,c(2,5,6)]
str(myd3)
suicides_population_years=aggregate(.~year, data=myd3, sum, na.rm=TRUE)
head(suicides_population_years)
ggplot(suicides_population_years, aes(x=year,y=suicides_no,colour="Red"))+geom_line(stat="identity")+xlab("Year")+ylab("Suicides Population")
par(new=TRUE)
ggplot(suicides_population_years, aes(x=year, y=population))+geom_line(stat="identity")+xlab("Year")+ylab("Total Population")
plot(suicides_population_years$year,suicides_population_years$suicides_no, type="l", col="Red",xlab=" ", ylab=" ")
par(new=TRUE)
plot(suicides_population_years$year, format(suicides_population_years$population,scientific = FALSE), type="l", col="Blue",xlab="Year",ylab = "Population")
range(suicides_population_years$population)
range(suicides_population_years$suicides_no)
suicides_population_years$rate=suicides_population_years$suicides_no/suicides_population_years$population
ggplot(suicides_population_years, aes(x=year, y=rate*100))+geom_line(stat="identity")+xlab("Year")+ylab("Suicide Rate (%)")

#Sucides rats across different age groups over the years
myd4=myd[,c(2,4,5)]
head(myd4)
levels(as.factor(myd4$age))
myd4_aggr=aggregate(.~year+age,data=myd4, sum, na.rm=TRUE)
myd4_aggr
fifteen_twentyfour=subset(myd4_aggr, age=="15-24 years")
fifteen_twentyfour
ggplot(fifteen_twentyfour, aes(x=year, y=suicides_no), colours="Red")+geom_line(stat="identity")+ylab("Suicides Number")
g=ggplot(myd4_aggr, aes(year))
g+geom_line(aes(y=suicides_no,colour=age))+ylab("Suicides Number")


            