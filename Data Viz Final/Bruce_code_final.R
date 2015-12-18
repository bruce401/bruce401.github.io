gasprices <- read.csv("gasprices.csv")

require(ggplot2)

#for dotplot
gasdot<-ggplot(gasprices, aes(x=year, y=value))+
  geom_point(shape=19, size=5)+
  scale_x_continuous(limits=c(1990, 2010), breaks=1990:2010)+
  scale_y_continuous(limit=c(0, 3.5))+
  coord_flip()

print(gasdot)
ggsave("gasdot.pdf", gasdot, useDingbats = FALSE)

gasst<-ggplot(gasprices, aes(x=year, y=value))+
  geom_step()+
  scale_x_continuous(limits=c(1990, 2010), breaks=1990:2010)+
  scale_y_continuous(limit=c(0, 3.5))+
  coord_flip()

print(gasst)
ggsave('gasst.pdf', gasst, useDingbats=FALSE)
#for timeline- line graph
gasline<-ggplot(gasprices, aes(x=year, y=value))+
  geom_line()+
  scale_x_continuous(breaks=1990:2015)+coord_flip()
print(gasline)
ggsave("gasline.pdf", gasline, useDingbats=FALSE)
#for area plot
gasarea<-ggplot(gasprices, aes(x=year, y=value))+
  geom_area(alpha=0.5)+
  scale_x_continuous(limits=c(1990, 2010), breaks=1990:2010)+
  coord_flip()

print(gasarea)
ggsave("gasarea.pdf", gasarea, useDingbats=FALSE)

gasstep<-ggplot(gasprices, aes(x=year, y=value))+
  geom_step()+
  scale_x_continuous(breaks=1990:2015)
print(gasstep)

#oil consumption per capita
oilc <- read.csv("USoilconsumption.csv")
View(oilc)
oildot <- ggplot(oilc, aes(x=year, y=value))+
  geom_point(shape=19, size=5)+
  scale_x_continuous(limits=c(1990, 2010), breaks=1990:2010)+
  scale_y_continuous(limits=c(0, 3.5))+
  coord_flip()
print(oildot)
ggsave("oildot.pdf", oildot, useDingbats=FALSE)

oilstep <- ggplot(oilc, aes(x=year, y=value))+
  geom_step()+
  scale_x_continuous(limits=c(1990, 2010), breaks=1990:2010)+
  scale_y_continuous(limits=c(0, 3.5))+
  coord_flip()
print(oilstep)
ggsave('oilstep.pdf', oilstep, useDingbats=FALSE)



oilarea <- ggplot(oilc, aes(x=year, y=value))+
  geom_area(alpha=0.5)+
  scale_x_continuous(limits=c(1990, 2010), breaks=1990:2010)+
  scale_y_continuous(limits=c(0, 3.5))+
  coord_flip()
print(oilarea)

ggsave("oilarea.pdf", oilarea, useDingbats=FALSE)

#for timeline - oil line graph
oilline <- ggplot(oilc, aes(x=year, y=value))+
  geom_line()+
  scale_y_continuous(limits=c(0, 4))

print(oilline)
ggsave('oilline.pdf', oilline, useDingbats=FALSE)

#do overlapping in illustrator

#other long-term time series
#enrgy use per person time series
energyuse <- read.csv('energyuseperperson.csv')
names(energyuse)
energyuseplot <- ggplot(energyuse, 
                        aes(x=year, y=energ_use_per_cap))+
  geom_line()+
  scale_y_continuous(limits=c(0, 8.5)) +
  coord_flip()
print(energyuseplot)

ggsave("energyuse.pdf", energyuseplot, useDingbats=FALSE)

energyusestep <- ggplot(energyuse, 
                        aes(x=year, y=energ_use_per_cap))+
  geom_step()+
  scale_y_continuous(limits=c(0, 8.5)) +
  coord_flip()
print(energyusestep)
ggsave('energyusestep.pdf', energyusestep, useDingbats=FALSE)

#people affected by flood in US every year
flood <- read.csv("USaffectedbyflood.csv")

floods <- ggplot(flood, aes(x=year, y=amt_affected))+
  geom_area()
print(floods)
ggsave("floods.pdf", floods, useDingbats=FALSE)


#dot plot
floodsDot <- ggplot(flood, aes(x=year, y=amt_affected))+
  geom_point()
print(floodsDot)
ggsave("floodsDot.pdf", floodsDot, useDingbats=FALSE)


floodstep <- ggplot(flood, aes(x=year, y=amt_affected))+
  geom_step()
print(floodstep)
ggsave("floodstep.pdf", floodstep, useDingbats=FALSE)



#storms
storm <- read.csv("US_affectedstorms.csv")
#area
storms<- ggplot(storm, aes(x=year, y=amt_affected))+
  geom_area() + 
  scale_x_continuous(limits=c(1980, 2010), breaks=1980:2008)

print(storms)

ggsave("storms.pdf", storms, useDingbats=FALSE)


#dot
stormsDot<- ggplot(storm, aes(x=year, y=amt_affected))+
  geom_point()+ 
  scale_x_continuous(limits=c(1980, 2010), breaks=1980:2008)
print(stormsDot)
ggsave("stormsDot.pdf", stormsDot, useDingbats=FALSE)

stormstep<- ggplot(storm, aes(x=year, y=amt_affected))+
  geom_step()+ 
  scale_x_continuous(limits=c(1980, 2010), breaks=1980:2008)
print(stormstep)
ggsave("stormstep.pdf", stormstep, useDingbats=FALSE)




#gallup questions --> small multiples?
#opinion on economy vs. environment
econvenv <- read.csv("econvsenv.csv")
names(econvenv)
require('magrittr')
require(dplyr)
require(tidyr)
econenvdata<-econvenv %>%
  select(year, env_priority, econ_priority, equal) %>%
  gather(response, percentage, -year)

econenvplot <- ggplot(econenvdata, aes(x=year, y=percentage, group=response, color=response))+
  geom_line()+
  scale_x_continuous(breaks=2000:2015)
print(econenvplot)
ggsave("econenvplot.pdf", econenvplot, useDingbats = FALSE)
econenvreord <- econenvdata[order[response,-percentage]]
econenvarea <- ggplot(econenvdata, aes(x=year, y=percentage,  order=as.numeric(percentage)))+
  geom_area(aes(group=response,fill=response))

ggsave('econenvarea.pdf', econenvarea, useDingbats=FALSE)

econenvbar <- ggplot(econenvdata, aes(x=year, y=percentage, group=response, fill=response))+
  geom_bar(stat='identity')+coord_flip()
print(econenvbar)

ggsave('econenvbar.pdf', econenvbar, useDingbats=FALSE)
#environment vs. energy 
nrgvenv <- read.csv("envvsenergy.csv")

enerenvdata<-nrgvenv %>%
  select(year, env_priority, energy_priority, equal, neither) %>%
  gather(response, percentage, -year)

enrgenvplot <- ggplot(enerenvdata, 
                      aes(x=year,percentage, group=response, color=response))+
  geom_line()

print(enrgenvplot)
ggsave("enrgenvplot.pdf", enrgenvplot, useDingbats = FALSE)

#the following could be good slope charts:
#when will the effects of global warming start
effects <- read.csv('effects_start.csv')
names(effects)
fxdata <- effects %>%
  select(year, already_begun, start_few_years, within_lifetime, 
         future_generations, never) %>%
  gather(response, percentage, -year)


fxplot <- ggplot(fxdata, aes(x=year, y=percentage, group=response, color=response))+
  geom_line()

print(fxplot)
ggsave("fxplot.pdf", fxplot, useDingbats = FALSE)

fxfull <- effects %>%
  select(year, already_begun, start_few_years, within_lifetime, 
         future_generations, never, dk, rf) %>%
  gather(response, percentage, -year)
fxbar <- ggplot(fxfull, aes(x=year, y=percentage, group=response, fill=response))+
  geom_bar(stat='identity')
print(fxbar)
ggsave('fxbar.pdf', fxbar, useDingbats=FALSE)
names(fxfull)

fxord<- fxdata[order[response, -percentage]]
fxordplot <- ggplot(fxfull, aes(x=year, y=percentage, order=as.numeric(percentage)))+
  geom_area(aes(group=response, fill=response))+
  scale_x_continuous(breaks=2000:2015)

print(fxordplot)

ggsave('fxordplot.pdf', fxordplot, useDingbats = FALSE)
econenvreord <- econenvdata[order[response,-percentage]]
econenvarea <- ggplot(econenvdata, aes(x=year, y=percentage,  order=as.numeric(percentage)))+
  geom_area(aes(group=response,fill=response))+
  scale_x_continuous(breaks=2000:2015)
print(econenvarea)
ggsave('econenvarea.pdf', econenvarea, useDingbats=FALSE)
#sympathy/active with the environmental movement
envmvmt <- read.csv("envmovementparticip.csv")

names(envmvmt)
#move_data <_ envmvmt %>%
#select(year, active, sympathetic_not_active, neutral, unsympathetic, dk) %>%
#gather(response, percentage, -year)
envmv_data <-envmvmt %>%
  select(year, active, sympathetic_not_active, neutral, unsympathetic, dk) %>%
  gather(response, percentage, -year)

emvplot <- ggplot(envmv_data, aes(x=year, y=percentage, group=response, color=response))+
  geom_line()
print(emvplot)

ggsave("emvplot.pdf", emvplot, useDingbats = FALSE)
#do you think that the media is portraying seriousness of
#this issue accurately
serious<-read.csv('seriousnessin_news.csv')

names(serious)
srs_data <- serious %>%
  select(year, exaggerated, correct, underestimated, dk) %>%
  gather(response, percentage, -year)

srsplot<- ggplot(srs_data, aes(x=year, y=percentage, group=response, color=response))+
  geom_line()

print(srsplot)
ggsave("srsplot.pdf", srsplot, useDingbats = FALSE)
#personal worries
#worry1: climate change
ccworry<-read.csv('personally_worry_climatechange.csv')

names(ccworry)
ccw <- ccworry %>%
  select(year, great_deal, fair_amt, little, not_at_all, dk) %>%
  gather(response, percentage, -year)

ccwplot <- ggplot(ccw, aes(x=year, y=percentage, group=response, color=response))+
  geom_line()
print(ccwplot)
ggsave("ccwplot.pdf", ccwplot, useDingbats = FALSE)

#highlight peaks and valleys in each of these worries thing
#worry 2:economy

econworry<-read.csv('personally_worry_econ.csv')

names(econworry)
ew <- econworry %>%
  select(year, great_deal, fair_amt, little, not_at_all, dk) %>%
  gather(response, percentage, -year)

ewplot <- ggplot(ew, aes(x=year, y=percentage, group=response, color=response))+
  geom_line()
print(ewplot)
ggsave("ewplot.pdf", ewplot, useDingbats = FALSE)

#worry 3: qUALITy of environment
envworry<-read.csv('personally_worry_qual_env.csv')

names(envworry)
evw <- envworry %>%
  select(year, great_deal, fair_amt, little, not_at_all, dk) %>%
  gather(response, percentage, -year)

evwplot <- ggplot(evw, aes(x=year, y=percentage, group=response, color=response))+
  geom_line()
print(evwplot)

ggsave("evwplot.pdf", evwplot, useDingbats = FALSE)
#slopechart
#current biggest thing facing country right now
current<-read.csv('currentimportance_2.csv')
names(current)
current_ed <- current %>%
  select(year,econ,jobs, cost_living, recession,environment, fuel, 
         energy, guns, overpop, lack_money) %>%
  gather(response, percentage, -year)

issueplot <- ggplot(current_ed, aes(x=year, y=percentage, group=response, color=response))+
  geom_line()
print(issueplot)
ggsave("issueplot.pdf", issueplot, useDingbats = FALSE)
#importance 25 years from now
#current biggest thing facing country right now
future<-read.csv('importanceFuture_2.csv')
names(future)
future_ed <- future %>%
  select(year,econ,jobs, cost_living,
         recession, 
         environment,  
         fuel, energy, overpop, 
         lack_money, education,energy_crisis, naturaldisaster) %>%
  gather(response, percentage, -year)

issueplot2 <- ggplot(future_ed, aes(x=year, y=percentage, group=response, color=response))+
  geom_line()
print(issueplot2)
ggsave("issueplot2.pdf", issueplot2, useDingbats = FALSE)


# forest land?

#coal consumption
coal <- read.csv("coalconsumption_us.csv")
coalplot <- ggplot(coal, aes(x=year, y=amount))+
  geom_line() +
  scale_y_continuous(limits=c(0, 2.1))
print(coalplot)
ggsave('coalplot.pdf', coalplot, useDingbats = FALSE)

coalstep <- ggplot(coal, aes(x=year, y=amount))+
  geom_step() +
  scale_y_continuous(limits=c(0, 2.1))
print(coalstep)
ggsave('coalstep.pdf', coalstep, useDingbats = FALSE)

#energyemphasis 2015
nrg <- read.csv("energyemph2015.csv")
View(nrg)
nrg_ed <- nrg %>%
  select(energy, more_emph, less_emp, same, dk, rf) %>%
  gather(response, percentage, -energy)
nrgplot<- ggplot(nrg_ed, aes(x=energy, y=percentage, group=energy, fill=response))+
  geom_bar(stat='identity')+
  coord_flip()
print(nrgplot)
ggsave('nrgplot.pdf', nrgplot, useDingbats=FALSE)

?geom_tile
nrgtile <-ggplot(nrg_ed, aes(x=response, y=percentage, group=energy))+
  geom_tile()+ 
  scale_fill_gradient(low="dark green", high="light green")
print(nrgtile)



#extremetemp
temps <- read.csv('extremetemp_affectedUS.csv')
tempsplot <- ggplot(temps, aes(x=year, y=amt))+
  geom_point()
print(tempsplot)
ggsave('tempsplot.pdf', tempsplot, useDingbats=FALSE)

tempsline <- ggplot(temps, aes(x=year, y=amt))+
  geom_line()
print(tempsline)

ggsave('tempsline.pdf', tempsline, useDingbats = FALSE)

tempstep<-ggplot(temps, aes(x=year, y=amt))+
  geom_step()
print(tempstep)
ggsave('tempstep.pdf', tempstep, useDingbats=FALSE)

#forest land


#do you think thee quality of the environment has improved 2015
qual<- read.csv('qual_env_improve2015.csv')
qualplot <- ggplot(qual,aes(x=quality, y=percent, fill=quality))+
  geom_bar(stat="identity")+
  coord_flip()

print(qualplot)
ggsave('qualplot.pdf', qualplot, useDingbats = FALSE)

#scientist consensus 2015
cons2 <- read.csv('scientist_consensus_2015.csv')
reorder(cons2, -percent)
conplot <- ggplot(cons2,aes(x=response, y=percent))+
  geom_bar(stat="identity")

print(conplot)

ggsave('conplot.pdf', conplot, useDingbats = FALSE)
names(cons2)
consside <- ggplot(cons2,aes(x=response, y=percent, fill=response))+
  geom_bar(stat="identity")+
  coord_flip()

print(consside)
ggsave('consside.pdf', consside, useDingbats = FALSE)


#serious threat to life 2015
threat <- read.csv('seriousthreat_2015.csv')
names(threat)
threatplot <- ggplot(threat,aes(x=response, y=percent))+
  geom_bar(stat="identity")
print(threatplot)
ggsave('threatplot.pdf', threatplot, useDingbats = FALSE)

#water resources
water <- read.csv('waterresources.csv')

waterplot <- ggplot(water, aes(x=year, y=amount))+
  geom_line()+
  scale_y_continuous(limits=c(0,16000))

print(waterplot)

ggsave('waterplot.pdf', waterplot, useDingbats = FALSE)

waterstep <- ggplot(water, aes(x=year, y=amount))+
  geom_step()+
  scale_y_continuous(limits=c(0,16000))

print(waterstep)
ggsave('waterstep.pdf', waterstep, useDingbats = FALSE)


#2015 worries
envworries <- read.csv('worry_2015.csv')
names(envworries)
View(envworries)
envworry_edy <- envworries %>%
  select(worry, great_deal, fair_amt, little, not_at_all, dk, rf) %>%
  gather(response, amount, -worry)


worrybar <- ggplot(envworry_edy, aes(x=worry, y=amount, group=worry, fill=response))+
  geom_bar(stat='identity')+
  coord_flip()
print(worrybar)
ggsave('worrybar.pdf', worrybar, useDingbats=FALSE)

