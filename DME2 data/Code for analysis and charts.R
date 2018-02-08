library(tidyverse)

# Make a tibble

qldgen <- as.tibble(Book1)

#Remove unnecessary rows and columns

qldgen <- select(qldgen, Status:Other)
qldgen <- filter(qldgen, Status %in% c("Announced Withdrawal", "Existing less Announced Withdrawal", "Committed", "Proposed"))

#Create a variable that combines all gas
qldgen3 <- mutate(qldgen, `All gas` = CCGT + OCGT + `Gas other`)

#reshape to gather into variables

qldgen4 <- qldgen3 %>% gather("Coal", "CCGT", "OCGT", "Gas other", "Solar*", "Wind", "Water", "Biomass", "Storage", "Other", "All gas", key = "Energy_Source", value = "MW_Capacity")

#Add solar data (calculated below)

QLD_rooftop$Status1 <- QLD_rooftop$Status
QLD_rooftop_forecast$Status1 <- QLD_rooftop_forecast$Status
qldgen5 <- rbind(qldgen4, QLD_rooftop, QLD_rooftop_forecast)

#Change factor levels to change chart order (qldgen2 created below by adding rooftop solar figures from CER)

qldgen5$Status1 <- factor(qldgen5$Status, levels = c('Forecast to 2028', 'Proposed', 'Committed','Announced Withdrawal','Existing less Announced Withdrawal'))

#Chart as stacked bar chart

ggplot(qldgen5, aes(Energy_Source, MW_Capacity)) +
  geom_col(aes(fill = Status1)) +
  scale_y_continuous(breaks = seq(0, 9000, by = 1000)) +
  scale_x_discrete(limits = c("Coal", "All gas", "Solar*", "Solar (small-scale)", "Wind", "Water", "Biomass", "Storage", "Other")) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  labs(x = "Technology", y = "Generation capacity (MW)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14, face = "bold")) +
  ggtitle("Queensland existing and potential new developments by generation type (MW)") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=16, hjust=0.5)) +
  theme(legend.position="bottom")

#Clean Energy Regulator Data - small scale systems

CER_data <- select(Postcode_data_for_small_scale_installations_SGU_Solar, "Small Unit Installation Postcode", "Installations Quantity Total", "SGU Rated Output In kW Total")
CER_data <- as.tibble(CER_data)

#Total installed small-scale solar QLD
CER_data_QLD <- filter(CER_data, `Small Unit Installation Postcode` %in% c(4000:4999, 9000:9999))
CER_data_QLD_total_kW <- summarise(CER_data_QLD, total = sum(`SGU Rated Output In kW Total`))
CER_data_QLD_total_Installs <- summarise(CER_data_QLD, total = sum(`Installations Quantity Total`))
CER_data_total_kW <- summarise(CER_data, total= sum(`SGU Rated Output In kW Total`)) 
CER_data_total_installs <- summarise(CER_data, total= sum(`Installations Quantity Total`)) 

#Recode whole table using Car package
install.packages("car")
library(car)

CER_data$State <-recode(CER_data$`Small Unit Installation Postcode`, "4000:4999='QLD'; 9000:9999 = 'QLD'; 1000:2999 = 'NSW'; 200:299 = 'NSW'; 3000:3999 = 'VIC'; 8000:8999 = 'VIC'; 5000:5999 = 'SA'; 6000:6999 = 'WA'; 800:999 = 'NT'; 7000:7999 = 'TAS'")

#Add new row for dataframe
#Note that the forecast to 2028 for small-scale solar comes from Jacobs, Projections of uptake of small-scale systems for the Austrailan Energy Market Operator, Final Report (2016), Jacobs projections include integrated PV and Storage System uptake.

QLD_rooftop <- data.frame(Status = "Existing less Announced Withdrawal", Energy_Source = "Solar (small-scale)", MW_Capacity = 1948)
QLD_rooftop_forecast <- data.frame(Status = "Forecast to 2028", Energy_Source = "Solar (small-scale)", MW_Capacity = 2552)
QLD_rooftop <- as.tibble(QLD_roofop)
QLD_rooftop_forecast <- as.tibble(QLD_rooftop_forecast)
qldgen2 <- rbind(qldgen1, QLD_rooftop, QLD_rooftop_forecast)




#AEMO max demand chart code

AEMO_forecast_max_demand <- as.tibble(AEMO_forecast_max_demand)
AEMO_forecast_max_demand1 <- gather(AEMO_forecast_max_demand, "Neutral", "Strong", "Weak", key = "Scenario", value = "Maximum demand (MW)")

ggplot(AEMO_forecast_max_demand1, aes(Year, `Maximum demand (MW)`)) + 
  geom_line(aes(colour = Scenario)) +
  expand_limits(y = 0) +
  scale_y_continuous(breaks = seq(0, 12000, by = 2000)) +
  scale_colour_manual(values=c(Neutral = "blue", Strong = "red", Weak = "green"))+
  theme_bw() +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=24,face="bold")) +
  theme(legend.text = element_text(size = 20), legend.title = element_text(size = 24, face = "bold")) 

#AER Generation in Queensland over time
AER_gen_region <- rename(AER_gen_region, c("X__1" = "Technology"))
AER_gen_region1 <- gather(AER_gen_region, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, key = "Year", value = "TWh")
AER_gen_region1 <- as.tibble(AER_gen_region1)
AER_gen_region1$Year <- as.numeric((AER_gen_region1$Year))
AER_gen_region1 <- filter(AER_gen_region1, Technology != "Brown coal")

#Change factor levels to change chart order
AER_gen_region1$Technology2 <- factor(AER_gen_region1$Technology, levels = c('Rooftop solar', 'Solar farm', 'Wind', 'Hydro', 'Other dispatched', 'Gas', 'Black coal'))

#Chart
ggplot(AER_gen_region1, aes(Year, TWh)) +
  geom_area(aes(fill = Technology2)) +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  #expand_limits(x = 2030) +
  scale_x_continuous(breaks = seq(2005, 2030, by = 1)) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=24,face="bold")) +
  theme(legend.text = element_text(size = 20), legend.title = element_text(size = 24, face = "bold")) +
  #theme(legend.position="bottom") +
  theme(panel.grid.minor.x = element_blank()) 
