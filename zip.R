

zip <- read.csv('data/zip_codes.csv')
zip <- zip[,c('Zipcode','EstimatedPopulation','TotalWages','TaxReturnsFiled')]
zip <- na.omit(zip)
zip$Zipcode <- str_pad(zip$Zipcode, 5, pad = "0", side="left")
zip$addrZip <- substr(zip$Zipcode,1,3)
zip$addrZip <- as.factor(str_pad(zip$addrZip, 5, pad = "x", side="right"))
zip <- zip %>% 
  group_by(addrZip) %>%
  summarise(
    population=sum(as.numeric(EstimatedPopulation)),
    Wages=sum(as.numeric(TotalWages)),
    Returns=sum(as.numeric(TaxReturnsFiled)),
    avgWage=round(Wages/Returns)
  ) %>% 
  select (addrZip,population,avgWage)