
head(profile_DF)

performace <- profile_DF %>% select(- Variable) %>% distinct()

performace <- performace %>% 
  group_by(Hospital.Name, Axis, Level) %>% summarise(mean_score = mean(Value)) %>% distinct()


name = "Baptist Medical Center South"

performace_h <- performace %>% filter(Hospital.Name == name)




performace_hosp <- performace_h %>% filter(Axis == "Knowledge" , Level == "Hospital Average" )
t <- tibble(Hospital.Name = "Total" , Axis = "Knowledge" , Level = "total" , mean_score = 1)
t$Hospital.Name <- as.factor(t$Hospital.Name)
performace_hosp <- bind_rows(performace_hosp , t)

performace_hosp$fraction <- performace_hosp$mean_score/1

# Compute the cumulative percentages (top of each rectangle)
performace_hosp$ymax = cumsum(performace_hosp$fraction)

# Compute the bottom of each rectangle
performace_hosp$ymin = c(0, head(performace_hosp$ymax, n=-1))
performace_hosp$labelPosition <- (performace_hosp$ymax + performace_hosp$ymin) / 2

# Compute a good label
performace_hosp$label <- paste0(performace_hosp$Hospital.Name, "\n value: ", performace_hosp$fraction)

# Make the plot
ggplot(performace_hosp, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Level)) +
  geom_rect() +
  scale_fill_brewer(palette=5)  +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() + ggtitle(paste0(name, " - Knowledge")) +
  theme(legend.position = "none") 
             