fileName = "xDate-yValue_Age-All_State-Australia_default"

quartz(fileName, width = 14/2.54, height = 14/2.54, dpi = 72) 

ggplot(filter(APdata,
              Age == "All ages",
              State == "Australia"),
       aes(x = Date, y = Value/1e6, color = Sex)) + 
  geom_line() + 
  labs(x = "Year",
       y = "Population (million)",
       title = "Population of Australia")

ggsave(paste(fileName, "pdf", sep = "."), device = cairo_pdf)