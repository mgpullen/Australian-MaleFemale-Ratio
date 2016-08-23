# Define the filename of the output image
fileName <- "xDate-yMFratio_Ages-30-60sum_State-Australia"

# Open the macOS graphics device driver
quartz(fileName, width = 14/2.54, height = 14/2.54, dpi = 72)

# Filter the APdata frame to only select the data that I want to analyse
# Select all of the Australian population that are 30, 40, 50 or 60 years old
Male <- filter(APdata, State == "Australia", Sex == "Male")
Female <- filter(APdata, State == "Australia", Sex == "Female")

# Sum the male population over the 30-34, 35-39, 40-44, 45-49, 50-54, 55-59 age ranges
Male30_34_y = (filter(Male, Age == "30")$Value) +
  (filter(Male, Age == "31")$Value) +
  (filter(Male, Age == "32")$Value) +
  (filter(Male, Age == "33")$Value) +
  (filter(Male, Age == "34")$Value)
Male35_39_y = (filter(Male, Age == "35")$Value) +
  (filter(Male, Age == "36")$Value) +
  (filter(Male, Age == "37")$Value) +
  (filter(Male, Age == "38")$Value) +
  (filter(Male, Age == "39")$Value)
Male40_44_y = (filter(Male, Age == "40")$Value) +
  (filter(Male, Age == "41")$Value) +
  (filter(Male, Age == "42")$Value) +
  (filter(Male, Age == "43")$Value) +
  (filter(Male, Age == "44")$Value)
Male45_49_y = (filter(Male, Age == "45")$Value) +
  (filter(Male, Age == "46")$Value) +
  (filter(Male, Age == "47")$Value) +
  (filter(Male, Age == "48")$Value) +
  (filter(Male, Age == "49")$Value)
Male50_54_y = (filter(Male, Age == "50")$Value) +
  (filter(Male, Age == "51")$Value) +
  (filter(Male, Age == "52")$Value) +
  (filter(Male, Age == "53")$Value) +
  (filter(Male, Age == "54")$Value)
Male55_59_y = (filter(Male, Age == "55")$Value) +
  (filter(Male, Age == "56")$Value) +
  (filter(Male, Age == "57")$Value) +
  (filter(Male, Age == "58")$Value) +
  (filter(Male, Age == "59")$Value)

# Sum the female population over the 30-34, 35-39, 40-44, 45-49, 50-54, 55-59 age ranges
Female30_34_y = (filter(Female, Age == "30")$Value) +
  (filter(Female, Age == "31")$Value) +
  (filter(Female, Age == "32")$Value) +
  (filter(Female, Age == "33")$Value) +
  (filter(Female, Age == "34")$Value)
Female35_39_y = (filter(Female, Age == "35")$Value) +
  (filter(Female, Age == "36")$Value) +
  (filter(Female, Age == "37")$Value) +
  (filter(Female, Age == "38")$Value) +
  (filter(Female, Age == "39")$Value)
Female40_44_y = (filter(Female, Age == "40")$Value) +
  (filter(Female, Age == "41")$Value) +
  (filter(Female, Age == "42")$Value) +
  (filter(Female, Age == "43")$Value) +
  (filter(Female, Age == "44")$Value)
Female45_49_y = (filter(Female, Age == "45")$Value) +
  (filter(Female, Age == "46")$Value) +
  (filter(Female, Age == "47")$Value) +
  (filter(Female, Age == "48")$Value) +
  (filter(Female, Age == "49")$Value)
Female50_54_y = (filter(Female, Age == "50")$Value) +
  (filter(Female, Age == "51")$Value) +
  (filter(Female, Age == "52")$Value) +
  (filter(Female, Age == "53")$Value) +
  (filter(Female, Age == "54")$Value)
Female55_59_y = (filter(Female, Age == "55")$Value) +
  (filter(Female, Age == "56")$Value) +
  (filter(Female, Age == "57")$Value) +
  (filter(Female, Age == "58")$Value) +
  (filter(Female, Age == "59")$Value)

# Calculate the male-to-female population ratios
y30_34 <- Male30_34_y / Female30_34_y
y35_39 <- Male35_39_y / Female35_39_y
y40_44 <- Male40_44_y / Female40_44_y
y45_49 <- Male45_49_y / Female45_49_y
y50_54 <- Male50_54_y / Female50_54_y
y55_59 <- Male55_59_y / Female55_59_y

# Create the new data frame with the summed population ratios
x <- rep(filter(Male, Age == "0")$Date, times = 6) # Make six copies of the dates
y <- c(y30_34, y35_39, y40_44, y45_49, y50_54, y55_59) # Append the different ratios to each other
Ages <- rep(c("30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59"), each = 138)

MFratio <- data.frame(x, y, Ages)
MFratio <- tbl_df(MFratio)
MFratio$Ages <- as.character(MFratio$Ages)

# Create the figure using ggplot
ggplot(MFratio, aes(x = x, y = y*100, color = Ages)) + 
  geom_line(lwd = 1.0, alpha = 1.00) + 
  scale_y_continuous(expand = c(0, 0), limit = c(95, 110),
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(x = "Year", y = "M/F ratio (%)",
       title = "Male-to-female ratio in Australia",
       color = "Age interval") + 
  theme_bw(base_family = "Calibri") + 
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(margin = margin(12, 0, 0, 0), angle = 45),
        axis.text.y = element_text(margin = margin(0, 7, 0, 0)), 
        axis.ticks = element_line(colour = "lightgrey", size = 0.25),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(0, 0, 15, 0)), 
        axis.title.y = element_text(margin = margin(0, 10, 0, 15)),
        
        legend.background = element_rect(fill = "#333333", size = 0.5, linetype = "solid"),
        legend.key = element_blank(),
        legend.margin = unit(0.5, "lines"),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.title.align = 0.5,
        
        panel.background = element_rect(fill = "#333333"),
        panel.border = element_rect(fill = NULL, size = 0.25, color = "lightgrey"),
        panel.grid.major = element_line(colour = "lightgrey", size = 0.25),
        panel.grid.minor = element_line(colour = "lightgrey", size = 0.10),
        panel.margin.x = unit(0.5, "lines"),
        panel.margin.y = unit(0.5, "lines"),
        
        plot.background = element_rect(fill = "#333333"),
        plot.margin = unit(c(5, 5, 0, 0), "mm"),
        plot.title = element_text(size = 15, margin = margin(0, 0, 15, 0)),
        
        strip.text = element_text(size = 10),
        
        text = element_text(color = "lightgrey")
  )

# Save the figure as a pdf using the ggsave function
# cairo_pdf is required since without it the Calibri font won't work
ggsave(paste(fileName, "pdf", sep = "."), device = cairo_pdf)