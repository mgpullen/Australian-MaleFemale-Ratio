# Define the filename of the output image
fileName <- "xDate-yMFratio_Ages-Multi_State-Australia"

# Open the macOS graphics device driver
quartz(fileName, width = 18/2.54, height = 18/2.54, dpi = 72)

# Filter the APdata frame to only select the data that I want to analyse
# Select all of the Australian population that are 0, 10, 30, 50, 70 or 90 years old
Male <- filter(APdata, State == "Australia", Sex == "Male", Age == "0" | Age == "10" | Age == "30" | Age == "50" | Age == "70" | Age == "90")
Female <- filter(APdata, State == "Australia", Sex == "Female", Age == "0" | Age == "10" | Age == "30" | Age == "50" | Age == "70" | Age == "90")

# Calculate the male-to-female population ratios
y <- Male$Value / Female$Value
x <- Male$Date
Age <- Male$Age
# Create new data frame
MFratio <- data.frame(x, y, Age)
MFratio <- tbl_df(MFratio)

# Create the figure using ggplot
ggplot(MFratio, aes(x = x, y = y*100, color = "red")) + 
  geom_line(lwd = 1.0, alpha = 1.00) + 
  facet_wrap(~Age, scales = "free_y") + 
  scale_y_continuous(expand = c(0, 0),
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(x = "Year", y = "M/F ratio (%)",
       title = "Male-to-female ratio in Australia") + 
  theme_bw(base_family = "Calibri") + 
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(margin = margin(12, 0, 0, 0), angle = 45),
        axis.text.y = element_text(margin = margin(0, 7, 0, 0)), 
        axis.ticks = element_line(colour = "lightgrey", size = 0.25),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(0, 0, 15, 0)), 
        axis.title.y = element_text(margin = margin(0, 10, 0, 15)),
        
        legend.position = "none",
        
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