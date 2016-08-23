# Define the filename of the output image
fileName <- "xDate-yValue_Age-All_State-All"

# Open the macOS graphics device driver
quartz(fileName, width = 18/2.54, height = 18/2.54, dpi = 72)

# Create the figure using ggplot
ggplot(filter(APdata,Age == "All ages"),
       aes(x = Date, y = Value/1e6, color = Sex)) + 
  geom_line(lwd = 1.00, alpha = 1.00) + 
  facet_wrap(~State, scales = "free_y") + 
  labs(x = "Year", y = "Population (millions)",
       title = "Population of Australian States") + 
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
        legend.position = "top",
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.title.align = 0.5,
        
        panel.background = element_rect(fill = "#333333"),
        panel.border = element_rect(fill = NULL, size = 0.25, color = "lightgrey"),
        panel.grid.major = element_line(colour = "lightgrey", size = 0.25),
        panel.grid.minor = element_line(colour = "lightgrey", size = 0.10),
        panel.margin.x = unit(0.5, "lines"),
        panel.margin.y = unit(0.5, "lines"),
        
        plot.background = element_rect(fill = "#333333"),
        plot.margin = unit(c(0, 5, 0, 0), "mm"),
        plot.title = element_text(size = 15, margin = margin(5, 0, 0, 0)),
        
        strip.text = element_text(size = 10),
        
        text = element_text(color = "lightgrey")
  )

# Save the figure as a pdf using the ggsave function
# cairo_pdf is required since without it the Calibri font won't work
ggsave(paste(fileName, "pdf", sep = "."), device = cairo_pdf)