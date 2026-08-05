library(ggplot2)
library(magick)
library(cowplot)
library(gridExtra)

# Clear environment
rm(list=ls())

# Import data
WTSP1 <- read.csv("Output/Tabular Data/WTSP_data_for_analysis.csv")


######### HISTOGRAMS ###########


### P1

p1 <- ggplot(dat1) +
  geom_histogram(aes(x = mean_SR_non_smoky, fill = "Non-Smoky"), alpha = 0.4, color = "black", binwidth = 1) +
  labs(x = "Singing rate",
       y = "Number of sites") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(breaks = seq(0, 14, by = 2), limits = c(0, 14)) +
  scale_x_continuous(breaks = seq(0, 6, by = 1)) +
  scale_fill_manual(values = c("Non-Smoky" = "lightblue"))




### P2

p2 <- ggplot(dat1) +
  geom_histogram(aes(x = mean_SR_smoky, fill = "Smoky"), alpha = 0.4, color = "black", binwidth = 1) +
  labs(x = "Singing rate",
       y = "Number of sites") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(breaks = seq(0, 12, by = 2), limits = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 6, by = 1)) +
  scale_fill_manual(values = c("Smoky" = "orange"))



### P3


p3 <- ggplot(dat1) +
  geom_histogram(aes(x = num_WTSP_non_smoky, fill = "Non-Smoky"), alpha = 0.4, color = "black", binwidth = 1) +
  labs(x = "Number of WTSP",
       y = "Number of sites") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(breaks = seq(0, 24, by = 4), limits = c(0, 24)) +
  scale_x_continuous(breaks = seq(0, 6, by = 1)) +
  scale_fill_manual(values = c("Non-Smoky" = "lightblue"))




### P4

p4 <- ggplot(dat1) +
  geom_histogram(aes(x = num_WTSP_smoky, fill = "Smoky"), alpha = 0.4, color = "black", binwidth = 1) +
  labs(x = "Number of WTSP",
       y = "Number of sites") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(breaks = seq(0, 12, by = 4), limits = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 6, by = 1)) +
  scale_fill_manual(values = c("Smoky" = "orange"))

# Combine plots into a 2x2 grid
combined_plot <- grid.arrange(p1, p2, p3, p4, nrow = 2)


# Save the plot as a PNG file in the "Graphics" folder
ggsave(filename = "Graphics/histogram.png", plot = combined_plot, width = 8, height = 6)









# Q-Q plot - SINGING RATE

dat1$difference <- dat1$mean_SR_smoky - dat1$mean_SR_non_smoky
qqnorm(dat1$difference)
qqline(dat1$difference)

# Save Q-Q plot

png(file = "Graphics/QQ_SR.png", width = 600, height = 800)  # Adjust width and height as needed
qqnorm(dat1$difference, cex.lab = 1.4, cex.axis = 1.2, main = "")  # Remove the title
qqline(dat1$difference)
dev.off()




# Q-Q plot - NUM WTSP

qqnorm(dat1$num_difference)
qqline(dat1$num_difference)

# Save Q-Q plot

png(file = "Graphics/QQ_num_WTSP.png", width = 600, height = 800)  # Adjust width and height as needed
qqnorm(dat1$num_difference, cex.lab = 1.4, cex.axis = 1.2, main = "")  # Remove the title
qqline(dat1$num_difference)
dev.off()











### Difference bar plot - singing rate

pairedttest <- t.test(dat1$mean_SR_smoky, dat1$mean_SR_non_smoky, paired = TRUE)
dat1$difference <- dat1$mean_SR_smoky - dat1$mean_SR_non_smoky
mean_value <- mean(dat1$difference)
ci_lower <- pairedttest$conf.int[1]
ci_upper <- pairedttest$conf.int[2]


# Your ggplot code
p3 <- ggplot(data, aes(x = id, y = value)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black", width = 0.3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  # Adjusted line thickness
  ylab("Difference in singing rate") +
  ylim(-1.5, NA) + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)
  )
 


# Export the plot as a .png file
ggsave(filename = "Graphics/SR_barplot.png", plot = p3, width = 10, height = 8, units = "in")



### Difference bar plot - NUM_WTSP


# Calculate mean and confidence interval
mean_val <- mean(dat1$num_difference)
std_dev <- sd(dat1$num_difference)
n <- nrow(dat1)
margin_of_error <- qt(0.975, n - 1) * (std_dev / sqrt(n))

# Create dataframe for plotting
plot_data <- data.frame(Mean = mean_val, CI_low = mean_val - margin_of_error, CI_high = mean_val + margin_of_error)

# Plot
# Plot
p4 <- ggplot(plot_data, aes(x = "Mean", y = Mean)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.3, color = "black") +  # Adjust width and add black outline
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, color = "black", size = 1) +  # Adjust width
  labs(title = "Mean Values of num_difference with 95% Confidence Interval", y = "Difference in # of WTSP singing") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  # Adjusted line thickness
  theme(axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5))


# Export the plot as a .png file
ggsave(filename = "Graphics/num_WTSP_barplot.png", plot = p4, width = 10, height = 8, units = "in")



# Combine plots


plot_grid(p3, p4, labels = "", ncol = 2)
ggsave(filename = "Graphics/combined_barplot.png", plot = plot_grid(p3, p4, labels = "", ncol = 2), width = 12, height = 6)