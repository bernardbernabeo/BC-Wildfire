setwd("/Users/khulanj/Documents/project2/Cleaned/")

# Install packages
install.packages("ggplot2")
install.packages("lubridate")
install.packages("data.table")
install.packages("pheatmap")
install.packages("corrplot")
install.packages("tidyverse")

# Load necessary library 
library(dplyr) 
library(readxl)
library(ggplot2)
library(lubridate)
library(data.table)
library(pheatmap)
library(corrplot)

data_2023 <- read.csv("2023_hotspots_cleaned.csv")
data_2022 <- read.csv("2022_hotspots_cleaned.csv")
data_2021 <- read.csv("2021_hotspots_cleaned.csv")
data_2020 <- read.csv("2020_hotspots_cleaned.csv")
data_2019 <- read.csv("2019_hotspots_cleaned.csv")
data_2018 <- read.csv("2018_hotspots_cleaned.csv")
data_2017 <- read.csv("2017_hotspots_cleaned.csv")
data_2016 <- read.csv("2016_hotspots_cleaned.csv")
data_2015 <- read.csv("2015_hotspots_cleaned.csv")
data_2014 <- read.csv("2014_hotspots_cleaned.csv")

datasets <- list(data_2023, data_2022, data_2021, data_2020, data_2019, data_2018, data_2017, data_2016, data_2015, data_2014)
datasets <- lapply(datasets, function(df) {
  colnames(df) <- tolower(colnames(df))  # Convert column names to lowercase
  return(df)
})

full_data <- bind_rows(datasets) 

# Converting year-month to date format
date_parsed <- ym(full_data$year.month)
year_month <- format(date_parsed, "%Y-%m")

# Creating pivot table based on year-month 

setDT(full_data) 
pivot_table_ave_year_month <- full_data[, .(
  average_temp = mean(temp, na.rm = TRUE),
  average_rh = mean(rh, na.rm = TRUE),
  average_ws = mean(ws, na.rm = TRUE),
  average_wd = mean(wd, na.rm = TRUE),
  average_pcp = mean(pcp, na.rm = TRUE), 
  average_ffmc = mean(ffmc, na.rm = TRUE), 
  average_dmc = mean(dmc, na.rm = TRUE), 
  average_dc = mean(dc, na.rm = TRUE),
  average_isi = mean(isi, na.rm = TRUE),
  average_bui = mean(bui, na.rm = TRUE), 
  average_fwi = mean(fwi, na.rm = TRUE), 
  average_ros = mean(ros, na.rm = TRUE),
  average_sfc = mean(sfc, na.rm = TRUE), 
  average_tfc = mean(tfc, na.rm = TRUE),
  average_hfi = mean(hfi, na.rm = TRUE),
  average_cfb = mean(cfb, na.rm = TRUE),
  average_pcuring = mean(pcuring, na.rm = TRUE)),
  by = .(year_month)]
print(pivot_table_ave_year_month)

# ---------------------------------BUI------------------------------------------
# PLOTTING - BUI 
# TIME-SERIES (LINE PLOT)

# Create the time-series BUI _WHOLE 10-YR PERIOD_WHOLE CANADA
pivot_table_ave_year_month[, year_month := as.Date(paste0(year_month, "-01"), format = "%Y-%m-%d")]
ggplot(pivot_table_ave_year_month, aes(x = year_month, y = average_bui)) +
  geom_line(color = "blue3", size = 1, alpha = 0.7) + 
  geom_point(color = "blue3", size = 2, shape = 16) +
  labs(
    title = "Average Monthly Buildup Index (2014 to 2023)",
    x = "Month",
    y = "Average BUI"
  ) +
  theme_minimal() +  
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(
    breaks = seq(0, max(pivot_table_ave_year_month$average_bui), by = 20)
  ) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "red", size = 1) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12) 
  )


#Correlation coefficient BUI vs FFMC per year-month
cor_bui_ffmc <- cor(pivot_table_ave_year_month$average_bui, pivot_table_ave_year_month$average_ffmc)

# Scatter plot BUI vs FFMC per year-month
ggplot(pivot_table_ave_year_month, aes(x = average_ffmc, y = average_bui)) +
  geom_point(color = "blue",size = 4, alpha = 0.7) +           
  ggtitle("Scatter Plot of BUI vs FFMC") +  
  xlab("Fine Fuel Moisture Code") +        
  ylab("Buildup Index") +     
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, max(pivot_table_ave_year_month$average_bui), by = 20)
  ) +
  annotate("text", x = min(pivot_table_ave_year_month$average_ffmc) * 0.9, 
           y = max(pivot_table_ave_year_month$average_bui) * 0.8, 
           label = paste("R =", round(cor_bui_ffmc, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13) 
  )

#Correlation coefficient BUI vs FFMC per year-month
cor_bui_dc <- cor(pivot_table_ave_year_month$average_bui, pivot_table_ave_year_month$average_dc)

# Scatter plot BUI vs DC per year-month
ggplot(pivot_table_ave_year_month, aes(x = average_dc, y = average_bui)) +
  geom_point(color = "blue",size = 4,alpha = 0.7) + 
  ggtitle("Scatter Plot of BUI vs DC") +  
  xlab("Drought Code") + 
  ylab("Buildup Index") +  
  theme_minimal()  +
  scale_y_continuous(
    breaks = seq(0, max(pivot_table_ave_year_month$average_bui), by = 20)
  ) +
  annotate("text", x = min(pivot_table_ave_year_month$average_dc) * 0.9, 
           y = max(pivot_table_ave_year_month$average_bui) * 0.8, 
           label = paste("R =", round(cor_bui_dc, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13) 
  )

#Correlation coefficient BUI vs DMC per year-month
cor_bui_dmc <- cor(pivot_table_ave_year_month$average_bui, pivot_table_ave_year_month$average_dmc)

# Scatter plot BUI vs DMC per year-month
ggplot(pivot_table_ave_year_month, aes(x = average_dmc, y = average_bui)) +
  geom_point(color = "blue",size = 4, alpha = 0.7) + 
  ggtitle("Scatter Plot of BUI vs DMC") +  
  xlab("Duff Moisture Code") + 
  ylab("Buildup Index") +  
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, max(pivot_table_ave_year_month$average_bui), by = 20)
  ) +
  annotate("text", x = min(pivot_table_ave_year_month$average_dmc) * 0.9, 
           y = max(pivot_table_ave_year_month$average_bui) * 0.8, 
           label = paste("R =", round(cor_bui_dmc, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13) 
  )

#Correlation coefficient BUI vs TEMP per year-month
cor_bui_temp <- cor(pivot_table_ave_year_month$average_bui, pivot_table_ave_year_month$average_temp)

# Scatter plot BUI vs temp per year-month
ggplot(pivot_table_ave_year_month, aes(x = average_temp, y = average_bui)) +
  geom_point(color = "blue", size = 4, alpha = 0.7) + 
  ggtitle("Scatter Plot of BUI vs Temperature") + 
  xlab("Temperature (°C)") + 
  ylab("Buildup Index") +  
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, max(pivot_table_ave_year_month$average_bui), by = 20)
  ) +
  annotate("text", x = min(pivot_table_ave_year_month$average_temp) * 0.9, 
           y = max(pivot_table_ave_year_month$average_bui) * 0.8, 
           label = paste("R =", round(cor_bui_temp, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13) 
  )

#Correlation coefficient BUI vs pcp per year-month
cor_bui_pcp <- cor(pivot_table_ave_year_month$average_bui, pivot_table_ave_year_month$average_pcp)

# Scatter plot BUI vs pcp per year-month
ggplot(pivot_table_ave_year_month, aes(x = average_pcp, y = average_bui)) +
  geom_point(color = "blue", size = 4, alpha = 0.7) + 
  ggtitle("Scatter Plot of BUI vs Precipitation") + 
  xlab("Precipitation (mm)") + 
  ylab("Buildup Index") +  
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, max(pivot_table_ave_year_month$average_bui), by = 20)
  ) +
  annotate("text", x = max(pivot_table_ave_year_month$average_pcp) * 0.8, 
           y = max(pivot_table_ave_year_month$average_bui) * 0.8, 
           label = paste("R =", round(cor_bui_pcp, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13) 
  )

#Correlation coefficient BUI vs rh per year-month
cor_bui_rh <- cor(pivot_table_ave_year_month$average_bui, pivot_table_ave_year_month$average_rh)

# Scatter plot BUI vs rh per year-month
ggplot(pivot_table_ave_year_month, aes(x = average_rh, y = average_bui)) +
  geom_point(color = "blue", size = 4, alpha = 0.7) + 
  ggtitle("Scatter Plot of BUI vs Relative Humidity") +
  xlab("Relative Humidity (%)") + 
  ylab("Buildup Index") +  
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, max(pivot_table_ave_year_month$average_bui), by = 20)
  ) +
  annotate("text", x = max(pivot_table_ave_year_month$average_rh) * 0.8, 
           y = max(pivot_table_ave_year_month$average_bui) * 0.8, 
           label = paste("R =", round(cor_bui_rh, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13) 
  )

# -----------------------------------FWI----------------------------------------
# PLOTTING - FWI 
# Time series for fwi
pivot_table_ave_year_month[, year_month := as.Date(paste0(year_month, "-01"), format = "%Y-%m-%d")]
ggplot(pivot_table_ave_year_month, aes(x = year_month, y = average_fwi)) +
  geom_line(color = "blue3", size = 1, alpha = 0.7) + 
  geom_point(color = "blue3", size = 2, shape = 16) +
  labs(
    title = "Average Monthly Fire Weather Index (2014 to 2023)",
    x = "Month",
    y = "Fire Weather Index"
  ) +
  theme_minimal() +  
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(
    breaks = seq(0, max(pivot_table_ave_year_month$average_fwi), by = 10)
  ) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "red", size = 1) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

# Timer series for temp
pivot_table_ave_year_month[, year_month := as.Date(paste0(year_month, "-01"), format = "%Y-%m-%d")]
ggplot(pivot_table_ave_year_month, aes(x = year_month, y = average_temp)) +
  geom_line(color = "red", size = 1, alpha = 0.7) + 
  geom_point(color = "red", size = 2, shape = 16) +
  labs(
    title = "Average Monthly Temperature (2014 to 2023)",
    x = "Month",
    y = "Temperature (°C)"
  ) +
  theme_minimal() +  
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(
    breaks = seq(0, max(pivot_table_ave_year_month$average_temp), by = 10)
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

# time series for fwi and temp
pivot_table_ave_year_month[, year_month := as.Date(paste0(year_month, "-01"), format = "%Y-%m-%d")]

# Plotting both FWI and temperature on the same graph with two y-axes
ggplot(pivot_table_ave_year_month, aes(x = year_month)) +
  # Plot the first line (average FWI)
  geom_line(aes(y = average_fwi, color = "Fire Weather Index"), size = 1, alpha = 0.7) + 
  geom_point(aes(y = average_fwi, color = "Fire Weather Index"), size = 2, shape = 16) + 
  
  # Plot the second line (average temperature)
  geom_line(aes(y = average_temp * (max(pivot_table_ave_year_month$average_fwi) / max(pivot_table_ave_year_month$average_temp)), color = "Temperature"), size = 1, alpha = 0.7) + 
  geom_point(aes(y = average_temp * (max(pivot_table_ave_year_month$average_fwi) / max(pivot_table_ave_year_month$average_temp)), color = "Temperature"), size = 2, shape = 16) + 
  
  # Add title and axis labels
  labs(
    title = "Average Monthly Fire Weather Index and Temperature (2014 to 2023)",
    x = "Month",
    y = "Fire Weather Index",
    color = "Variable"  # Title for the legend
  ) +
  
  # Customize the x-axis and y-axis
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(
    name = "Fire Weather Index",
    breaks = seq(0, max(pivot_table_ave_year_month$average_fwi), by = 10)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(
      trans = ~ . * (max(pivot_table_ave_year_month$average_temp) / max(pivot_table_ave_year_month$average_fwi)),
      name = "Temperature (°C)",
      breaks = seq(0, max(pivot_table_ave_year_month$average_temp), by = 10)
    )
  ) +
  
  # Customize the colors for the lines
  scale_color_manual(
    values = c("Fire Weather Index" = "blue3", "Temperature" = "red")
  ) +
  
  # Customize the plot's appearance
  theme_minimal() +  
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.text.y.right = element_text(size = 12)
  )

# scatterplot of fwi vs temp
cor_fwi_temp = cor(pivot_table_ave_year_month$average_fwi, pivot_table_ave_year_month$average_temp)

ggplot(pivot_table_ave_year_month, aes(x = average_temp, y = average_fwi)) +
  geom_point(color = "red",size = 4, alpha = 0.7) +           
  ggtitle("Scatter Plot of FWI vs Temperature") +  
  xlab("Temperature (°C)") +        
  ylab("Fire Weather Index") +     
  theme_minimal() +
  annotate("text", x = min(pivot_table_ave_year_month$average_temp) * 0.9, 
           y = max(pivot_table_ave_year_month$average_fwi) * 0.8, 
           label = paste("R =", round(cor_fwi_temp, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

# Time series for rh
pivot_table_ave_year_month[, year_month := as.Date(paste0(year_month, "-01"), format = "%Y-%m-%d")]
ggplot(pivot_table_ave_year_month, aes(x = year_month, y = average_rh)) +
  geom_line(color = "red", size = 1, alpha = 0.7) + 
  geom_point(color = "red", size = 2, shape = 16) +
  labs(
    title = "Average Monthly Relative Humidity (2014 to 2023)",
    x = "Month",
    y = "Relative Humidity (%)"
  ) +
  theme_minimal() +  
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(
    breaks = seq(0, max(pivot_table_ave_year_month$average_rh), by = 10)
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

# scatterplot of fwi vs rh
cor_fwi_rh = cor(pivot_table_ave_year_month$average_fwi, pivot_table_ave_year_month$average_rh)

ggplot(pivot_table_ave_year_month, aes(x = average_rh, y = average_fwi)) +
  geom_point(color = "red",size = 4, alpha = 0.7) +           
  ggtitle("Scatter Plot of FWI vs Relative Humidity") +  
  xlab("Relative Humidity (%)") +        
  ylab("Fire Weather Index") +     
  theme_minimal() +
  annotate("text", x = max(pivot_table_ave_year_month$average_rh) * 0.9, 
           y = max(pivot_table_ave_year_month$average_fwi) * 0.8, 
           label = paste("R =", round(cor_fwi_rh, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

# Time series for pcp
pivot_table_ave_year_month[, year_month := as.Date(paste0(year_month, "-01"), format = "%Y-%m-%d")]
ggplot(pivot_table_ave_year_month, aes(x = year_month, y = average_pcp)) +
  geom_line(color = "green2", size = 1, alpha = 0.7) + 
  geom_point(color = "green2", size = 2, shape = 16) +
  labs(
    title = "Average Monthly Precipitation (2014 to 2023)",
    x = "Month",
    y = "Precipitation (mm)"
  ) +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "red", size = 1) +
  theme_minimal() +  
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(
    breaks = seq(0, max(pivot_table_ave_year_month$average_pcp), by = 1)
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

# scatterplot of fwi vs pcp
cor_fwi_pcp = cor(pivot_table_ave_year_month$average_fwi, pivot_table_ave_year_month$average_pcp)

ggplot(pivot_table_ave_year_month, aes(x = average_pcp, y = average_fwi)) +
  geom_point(color = "red",size = 4, alpha = 0.7) +           
  ggtitle("Scatter Plot of FWI vs Precipitation") +  
  xlab("Precipitation (mm)") +        
  ylab("Fire Weather Index") +     
  theme_minimal() +
  annotate("text", x = min(pivot_table_ave_year_month$average_pcp) * 0.9, 
           y = max(pivot_table_ave_year_month$average_fwi) * 0.8, 
           label = paste("R =", round(cor_fwi_pcp, 2)), 
           color = "black", size = 6, hjust = -4) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

# ---------------------------------ISI------------------------------------------
# PLOTTING - ISI
# Time-series (Initial Spread Index) _Average per MONTH per YEAR
pivot_table_ave_year_month[, year_month := as.Date(paste0(year_month, "-01"), format = "%Y-%m-%d")]
ggplot(pivot_table_ave_year_month, aes(x = year_month, y = average_isi)) +
  geom_line(color = "blue3", size = 1, alpha = 0.7) + 
  geom_point(color = "blue3", size = 2, shape = 16) +
  labs(
    title = "Average Monthly Initial Spread Index (2014 to 2023)",
    x = "Month",
    y = "Initial Spread Index"
  ) +
  theme_minimal() +  
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(
    breaks = seq(0, max(pivot_table_ave_year_month$average_isi), by = 10)
  ) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", size = 1) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12) 
  )

# Time-series(MULTIPLE VARIABLES_ISI, FFMC, WS) _Average per MONTH per YEAR
pivot_table_long <- melt(pivot_table_ave_year_month, 
                         id.vars = "year_month", 
                         measure.vars = c("average_isi", "average_ffmc", "average_ws"),
                         variable.name = "Variable",
                         value.name = "Value")

ggplot(pivot_table_long, aes(x = year_month, y = Value, color = Variable)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 2) + 
  scale_y_continuous(
    name = "Average ISI and Average FFMC",
    sec.axis = sec_axis(~ . * 0.5, name = "Average WS (scaled)")  
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") + 
  labs(
    title = "Average Monthly ISI, FFMC, and WS",
    x = "Month",
    y = "Values",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    axis.title.x = element_text(size = 13, face = "bold"),  
    axis.title.y = element_text(size = 13, face = "bold"),  
    legend.title = element_text(size = 12, face = "bold") 
  ) +
  scale_color_manual(values = c("average_isi" = "blue", "average_ffmc" = "forestgreen", "average_ws" = "red"))

# SCATTER PLOTS
# Scatter Plot (ISI and FFMC) _Average per MONTH per YEAR

cor_isi_ffmc <-cor(pivot_table_ave_year_month$average_ffmc,pivot_table_ave_year_month$average_isi)

ggplot(pivot_table_ave_year_month, aes(x = average_ffmc, y = average_isi)) +
  geom_point(color = "blue",size = 4, alpha = 0.7) +           
  ggtitle("Scatter Plot of FFMC vs ISI") +  
  xlab("Fine Fuel Moisture Code") +        
  ylab("Initial Spread Index") +     
  theme_minimal() +
  annotate("text", 
           x = min(pivot_table_ave_year_month$average_ffmc) * 0.9, 
           y = max(pivot_table_ave_year_month$average_isi) * 0.8, 
           label = paste("R =", round(cor_isi_ffmc, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13) 
  )

cor_isi_ws <-cor(pivot_table_ave_year_month$average_ws,pivot_table_ave_year_month$average_isi)

# Scatter Plot (ISI and WS) _Average per MONTH per YEAR
ggplot(pivot_table_ave_year_month, aes(x = average_ws, y = average_isi)) +
  geom_point(color = "blue",size = 4, alpha = 0.7) +           
  ggtitle("Scatter Plot of WS vs ISI") +  
  xlab("Wind Speed (km/h)") +        
  ylab("Initial Spread Index") +     
  theme_minimal() +
  annotate("text", 
           x = min(pivot_table_ave_year_month$average_ws) * 0.9, 
           y = max(pivot_table_ave_year_month$average_isi) * 0.8, 
           label = paste("R =", round(cor_isi_ws, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13) 
  )

cor_isi_rh <-cor(pivot_table_ave_year_month$average_rh,pivot_table_ave_year_month$average_isi)
# Scatter Plot (ISI and RH) _Average per MONTH per YEAR
ggplot(pivot_table_ave_year_month, aes(x = average_rh, y = average_isi)) +
  geom_point(color = "blue",size = 4, alpha = 0.7) +           
  ggtitle("Scatter Plot of RH vs ISI") +  
  xlab("Relative Humidity (%)") +        
  ylab("Initial Spread Index") +     
  theme_minimal() +
  annotate("text", 
           x = min(pivot_table_ave_year_month$average_rh) * 2.5, 
           y = max(pivot_table_ave_year_month$average_isi) * 0.8, 
           label = paste("R =", round(cor_isi_rh, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13) 
  )

cor_isi_pcp <-cor(pivot_table_ave_year_month$average_pcp,pivot_table_ave_year_month$average_isi)
# Scatter Plot (ISI and PCP) _Average per MONTH per YEAR
ggplot(pivot_table_ave_year_month, aes(x = average_pcp, y = average_isi)) +
  geom_point(color = "blue",size = 4, alpha = 0.7) +           
  ggtitle("Scatter Plot of PCP vs ISI") +  
  xlab("Precipitation (mm)") +        
  ylab("Initial Spread Index") +     
  theme_minimal() +
  annotate("text", 
           x = min(pivot_table_ave_year_month$average_pcp) * 150, 
           y = max(pivot_table_ave_year_month$average_isi) * 0.8, 
           label = paste("R =", round(cor_isi_pcp, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13) 
  )

cor_ffmc_rh <-cor(pivot_table_ave_year_month$average_ffmc,pivot_table_ave_year_month$average_rh)
# Scatter Plot (FFMC and RH) _Average per MONTH per YEAR
ggplot(pivot_table_ave_year_month, aes(x = average_rh, y = average_ffmc)) +
  geom_point(color = "blue",size = 4, alpha = 0.7) +           
  ggtitle("Scatter Plot of RH vs FFMC") +  
  xlab("Relative Humidity (%)") +        
  ylab("Fine Fuel Moisture Code") +     
  theme_minimal() +
  annotate("text", 
           x = min(pivot_table_ave_year_month$average_rh) * 1, 
           y = max(pivot_table_ave_year_month$average_ffmc) * 0.8, 
           label = paste("R =", round(cor_ffmc_rh, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13) 
  )

cor_ffmc_pcp <-cor(pivot_table_ave_year_month$average_ffmc,pivot_table_ave_year_month$average_pcp)
# Scatter Plot (FFMC and RH) _Average per MONTH per YEAR
ggplot(pivot_table_ave_year_month, aes(x = average_pcp, y = average_ffmc)) +
  geom_point(color = "blue",size = 4, alpha = 0.7) +           
  ggtitle("Scatter Plot of PCP vs FFMC") +  
  xlab("25-hr Precipitation (mm)") +        
  ylab("Fine Fuel Moisture Code") +     
  theme_minimal() +
  annotate("text", 
           x = min(pivot_table_ave_year_month$average_pcp) * 100, 
           y = max(pivot_table_ave_year_month$average_ffmc) * 0.8, 
           label = paste("R =", round(cor_ffmc_pcp, 2)), 
           color = "black", size = 6, hjust = 0) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13) 
  )
