library(ggplot2)
data(txhousing)

#Line Plot of Median Home Prices Over Time
ggplot(txhousing, aes(x = date, y = median)) +
  geom_line(color = "blue") +
  labs(title = "Median Home Prices Over Time in Texas",
       x = "Date",
       y = "Median Home Price") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3)) +  # Adjust the formatting here
  theme_minimal()



#Scatter Plot of Sales Volume vs. Median Home Price
ggplot(txhousing, aes(x = sales, y = median)) +
  geom_point(color = "black", alpha = 0.7) +
  labs(title = "Scatter Plot of Sales Volume vs Median Home Price in Texas",
       x = "Sales Volume",
       y = "Median Home Price") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3)) +  # Adjust the formatting here
  theme_minimal()



#Box Plot of Median Home Prices for Top Counties
top_counties <- txhousing %>%
  group_by(city) %>%
  summarize(MedianPrice = median(median)) %>%
  arrange(desc(MedianPrice)) %>%
  head(5) %>%
  pull(city)

subset_data <- txhousing %>%
  filter(city %in% top_counties)

ggplot(subset_data, aes(x = city, y = median)) +
  geom_boxplot() +
  geom_smooth(aes(group = city), method = "loess", se = FALSE, color = "blue") +
  labs(title = "Box Plot of Median Home Prices for Top Counties",
       x = "County",
       y = "Median Home Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))