install.packages("stringr")  # Run this line if you haven't installed stringr
library(stringr)
library(rpart)
library(rpart.plot)


# Check unique values in Loyalty
unique_loyalty <- unique(df$Loyalty)
print(unique_loyalty)

# Check unique values in House
unique_house <- unique(df$House)
print(unique_house)


# Check for any loyalty values that include the keyword "Phoenix"
matches <- df[grepl("Phoenix", df$Loyalty, ignore.case = TRUE), ]
print(matches)

# Filter for characters with "Phoenix" in their loyalty
matches_phoenix <- df[grepl("Phoenix", df$Loyalty, ignore.case = TRUE), ]
matches_dumbledore <- df[grepl("DumbleDore", df$Loyalty, ignore.case = TRUE), ]
matches_sirius <- df[grepl("Sirius Black", df$Loyalty, ignore.case = TRUE), ]
matches_gryffindor <- df[df$House == "Gryffindor", ]

allies <- rbind(matches_phoenix, matches_dumbledore, matches_sirius, matches_gryffindor)
allies <- unique(allies)
print(allies)


matches_voldemort <- df[grepl("Voldemort", df$Loyalty, ignore.case = TRUE), ]
matches_deathEaters <- df[grepl("Death Eaters", df$Loyalty, ignore.case = TRUE), ]
matches_slytherin <- df[df$House == "Slytherin", ]

enemies <- rbind(matches_voldemort, matches_deathEaters, matches_slytherin)
enemies <- unique(enemies)
print(enemies)

print("Allies of Harry Potter,")
print(allies$Name)
print("Enemies of Harry Potter,")
print(enemies$Name)



# Count allies and enemies
allies_count <- nrow(allies)
enemies_count <- nrow(enemies)

# Create a summary data frame
summary_df <- data.frame(
  Group = c("Allies", "Enemies"),
  Count = c(allies_count, enemies_count)
)

summary_df

# Create a bar plot for Allies vs Enemies
ggplot(summary_df, aes(x = Group, y = Count, fill = Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Allies vs. Enemies of Harry Potter",
       y = "Count") +
  theme_minimal()

# Save the plot to a PNG file
png("allies_vs_enemies.png")
ggplot(summary_df, aes(x = Group, y = Count, fill = Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Allies vs. Enemies of Harry Potter",
       y = "Count",
       x = "") +
  theme_minimal()
dev.off()



# Boxplot for Gender distribution among allies and enemies
ggplot() +
  geom_bar(data = allies, aes(x = Gender), fill = "blue", alpha = 0.5) +
  geom_bar(data = enemies, aes(x = Gender), fill = "red", alpha = 0.5) +
  labs(title = "Gender Distribution among Allies and Enemies",
       x = "Gender",
       y = "Count") +
  theme_minimal()


allies$Type <- "Ally"
enemies$Type <- "Enemy"
combined_data <- rbind(allies, enemies)

ggplot(combined_data, aes(x = Gender, fill = Type)) +
  geom_bar(alpha = 1) +  # Use alpha for transparency
  labs(title = "Gender Distribution among Allies and Enemies",
       x = "Gender",
       y = "Count") +
  scale_fill_manual(values = c("Ally" = "skyblue", "Enemy" = "gray")) +  # Custom colors for legend
  theme_minimal() 

# Stacked bar plot for House distribution among Allies and Enemies
ggplot(combined_data, aes(x = House, fill = Type)) +
  geom_bar(position = "stack") +  # Stacked bars
  labs(title = "House Distribution among Allies and Enemies",
       x = "House",
       y = "Count") +
  scale_fill_manual(values = c("Ally" = "skyblue", "Enemy" = "gray")) +  # Custom colors for legend
  theme_minimal() +
  theme(legend.title = element_blank())



summary_table <- df %>%
  group_by(House, Loyalty) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(summary_table)
