
# Loyalty distribution
loyalty_count <- df %>%
  group_by(Loyalty) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Convert Loyalty to a factor with specified levels
loyalty_count$Loyalty <- factor(loyalty_count$Loyalty, levels = loyalty_count$Loyalty)

# Bar plot for Loyalty distribution without legend
ggplot(loyalty_count, aes(x = Loyalty, y = Count, fill = Loyalty)) +
  geom_bar(stat = "identity") +
  labs(title = "Loyalty Distribution of Characters", x = "Loyalty", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1), 
        legend.position = "none")  # Remove legend



# Loyalty vs. House
house_loyalty <- df %>%
  group_by(House, Loyalty) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Bar plot for Loyalty by House
ggplot(house_loyalty, aes(x = House, y = Count, fill = Loyalty)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Loyalty Distribution by House", x = "House", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Loyalty vs. Gender
gender_loyalty <- df %>%
  group_by(Gender, Loyalty) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Bar plot for Loyalty by Gender
ggplot(gender_loyalty, aes(x = Gender, y = Count, fill = Loyalty)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Loyalty Distribution by Gender", x = "Gender", y = "Count") +
  theme_minimal()

# Count of characters by Loyalty and Blood Status
blood_loyalty_count <- df %>%
  group_by(Blood.status, Loyalty) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(blood_loyalty_count)

ggplot(blood_loyalty_count, aes(x = Blood.status, y = Count, color = Loyalty)) +
  geom_point(size = 4) +
  labs(title = "Dot Plot of Loyalty Distribution by Blood Status",
       x = "Blood Status", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Chi-square test for independence

# Filter out rows with "unknown" in Gender or Loyalty
filtered_gender <- df %>%
  filter(Gender != "unknown" & Loyalty != "unknown")

# Chi-square test for Gender vs. Loyalty
chisq_test_gender <- chisq.test(table(filtered_gender$Gender, filtered_gender$Loyalty))
print(chisq_test_gender)

# Filter out rows with "unknown" in House or Loyalty
filtered_house <- df %>%
  filter(House != "unknown" & Loyalty != "unknown")

# Chi-square test for House vs. Loyalty
chisq_test_house <- chisq.test(table(filtered_house$House, filtered_house$Loyalty))
print(chisq_test_house)

# Filter out rows with "unknown" in Species or Loyalty
filtered_species <- df %>%
  filter(Species != "unknown" & Loyalty != "unknown")

# Chi-square test for Species vs. Loyalty
chisq_test_species <- chisq.test(table(filtered_species$Species, filtered_species$Loyalty))
print(chisq_test_species)

# Filter out rows with "unknown" in Blood.status or Loyalty
filtered_blood <- df %>%
  filter(Blood.status != "unknown" & Loyalty != "unknown")

# Chi-square test for Blood status vs. Loyalty
chisq_test_blood <- chisq.test(table(filtered_blood$Blood.status, filtered_blood$Loyalty))
print(chisq_test_blood)



names(df)
summary(df)


