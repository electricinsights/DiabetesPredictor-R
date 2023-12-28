####Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)
library(treemapify)
library(naivebayes)
library(psych)
library(e1071)
library(rmarkdown)
library(corrplot)
library(plotly)


####Data Validation
map(diabetes,
    ~sum(is.na(.)))

any(duplicated(diabetes))
sum(duplicated(diabetes))

dia <- as.data.frame(unique(diabetes))
dim(dia)
dim(diabetes)
head(dia)
view(diabetes)
names(dia) <- tolower(names(dia))
colnames(dia)
head(dia)

###Now Finding Correlation
correlation_matrix <- cor(dia)
corrplot(correlation_matrix, main = "Correlation Matrix")


###Calculating diabetes and non diabetes in binary_diabetes
diabetes_counts <- table(dia$diabetes_binary)
count_0=diabetes_counts[["0"]]
print(count_0)
count_1=diabetes_counts[["1"]]
print(count_1)
# Sot there are total 35097 people have diabetes and 194377 with non-diabetes
diabetes_column <-dia$diabetes_binary
diabetes_table <- table(diabetes_column)
print(diabetes_table)
library(tidyverse)
view(diabetes_table)
#We make pie chart to show there percentage
pie(diabetes_table, 
    labels = c("No Diabetes", "Diabetes"),
    main = "Diabetes Status",
    #radius = 1.5,
    cex=1.5,
    col = c("blue", "orange"))

##Creating barplot to show distribution
options(repr.plot.width=8, repr.plot.height=4)  # Adjust the width and height as needed
barplot(diabetes_table,
        names.arg = c("No Diabetes", "Diabetes"),
        main = "Diabetes Status",
        xlab = "Diabetes Status",
        ylab = "Count",
        col = c("blue", "orange"))


###Diabetes Relation with Major Factors
#1. HighBP 2.HighChol 3.BMI 4.Stroke 5.HeartDisease 6. genhlth 7.
#High BP Distribution
bp_column <-dia$highbp
bp_table <- table(bp_column)
print(bp_table)
#pie_chart
pie(bp_table,
    labels = c("Non High BP", "High BP"),
    main = "BP Status",
    cex=1.5,
    col = c("blue", "orange")
    )
#Diabetes vs BP bar chart
library(ggplot2)
ggplot(dia, aes(x = factor(diabetes_binary), fill = factor(highbp))) +
  geom_bar() +
  labs(title = "High BP vs Diabetes",
       x = "Diabetes Status",
       y = "Count") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightcoral"))+
  theme_classic()

#For High Cholesterol
chl_column <- dia$highchol
chl_table <- table(chl_column)
print(chl_table)

#Creating PIE Chart
pie(chl_table,
    labels = c("Non High Cholesterol", "High Cholesterol"),    main = "Cholesterol Status",
    cex = 1.5,
    col = c("blue", "orange"))

##Checking Combined effect of cholesterol and high bp
ggplot(dia, aes(x = factor(diabetes_binary), fill = interaction(highchol, highbp))) +
  geom_bar(position = "stack", stat = "count") +
  labs(title = "Relationship between Diabetes, High Cholesterol, and High BP",
       x = "Diabetes Status",
       y = "Count") +
  scale_fill_manual(values = c("0.0" = "lightblue", "0.1" = "lightcoral",
                               "1.0" = "lightgreen", "1.1" = "lightyellow")) +
  theme_classic()


#BMI Distribution
bmi_relation <- dia %>% 
  count(bmi)


ggplot(bmi_relation, 
       aes(fill = bmi, area = n)) +
  geom_treemap() + 
  geom_treemap_text(aes(label=n),
                    place = "centre",
                    reflow = TRUE)+
  labs(title = "BMI Distribution")+
  scale_fill_brewer(palette = "Set2")

#BMI Relation with diabetes
ggplot(dia, aes(x = factor(diabetes_binary), y = bmi, fill = factor(diabetes_binary))) +
  geom_boxplot() +
  labs(title = "Effect of BMI on Diabetes",
       x = "Diabetes Status",
       y = "BMI") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightcoral")) 


##Smokers Distibution
smoker_column <-dia$smoker
smoker_table <- table(smoker_column)
print(smoker_table)
pie(smoker_table,
    labels = c("Non Smokers", "Smokers"),
    main = "Smoker Status",
    cex=1.5,
    col = c("blue", "orange")
)
# Smoker vs Diabetes
ggplot(dia, aes(x = factor(diabetes_binary), fill = factor(smoker))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Relationship between Diabetes and Smoking",
       x = "Diabetes Status",
       y = "Count") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightcoral")) +
  theme_minimal()
##Combined Effect of alcoholconsumptiona and smokers
alc_n_smoker_summary <- dia %>%
  mutate(diabetes_status = ifelse(diabetes_binary == 1, "Diabetic", "Non-Diabetic")) %>%
  group_by(diabetes_binary, hvyalcoholconsump, smoker) %>%
  summarize(count = n())
print(alc_n_smoker_summary)
#Stacked bar chart is used to show visually
ggplot(your_data_summary, aes(x = factor(diabetes_binary), fill = interaction(hvyalcoholconsump, smoker))) +
  geom_bar(position = "stack", aes(y = count), stat = "identity") +
  labs(title = "Relationship between Diabetes, Heavy Alcohol Consumption, and Smoking",
       x = "Diabetes Status",
       y = "Count") +
  scale_fill_manual(values = c("0.0" = "lightblue", "0.1" = "lightcoral",
                               "1.0" = "lightgreen", "1.1" = "lightyellow")) +
  theme_minimal()

##Stroke and Heart Disease Attacks Relation 
stroke_n_attack_summary <- dia %>%
  mutate(diabetes_status = ifelse(diabetes_binary == 1, "Diabetic", "Non-Diabetic")) %>%
  group_by(diabetes_binary, stroke, heartdiseaseorattack) %>%
  summarize(count = n())
print(stroke_n_attack_summary)

##We use stacked bar chart to show relations

ggplot(stroke_n_attack_summary, aes(x = factor(diabetes_binary), fill = interaction(stroke, heartdiseaseorattack))) +
  geom_bar(position = "stack", aes(y = count), stat = "identity") +
  labs(title = "Relationship between Diabetes, Stroke, and Heart Disease/Attack",
       x = "Diabetes Status",
       y = "Count") +
  scale_fill_manual(values = c("0.0" = "lightblue", "0.1" = "lightcoral",
                               "1.0" = "lightgreen", "1.1" = "lightyellow")) +
  theme_minimal()


####Health measures and their relation with diabetes
##Here we show genHealth(General Health).
#We make tree map to show relation
genhlth_relation <- dia %>% 
  count(genhlth)

ggplot(genhlth_relation, 
       aes(fill = genhlth, area = n)) +
  geom_treemap() + 
  geom_treemap_text(aes(label=n),
                    place = "centre",
                    reflow = TRUE)+
  labs(title ="General Health Distribuion")

##General Health vs Diabetes

ggplot(dia, aes(x = factor(diabetes_binary), y = genhlth, fill = factor(diabetes_binary))) +
  geom_boxplot() +
  labs(title = "General Health vs Diabetes",
       x = "Diabetes Status",
       y = "GeneralHealth") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightcoral")) 

###Diaetes vs Physical Activity

phyactivity_summary <- dia %>%
  group_by(diabetes_binary, physactivity) %>% 
  summarize(count = n())
View(phyactivity_summary)

#Graphically
library(ggplot2)
ggplot(phyactivity_summary, aes(x = factor(diabetes_binary), fill = factor(physactivity))) +
  geom_bar(position = "dodge", aes(y = count), stat = "identity") +
  labs(title = "Relationship between Diabetes and Physical Activity",
       x = "Diabetes Status",
       y = "Count") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightcoral")) +
  theme_classic()


library(dplyr)

###Sex , Age , Education , and Income's effect on Diabetes 
##For Sex: 0 as Female and 1 as male
sex_summary <- dia %>%
  group_by(diabetes_binary, sex) %>% 
  summarize(count = n())
print(sex_summary)
#We use bar chart to show relation between diabetes and sex
ggplot(sex_summary, aes(x = factor(diabetes_binary), fill = factor(sex))) +
  geom_bar(position = "dodge", aes(y = count), stat = "identity") +
  labs(title = "Relationship between Diabetes and Sex",
       x = "Diabetes Status",
       y = "Count") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightcoral")) +
  theme_minimal()


#For age
age_relation <- dia %>% 
  count(age)
print(age_relation)
library(treemapify)
library(plotly)
library(treemapify)

ggplot(age_relation, aes(fill = as.factor(age), area = n)) +
  geom_treemap() +
  geom_treemap_text(aes(label = n),
                    place = "centre",
                    reflow = TRUE) +
  labs(title = "Age Distribution")

##Diabetes vs Age
ggplot(data = dia,
       aes(x = factor(diabetes_binary), y = age, fill = factor(diabetes_binary))) +
  geom_boxplot() +
  labs(title = "Diabetes vs Age",
       x = "Diabetes Status",
       y = "Age") +
  scale_fill_brewer(palette = "Set3")+
  theme_classic()
  
### Diabetes vs No Doctor bcs of cost
doctor_summary <- dia %>%
  group_by(diabetes_binary, nodocbccost) %>% 
  summarize(count = n())
print(doctor_summary)

ggplot(doctor_summary, aes(x = factor(diabetes_binary), fill = factor(nodocbccost))) +
  geom_bar(position = "dodge", aes(y = count), stat = "identity") +
  labs(title = "Relationship between Diabetes and Consultantion with Doctor",
       x = "Diabetes Status",
       y = "Count") +
  scale_fill_manual(values = c("0" = "light", "1" = "lightcoral")) +
  theme_classic()



library(ggplot2)
library(heatmaply)
# Assuming you are using the dplyr package
library(dplyr)

# Remove specified columns from dia_2
head(dia_2)




dia_2 <- select(dia,-veggies,-education,-income)

# Assuming 'dia' is your diabetes dataset with columns named appropriately

# Split the dataset into training and testing sets (you can adjust the ratio)
set.seed(123)
split_index <- sample(1:nrow(dia_2), 0.8 * nrow(dia_2))
train_data <- dia_2[split_index, ]
test_data <- dia_2[-split_index, ]

# Build Naive Bayes model
nb_model <- naiveBayes(diabetes_binary ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(nb_model, test_data)

# Confusion matrix to evaluate the performance
confusion_matrix <- table(predictions, test_data$diabetes_binary)
print(confusion_matrix)


# Convert confusion matrix to a data frame
confusion_matrix_df <- as.data.frame.matrix(confusion_matrix)
print(confusion_matrix_df)
##Visualization
rows <- rownames(confusion_matrix_df)
cols <- colnames(confusion_matrix_df)
print(rows)

library(ggplot2)

# Create a data frame for the confusion matrix
library(ggplot2)

# Assuming confusion_matrix_plot_df is the data frame obtained from the confusion matrix
library(ggplot2)

# Assuming confusion_matrix_plot_df is the data frame obtained from the confusion matrix
confusion_matrix_plot_df <- data.frame(
  Actual = c(0, 1, 0, 1),
  Predicted = c(0, 0, 1, 1),
  Frequency = c(3921, 30736, 3149, 8089)
)

# Create a ggplot for the confusion matrix with text labels
ggplot(data = confusion_matrix_plot_df, 
       aes(x = Predicted, y = Actual, fill = as.factor(Frequency), label = Frequency)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frequency), vjust = 1) +  # Add text labels
  scale_fill_manual(values = c("3921" = "lightgrey", "30736" = "lightblue", "3149" = "lightblue", "8089" = "lightgrey"),
                    breaks = c("3921", "30736", "3149", "8089"),
                    labels = c("3921", "30736", "3149", "8089"),
                    guide = guide_legend(title = "Frequency")) +
  theme_minimal() +
  labs(title = "Confusion Matrix",
       x = "Predicted",
       y = "Actual",
       fill = "Frequency") +
  theme_classic()




# Visualize ROC curve
install.packages("pROC")
library(pROC)
roc_curve <- roc(test_data$diabetes_binary, as.numeric(predictions == 1))
plot(roc_curve, col = "blue", main = "ROC Curve", lwd = 2, xlab = "1 - Specificity")


str(diabetes)
