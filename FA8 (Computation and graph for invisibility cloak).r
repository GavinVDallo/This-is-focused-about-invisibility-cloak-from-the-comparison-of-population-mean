# ==========================================================
# ANALYSIS: Invisibility Cloak & Mischievous Acts
# Goal: Independent-Samples t-test using only Base R
# ==========================================================

# 1. DATA SETUP
# Creating the dataset exactly as shown in the provided table
mischief_scores <- c(3, 1, 5, 4, 6, 4, 6, 2, 0, 5, 4, 5,  # No Cloak (0)
                     4, 3, 6, 6, 8, 5, 5, 4, 2, 5, 7, 5)  # Cloak (1)

# Categorizing the groups: 0 = No Cloak, 1 = Cloak
cloak_condition <- factor(c(rep(0, 12), rep(1, 12)), 
                          labels = c("No Cloak", "Cloak"))

# Combine into a data frame
cloak_data <- data.frame(condition = cloak_condition, mischief = mischief_scores)

# ----------------------------------------------------------
# 2. CHECKING THE 5 ASSUMPTIONS
# ----------------------------------------------------------

# Assumption 1: Dichotomous Independent Variable (Exactly 2 levels)
levels(cloak_data$condition) 

# Assumption 2: Continuous Dependent Variable (Is it numeric?)
is.numeric(cloak_data$mischief)

# Assumption 3: Independence of Observations
# (Verified by study design: 24 unique participants in two distinct groups)

# Assumption 4: Normality (Shapiro-Wilk Test)
# Check for each group. p > 0.05 suggests normality is met.
shapiro.test(cloak_data$mischief[cloak_data$condition == "No Cloak"])
shapiro.test(cloak_data$mischief[cloak_data$condition == "Cloak"])

# Assumption 5: Homogeneity of Variance (F-test for Equality of Variances)
# In Base R, use var.test(). p > 0.05 means variances are equal.
var.test(mischief ~ condition, data = cloak_data)

# ----------------------------------------------------------
# 3. THE COMPUTATION (Independent-Samples t-test)
# ----------------------------------------------------------
# To match the document's results (t = -1.713, p = 0.101), 
# we must assume equal variances (var.equal = TRUE).
final_results <- t.test(mischief ~ condition, 
                        data = cloak_data, 
                        var.equal = TRUE)

print(final_results)

# ----------------------------------------------------------
# 4. VISUALIZATION (Optional)
# ----------------------------------------------------------
boxplot(mischief ~ condition, data = cloak_data,
        main = "Mischief Levels by Cloak Condition",
        ylab = "Number of Mischievous Acts",
        col = c("lightblue", "orange"))
