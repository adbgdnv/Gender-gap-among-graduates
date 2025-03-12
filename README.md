## 📊 Econometric Research | Analysis of Factors Affecting Graduate Salaries  

### 📌 Project Overview  
This project presents an econometric analysis of the factors influencing the salaries of university graduates in Russia. The study is based on data from graduates of various specialties and examines the impact of gender, work experience, education level, and other characteristics.  

### 🔍 Research Objectives  
- **Identify key factors affecting graduate salaries**  
- **Evaluate the impact of gender, work experience, and education level**  
- **Analyze salary differences across federal districts and fields of study**  

### 📑 Data  
The dataset includes official graduate data, which has been cleaned and processed using R.  

### 🛠 Technologies Used  
- **R** – data analysis and visualization  
- **Dplyr, GGplot2** – data preprocessing and chart creation  
- **LM, Stargazer** – regression modeling and interpretation  
- **Sandwich, lmtest** – heteroskedasticity-consistent standard error adjustments  

### 📄 Methods & Models  
The study includes a series of linear regression models evaluating various factors:  
1️⃣ Gender (gender)  
2️⃣ Gender + work experience (yearofexp)  
3️⃣ Gender + work experience + education level (education_level)  
4️⃣ Gender + work experience + education level + field of study (study_area)  
5️⃣ Gender + work experience + education level + field of study + region (object_name)  

Robust standard errors (heteroskedasticity-consistent) are applied for accuracy.  

### 📊 Visualizations  
The project includes:  
✔ Boxplots of salaries across federal districts and fields of study  
✔ Salary comparisons by gender  
✔ Scatter plots of salaries vs. work experience  
✔ Residual plots for model evaluation  

### 🔬 Key Findings  
- **Gender impacts salaries** – significant differences were observed.  
- **Work experience positively correlates with salary** – more years after graduation lead to higher earnings.  
- **Regional and industry differences matter** – salaries vary based on location and field of study.  

### 🔄 Reproducibility  
The project code is written in R and is fully reproducible. To rerun the analysis, simply upload updated data.  
