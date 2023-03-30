
library(tidyverse)
library(ggplot2)
library(broom)
library(lubridate)

#Reading in the file

setwd("/Users/eugenechua/Downloads/skillsfuture_interview")
list.files()

survey_data<-read.csv("kaggle_survey_2017_2021.csv",skip=1)
str(survey_data)


#Looking at age categories breakdown
survey_data %>%
  group_by(What.is.your.age....years..) %>%
  summarize(Counts=n()) %>%
  mutate(proportions=Counts/sum(Counts)) %>%
  arrange(desc(Counts))

#Cleaning up a little of the education level data
survey_data<-survey_data %>%
  mutate(What.is.the.highest.level.of.formal.education.that.you.have.attained.or.plan.to.attain.within.the.next.2.years.=recode(What.is.the.highest.level.of.formal.education.that.you.have.attained.or.plan.to.attain.within.the.next.2.years.,
                                                                                                                                "Masterâ€™s degree" = "Master's degree", "Bachelorâ€™s degree" = "Bachelor's degree"))


#Looking at education level breakdown
survey_data %>%
  group_by(What.is.the.highest.level.of.formal.education.that.you.have.attained.or.plan.to.attain.within.the.next.2.years.) %>%
  summarize(Counts=n()) %>%
  mutate(proportions=Counts/sum(Counts)) %>%
  arrange(desc(Counts))

#Cleaning up a little for the gender level data
survey_data<-survey_data %>%
  mutate(What.is.your.gender....Selected.Choice=recode(What.is.your.gender....Selected.Choice,
                                                       "Man" = "Male", "Woman" = "Female"))

#Looking at gender level breakdown
survey_data %>%
  group_by(What.is.your.gender....Selected.Choice) %>%
  summarize(Counts=n()) %>%
  mutate(proportions=Counts/sum(Counts)) %>%
  arrange(desc(Counts))

weird_stuffs<-survey_data %>%
  filter(For.how.many.years.have.you.used.machine.learning.methods. == "10-20 years") %>%
  select(What.is.your.age....years.., What.is.your.gender....Selected.Choice, What.is.the.highest.level.of.formal.education.that.you.have.attained.or.plan.to.attain.within.the.next.2.years., 68:78) %>%
  as.data.frame()

colnames(weird_stuffs)[1:3]<-c("Age","Gender","Highest Education Level")

head(weird_stuffs)


#subset programming language regular basis dataset
survey_data_progreg<-survey_data[,c(1:21)]

colnames(survey_data_progreg)[9:21]<- c("regular_programming_language_python","regular_programming_language_R","regular_programming_language_SQL","regular_programming_language_C",
                                        "regular_programming_language_C++","regular_programming_language_Java","regular_programming_language_Javascript",
                                        "regular_programming_language_Julia","regular_programming_language_Swift","regular_programming_loanguage_Bash","regular_programming_language_MATLAB",
                                        "regular_programming_language_None","regular_programming_language_other")


survey_data_progreg<-survey_data_progreg %>%
  pivot_longer(9:21, names_to="programming_lang_reg", values_to="language")

unique(survey_data_progreg$language)

#looking at prog lang trend across years
lang_year<-survey_data_progreg %>%
  group_by(Year, language) %>%
  summarize(language_count=n()) %>%
  filter(language!="") %>%
  as.data.frame()

#adding in extra column for survey counts across years so we can normalize abit
lang_year<-lang_year %>%
  mutate(survey_counts=case_when(Year==2017 ~ 16716,
                                 Year==2018 ~ 23859,
                                 Year==2019 ~ 19717,
                                 Year==2020 ~ 20036,
                                 Year==2021 ~ 25973))

lang_year<-lang_year %>%
  mutate(language_count_nor = round(language_count/survey_counts,2))

#Visualizing the plots across programming languages
lang_year %>%
  ggplot(aes(Year, language_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~language,nrow=3,scales="free_y")


##Analyzing across gender --> REDOING SOME OF THE RECODING FIRST.
#For males only
lang_year_male<-survey_data_progreg %>%
  filter(What.is.your.gender....Selected.Choice=='Male') %>%
  group_by(Year, language) %>%
  summarize(language_count=n()) %>%
  filter(language!="") %>%
  as.data.frame()


lang_year_male<-lang_year_male %>%
  mutate(survey_counts=case_when(Year==2017 ~ 13610,
                                 Year==2018 ~ 19430,
                                 Year==2019 ~ 16138,
                                 Year==2020 ~ 15789,
                                 Year==2021 ~ 20598))  

lang_year_male<-lang_year_male %>%
  mutate(language_count_nor = round(language_count/survey_counts,2))

#Visualizing the plots across programming languages
lang_year_male %>%
  ggplot(aes(Year, language_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~language,nrow=3,scales="free_y")


#For Females only
lang_year_female<-survey_data_progreg %>%
  filter(What.is.your.gender....Selected.Choice=='Female') %>%
  group_by(Year, language) %>%
  summarize(language_count=n()) %>%
  filter(language!="") %>%
  as.data.frame()


lang_year_female<-lang_year_female %>%
  mutate(survey_counts=case_when(Year==2017 ~ 2778,
                                 Year==2018 ~ 4010,
                                 Year==2019 ~ 3212,
                                 Year==2020 ~ 3878,
                                 Year==2021 ~ 4890))  

lang_year_female<-lang_year_female %>%
  mutate(language_count_nor = round(language_count/survey_counts,2))




#Visualizing the plots across programming languages
lang_year_female %>%
  ggplot(aes(Year, language_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~language,nrow=3,scales="free_y")

#overall:2017 - 16716, 2018 - 23859, 2019 - 19717, 2020 - 20036, 2021 - 25973
#males:2017 - 13610, 2018 - 19430, 2019 - 16138, 2020 - 15789, 2021 - 20598
#female: 2017 - 2778, 2018 - 4010, 2019 - 3212, 2020 - 3878, 2021 - 4890

survey_data %>%
  filter(Select.the.title.most.similar.to.your.current.role..or.most.recent.title.if.retired.....Selected.Choice == "Data Scientist") %>%
  group_by(Year, Select.the.title.most.similar.to.your.current.role..or.most.recent.title.if.retired.....Selected.Choice) %>%
  summarize(total_count=n()) %>%
  as.data.frame()

colnames(survey_data)[22]<-"recommended_first_language_forDS"

#What programming language would you encourage aspiring data scientist learn first?
first_lang<-survey_data %>%
  group_by(Year, recommended_first_language_forDS) %>%
  summarize(language_count=n()) %>%
  filter(recommended_first_language_forDS != "") %>%
  as.data.frame()

first_lang<-first_lang %>%
  mutate(survey_counts=case_when(Year==2017 ~ 16716,
                                 Year==2018 ~ 23859,
                                 Year==2019 ~ 19717,
                                 Year==2020 ~ 20036,
                                 Year==2021 ~ 25973))

first_lang<-first_lang %>%
  mutate(language_count_nor = round(language_count/survey_counts,2))


first_lang %>%
  filter(recommended_first_language_forDS %in% c("Java","Julia","SAS","Python","R","C","C/C++/C#","Javascript","MATLAB")) %>%
  ggplot(aes(Year, language_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~recommended_first_language_forDS,nrow=3,scales="free_y")

#Now going into gender analysis for Man|Male
first_lang_male<-survey_data %>%
  filter(What.is.your.gender....Selected.Choice == "Male") %>%
  group_by(Year, recommended_first_language_forDS) %>%
  summarize(language_count=n()) %>%
  filter(recommended_first_language_forDS != "") %>%
  as.data.frame()


first_lang_male<-first_lang_male %>%
  mutate(survey_counts=case_when(Year==2017 ~ 13610,
                                 Year==2018 ~ 19430,
                                 Year==2019 ~ 16138,
                                 Year==2020 ~ 15789,
                                 Year==2021 ~ 20598))  


first_lang_male<-first_lang_male %>%
  mutate(language_count_nor = round(language_count/survey_counts,2))


first_lang_male %>%
  filter(recommended_first_language_forDS %in% c("Java","Julia","SAS","Python","R","C","C/C++/C#","Javascript","MATLAB")) %>%
  ggplot(aes(Year, language_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~recommended_first_language_forDS,nrow=3,scales="free_y")


#Now going into gender analysis for Female|Woman
first_lang_female<-survey_data %>%
  filter(What.is.your.gender....Selected.Choice == "Female") %>%
  group_by(Year, recommended_first_language_forDS) %>%
  summarize(language_count=n()) %>%
  filter(recommended_first_language_forDS != "") %>%
  as.data.frame()


first_lang_female<-first_lang_female %>%
  mutate(survey_counts=case_when(Year==2017 ~ 2778,
                                 Year==2018 ~ 4010,
                                 Year==2019 ~ 3212,
                                 Year==2020 ~ 3878,
                                 Year==2021 ~ 4890))  

first_lang_female<-first_lang_female %>%
  mutate(language_count_nor = round(language_count/survey_counts,2))


first_lang_female %>%
  filter(recommended_first_language_forDS %in% c("Java","Julia","SAS","Python","R","C","C/C++/C#","Javascript","MATLAB")) %>%
  ggplot(aes(Year, language_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~recommended_first_language_forDS,nrow=3,scales="free_y")

### IDE Analysis
#subset IDE environment data
survey_data_IDEenv<-survey_data[,c(1:8,23:33)]

colnames(survey_data_IDEenv)[9:19]<-c("regular_IDE_Jupyter", "regular_IDE_Rstudio","regular_IDE_VScode","regular_IDE_Pycharm","regular_IDE_Spyder",
                                      "regular_IDE_notepad++", "regular_IDE_sublimetext","regular_IDE_Vim","regular_IDE_MATLAB",
                                      "regular_IDE_None","regular_IDE_Other")

survey_data_IDEenv<-survey_data_IDEenv %>%
  pivot_longer(9:19, names_to="programming_IDE", values_to="IDEs")

#Sadly I have to do some form of recoding first because the data is really quite dirty
survey_data_IDEenv<-survey_data_IDEenv %>%
  mutate(IDEs=recode(IDEs,"Jupyter (JupyterLab, Jupyter Notebooks, etc) " = "Jupyter/IPython", "Visual Studio" = "Visual Studio/ Visual Studio Code", 
                     " Visual Studio / Visual Studio Code " = "Visual Studio/ Visual Studio Code", "  Spyder  " = "Spyder", "  Sublime Text  " = "Sublime Text",
                     "  Vim / Emacs  "  = "Vim", " MATLAB "  = "MATLAB", " PyCharm " = "PyCharm" , " RStudio "  = "RStudio" , "  Notepad++  " = "Notepad++"))
  


#looking at prog lang trend across years
IDE_year<-survey_data_IDEenv %>%
  group_by(Year, IDEs) %>%
  summarize(IDEs_count=n()) %>%
  filter(IDEs!="") %>%
  as.data.frame()


#adding in extra column for survey counts across years so we can normalize abit
IDE_year<-IDE_year %>%
  mutate(survey_counts=case_when(Year==2017 ~ 16716,
                                 Year==2018 ~ 23859,
                                 Year==2019 ~ 19717,
                                 Year==2020 ~ 20036,
                                 Year==2021 ~ 25973))

IDE_year<-IDE_year %>%
  mutate(IDEs_count_nor = round(IDEs_count/survey_counts,2))

#Visualizing the plots across IDEs

IDE_year %>%
  filter(IDEs %in% c("Jupyter/IPython", "MATLAB","Notepad++","PyCharm","RStudio","Spyder","Sublime Text","Vim","Visual Studio/ Visual Studio Code")) %>%
  ggplot(aes(Year, IDEs_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~IDEs,nrow=3,scales="free_y")


###IDEs analysis for males
#looking at prog lang trend across years
IDE_year_male<-survey_data_IDEenv %>%
  filter(What.is.your.gender....Selected.Choice == "Male") %>%
  group_by(Year, IDEs) %>%
  summarize(IDEs_count=n()) %>%
  filter(IDEs!="") %>%
  as.data.frame()


IDE_year_male<-IDE_year_male %>%
  mutate(survey_counts=case_when(Year==2017 ~ 13610,
                                 Year==2018 ~ 19430,
                                 Year==2019 ~ 16138,
                                 Year==2020 ~ 15789,
                                 Year==2021 ~ 20598))  

#normalizing the male counts by dividing my total male respondents
IDE_year_male<-IDE_year_male %>%
  mutate(IDEs_count_nor = round(IDEs_count/survey_counts,2))


#Visualizing the plots
IDE_year_male %>%
  filter(IDEs %in% c("Jupyter/IPython", "MATLAB","Notepad++","PyCharm","RStudio","Spyder","Sublime Text","Vim","Visual Studio/ Visual Studio Code")) %>%
  ggplot(aes(Year, IDEs_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~IDEs,nrow=3,scales="free_y")


###IDEs analysis for females
#looking at prog lang trend across years
IDE_year_female<-survey_data_IDEenv %>%
  filter(What.is.your.gender....Selected.Choice == "Female") %>%
  group_by(Year, IDEs) %>%
  summarize(IDEs_count=n()) %>%
  filter(IDEs!="") %>%
  as.data.frame()


IDE_year_female<-IDE_year_female %>%
  mutate(survey_counts=case_when(Year==2017 ~ 2778,
                                 Year==2018 ~ 4010,
                                 Year==2019 ~ 3212,
                                 Year==2020 ~ 3878,
                                 Year==2021 ~ 4890))  

IDE_year_female<-IDE_year_female %>%
  mutate(IDEs_count_nor = round(IDEs_count/survey_counts,2))


#Visualizing the plots
IDE_year_female %>%
  filter(IDEs %in% c("Jupyter/IPython", "MATLAB","Notepad++","PyCharm","RStudio","Spyder","Sublime Text","Vim","Visual Studio/ Visual Studio Code")) %>%
  ggplot(aes(Year, IDEs_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~IDEs,nrow=3,scales="free_y")



#Analyzing ML framework across time
survey_data_ML_framework<-survey_data[,c(1:8,68:83)]

survey_data_ML_framework<-survey_data_ML_framework %>%
  pivot_longer(9:24, names_to="ML_Frameworks", values_to="package")

unique(survey_data_ML_framework$package)

survey_data_ML_framework<-survey_data_ML_framework %>%
  mutate(package=recode(package,"  Scikit-learn " = "Scikit-Learn", "  TensorFlow " = "TensorFlow", " Caret " = "Caret",
                        " Keras " = "Keras", " PyTorch " = "PyTorch", " LightGBM " = "lightgbm", " Fast.ai " = "Fastai", " Xgboost " = "Xgboost"))

#looking at prog lang trend across years
MLframework_year<-survey_data_ML_framework %>%
  group_by(Year, package) %>%
  summarize(package_count=n()) %>%
  filter(package!="") %>%
  as.data.frame()

#adding in extra column for survey counts across years so we can normalize abit
MLframework_year<-MLframework_year %>%
  mutate(survey_counts=case_when(Year==2017 ~ 16716,
                                 Year==2018 ~ 23859,
                                 Year==2019 ~ 19717,
                                 Year==2020 ~ 20036,
                                 Year==2021 ~ 25973))

MLframework_year<-MLframework_year %>%
  mutate(package_count_nor = round(package_count/survey_counts,2))


#Analyzing it graphically
MLframework_year %>%
  filter(package %in% c("Keras","TensorFlow","PyTorch","Fastai","Caret", "Xgboost", " Tidymodels ", " H2O 3 ")) %>%
  ggplot(aes(Year, package_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~package,nrow=3,scales="free_y")

#Filter for ML framework MALE
#looking at prog lang trend across years
MLframework_male_year<-survey_data_ML_framework %>%
  filter(What.is.your.gender....Selected.Choice == "Male") %>%
  group_by(Year, package) %>%
  summarize(package_count=n()) %>%
  filter(package!="") %>%
  as.data.frame()


MLframework_male_year<-MLframework_male_year %>%
  mutate(survey_counts=case_when(Year==2017 ~ 13610,
                                 Year==2018 ~ 19430,
                                 Year==2019 ~ 16138,
                                 Year==2020 ~ 15789,
                                 Year==2021 ~ 20598))  

#normalizing the male counts by dividing my total male respondents
MLframework_male_year<-MLframework_male_year %>%
  mutate(package_count_nor = round(package_count/survey_counts,2))


#Analyzing it graphically!
MLframework_male_year %>%
  filter(package %in% c("Keras","TensorFlow","PyTorch","Fastai","Caret", "Xgboost", " Tidymodels ", " H2O 3 ")) %>%
  ggplot(aes(Year, package_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~package,nrow=3,scales="free_y")


#Filter for ML framework Female!
MLframework_female_year<-survey_data_ML_framework %>%
  filter(What.is.your.gender....Selected.Choice == "Female") %>%
  group_by(Year, package) %>%
  summarize(package_count=n()) %>%
  filter(package!="") %>%
  as.data.frame()



MLframework_female_year<-MLframework_female_year %>%
  mutate(survey_counts=case_when(Year==2017 ~ 2778,
                                 Year==2018 ~ 4010,
                                 Year==2019 ~ 3212,
                                 Year==2020 ~ 3878,
                                 Year==2021 ~ 4890))  

#normalizing the male counts by dividing my total male respondents
MLframework_female_year<-MLframework_female_year %>%
  mutate(package_count_nor = round(package_count/survey_counts,2))


MLframework_female_year %>%
  filter(package %in% c("Keras","TensorFlow","PyTorch","Fastai","Caret", "Xgboost", " Tidymodels ", " H2O 3 ")) %>%
  ggplot(aes(Year, package_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~package,nrow=3,scales="free_y")

#ML Algo do you use across time?
survey_data_ML_algo<-survey_data[,c(1:8,84:95)]

dim(survey_data_ML_algo)

survey_data_ML_algo<-survey_data_ML_algo %>%
  pivot_longer(9:20, names_to="ML_algos", values_to="ML_Algorithms")

unique(survey_data_ML_algo$ML_Algorithms)

survey_data_ML_algo<-survey_data_ML_algo %>%
  mutate(ML_Algorithms=recode(ML_Algorithms,"Transformer Networks (BERT, gpt-2, etc)" = "Transformer Networks (BERT, gpt-3, etc)"))


#looking at ML algo trend across years
MLalgo_year<-survey_data_ML_algo %>%
  group_by(Year, ML_Algorithms) %>%
  summarize(MLalgo_count=n()) %>%
  filter(ML_Algorithms!="") %>%
  as.data.frame()


#adding in extra column for survey counts across years so we can normalize abit
MLalgo_year<-MLalgo_year %>%
  mutate(survey_counts=case_when(Year==2017 ~ 16716,
                                 Year==2018 ~ 23859,
                                 Year==2019 ~ 19717,
                                 Year==2020 ~ 20036,
                                 Year==2021 ~ 25973))

MLalgo_year<-MLalgo_year %>%
  mutate(MLalgo_count_nor = round(MLalgo_count/survey_counts,2))


#Analyzing it graphically
MLalgo_year %>%
  filter(ML_Algorithms %in% c("Bayesian Approaches","Convolutional Neural Networks","Decision Trees or Random Forests","Dense Neural Networks (MLPs, etc)",
                              "Generative Adversarial Networks", "Gradient Boosting Machines (xgboost, lightgbm, etc)","Linear or Logistic Regression", "Recurrent Neural Networks", "Transformer Networks (BERT, gpt-3, etc)")) %>%
  ggplot(aes(Year, MLalgo_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~ML_Algorithms,nrow=3,scales="free_y")



### Analysis for ML algo across time (FOR MALES)
#looking at ML algo trend across years
MLalgo_male_year<-survey_data_ML_algo %>%
  filter(What.is.your.gender....Selected.Choice == "Male") %>%
  group_by(Year, ML_Algorithms) %>%
  summarize(MLalgo_count=n()) %>%
  filter(ML_Algorithms!="") %>%
  as.data.frame()


#adding in extra column for survey counts across years so we can normalize abit
MLalgo_male_year<-MLalgo_male_year %>%
  mutate(survey_counts=case_when(Year==2017 ~ 13610,
                                 Year==2018 ~ 19430,
                                 Year==2019 ~ 16138,
                                 Year==2020 ~ 15789,
                                 Year==2021 ~ 20598))

MLalgo_male_year<-MLalgo_male_year %>%
  mutate(MLalgo_count_nor = round(MLalgo_count/survey_counts,2))


#Analyzing it graphically
MLalgo_male_year %>%
  filter(ML_Algorithms %in% c("Bayesian Approaches","Convolutional Neural Networks","Decision Trees or Random Forests","Dense Neural Networks (MLPs, etc)",
                              "Generative Adversarial Networks", "Gradient Boosting Machines (xgboost, lightgbm, etc)","Linear or Logistic Regression", "Recurrent Neural Networks", "Transformer Networks (BERT, gpt-3, etc)")) %>%
  ggplot(aes(Year, MLalgo_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~ML_Algorithms,nrow=3,scales="free_y")


##For Female

MLalgo_female_year<-survey_data_ML_algo %>%
  filter(What.is.your.gender....Selected.Choice == "Female") %>%
  group_by(Year, ML_Algorithms) %>%
  summarize(MLalgo_count=n()) %>%
  filter(ML_Algorithms!="") %>%
  as.data.frame()


#adding in extra column for survey counts across years so we can normalize abit
MLalgo_female_year<-MLalgo_female_year %>%
  mutate(survey_counts=case_when(Year==2017 ~ 2778,
                                 Year==2018 ~ 4010,
                                 Year==2019 ~ 3212,
                                 Year==2020 ~ 3878,
                                 Year==2021 ~ 4890))

MLalgo_female_year<-MLalgo_female_year %>%
  mutate(MLalgo_count_nor = round(MLalgo_count/survey_counts,2))


#Analyzing it graphically
MLalgo_female_year %>%
  filter(ML_Algorithms %in% c("Bayesian Approaches","Convolutional Neural Networks","Decision Trees or Random Forests","Dense Neural Networks (MLPs, etc)",
                              "Generative Adversarial Networks", "Gradient Boosting Machines (xgboost, lightgbm, etc)","Linear or Logistic Regression", "Recurrent Neural Networks", "Transformer Networks (BERT, gpt-3, etc)")) %>%
  ggplot(aes(Year, MLalgo_count_nor)) + geom_line(color="blue") + geom_point(color="blue") +
  facet_wrap(~ML_Algorithms,nrow=3,scales="free_y")




