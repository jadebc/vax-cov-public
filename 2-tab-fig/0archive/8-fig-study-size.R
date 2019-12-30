########################################
# Other SLIV studies and their size
########################################
library(ggplot2)
library(dplyr)
s = read.csv("~/Dropbox/Flu/StFData/Vax cov/SLIV studies - number of intervention schools - Sheet1.csv")

s = s %>% mutate(Num_SLIV_students = as.numeric(as.character(Num_SLIV_students)))

# dropping pebody
s = s %>% filter(Study != "Pebody")

# order s by # students
s$Study <- factor(s$Study, levels = s$Study[rev(order(s$Num_SLIV_students))])

# create variable for whether the study was controlled, 
# matched, used randomization, or did
s = s %>% mutate(quality= ifelse(matched == "yes" , "Matched",
                                ifelse(randomized == "yes",  "Randomized", "Other"))) %>%
  mutate(quality = factor(quality, levels=c("Randomized", "Matched", "Other")))

s = s %>% mutate(Num_SLIV_students.f = format(Num_SLIV_students,big.mark=",",scientific=FALSE))

darkgray = "#707070"
lightgray = "#D9D9D9"

pdf(file= "~/Dropbox/Flu/StFData/Vax cov/Figures/study-size.pdf",
    width=8, height=4)
ggplot(s, aes(x = Study, y=Num_SLIV_students, fill = quality)) +
  geom_bar(colour="black", stat="identity") +
  coord_flip() +
  scale_fill_manual("", values=c("black", darkgray, lightgray))+
  theme_bw() + 
  ylab("Number of students at schools with influenza vaccination")+
  annotate("text",x = s$Study, y=s$Num_SLIV_students, 
           label =s$Num_SLIV_students.f, hjust=-0.15)+
  scale_y_continuous(limits=c(0,40000))
dev.off()



