#create linear model of model 1
model_1 <- lm(profit_per_rope ~ez+region+ education_max + read_max + write_max + calc_max +   
                district+loc2 + loc5 + road + bank + daily_market,data=hh_agri_edu_profit)
#view summary of linear model
summary(model_1)
#create scatter plot of education_max
ggplot(data=model_1,
       mapping = aes(x =education_max,y=profit_per_rope,color='education_max'))+
  geom_point(alpha=.1)+
  xlab("Level of Education")+
  ylab("profit_per_rope")+
  labs(title=" Household Profit Per Rope By Level of Household Maximum Education")

#create scatter plot calc_max
ggplot(data=model_1,
       mapping = aes(x =calc_max,y=profit_per_rope,color='calc_max'))+
  geom_point(alpha=.1)+
  xlab("Skill Level")+
  ylab("profit_per_rope")+
  labs(title=" Household Profit Per Rope  By Household Maximum Skill level In Mathematical Calculations")  

#create scatter plot read_max
ggplot(data=model_1,
       mapping = aes(x =read_max,y=profit_per_rope,color='read_max'))+
  geom_point(alpha=.1)+
  xlab("Skill Level")+
  ylab("profit_per_rope")+
  labs(title=" Household Profit Per Rope By Household Maximum Skill level In Reading")  

#create scatter plot write_max
ggplot(data=model_1,
       mapping = aes(x =write_max,y=profit_per_rope,color='write_max'))+
  geom_point(alpha=.1)+
  xlab("Skill Level")+
  ylab("profit_per_rope")+
  labs(title=" Household Profit Per Rope By Household Maximum Skill level In Writting")  

#create scatter plot household profit per cluster
ggplot(data=model_1,
       mapping = aes(x =clust,y=profit_per_rope,color='clust'))+
  geom_point(alpha=.1)+
  xlab(" cluster")+
  ylab("profit_per_rope")+
  labs(title=" Cluster Household Profit Per Rope")  


#create scatter plot education max, calc max, read max, write max


ggplot(data=model_1,
       mapping = aes(x =education_max,y=profit_per_rope,color='education_max'))+
  geom_point(alpha=0.1)+
  geom_point( mapping = aes(x =calc_max,y=profit_per_rope,color='calc_max'))+
  geom_point(alpha=0.1)+
  geom_point( mapping = aes(x =write_max,y=profit_per_rope,color='write_max'))+
  geom_point(alpha=0.1)+
  geom_point( mapping = aes(x =read_max,y=profit_per_rope,color='read_max'))+
  geom_point(alpha=0.1)+
  xlab("Level of Education")+
  ylab("profit_per_rope")+
  labs(title=" Household Profit Per Rope by Level and Type of Education")

#scatter plot based on community info
ggplot(data=model_1,
       mapping = aes(x =ez,y=profit_per_rope,color='ez'))+
  geom_point(alpha=.1)+
  geom_point( mapping = aes(x =loc2,y=profit_per_rope,color='loc2'))+
  geom_point(alpha=.1)+
  geom_point( mapping = aes(x =loc5,y=profit_per_rope,color='loc5'))+
  geom_point(alpha=.1)+
  geom_point( mapping = aes(x =road,y=profit_per_rope,color='road'))+
  geom_point(alpha=.1)+
  geom_point( mapping = aes(x= bank,y=profit_per_rope,color='bank'))+
  geom_point(alpha=.1)+
  geom_point( mapping = aes(x =daily_market,y=profit_per_rope,color='daily_market'))+
  geom_point(alpha=.1)+
  xlab("Community Feature")+
  ylab("profit_per_rope")+
  labs(title=" Household Profit Per Rope by Community Charactoristics")


#create scatter plot education max, calc max, read max, write max
ggplot(data=model_1,
  geom_point(alpha=0.1)(mapping = aes(x =education_max,y=profit_per_rope,color='education_max'))+
  geom_point(alpha=0.1)(mapping = aes(x =calc_max,y=profit_per_rope,color='calc_max'))+
  geom_point(alpha=0.1)( mapping = aes(x =write_max,y=profit_per_rope,color='write_max'))+
  geom_point(alpha=0.1)( mapping = aes(x =read_max,y=profit_per_rope,color='read_max'))+
  xlab("Level of Education")+
  ylab("profit_per_rope")+
  labs(title=" Household Profit Per Rope by Level and Type of Education")

#create histogram of calc max and education max and read max and write max combined
ggplot(data = model_1)+ 
  geom_histogram( mapping = aes(x =education_max,fill='education_max'))+
  geom_histogram( mapping = aes(x =calc_max,fill='calc_max',))+
  geom_histogram( mapping = aes(x =read_max,fill='read_max',))+
  geom_histogram( mapping = aes(x =write_max,fill='write_max',))+
  labs(title="Household Maximum Education by Type ")+
  xlab("Level of Education")

#scatter histogram on community info
ggplot(data=model_1)+
  geom_histogram(mapping = aes(x =ez,fill='ez'))+
  geom_histogram(mapping = aes(x =loc2,fill='loc2'))+
  geom_histogram(mapping = aes(x =loc5,fill='loc5'))+
  geom_histogram(mapping = aes(x =road,fill='road'))+
  geom_histogram(mapping = aes(x= bank,fill='bank'))+
  geom_histogram(mapping = aes(x =daily_market,fill='daily_market'))+
  xlab("Community Feature")+
  labs(title=" Household Profit Per Rope by Community Charactoristics")



#create density graph calc max and education max and read max and write max combined
ggplot(data = model_1)+ 
  geom_density( mapping = aes(x =education_max,fill='education_max'))+
  geom_density( mapping = aes(x =calc_max,fill='calc_max',))+
  geom_density mapping = aes(x =read_max,fill='read_max',))+
  geom_density( mapping = aes(x =write_max,fill='write_max',))+
  labs(title="Household Maximum Education by Type ")+
  xlab("Level of Education")

#create histogram plot for community region info
ggplot(data = model_1)+ 
  geom_histogram( mapping = aes(x =profit_per_rope,fill='profit_per_rope',))+
 labs(title="Household Profit Per Rope")+
  xlab("Profit")
  

