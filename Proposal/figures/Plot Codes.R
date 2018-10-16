attach(rslt)
#convert all the variables levels into factors
rslt$perf<-revalue(as.factor(rslt$persistence), c("1"="Cont","0"="Once"))
rslt$reasonf<-revalue(as.factor(rslt$reason), c("1"="Safety","2"="Commercial", "3"="Social", "4"="Your", "5"="Health","6"="None"))
rslt$wheref<-revalue(as.factor(rslt$where), c("0"="Self","1"="Some", "2"="Spublic", "3"="Public"))
rslt$whof<-revalue(as.factor(rslt$who), c("1"="Dunno","2"="Colleague", "3"="Friend", "4"="Me","5"="Vendor","6"="Office","7"="Gvt"))
rslt$whatf<-revalue(as.factor(rslt$what), c("1"="Vidpres","2"="Vocage", "3"="Pic", "4"="Vocpres", "5"="Picage","6"="Vocgen", "7"="Picmood","8"="Picgen","9"="Vidw","10"="Vocpres","11"="Pid","12"="Vid","13"="Vidpres","14"="Pidid","15"="Locpres","16"="Picpres","17"="Gaze","18"="Locpres","19"="Picid","20"="Voc","21"="Loc","22"="Vocmod","23"="Gazelook","24"="Vidgen","25"="Vocid","26"="Vidage","27"="Picpres","28"="Vidmod"))
#make all the basic plots
#comfort v/s factors
plot_c1<-ggplot(rslt,aes(whatf,comfort))
plot_c2<-ggplot(rslt,aes(whof,comfort))
plot_c3<-ggplot(rslt,aes(wheref,comfort))
plot_c4<-ggplot(rslt,aes(reasonf,comfort))
plot_c5<-ggplot(rslt,aes(perf,comfort))
#risk v/s factors
plot_r1<-ggplot(rslt,aes(whatf,risk))
plot_r2<-ggplot(rslt,aes(whof,risk))
plot_r3<-ggplot(rslt,aes(wheref,risk))
plot_r4<-ggplot(rslt,aes(reasonf,risk))
plot_r5<-ggplot(rslt,aes(perf,risk))
#approp v/s factors
plot_a1<-ggplot(rslt,aes(whatf,approp))
plot_a2<-ggplot(rslt,aes(whof,approp))
plot_a3<-ggplot(rslt,aes(wheref,approp))
plot_a4<-ggplot(rslt,aes(reasonf,approp))
plot_a5<-ggplot(rslt,aes(perf,approp))
#main plots - COMFORT
plot_c1+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Comfort v/s What", x= "What - 24 levels", y= "Comfort (Very Uncomfortable to Very Comfortable)")+theme(axis.text.x=element_text(angle = 75, hjust=1, size=11), panel.background=element_rect(fill="lightblue"))
plot_c2+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Comfort v/s Who", x= "Who - 7 levels", y= "Comfort (Very Uncomfortable to Very Comfortable)")+theme(axis.text.x=element_text(angle = 0, hjust=0.5, size=11), panel.background=element_rect(fill="lightblue"))
plot_c3+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Comfort v/s Where", x= "Where - 3 levels", y= "Comfort (Very Uncomfortable to Very Comfortable)")+theme(axis.text.x=element_text(angle = 0, hjust=0.5, size=11), panel.background=element_rect(fill="lightblue"))
plot_c4+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Comfort v/s Reason", x= "Reason - 6 levels", y= "Comfort (Very Uncomfortable to Very Comfortable)")+theme(axis.text.x=element_text(angle = 0, hjust=0.5, size=11), panel.background=element_rect(fill="lightblue"))
plot_c5+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Comfort v/s Persistence", x= "Persistence - 2 levels", y= "Comfort (Very Uncomfortable to Very Comfortable)")+theme(axis.text.x=element_text(angle = 0, hjust=0.5, size=11), panel.background=element_rect(fill="lightblue"))
#main plots - RISK
plot_r1+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Risk v/s What", x= "What - 24 levels", y= "Risk (Very Risky to Very Safe)")+theme(axis.text.x=element_text(angle = 75, hjust=1, size=11), panel.background=element_rect(fill="lightsalmon"))
plot_r2+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Risk v/s Who", x= "Who - 7 levels", y= "Risk (Very Risky to Very Safe)")+theme(axis.text.x=element_text(angle = 0, hjust=0.5, size=11), panel.background=element_rect(fill="lightsalmon"))
plot_r3+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Risk v/s Where", x= "Where - 3 levels", y= "Risk (Very Risky to Very Safe)")+theme(axis.text.x=element_text(angle = 0, hjust=0.5, size=11), panel.background=element_rect(fill="lightsalmon"))
plot_r4+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Risk v/s Reason", x= "Reason - 6 levels", y= "Risk (Very Risky to Very Safe)")+theme(axis.text.x=element_text(angle = 0, hjust=0.5, size=11), panel.background=element_rect(fill="lightsalmon"))
plot_r5+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Risk v/s Persistence", x= "Persistence - 2 levels", y= "Risk (Very Risky to Very Safe)")+theme(axis.text.x=element_text(angle = 0, hjust=0.5, size=11), panel.background=element_rect(fill="lightsalmon"))
#main plots - APPROP
plot_a1+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Appropriateness v/s What", x= "What - 24 levels", y= "Appropriateness (Very Approp to Very Inapprop)")+theme(axis.text.x=element_text(angle = 75, hjust=1, size=11), panel.background=element_rect(fill="olivedrab1"))
plot_a2+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Appropriateness v/s Who", x= "Who - 7 levels", y= "Appropriateness (Very Approp to Very Inapprop)")+theme(axis.text.x=element_text(angle = 0, hjust=0.5, size=11), panel.background=element_rect(fill="olivedrab1"))
plot_a3+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Appropriateness v/s Where", x= "Where - 3 levels", y= "Appropriateness (Very Approp to Very Inapprop)")+theme(axis.text.x=element_text(angle = 0, hjust=0.5, size=11), panel.background=element_rect(fill="olivedrab1"))
plot_a4+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Appropriateness v/s Reason", x= "Reason - 6 levels", y= "Appropriateness (Very Approp to Very Inapprop)")+theme(axis.text.x=element_text(angle = 0, hjust=0.5, size=11), panel.background=element_rect(fill="olivedrab1"))
plot_a5+stat_summary(fun.y = mean, geom="point", color = "black", size = 2)+stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.5)+ylim(-3,3)+labs(title= "Appropriateness v/s Persistence", x= "Persistence - 2 levels", y= "Appropriateness (Very Approp to Very Inapprop)")+theme(axis.text.x=element_text(angle = 0, hjust=0.5, size=11), panel.background=element_rect(fill="olivedrab1"))
