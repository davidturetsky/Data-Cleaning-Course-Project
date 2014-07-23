run_analysis<-function(){
        ##  Please note that my use of numbering does not necessarily correspond to the 
        ##  problem numbering in the assignment.  It is used for neatness and provides
        ##  the ability to break activities into logical and reproducible steps.
        
        #0 Environmental issues to take care of
        library(plyr) 
        library(reshape2)
        library(stats)
        
        ##1. Load data files
        ##1.1 Load test data files
        subject_test<-read.table("subject_test.txt", header = FALSE, stringsAsFactors=FALSE)
        unlink("subject_test.txt")
        x_test<-read.table("X_test.txt", header = FALSE, stringsAsFactors=FALSE)
        unlink("X_test.txt")
        y_test<-read.table("y_test.txt", header = FALSE, stringsAsFactors=FALSE)
        unlink("y_test.txt")
        
        ##1.2 Load train data files
        subject_train<-read.table("subject_train.txt", header = FALSE, stringsAsFactors=FALSE)
        unlink("subject_train.txt")
        x_train<-read.table("X_train.txt", header = FALSE, stringsAsFactors=FALSE)
        unlink("X_train.txt")
        y_train<-read.table("y_train.txt", header = FALSE, stringsAsFactors=FALSE)
        unlink("y_train.txt")
        
        ##1.3 Load label files
        raw_activities<-read.table("activity_labels.txt", header = FALSE, stringsAsFactors=FALSE)
        unlink("activity_labels.txt")
        raw_features<-read.table("features.txt", header = FALSE, stringsAsFactors=FALSE)
        unlink("features.txt")
        raw_features_info<-read.table("features_info.txt",skip=12,nrows=16, header = FALSE, stringsAsFactors=FALSE)
        unlink("features_info.txt")
        
        
        ##1.4 Transform label names into R friendly names
        feat_1<-gsub("\\(","",raw_features$V2)
        feat_2<-gsub("\\)","",feat_1)
        feat_3<-gsub("\\-","",feat_2)
        feature_labels<-gsub("\\,","",feat_3)
        
        #1.5 Apply label names to columns
        names(x_test)<-feature_labels
        names(x_train)<-feature_labels
        names(y_test)<-"Activity"
        names(y_train)<-"Activity"
        names(subject_test)<-"Subject"
        names(subject_train)<-"Subject"
        names(raw_features_info)<-"Feature"
        
        ##2. Combine data files into wide data sets
        ##2.1 Test data set
        raw_test<-data.frame()
        ds_label<-"test"
        raw_test<-cbind("Data_Set"=ds_label,subject_test,y_test,x_test)
        
        ##2.2 Train data set
        raw_train<-data.frame()
        ds_label<-"train"
        raw_train<-cbind("Data_Set"=ds_label,subject_train,y_train,x_train)
        
        ##3. Combining the data sets
        ##3.1 Complete raw data set
        raw_total<-data.frame()
        raw_total<-rbind(raw_test,raw_train)
        
        ##3.2 Removing unnecessary columns from raw_total data set
        ##via incidies to create a clean, wide data set with only std and mean observations
        cols1<-grep("mean[^Freq]",names(raw_total))
        cols2<-grep("std",names(raw_total))
        cols0<-c(1,2,3)
        
        cols<-c(cols0,cols1,cols2)
        cols_result<-sort(cols)
        total_wide<-data.frame()
        total_wide<-raw_total[,cols_result]
        
        ##4. Assign descriptive activity names to the activities in the data set
        ##4.1 X
        act_cols<-c("actno","actname")
        names(raw_activities)<-act_cols
        total_wide$Activity <- factor(total_wide$Activity, levels=c(1:6), labels=raw_activities$actname)
        
        ##5. Make tidy data set before computing means.
        ##5.1 Create vars
        var_names<-names(total_wide)[1:3]
        tw_names<-names(total_wide)[4:60]
        
        ##5.2 Make tidy data set using the melt command
        total_narrow<-melt(total_wide,id.vars=var_names,measure.vars=tw_names,value.name="Measurements")
        names(total_narrow)[4]<-"Variable"
        
        ##6. Creating a second tidy data set by computing means on each variable for each activity 
        ##   and each subject. 
        
        ##6.1 Compute the means for each variable by activity and subject.
        #tidy_means<-by(total_narrow[, 2:4],total_narrow[,"Measurements"],mean)
        tidy_means<-aggregate(Measurements~Subject+Activity+Variable, data=total_narrow, mean, na.rm=TRUE)
        
        ##7. Save the tidy data set
        write.table(tidy_means,file="tidy_dataset.txt")
        
}