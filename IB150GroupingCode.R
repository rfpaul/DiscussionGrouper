#This is just to generate some dummy data to test the code below
X=c((rep("White", 66)), (rep("Asian", 16)), (rep("Hispanic", 10)), (rep("African-American", 8)))
#50-50 chance of male or female
Gender=rbinom(20,1,.5)
#20 random draws from a list based on reported demographics of UIUC
Race=sample(X, 20, replace=FALSE)
#0,1,or 2 possible
Math=rbinom(20,2,.25)
#10% will be "Leaders"
Personality=rbinom(20,1,.1)
#Binding everything into a test "Student List"
StudentList=as.data.frame(cbind(Gender, Race, Math, Personality))

#START HERE WITH REAL DATA, SKIP THE READ IN LINE IF USING THE PRACTICE DATA
#Read in the csv file of students.  This will pull up a window so you can select your student list
StudentList=read.csv(file=file.choose(), header=TRUE)

#Names and or NetID as the first column(s)
#Gender coded as: male=1, female or other=0
#Race coded as a character variable, White, Hispanic, Asian, African-American
#Math score category dummy coded as: low=1, med=0, high=2
  #Math scores are coded this way to make it easier to say it is okay to have groups with up to two with low scores or one high score, but not a high and a low score in the same group
#Personality dummy coded as binary, "Leader"=1, other=0
#To avoid causing issues with having only one student of a given race, if they are alone then the category gets changed to something that isn't in the later code
#Making sure the column is in the correct format first
#IF USING THE PRACTICE DATA, START UP AGAIN HERE
StudentList$Race=as.character(StudentList$Race)
if(sum(StudentList$Race == "African-American")==1){(StudentList$Race[StudentList$Race%in%"African-American"]<-"Solo")}
if(sum(StudentList$Race == "Asian")==1){(StudentList$Race[StudentList$Race%in%"Asian"]<-"Solo")}
if(sum(StudentList$Race == "Hispanic")==1){(StudentList$Race[StudentList$Race%in%"Hispanic"]<-"Solo")}


#Number of groups to divide into
A=5
#The size of your groups
B=4
#Setting a starter value to what you want the sum to be
D=0
#While loop means that it will keep running until you meet the criteria
#Each group that fulfills the rules will get a value of 6
#If you want to allow one group to have violations of math or personality then you can change E from 0 to 1
E=0
while(D<((A*6)-E){
#assign group number randomly to the student list
StudentList$Group=(sample((rep(1:A, each=B)), replace=F))
#a matrix holding the values of how well each group fills the rules
C=matrix(nrow=A, ncol=1)
#Loop through each group
    for(i in 1:A){
      #Check that there are not more than two males in a group
gendercheck=ifelse((nrow(StudentList[StudentList$Gender==1&StudentList$Group==i,]))>2,0,2)
  #Check that if there is an ethnic minority in the group, there is at least one other person that shares their ethnicity if possible
#If there is only one student of a given ethnic group, we've changed their ethnicity to "Solo" already to not mess up this portion
racecheck=  ifelse((sum(StudentList$Race[which(StudentList$Group==i)] == "African-American")==1), 0,
      (ifelse((sum(StudentList$Race[which(StudentList$Group==i)]  == "Asian")==1),0,
       (ifelse((sum(StudentList$Race[which(StudentList$Group==i)] == "Hispanic")==1),0,2)))))
#Make sure that you either have one high scorer with the rest medium OR you have up to two low scorers with the rest medium
mathcheck=ifelse((sum(as.numeric(as.character(StudentList$Math[which(StudentList$Group==i)]))))>2,0,1)
#Make sure you don't have more than one "Leader" type
personalitycheck=ifelse((nrow(StudentList[StudentList$Personality==1&StudentList$Group==i,]))>1,0,1)
#Sum the values of whether or not the group in question met the rules 
     C[i,1]=(gendercheck+racecheck+mathcheck+personalitycheck)}
#Sum all the groups if they met the rules
#If this is less than A*6, then the while loop will run over again
#Once the while loop is fulfilled, then the dataframe "Student List" will have everyone assigned to a group that fills the rules
D=sum(C)}

