library(dummies)

DF <- read.csv("Medical costs.csv")
DF.new <- dummy.data.frame(DF,sep= ".")
attach(DF.new)
model0 <- lm(charges~1)
summary(model0)

cor(DF.new)

model1 <- lm(charges~smoker.yes)
n1 <- summary(model1)$adj

v2 <- data.frame(age,sex.male,bmi,children,region.northeast,region.northwest,region.southeast)

names(v2)

rsq2 <- rep(0,ncol(v2))
for(i in 1:ncol(v2))
 {
  modeln <- update(model1,.~.+v2[,i])
  rsq2[i] <- summary(modeln)$adj
 }
index2 <- which.max(rsq2)
index2
max2 <- max(rsq2)
max2>n1
names(v2)
model2 <- update(model1,.~.+age)
n2 <- summary(model2)$adj  
#model2
v3 <- subset(v2,select = -index2)
names(v3)
rsq3 <- rep(0,ncol(v3))
for(i in 1:ncol(v3))
{
  modeln <- update(model2,.~.+v3[,i])
  rsq3[i] <- summary(modeln)$adj
}
index3 <- which.max(rsq3)
index3
rsq3
max3 <- max(rsq3)

max3>n2
names(v3)
model3 <- update(model2,.~.+bmi)
n3 <- summary(model3)$adj

v4 <- subset(v3,select = -index3)
names(v4)

rsq4 <- rep(0,ncol(v4))
for(i in 1:ncol(v4))
{
  modeln <- update(model3,.~.+v4[,i])
  rsq4[i] <- summary(modeln)$adj
}
index4 <- which.max(rsq4)
index4
rsq4
max4 <- max(rsq4)
max4>n3
names(v4)
model4 <- update(model3,.~.+children)
n4 <- summary(model4)$adj

v5 <- subset(v4,select = -index4)
names(v5)

rsq5 <- rep(0,ncol(v5))
for(i in 1:ncol(v5))
{
  modeln <- update(model4,.~.+v5[,i])
  rsq5[i] <- summary(modeln)$adj
}
index5 <- which.max(rsq5)
index5
rsq5
max5 <- max(rsq5)
max5>n4
names(v5)
model5 <- update(model4,.~.+region.northeast)
n5 <- summary(model5)$adj

v6 <- subset(v5,select = -index5)
names(v6)

rsq6 <- rep(0,ncol(v6))
for(i in 1:ncol(v6))
{
  modeln <- update(model5,.~.+v6[,i])
  rsq6[i] <- summary(modeln)$adj
}
index6 <- which.max(rsq6)
index6
rsq6
max6 <- max(rsq6)
max6>n5
names(v6)
model6 <- update(model5,.~.+region.northwest)
n6 <- summary(model6)$adj

v7 <- subset(v6,select = -index6)
names(v7)

rsq7 <- rep(0,ncol(v7))
for(i in 1:ncol(v7))
{
  modeln <- update(model6,.~.+v7[,i])
  rsq7[i] <- summary(modeln)$adj
}
index7 <- which.max(rsq7)
index7
rsq7
max7 <- max(rsq7)
max7>n6

#model6
summary(model6)

v <- data.frame(smoker.yes,age,bmi,children,region.northeast,region.northwest)
rsq8 <- matrix(rep(0),nrow=ncol(v),ncol=ncol(v))

for(i in 1:ncol(v))
  {
  for(j in i:ncol(v))
  {
    if(i==j)
    {
      next
    }
   modeln <- update(model6,.~.+v[,i]:v[,j])
   rsq8[i,j] <- summary(modeln)$adj
  }
}
rsq8
max8 <- max(rsq8)

max8>n6
names(v)
model8 <- update(model6,.~.+smoker.yes:bmi)
n8 <- summary(model8)$adj

rsq9 <- matrix(rep(0),nrow=ncol(v),ncol=ncol(v))

for(i in 1:ncol(v))
{
  for(j in i:ncol(v))
  {
    if(i==1 && j==3){
      next
    }
    if(i==j)
    {
      next
    }
    modeln <- update(model8,.~.+v[,i]:v[,j])
    rsq9[i,j] <- summary(modeln)$adj
  
  }
}
rsq9
max9 <- max(rsq9)
max9>n8
names(v)
model9 <- update(model8,.~.+bmi:region.northeast)
n9 <- summary(model9)$adj

rsq10 <- matrix(rep(0),nrow=ncol(v),ncol=ncol(v))

for(i in 1:ncol(v))
{
  for(j in i:ncol(v))
  {
    if(i==1 && j==3){
      next
    }
    if(i==j)
    {
      next
    }
    if(i==3&&j==5){
      next
    }
    modeln <- update(model9,.~.+v[,i]:v[,j])
    rsq10[i,j] <- summary(modeln)$adj
    
  }
}

rsq10
max10 <- max(rsq10)
max10
max10>n9
names(v)
summary(model9)
model10 <- update(model9,.~.+bmi:region.northwest)
n10 <- summary(model10)$adj

rsq11 <- matrix(rep(0),nrow=ncol(v),ncol=ncol(v))

for(i in 1:ncol(v))
{
  for(j in i:ncol(v))
  {
    if(i==1 && j==3){
      next
    }
    if(i==j)
    {
      next
    }
    if(i==3&&j==5){
      next
    }
    if(i==3&&j==6){
      next
    }
    modeln <- update(model10,.~.+v[,i]:v[,j])
    rsq11[i,j] <- summary(modeln)$adj
    
  }
}
rsq11
max11 <- max(rsq11)
max11
max11>n10
names(v)
model11 <- update(model10,.~.+age:region.northeast)
n11 <- summary(model11)$adj
n11==max11

rsq12 <- matrix(rep(0),nrow=ncol(v),ncol=ncol(v))

for(i in 1:ncol(v))
{
  for(j in i:ncol(v))
  {
    if(i==1 && j==3){
      next
    }
    if(i==j)
    {
      next
    }
    if(i==3&&j==5){
      next
    }
    if(i==3&&j==6){
      next
    }
    if(i==2&&j==5){
      next
    }
    modeln <- update(model11,.~.+v[,i]:v[,j])
    rsq12[i,j] <- summary(modeln)$adj
    
  }
}

rsq12
max12 <- max(rsq12)
max12>n11
max12
names(v)

model12 <- update(model11,.~.+children:region.northwest)
n12 <- summary(model12)$adj
max12==n12


rsq13 <- matrix(rep(0),nrow=ncol(v),ncol=ncol(v))

for(i in 1:ncol(v))
{
  for(j in i:ncol(v))
  {
    if(i==1 && j==3){
      next
    }
    if(i==j)
    {
      next
    }
    if(i==3&&j==5){
      next
    }
    if(i==3&&j==6){
      next
    }
    if(i==2&&j==5){
      next
    }
    if(i==4&&j==6){
      next
    }
    modeln <- update(model12,.~.+v[,i]:v[,j])
    rsq13[i,j] <- summary(modeln)$adj
    
  }
}

rsq13
max13 <- max(rsq13)
max13>n12
max13
names(v)

model13 <- update(model12,.~.+age:region.northwest)
n13 <- summary(model13)$adj
n13==max13

rsq14 <- matrix(rep(0),nrow=ncol(v),ncol=ncol(v))

for(i in 1:ncol(v))
{
  for(j in i:ncol(v))
  {
    if(i==1 && j==3){
      next
    }
    if(i==j)
    {
      next
    }
    if(i==3&&j==5){
      next
    }
    if(i==3&&j==6){
      next
    }
    if(i==2&&j==5){
      next
    }
    if(i==4&&j==6){
      next
    }
    if(i==2&&j==6){
      next
    }
    modeln <- update(model13,.~.+v[,i]:v[,j])
    rsq14[i,j] <- summary(modeln)$adj
    
  }
}

rsq14
max14 <- max(rsq14)

max14>n13
max14
names(v)
model14 <- update(model13,.~.+smoker.yes:children)
n14 <- summary(model14)$adj
n14==max14

rsq15 <- matrix(rep(0),nrow=ncol(v),ncol=ncol(v))

for(i in 1:ncol(v))
{
  for(j in i:ncol(v))
  {
    if(i==1 && j==3){
      next
    }
    if(i==j)
    {
      next
    }
    if(i==3&&j==5){
      next
    }
    if(i==3&&j==6){
      next
    }
    if(i==2&&j==5){
      next
    }
    if(i==4&&j==6){
      next
    }
    if(i==2&&j==6){
      next
    }
    if(i==1&&j==4){
      next
    }
    modeln <- update(model14,.~.+v[,i]:v[,j])
    rsq15[i,j] <- summary(modeln)$adj
    
  }
}

rsq15
max15 <- max(rsq15)
max15>n14
max15
#model14
summary(model14)

rsq16 <- array(rep(0,6*6*6),dim=c(6,6,6))
rsq16

for(i in 1:6){
  for(j in i:6){
    for(k in j:6){
      if(i==j){
        next
      }
      if(i==k){
        next
      }
      if(j==k){
        next
      }
      modeln <- update(model14,.~.+v[,i]:v[,j]:v[,k])
      rsq16[i,j,k] <- summary(modeln)$adj
    }
  }
}
rsq16
max16 <- max(rsq16)
max16
max16>n14
which( rsq16==(max(rsq16,na.rm= T)),arr.ind= T)
names(v)

model16 <- update(model14,.~.+smoker.yes:bmi:children)
n16 <- summary(model16)$adj
max16==n16

rsq17 <- array(rep(0,6*6*6),dim=c(6,6,6))


for(i in 1:6){
  for(j in i:6){
    for(k in j:6){
      if(i==j){
        next
      }
      if(i==k){
        next
      }
      if(j==k){
        next
      }
      if(i==1&&j==3&&k==4){
        next
      }
      modeln <- update(model16,.~.+v[,i]:v[,j]:v[,k])
      rsq17[i,j,k] <- summary(modeln)$adj
    }
  }
}

rsq17
max17 <- max(rsq17)
max17>n16

which( rsq17==(max(rsq17,na.rm= T)),arr.ind= T)
names(v)
model17 <- update(model16,.~.+smoker.yes:children:region.northwest)
n17 <- summary(model17)$adj
n17==max17

rsq18 <- array(rep(0,6*6*6),dim=c(6,6,6))


for(i in 1:6){
  for(j in i:6){
    for(k in j:6){
      if(i==j){
        next
      }
      if(i==k){
        next
      }
      if(j==k){
        next
      }
      if(i==1&&j==3&&k==4){
        next
      }
      if(i==1&&j==4&&k==6){
        next
      }
      modeln <- update(model17,.~.+v[,i]:v[,j]:v[,k])
      rsq18[i,j,k] <- summary(modeln)$adj
    }
  }
}

max18 <- max(rsq18)
max18>n17

which( rsq18==(max(rsq18,na.rm= T)),arr.ind= T)
names(v)

model18 <- update(model17,.~.+smoker.yes:age:region.northwest)
n18 <- summary(model18)$adj
n18==max18

rsq19 <- array(rep(0,6*6*6),dim=c(6,6,6))


for(i in 1:6){
  for(j in i:6){
    for(k in j:6){
      if(i==j){
        next
      }
      if(i==k){
        next
      }
      if(j==k){
        next
      }
      if(i==1&&j==3&&k==4){
        next
      }
      if(i==1&&j==4&&k==6){
        next
      }
      if(i==1&&j==2&&k==6){
        next
      }
      modeln <- update(model18,.~.+v[,i]:v[,j]:v[,k])
      rsq19[i,j,k] <- summary(modeln)$adj
    }
  }
}

max19 <- max(rsq19)
max19>n18

which( rsq19==(max(rsq19,na.rm= T)),arr.ind= T)
names(v)

model19 <- update(model18,.~.+bmi:children:region.northwest)
n19 <- summary(model19)$adj
n19==max19

rsq20 <- array(rep(0,6*6*6),dim=c(6,6,6))

for(i in 1:6){
  for(j in i:6){
    for(k in j:6){
      if(i==j){
        next
      }
      if(i==k){
        next
      }
      if(j==k){
        next
      }
      if(i==1&&j==3&&k==4){
        next
      }
      if(i==1&&j==4&&k==6){
        next
      }
      if(i==1&&j==2&&k==6){
        next
      }
      if(i==3&&j==4&&k==6){
        next
      }
      modeln <- update(model19,.~.+v[,i]:v[,j]:v[,k])
      rsq20[i,j,k] <- summary(modeln)$adj
    }
  }
}

max20 <- max(rsq20)
max20>n19
#model19
summary(model19)

rsq21 <- array(rep(0,6*6*6*6),c(6,6,6,6))

for(i in 1:6){
  for(j in i:6){
    for(k in j:6){
      for(l in k:6){
        if(i==j){
          next
        }
        if(i==k){
          next
        }
        if(i==l){
          next
        }
        if(j==k){
          next
        }
        if(j==l){
          next
        }
        if(k==l){
          next
        }
        modeln <- update(model19,.~.+v[,i]:v[,j]:v[,k]:v[,l])
        rsq21[i,j,k,l] <- summary(modeln)$adj
      }
    }
  }
}

max21 <- max(rsq21)
max21>n19

which( rsq21==(max(rsq21,na.rm= T)),arr.ind= T)
names(v)

model21 <- update(model19,.~.+smoker.yes:age:bmi:region.northwest)
n21 <- summary(model21)$adj
n21==max21

rsq22 <- array(rep(0,6*6*6*6),c(6,6,6,6))

for(i in 1:6){
  for(j in i:6){
    for(k in j:6){
      for(l in k:6){
        if(i==j){
          next
        }
        if(i==k){
          next
        }
        if(i==l){
          next
        }
        if(j==k){
          next
        }
        if(j==l){
          next
        }
        if(k==l){
          next
        }
        if(i==1&&j==2&&k==3&&k==6){
          next
        }
        modeln <- update(model21,.~.+v[,i]:v[,j]:v[,k]:v[,l])
        rsq22[i,j,k,l] <- summary(modeln)$adj
      }
    }
  }
}

max22 <- max(rsq22)
max22>n21

which( rsq22==(max(rsq22,na.rm= T)),arr.ind= T)
names(v)

model22 <- update(model21,.~.+smoker.yes:bmi:children:region.northwest)
n22 <- summary(model22)$adj
n22==max22

rsq23 <- array(rep(0,6*6*6*6),c(6,6,6,6))

for(i in 1:6){
  for(j in i:6){
    for(k in j:6){
      for(l in k:6){
        if(i==j){
          next
        }
        if(i==k){
          next
        }
        if(i==l){
          next
        }
        if(j==k){
          next
        }
        if(j==l){
          next
        }
        if(k==l){
          next
        }
        if(i==1&&j==2&&k==3&&k==6){
          next
        }
        if(i==1&&j==3&&k==4&&l==6){
          next
        }
        modeln <- update(model22,.~.+v[,i]:v[,j]:v[,k]:v[,l])
        rsq23[i,j,k,l] <- summary(modeln)$adj
      }
    }
  }
}

max23 <- max(rsq23)
max23>n22

which( rsq23==(max(rsq23,na.rm= T)),arr.ind= T)
names(v)

model23 <- update(model22,.~.+smoker.yes:age:children:region.northwest)
n23 <- summary(model23)$adj
n23==max23

rsq24 <- array(rep(0,6*6*6*6),c(6,6,6,6))

for(i in 1:6){
  for(j in i:6){
    for(k in j:6){
      for(l in k:6){
        if(i==j){
          next
        }
        if(i==k){
          next
        }
        if(i==l){
          next
        }
        if(j==k){
          next
        }
        if(j==l){
          next
        }
        if(k==l){
          next
        }
        if(i==1&&j==2&&k==3&&k==6){
          next
        }
        if(i==1&&j==3&&k==4&&l==6){
          next
        }
        if(i==1&&j==2&&k==4&&l==6){
          next
        }
        modeln <- update(model23,.~.+v[,i]:v[,j]:v[,k]:v[,l])
        rsq24[i,j,k,l] <- summary(modeln)$adj
      }
    }
  }
}

max24 <- max(rsq24)
max24>n23

which( rsq24==(max(rsq24,na.rm= T)),arr.ind= T)
names(v)

model24 <- update(model23,.~.+age:bmi:children:region.northwest)
n24 <- summary(model24)$adj
n24==max24

rsq25 <- array(rep(0,6*6*6*6),c(6,6,6,6))

for(i in 1:6){
  for(j in i:6){
    for(k in j:6){
      for(l in k:6){
        if(i==j){
          next
        }
        if(i==k){
          next
        }
        if(i==l){
          next
        }
        if(j==k){
          next
        }
        if(j==l){
          next
        }
        if(k==l){
          next
        }
        if(i==1&&j==2&&k==3&&k==6){
          next
        }
        if(i==1&&j==3&&k==4&&l==6){
          next
        }
        if(i==1&&j==2&&k==4&&l==6){
          next
        }
        if(i==2&&j==3&&k==4&&l==6){
          next
        }
        modeln <- update(model24,.~.+v[,i]:v[,j]:v[,k]:v[,l])
        rsq25[i,j,k,l] <- summary(modeln)$adj
      }
    }
  }
}

max25 <- max(rsq25)
max25>n24
#model24

rsq26 <- array(rep(0,6*6*6*6*6),dim=c(6,6,6,6,6))

for(i in 1:6){
  for(j in i:6){
    for(k in j:6){
      for(l in k:6){
        for(m in l:6){
          if(i==j){
            next
          }
          if(i==k){
            next
          }
          if(i==l){
            next
          }
          if(i==m){
            next
          }
          if(j==k){
            next
          }
          if(j==l){
            next
          }
          if(j==m){
            next
          }
          if(k==l){
            next
          }
          if(k==m){
            next
          }
          if(l==m){
            next
          }
          modeln <- update(model24,.~.+v[,i]:v[,j]:v[,k]:v[,l]:v[,m])
          rsq26[i,j,k,l,m] <- summary(modeln)$adj
        }
      }
    }
  }
}

max26 <- max(rsq26)
max26>n24
#model24
n24
summary(model24)
#Enter the variables 
#x1 If smoker feed 1
#or else 0
#x2 the age of policy holder
#x3 the BMI of policy holder
#x4 the no of children
#x5 If the policy holder belongs to region North East
#feed 1 or else 0
#x6 If the policy holder belongs to region North West
#feed 1 or else 0
 
x1 <- as.numeric(readline("Enter 1 if smoker or else 0:"))
x2 <- as.numeric(readline("Enter the age of policy holder:"))
x3 <- as.numeric(readline("Enter BMI of policy holder:"))
x4 <- as.numeric(readline("Enter the no of children of policy holder:"))
x5 <- as.numeric(readline("Enter 1, if policy holder belongs to region North East"))
x6 <- as.numeric(readline("Enter 1, if policy holder belongs to region North West:"))

data <- data.frame(smoker.yes=x1,age=x2,bmi=x3,children=x4,region.northeast=x5,region.northwest=x6) 
predict(model24,data)
detach(DF.new)
