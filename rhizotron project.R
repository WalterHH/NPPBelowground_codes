# colnames in database:
# plot_code
# elevation
# rhizotron
# session
# day
# month
# year                      
# soil_layer_depth_cm
# soil_layer_heigth_num_cm
# alive_lower_1_mm
# alive_1to2_mm
# alive_2to3_mm
# alive_upper_3mm
# dead_lower_1_mm
# dead_1to2_mm
# dead_2to3_mm
# dead_upper_3mm
# comments   

### Read test data:

#### function to calcutale the root area for each diameter class
### values of radius of each diameter class of root
## < 1mm=0.25mm
## 1-2mm=0.75mm
## 2-3mm=1.25mm
## > 3mm=1.75mm

#### function to calcutale the root biomas and necromas for each diameter class, information get from D.Metcalfe
# area multiplier 20000
# root density 0.00029 g/mm3
# coarse fraction 1.00, proportion assumes that the soil has not coarse fraction
# sin a 1.57 radiants assumes the rhizotron is inserted vertically into the soil, angle=90 degrees
# cos y 1.00 assumes the ground surface is horizontal angle =0
# w 210 mm 


########## function to calculate npp from rhizotrons########
fr<-read.csv("/Users/Walter/Desktop/rhizotron.data.all.plots.csv", sep=",", header=T)
names(fr)
head(fr)
attach(fr)
detach(fr)

frarea<-function(x){
  a1<-pi^2*alive_lower_1_mm*0.25^2/sqrt(2)#### to calculate root area at different diameter class,but only for montly growth  
  a2<-pi^2*alive_1to2_mm*0.75^2/sqrt(2)
  a3<-pi^2*alive_2to3_mm*1.25^2/sqrt(2)
  a4<-pi^2*alive_upper_3mm*1.75^2/sqrt(2)
  d1<-pi^2*dead_lower_1_mm*0.25^2/sqrt(2)#### to calculate root area at different diameter class,but only for montly disaper
  d2<-pi^2*dead_1to2_mm*0.75^2/sqrt(2)
  d3<-pi^2*dead_2to3_mm*1.25^2/sqrt(2)
  d4<-pi^2*dead_upper_3mm*1.75^2/sqrt(2)
  return(data.frame(a1=a1,a2=a2,a3=a3,a4=a4,d1=d1,d2=d2,d3=d3,d4=d4))
}
output<-frarea(fr)
output

#########  function for surface area and biomass
biomass<-function(y){
  bio<-20000*0.00029*output*1.57/210
  return(data.frame(bio=bio))
}
output1<-biomass(output)
output1

biomas1<-function(z){
  biom<-sum(output1)
  return(c(biom=biom))
}
output2<-biomas1(output1)
output2



getwd("/Users/Walter/Documents/Statistic/Root sripts")

plot_code<-vector("character",length(named.med))
elevation<-vector("numeric",length(named.med))
rhizotron_num<-vector("numeric",length(named.med))
session<-vector("numeric",length(named.med))
day<-vector("numeric",length(named.med))
month<-vector("numeric",length(named.med))
year<-vector("numeric",length(named.med))
soil_layer_depth_cm<-vector("character",length(named.med))
soil_layer_heigth_num_cm<-vector("numeric",length(named.med))
alive_lower_1_mm<-vector("numeric",length(named.med))
alive_1to2_mm<-vector("numeric",length(named.med))
alive_2to3_mm<-vector("numeric",length(named.med))
alive_upper_3mm<-vector("numeric",length(named.med))
dead_lower_1_mm<-vector("numeric",length(named.med))
dead_1to2_mm<-vector("numeric",length(named.med))
dead_2to3_mm<-vector("numeric",length(named.med))
dead_upper_3mm<-vector("numeric",length(named.med))
comments<-vector("numeric",length(named.med))

rhiz_column_types = c(
  "plot_code" = "character",
  "elevation" = "numeric",
  "rhizothron_num" = "mumeric",
  "session" = "numeric",
  "day" = "numeric",
  "month" = "numeric",
  "year" = "numeric",
  "soil_humidity_pcnt" = "numeric",
  "soil_temperature_c" = "numeric",
  "soil_layer_depth_cm" = "character",
  "soil_layer_depth_num_cm" = "numeric",
  "alive_lower_1_mm" = "numeric",
  "alive_1to2_mm" = "numeric",
  "alive_2to3_mm" = "numeric",
  "alive_upper_3mm"= "numeric",
  "dead_lower_1_mm" = "numeric",
  "dead_1to2_mm" = "numeric",
  "dead_2to3_mm" = "numeric",
  "dead_upper_3mm"= "numeric",
  "comments" = "character"
)
















### for loops

for (variable in vector){comands}
for(i in 1:6){print(i^3)}

for(degc in c(4,5,9)){degf<-degc*(9/5)+32
print(c(degc,degf))
}

temp<-c(-4,5,10,-10,-40,30)
if(temp>0){
  print("warm")}else{
    print("not so warm")
  }


#### while loops
storage<-c()
storage
x<-1
while(x<10){
  storage<-c(storage,x)
  x<-x+1
}
storage


##### family of functions 

### what will we look at?
apply()
lapply()
sapply()
tapply()

#### what else is there
rapply()
vapply()
eapply()
mapply()

## apply() takes three arguments

aggregate(formula,data,function)
### aggregate(cbind(alive_1to2_mm,dead_1to2_mm)~rhizotron_num+soil_layer_heigth_num_cm,fr,sum)
apply(fr[10:17],1,sum)

#### FOR LOPS#######
### General loop
#for (variable in vector){comands}
##for(i in 1:6){print(i^3)}

### Nested loop
# for(var1 in vertor1){
# for(var2 in vector 2){
# for(varn in vector n){
#commands
#}
#}
#}
for(temp in c(-4,5,10,-10,-40,30)){
  if(temp>0){print("warm")}
  else{print("not warm")}
}

for(i in 1:3){
  for(j in 1:2){
    print(i+j)
  }
}


#### Conditional statments
forest<-2
if(forest=="2"){print(TRUE)}

age<-30
if(age==21){print("me too")}else{print("not me, I'm 21")}

age<-30
ifelse(age==21, "me too","not me...I am 21")

animal<-c("bird","fish","mammal","fish","mammal","fish","bird")
animal

e<-ifelse(animal=="fish","swims","doesn't swim")
e

## funtion PASTE

species<-c("laur", "Melas","Clusi","Prun","Cletr")
num<-1:5
paste(species,num,sep="-")
paste(species,num,sep="__")
paste(species,num,sep="/")
paste(fr$rhizotron_num,fr$soil_layer_heigth_num_cm,sep="-")

