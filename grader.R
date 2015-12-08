


#====== read file ==========================

file = "U03.csv"
fl = read.csv(file,header = TRUE)
#str(tu)
ln = dim(fl)[1]

#===== normalize to fool mark 10 ============
fl$LAB1 <- (fl[,3]/20)*10
fl$LAB2 <- (fl[,4]/25)*10
fl$LAB3 <- (fl[,5]/30)*10
fl$LAB4 <- (fl[,6]/20)*10

std<-  fl[1:ln,2]
scr = c()
grd = c()


#======== sum of 11 labs + Night lab ======
for (i in 1:ln){
  scr[i] =  sum(fl[i,3:13])+fl[i,15]
 } 


#======== replace lowest grade(>=0) lab by lab 12(if lab12 grade>0)===== 


for (i in 1:ln){
     if(fl[i,14]> 0){
           min = fl[i,3]
           for(j in 4:13){
               if(fl[i,j] < min){
                    min = fl[i,j]}}
                  
           scr[i] = scr[i]-min + fl[i,14]}
           scr[i] = (scr[i]/12)*10
                            }
                              
#============   grade   ==========================================

for (i in 1:ln){ 
  if (scr[i] > 95){
    grd[i] = "A"
  }else if(scr[i] > 90 & scr[i] < 95 ){
    grd[i] = "A-"
  }else if(scr[i] > 86.66 & scr[i] < 90 ){
    grd[i] = "B+"
  }else if(scr[i] > 83.33 & scr[i] < 86.66 ){
    grd[i] = "B"
  }else if(scr[i] > 80.0 & scr[i] < 83.33 ){
    grd[i] = "B-"
  }else if(scr[i] > 76.66 & scr[i] < 80 ){
    grd[i] = "C+"
  }else if(scr[i] > 73.33 & scr[i] < 76.66 ){
    grd[i] = "C"
  }else if(scr[i] > 70.0 & scr[i] < 73.33 ){
    grd[i] = "C-"
  }else if(scr[i] > 66.66 & scr[i] < 70 ){
    grd[i] = "D+"
  }else if(scr[i] > 63.3 & scr[i] < 66.6 ){
    grd[i] = "D"
  }else if(scr[i] > 60 & scr[i] < 63.33 ){
    grd[i] = "D-"
  }else{
    grd[i] = "D"}
  }

#=============== write final scores===================

fl$scr <- scr
fl$grd <- grd
#result = cbind(std,grd)
rtl =data.frame(std,scr,grd)
#rtl
write.csv(rtl, file = "U03graded.csv")
write.csv(fl, file = "U03final.csv")

#======================= <><><> =======================




