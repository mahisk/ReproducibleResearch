## Function to create the plots for 
# (1) Make a plot that answers the question: what is the relationship between 
#     mean covered charges (Average.Covered.Charges) and mean total payments 
#     (Average.Total.Payments) in New York?
# (2) Make a plot (possibly multi-panel) that answers the question: how does 
#     the relationship between mean covered charges (Average.Covered.Charges) 
#     and mean total payments (Average.Total.Payments) vary by medical condition 
#     (DRG.Definition) and the state in which care was received (Provider.State)?

plots <- function(){
  #read the data from "subset of a United States medical expenditures dataset 
  #with information on costs for different medical conditions and in different 
  #areas of the country"  
  usmedexpdata <- read.csv("payments.csv")
  nydata <- usmedexpdata[usmedexpdata$Provider.City == "NEW YORK",10:12]
  
  #get unique states and medical conditions
  states <- unique(usmedexpdata$Provider.State)
  medconds <- unique(usmedexpdata$DRG.Definition)
  
  # (1) Make a plot that answers the question: what is the relationship between 
  #     mean covered charges (Average.Covered.Charges) and mean total payments 
  #     (Average.Total.Payments) in New York?
  
  pdf("plot1.pdf")
  plot(nydata$Average.Covered.Charges, nydata$Average.Total.Payments, 
       xlab = "Average Covered Charges ($)", ylab = "Average Total Payments ($)",
       main = "Average Charges vs Payments in New York", col="blue", pch = 16)
  abline(lm(nydata$Average.Total.Payments ~ nydata$Average.Covered.Charges), 
         col = "red", lwd = 1.5)
  dev.off()
  
  
  # (2) Make a plot (possibly multi-panel) that answers the question: how does 
  #     the relationship between mean covered charges (Average.Covered.Charges) 
  #     and mean total payments (Average.Total.Payments) vary by medical condition 
  #     (DRG.Definition) and the state in which care was received (Provider.State)?
  
  pdf("plot2.pdf")
  
  #we have 6 medical conditions & 6 states.
  #use par to divide the screen into multi panel
  par(mfrow = c(6,6), oma = c(4,4,4,2), mar = rep(2,4))
  
  #loop through each states
  for(state in states){
    #loop through each medical condition
    for(medcond in medconds){
      #get the medcondition ID
      medconID = substring("194 - SIMPLE PNEUMONIA & PLEURISY W CC", 0,3)
      
      #get the subset of data
      subdata <- subset(usmedexpdata, Provider.State == state & DRG.Definition == medcond)
      #plot for the selected state and medcondition
      plot(subdata$Average.Covered.Charges, subdata$Average.Total.Payments, 
           main = paste(state, medconID, sep = " "), xlab = "Average Covered Charges ($)", 
           ylab = "Average Total Payments ($)", col="blue", pch = 16,
           xlim = range(usmedexpdata$Average.Covered.Charges),
           ylim = range(usmedexpdata$Average.Total.Payments))
      
      #draw the line
      abline(lm(usmedexpdata$Average.Total.Payments ~ usmedexpdata$Average.Covered.Charges), 
             col = "red", lwd = 1.5)
    }
  }
  
  mtext("Covered Charges", side = 1, outer = TRUE, line = 1)
  mtext("Total Payments", side = 2, outer = TRUE, line = 1)
  mtext("Avarage Covered Charges and Total Payments
      by Medical Condition and State", side = 3, outer = TRUE, font = 2,
        line = 1)
  
  dev.off()
}