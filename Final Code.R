car_train_final <- car_train
car_train_final
dim(car_train_final)

car_train_final <- transform(car_train_final, lPriceNew = log(PriceNew))

car_train_final <- transform(car_train_final, lHorsepower = log(Horsepower))

car_train_final$type_new <- ifelse(car_train_final$Type == "Small", "Small", 
                              ifelse(car_train_final$Type == "Midsize", "Large", "Other cars"))
car_train_final$type_new <- factor(car_train_final$type_new)

car_train_final$new_AirBags <- revalue(car_train_final$AirBags, c("Driver & Passenger" = "Yes",
                                                        "Driver only" = "Yes",
                                                        "None" = "None"))
car_train_final$new_AirBags <- factor(car_train_final$new_AirBags)
car_train_final$cylinders_new <- revalue(car_train_final$Cylinders, c("3" = "3-4",
                                                            "4" = "3-4", 
                                                            "5" = "Others",
                                                            "6" = "Others",
                                                            "8" = "Others",
                                                            "rotary" = "Others"))
car_train_final$cylinders_new <- factor(car_train_final$cylinders_new)

luxury_car_make <- names(which(tapply(exp(car_train_final$lPriceNew), car_train_final$Make, mean) >= 30000))
others_car_make <- names(which((tapply(exp(car_train_final$lPriceNew), car_train_final$Make, mean) <= 30000) &
                                 (tapply(exp(car_train_final$lPriceNew), car_train_final$Make, mean)>= 17000)))
cheap_car_make <- names(which(tapply(exp(car_train_final$lPriceNew), car_train_final$Make, mean) < 17000))

car_train_final$Luxury <- NA
car_train_final$Luxury[which(car_train_final$Make %in% luxury_car_make)] <- "Luxury"
car_train_final$Luxury[which(car_train_final$Make %in% others_car_make)] <- "Other"
car_train_final$Luxury[which(car_train_final$Make %in% cheap_car_make)] <- "Cheap"
car_train_final$Luxury <- factor(car_train_final$Luxury)

Japan <- c("Acura", "Honda", "Infiniti", "Lexus", "Mazda", "Mitsubishi", "Nissan", "Subaru", "Suzuki", "Toyota" )
Germany <- c("Audi", "BMW", "Ford", "Mercedes-Benz", "Volkswagen")
South_Korea <- c("Hyundai")
Sweden <- c("Saab", "Volvo")
car_train_final$country <-ifelse(car_train_final$Manufacturer %in% Sweden, "Sweden",
                            ifelse(car_train_final$Manufacturer %in% South_Korea, "South Korea", "Other Country"))
car_train_final$country <- factor(car_train_final$country)

car_train_final$EngineSize_cat <- cut(car_train_final$EngineSize,
                                 breaks = c(-Inf, 2.5, 6),
                                 labels = c("1-2.5", "2.5-6"))
car_train_final$EngineSize_cat <- factor(car_train_final$EngineSize_cat)
car_train_final$MPG_cat <- cut(car_train_final$MPG.highway,
                          breaks = c(-Inf, 26, 32, Inf),
                          labels = c("Below Average", "Average", "Good"))
car_train_final$MPG_cat <- factor(car_train_final$MPG_cat)

car_train_final <- transform(car_train_final, tWheelbase = (1/Wheelbase))

write.csv(car_train_final, "car_train_final.csv", row.names = FALSE)
