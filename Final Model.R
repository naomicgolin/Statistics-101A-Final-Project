model_final <- lm(lPriceNew ~ Luxury * Turn.circle + type_new * new_AirBags +
  lHorsepower * Man.trans.avail + country + MPG_cat +
  EngineSize_cat * tWheelbase, data = car_train_final)
