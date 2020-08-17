#unit changer

#Input File
data = read.csv("./data/wfp_market_food_prices_inUSD.csv")
key_units = read.csv("./data/wfp_key_units_v2.csv")

for(row in 1:nrow(data)){
  value = data[row, "um_id"]
  unit = data[row, "um_name"]
  
  for(row2 in 1:nrow(key_units)){
    value2 = key_units[row2, "um_id"]
    if(value == value2){
      valueNew = key_units[row2, "new_um_id"]
      unitNew = key_units[row2,"new_um_name"]
      data[row, "um_id"] = valueNew;
      data[row, "um_name"] = unitNew;
      break
    }
  }
}
  
  
  #for(unit in key_units)
  #  if(i[4] == unit[0]){
  #    i[4] = unit[3];
  #    i[5] = unit[4];
  #  }

#Output File
write.csv(data,"./data/wfp_market_food_prices_inUSD_changed_units.csv")
