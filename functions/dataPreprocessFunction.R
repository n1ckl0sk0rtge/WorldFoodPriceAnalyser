
preprocessData <- function(data, selectedProducts, selectedYears, selectedCountries){
  
  # filter countrys and product
  if (length( selectedCountries) == 0) {
    productData <- filter(data, product == selectedProducts)
  } else {
    productData <- filter(data, product == selectedProducts & country %in%  selectedCountries)
  }
  
  # filter years
  startYear <- paste( selectedYears[1], "-01-01", sep="")
  endYear <- paste( selectedYears[2], "-01-01", sep="")
  productDataInYears <- filter(productData, year >= startYear & year <= endYear)
  productDataInYears$year <- as.Date(productDataInYears$year)
  
  return(productDataInYears)
}