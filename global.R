library(dplyr)

allzips <- test
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zip

cleantable <- allzips %>%
  select(
    City = city,
    State = state,
    Zipcode = zip,
    Households = households,
    Exp_Count = exp_count,
    AAL = AAL,
    CTE_1 = CTE_1,
    TIV = TIV,
    Lat = latitude,
    Long = longitude
  )
