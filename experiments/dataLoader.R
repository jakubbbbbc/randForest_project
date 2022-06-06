library (dplyr)
library (tidyverse)

# function to get mode of column
calc_mode <- function(x){
  distinct_values <- unique(x[!is.na(x)])
  distinct_tabulate <- tabulate(match(x, distinct_values))
  distinct_values[which.max(distinct_tabulate)]
}

# planes train and test data
df = read.csv(file='data/planes_train.csv')
df = subset(df, select=-c(X, id))
names(df)[names(df) == 'satisfaction'] = 'target'
df$target = as.factor(as.numeric(df$target)-1)
df = df %>% mutate(across(everything(), ~replace_na(.x, calc_mode(.x))))
df = transform(df,
               Inflight.wifi.service=factor(Inflight.wifi.service),
               Departure.Arrival.time.convenient=factor(Departure.Arrival.time.convenient),
               Ease.of.Online.booking=factor(Ease.of.Online.booking),
               Gate.location=as.factor(Gate.location),
               Food.and.drink=as.factor(Food.and.drink),
               Online.boarding=as.factor(Online.boarding),
               Seat.comfort=as.factor(Seat.comfort),
               Inflight.entertainment=as.factor(Inflight.entertainment),
               On.board.service=as.factor(On.board.service),
               Leg.room.service=as.factor(Leg.room.service),
               Baggage.handling=as.factor(Baggage.handling),
               Checkin.service=as.factor(Checkin.service),
               Inflight.service=as.factor(Inflight.service),
               Cleanliness=as.factor(Cleanliness))
df_planes_train = df


df = read.csv(file='data/planes_test.csv')
df = subset(df, select=-c(X, id))
names(df)[names(df) == 'satisfaction'] = 'target'
df$target = as.factor(as.numeric(df$target)-1)
df = df %>% mutate(across(everything(), ~replace_na(.x, calc_mode(.x))))
df = transform(df,
               Inflight.wifi.service=factor(Inflight.wifi.service),
               Departure.Arrival.time.convenient=factor(Departure.Arrival.time.convenient),
               Ease.of.Online.booking=factor(Ease.of.Online.booking),
               Gate.location=as.factor(Gate.location),
               Food.and.drink=as.factor(Food.and.drink),
               Online.boarding=as.factor(Online.boarding),
               Seat.comfort=as.factor(Seat.comfort),
               Inflight.entertainment=as.factor(Inflight.entertainment),
               On.board.service=as.factor(On.board.service),
               Leg.room.service=as.factor(Leg.room.service),
               Baggage.handling=as.factor(Baggage.handling),
               Checkin.service=as.factor(Checkin.service),
               Inflight.service=as.factor(Inflight.service),
               Cleanliness=as.factor(Cleanliness))
levels(df$Gate.location) = c(0,1,2,3,4,5)
levels(df$Seat.comfort) = c(0,1,2,3,4,5)
levels(df$Checkin.service) = c(0,1,2,3,4,5)
df_planes_test = df



# job change data
df = read.csv(file='data/job_change.csv')
df = subset(df, select=-c(enrollee_id, city))
df[df == ''] = NA
df = df %>% mutate(across(everything(), ~replace_na(.x, calc_mode(.x))))
df = transform(df,
               gender=factor(gender),
               enrolled_university=factor(enrolled_university),
               education_level=factor(education_level),
               target=as.factor(target))
# for randomForest n(train)%n(test)=0
df = head(df, -158)
df_jobs = df

# for development and testing process
# df_test = df_jobs[0:5,c(1,2,3,4,12)]
# df1 = df_jobs[1:3000,]
# df2 = df_jobs[18001:19000,]
