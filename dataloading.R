#dataloading

eumagine <- read_dta("Individual questionnaire - STUM 20121001 - incl hh and mgcount - mv.dta")


#Select variables used for analysis
eu <- eumagine %>%
  select(

        sweight,
        ## Filter variables
         ra, #research area (only Emirdag and Dinar)

        ## Dependent Variables
         a1, #ideally, if you had the opportunity, would you like to go abroad to live or work?
         a2, #which country would you like to go to?
       ## Independent Variables

         age, #age
         hh9, #marital status
         cf6, # Children - Do you have at least one child living in the household
         cf7, #Children - do you have a child who doesn't live in the household
         mg1, #Family Migration Experience -- "do you have any family members above 16 years old who are currently living in another country?"
        mg10, #other than the members of your household, do you have any family members above 16 years old, who have lived in another country?
       mg1count,
       mg2count,
         hh3, #Gender
         hh7, #years of schooling
       #Perception of human rights in Europe

         peu1, #the life of women in europe is...
         peu2, #the life of men in europe is...
         peu3, #schools in europe are...
         peu4, #health care in europe is...
         peu5, #the help from the government for poor people who need it in europe is...

       #Perception of human rights in their own country

         p1, # the life of women in [this country] is
         p2, # the life of men in [this country] is
         p3, # schools in this country are
         p4, # health care in this country is...
         p5, # the help from the government for poor people who need it in [this country] is...

       #Perception of job opportunities in their own country
         p8, #it is easy to find a good job in this country

       #perception of living and working conditions in Europe
         peu8, #it is easy to find a good job in europe

       #Perception of living and working conditions of migrants in europe
         a13, #going to live or working europe can be good for women
         a14, #going to live or work in Europe can be good for men
         a15, #most people who go to live or work in Europe become rich
         a16, #most people who go to or live in Europe gain valuable skills

       #wealth index
         w2, #electricity
         w3, #modern flush toilet
         w4, #running hot water
         w5, #shower in residence
         w6, #radio
         w7, #television
         w8, #satellite dish & receiver
         w9, #video/vcr
         w10, #telephone (landline or mobile)
         w11, # computer at home
         w12, #internet connection at home
         w13, #refrigerator
         w14, #gas/electric stove
         w15, #dishwasher
         w16, #air conditioning
         w17, #washing machine
         w18, #bicycle
         w19, #mobed
         w20 #car/truck/van
         )


# Filtering ---------------------------------------------------------------


# Only include respondents from Emirdag (21) & Dinar (22)
eu <- eu %>%
  filter(ra %in% c(21,22))




# Clean up ----------------------------------------------------------------


#Remove intermediate dataframes
rm(eumagine)




# Generate NA breakdown
na_breakdown <- eu %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "na_count_{col}")) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "na_count") %>%
  mutate(
    na_percentage = round((na_count / nrow(eu)) * 100, 2)
  ) %>%
  arrange(desc(na_count))

# View the NA breakdown
print(na_breakdown, n=100)

rm(na_breakdown)

