library(readxl)
library(reshape2)
library(tidyverse)

# Sharon data analysis
sheets <- excel_sheets("SHARON DATA ANALYSIS COMPUTATION FINAL.xlsx")
sharon <- read_excel("SHARON DATA ANALYSIS COMPUTATION FINAL.xlsx", 
                 col_names = FALSE, sheet = sheets[12])

# Remove row 1
sharon <- sharon[-c(1,2),]

# Selecting the needed columns
sharon <- sharon %>% 
  select(...2, ...4, 
         ...10,...11, ...12, ...13, ...14, ...15, ...16, ...17,
         ...21, ...22, ...23, ...24, ...25, ...26, ...27, ...28,
         ...32, ...33, ...34, ...35, ...36, ...37, ...38, ...39,
         ...43, ...44, ...45, ...46, ...47, ...48, ...49, ...50,
         ...54, ...55, ...56, ...57, ...58, ...59, ...60, ...61,
         ...65, ...66, ...67, ...68, ...69, ...70, ...71, ...72,
         ...76, ...77, ...78, ...79, ...80, ...81, ...82, ...83,
         ...85, ...86, ...87, ...88, ...89, ...90, ...91, ...92) 

# Plant Litter Quality
# Original
sharon$...10 = as.numeric(sharon$...10)
sharon$...11 = as.numeric(sharon$...11)
sharon$...12 = as.numeric(sharon$...12)
sharon$...13 = as.numeric(sharon$...13)
sharon$...14 = as.numeric(sharon$...14)
sharon$...15 = as.numeric(sharon$...15)
sharon$...16 = as.numeric(sharon$...16)
sharon$...17 = as.numeric(sharon$...17)

# Collection 1
sharon$...21 = as.numeric(sharon$...21)
sharon$...22 = as.numeric(sharon$...22)
sharon$...23 = as.numeric(sharon$...23)
sharon$...24 = as.numeric(sharon$...24)
sharon$...25 = as.numeric(sharon$...25)
sharon$...26 = as.numeric(sharon$...26)
sharon$...27 = as.numeric(sharon$...27)
sharon$...28 = as.numeric(sharon$...28)

# Collection 2
sharon$...32 = as.numeric(sharon$...32)
sharon$...33 = as.numeric(sharon$...33)
sharon$...34 = as.numeric(sharon$...34)
sharon$...35 = as.numeric(sharon$...35)
sharon$...36 = as.numeric(sharon$...36)
sharon$...37 = as.numeric(sharon$...37)
sharon$...38 = as.numeric(sharon$...38)
sharon$...39 = as.numeric(sharon$...39)

# Collection 3
sharon$...43 = as.numeric(sharon$...43)
sharon$...44 = as.numeric(sharon$...44)
sharon$...45 = as.numeric(sharon$...45)
sharon$...46 = as.numeric(sharon$...46)
sharon$...47 = as.numeric(sharon$...47)
sharon$...48 = as.numeric(sharon$...48)
sharon$...49 = as.numeric(sharon$...49)
sharon$...50 = as.numeric(sharon$...50)

# Collection 4
sharon$...54 = as.numeric(sharon$...54)
sharon$...55 = as.numeric(sharon$...55)
sharon$...56 = as.numeric(sharon$...56)
sharon$...57 = as.numeric(sharon$...57)
sharon$...58 = as.numeric(sharon$...58)
sharon$...59 = as.numeric(sharon$...59)
sharon$...60 = as.numeric(sharon$...60)
sharon$...61 = as.numeric(sharon$...61)


# Collection 5
sharon$...65 = as.numeric(sharon$...65)
sharon$...66 = as.numeric(sharon$...66)
sharon$...67 = as.numeric(sharon$...67)
sharon$...68 = as.numeric(sharon$...68)
sharon$...69 = as.numeric(sharon$...69)
sharon$...70 = as.numeric(sharon$...70)
sharon$...71 = as.numeric(sharon$...71)
sharon$...72 = as.numeric(sharon$...72)


# Collection 6
sharon$...76 = as.numeric(sharon$...76)
sharon$...77 = as.numeric(sharon$...77)
sharon$...78 = as.numeric(sharon$...78)
sharon$...79 = as.numeric(sharon$...79)
sharon$...80 = as.numeric(sharon$...80)
sharon$...81 = as.numeric(sharon$...81)
sharon$...82 = as.numeric(sharon$...82)
sharon$...83 = as.numeric(sharon$...83)



# sharon wide to long format
sharon_plq = melt(sharon,
     id.vars = c("...2","...4"),
     measure.vars = c("...10","...11","...12","...13","...14","...15","...16","...17",
                      "...21", "...22", "...23", "...24", "...25","...26",
                      "...27", "...28", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39",
                      "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50",
                      "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61",
                      "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72",
                      "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83"),
     variable.name = c("plq"),
     value.name = c("plq_amount")
     ) 

# Factorial data convertion 
sharon_plq$...2 = factor(sharon_plq$...2)  
sharon_plq$...4 = factor(sharon_plq$...4)  

sharon_plq = sharon_plq %>% 
  mutate(plq,fct_recode(plq,
original.om="...10",original.c="...11",original.n="...12",original.hemi="...13",original.cell="...14",original.lig="...15",original.cn="...16",original.ln="...17",
om1="...21",c1="...22",n1="...23",hemi1="...24",cell1="...25",lig1="...26",cn1="...27",ln1="...28",
om2="...32",c2="...33",n2="...34",hemi2="...35",cell2="...36",lig2="...37",cn2="...38",ln2="...39", 
om3="...43",c3="...44",n3="...45",hemi3="...46",cell3="...47",lig3="...48",cn3="...49",ln3="...50",
om4="...54",c4="...55",n4="...56",hemi4="...57",cell4="...58",lig4="...59",cn4="...60",ln4="...61",
om5="...65",c5="...66",n5="...67",hemi5="...68",cell5="...69",lig5="...70",cn5="...71",ln5="...72",
om6="...76",c6="...77",n6="...78",hemi6="...79",cell6="...80",lig6="...81",cn6="...82",ln6="...83",)) %>% 
  select(-plq) %>% 
  rename("degradation"="...2",
         "treatment"="...4",
         "plq"=`fct_recode(...)`)

# Removing Duplicates
sharon_plq = sharon_plq[!duplicated(sharon_plq),]
  

# Plant litter decomposition
# sharon_decomp
sharon_decomp = sharon %>% 
  select(...2, ...4, ...85, ...86, ...87, ...88, 
         ...89, ...90, ...91, ...92)

# Numerical data convertion
sharon_decomp$...85 = as.numeric(sharon$...85)
sharon_decomp$...86 = as.numeric(sharon$...86)
sharon_decomp$...87 = as.numeric(sharon$...87)
sharon_decomp$...88 = as.numeric(sharon$...88)
sharon_decomp$...89 = as.numeric(sharon$...89)
sharon_decomp$...90 = as.numeric(sharon$...90)
sharon_decomp$...91 = as.numeric(sharon$...91)
sharon_decomp$...92 = as.numeric(sharon$...92)

# Factorial Data convertion
sharon_decomp$...2 = factor(sharon_decomp$...2)  
sharon_decomp$...4 = factor(sharon_decomp$...4)  


# Renaming 
sharon_decomp = sharon_decomp %>% 
  rename("degradation"="...2","treatment"="...4",
    "om.k6"="...85","c.k6"="...86", "n.k6"="...87","hemi.k6"="...88",
    "cell.k6"="...89","lig.k6"="...90","cn.k6"="...91","ln.k6"="...92",
         )


# Decomposition Data
sharon_decomp = sharon_decomp %>% 
  pivot_longer(cols = om.k6:ln.k6, 
               names_to = "components", values_to = "rates")

# Factorial Data convertion
sharon_decomp$components = as.factor(sharon_decomp$components)

# Removing Duplicates
sharon_decomp = sharon_decomp[!duplicated(sharon_decomp),]


# Exporting the clean data
# write_csv(sharon_plq, "sharon_plq.csv") # plant litter quality
# write_csv(sharon_decomp, "sharon_decomposition.csv") # plant litter decomposition
  