#################################################################################################
################################ WOCBP vs RETINOID vs RAM counts ################################
#################################################################################################
# Tests
# 1. WOCBP counts >= Retinoid user counts
# 2. Retinoid users counts >= RAM users counts

WOCBP.total<-population_counts_flowchart[names_for_flowchart=="population_WOCBP", values_for_flowchart]
retinoid.users.in.WOCBP<-population_counts_flowchart[names_for_flowchart=="population_retinoid.users.in.WOCBP", values_for_flowchart]
RAM.users.in.retinoid.pop<-population_counts_flowchart[names_for_flowchart=="population_RAM.users.within.retinoid.users", values_for_flowchart]
RAM.users.in.retinoid.pop.acne<-population_counts_flowchart[names_for_flowchart=="population_RAM.users.within.retinoid.users_acne", values_for_flowchart]
RAM.users.in.retinoid.pop.derm<-population_counts_flowchart[names_for_flowchart=="population_RAM.users.within.retinoid.users_derm", values_for_flowchart]
RAM.users.in.retinoid.pop.psor<-population_counts_flowchart[names_for_flowchart=="population_RAM.users.within.retinoid.users_psor", values_for_flowchart]

WOCBP_morethan_retinoid.users<-WOCBP.total-retinoid.users.in.WOCBP>=0
retinoid.users_morethan_RAM.pop.in.retinoid.users<-retinoid.users.in.WOCBP-RAM.users.in.retinoid.pop>=0
RAM_indication_counts_equal_RAM.pop.in.retinoid.users<-RAM.users.in.retinoid.pop.acne+RAM.users.in.retinoid.pop.derm+RAM.users.in.retinoid.pop.psor == RAM.users.in.retinoid.pop

# flowchart 
names<-c(
  "WOCBP_morethan_retinoid.users",
  "retinoid.users_morethan_RAM.pop.in.retinoid.users",
  "RAM_indication_counts_equal_RAM.pop.in.retinoid.users"
  
)
values<-c(
  WOCBP_morethan_retinoid.users,
  retinoid.users_morethan_RAM.pop.in.retinoid.users,
  RAM_indication_counts_equal_RAM.pop.in.retinoid.users
  
)

flowchart_population_comparisons<-data.table(names, values)
View(flowchart_population_comparisons)