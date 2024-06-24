Folder PATH listing for volume Windows-SSD
Volume serial number is 425B-2CA8
C:.  
+---CDMInstances
|   \---LOT4
|       \---BIFAP
|           +---AR
|           |       CDM_SOURCE.csv
|           |       EVENTS.csv
|           |       INSTANCE.csv
|           |       MEDICINES.csv
|           |       METADATA.csv
|           |       OBSERVATION_PERIODS_sub.csv
|           |       PERSONS.csv
|           |       
|           +---AS
|           |       CDM_SOURCE.csv
|           |       EVENTS.csv
|           |       INSTANCE.csv
|           |       MEDICINES.csv
|           |       METADATA.csv
|           |       OBSERVATION_PERIODS_sub.csv
|           |       PERSONS.csv
|           |       
|           +---CA
|           |       CDM_SOURCE.csv
|           |       EVENTS.csv
|           |       INSTANCE.csv
|           |       MEDICINES.csv
|           |       METADATA.csv
|           |       OBSERVATION_PERIODS_sub.csv
|           |       PERSONS.csv
|           |       
|           +---CL
|           |       CDM_SOURCE.csv
|           |       EVENTS.csv
|           |       INSTANCE.csv
|           |       MEDICINES.csv
|           |       METADATA.csv
|           |       OBSERVATION_PERIODS_sub.csv
|           |       PERSONS.csv
|           |       
|           +---CM
|           |       CDM_SOURCE.csv
|           |       EVENTS.csv
|           |       INSTANCE.csv
|           |       MEDICINES.csv
|           |       METADATA.csv
|           |       OBSERVATION_PERIODS_sub.csv
|           |       PERSONS.csv
|           |       
|           +---CN
|           |       CDM_SOURCE.csv
|           |       EVENTS.csv
|           |       INSTANCE.csv
|           |       MEDICINES.csv
|           |       METADATA.csv
|           |       OBSERVATION_PERIODS_sub.csv
|           |       PERSONS.csv
|           |       
|           +---MA
|           |       CDM_SOURCE.csv
|           |       EVENTS.csv
|           |       INSTANCE.csv
|           |       MEDICINES.csv
|           |       METADATA.csv
|           |       OBSERVATION_PERIODS_sub.csv
|           |       PERSONS.csv
|           |       
|           +---MU
|           |       CDM_SOURCE.csv
|           |       EVENTS.csv
|           |       INSTANCE.csv
|           |       MEDICINES.csv
|           |       METADATA.csv
|           |       OBSERVATION_PERIODS_sub.csv
|           |       PERSONS.csv
|           |       
|           \---NA
|                   CDM_SOURCE.csv
|                   EVENTS.csv
|                   INSTANCE.csv
|                   MEDICINES.csv
|                   METADATA.csv
|                   OBSERVATION_PERIODS_sub.csv
|                   PERSONS.csv
|                   
\---RAM_scripts
    |   1_to_run_source_pop_counts.R
    |   2_to_run_final_counts.R
    |   99_path.R
    |   
    \---p_steps
        |   flowchart.R
        |   flowchart_base_population.R
        |   info.R
        |   packages.R
        |   
        +---baseline
        |       baseline_tables.R
        |       
        +---codelists
        |       ATC_lot4_formatted.xlsx
        |       Lot4_completediagnosis_codelist_20211110.xlsx
        |       Procedure_codes.xlsx
        |       
        +---conceptsets
        |       create_concept_sets_ATC.R
        |       create_concept_sets_dx_codes.R
        |       create_concept_sets_procedure_codes.R
        |       excluded_ICD.R
        |       
        +---counts
        |       counts_by_indication.R
        |       IndividualRAMCounts.R
        |       monthly_counts_ATC.R
        |       RAM_contraindicated.R
        |       RAM_general_concomitance_counts.R
        |       RAM_incidence_prevalence_discontinuation.R
        |       RAM_switching.R
        |       RAM_teratogenic.R
        |       retinoid_incidence_prevalence_discontinuation.R
        |       
        +---denominators
        |       create_entry_exit.R
        |       denominator_monthly_retinoid_users.R
        |       denominator_monthly_WOCBP.R
        |       load_denominator.R
        |       
        +---functions
        |       CountPersonTimeV13.6.R
        |       CreateSpells_v15.R
        |       FUNCTIONS.R
        |       LoadCodelist.R
        |       
        +---intermediate
        |       run_counts_final.R
        |       run_counts_final_each_pop.R
        |       run_counts_prelim.R
        |       run_counts_prelim_each_pop.R
        |       
        +---notinuse
        |       denominator_monthly_retinoid_users.R
        |       RAM_contraindicated-old.R
        |       RAM_teratogenic_old.R
        |       
        +---parameters
        |       DAP_specific_assumed_durations.R
        |       RAM_codes_per_indication.R
        |       set_DAP_params.R
        |       study_parameters.R
        |       
        +---sterility
        |       create_sterility_list.R
        |       
        +---studypopulation
        |       Step_00_SetParameters.R
        |       Step_01_CreateSpells.R
        |       Step_02_PreparePersonsTable.R
        |       Step_03_CreateSourceTable.R
        |       Step_04_CreateStudyPopulation.R
        |       Step_05_AddVariablesSourcePopulation.R
        |       Step_06_AddVariablesStudyPopulation.R
        |       Step_07_RunCountPersonTime.R
        |       study_source_population_script.R
        |       
        \---treatmentepisodes
                RAM_treatment_episodes.R
                retinoid_treatment_episodes.R
                
