! DUMMY FILE

MODULE mo_m4ago

     IMPLICIT NONE

     PRIVATE
     
     ! Public subroutines
     PUBLIC :: mean_aggregate_sinking_speed, init_m4ago_nml_params, init_m4ago_params, alloc_mem_m4ago
     
     ! Public fields and parameters
     PUBLIC :: ws_agg, POM_remin_q10, POM_remin_Tref, opal_remin_q10, opal_remin_Tref, &
             & aggregate_diagnostics,kav_dp,kav_rho_p,kav_d_C,kws_agg,kdf_agg,kstickiness_agg,kb_agg,kstickiness_frustule, &
             & kLmax_agg,kdynvis,kav_rhof_V,kav_por_V   
 
     REAL :: POM_remin_q10,POM_remin_Tref,opal_remin_q10,opal_remin_Tref
     REAL,ALLOCATABLE :: ws_agg(:,:,:)

     INTEGER, PARAMETER :: &
       kav_dp               =  1, & 
       kav_rho_p            =  2, &
       kav_d_C              =  3, &
       kws_agg              =  4, &
       kdf_agg              =  5, &
       kstickiness_agg      =  6, &
       kb_agg               =  7, &
       kstickiness_frustule =  8, &
       kLmax_agg            =  9, &
       kdynvis              = 10, &
       kav_rhof_V           = 11, &
       kav_por_V            = 12, &
       naggdiag             = 12

     REAL, DIMENSION (:,:,:,:), ALLOCATABLE, TARGET :: aggregate_diagnostics

   CONTAINS

  !===================================================================================== m4ago_init_params
  SUBROUTINE init_m4ago_nml_params
      POM_remin_q10     = 2.1 ! Bidle et al. 2002: Regulation of Oceanic Silicon...
      POM_remin_Tref    = 10.
  END SUBROUTINE init_m4ago_nml_params

  SUBROUTINE init_m4ago_params

  END SUBROUTINE init_m4ago_params

  SUBROUTINE alloc_mem_m4ago(kpie, kpje, kpke)
     IMPLICIT NONE

     INTEGER, INTENT(in)  :: kpie                  !< 1st REAL of model grid.
     INTEGER, INTENT(in)  :: kpje                  !< 2nd REAL of model grid.
     INTEGER, INTENT(in)  :: kpke                  !< 3rd (vertical) REAL of model grid.

     ! allocate memory space for aggregate properties 
     ALLOCATE(aggregate_diagnostics(kpie, kpje, kpke, naggdiag))
     
     ! mean sinking velocity
     ALLOCATE(ws_agg(kpie,kpje,kpke))

     aggregate_diagnostics = 0.

  END SUBROUTINE alloc_mem_m4ago

  SUBROUTINE mean_aggregate_sinking_speed(kpie, kpje, kpke, kbnd, pddpo, omask, ptho, psao, ppao, prho)
     IMPLICIT NONE
     
     INTEGER, INTENT(in)  :: kpie                  !< 1st REAL of model grid.
     INTEGER, INTENT(in)  :: kpje                  !< 2nd REAL of model grid.
     INTEGER, INTENT(in)  :: kpke                  !< 3rd (vertical) REAL of model grid.
     INTEGER, INTENT(in)  :: kbnd
     REAL, INTENT(in) :: pddpo(kpie,kpje,kpke) !< size of scalar grid cell (3rd dimension) [m]
     REAL, INTENT(in) :: omask(kpie,kpje) 
     REAL, INTENT(in) :: ptho (1-kbnd:kpie+kbnd,1-kbnd:kpje+kbnd,kpke) !< potential temperature [deg C]
     REAL, INTENT(in) :: psao (1-kbnd:kpie+kbnd,1-kbnd:kpje+kbnd,kpke) !< salinity [psu.].
     REAL, INTENT(in) :: ppao (1-kbnd:kpie+kbnd,1-kbnd:kpje+kbnd) !< pressure at sea level [Pa].
     REAL, INTENT(in) :: prho (kpie,kpje,kpke) !< density [g/cm3]
  END SUBROUTINE mean_aggregate_sinking_speed

END MODULE mo_m4ago
