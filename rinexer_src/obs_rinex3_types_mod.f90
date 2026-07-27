!> This module defines data structures and types for handling RINEX
!> version 3.x observation files.
!>
!> @version 1.0.1
module obs_rinex3_types_mod
  use, intrinsic :: iso_fortran_env, only: wp => real64, ip => int32
  implicit none

  public

  ! MAX SYS                 - Maximum number of satellite systems
  ! MAX_VIS_SATS_PER_SYS    - Maximum number of visible satellites per satellite system
  integer(ip), parameter :: MAX_SYS = 7
  integer(ip), parameter :: MAX_VIS_SATS_PER_SYS = 36

  
  !------------------------------------------------------------------
  ! HEADER TYPES
  !------------------------------------------------------------------

  type :: rinex_version_type_t
     real(wp)     :: version = 0.0_wp    !< Format version e.g. 3.04
     character(1) :: type    = " "       !< File type: O for observation
     character(1) :: sat_sys = " "       !< Satellite system: C, E, G, I, J, R, S, M
  end type rinex_version_type_t

  type :: pgm_run_by_date_t
     character(20) :: pgm = " " , run_by = " ", date  = " "
  end type pgm_run_by_date_t

  type :: marker_name_t
     character(60) :: name = " "
  end type marker_name_t

  type :: marker_number_t
     character(20) :: number = " "
     logical :: is_present = .false.
  end type marker_number_t

  type :: marker_type_t
     character(20) :: type = " "
     logical :: is_present = .false.
  end type marker_type_t

  type :: observer_agency_t
     character(20) :: observer = " "
     character(40) :: agency = " "
  end type observer_agency_t

  type :: rec_num_type_vers_t
     character(20) :: num  = " ", type = " ", vers = " "
  end type rec_num_type_vers_t

  type :: ant_num_type_t
     character(20) :: num  = " ", type = " "
  end type ant_num_type_t

  type :: approx_position_xyz_t
     real(wp) :: x = 0.0_wp, y = 0.0_wp, z = 0.0_wp
  end type approx_position_xyz_t

  type :: antenna_delta_hen_t
     real(wp) :: h = 0.0_wp, e = 0.0_wp, n = 0.0_wp
  end type antenna_delta_hen_t

  type :: antenna_delta_xyz_t
     real(wp) :: dx = 0.0_wp, dy = 0.0_wp, dz = 0.0_wp
     logical  :: is_present = .false.
  end type antenna_delta_xyz_t

  type :: phasecenter_t
     character(3) :: obs_type = " "
     real(wp)     :: xyz_neu(3) = [0.0_wp, 0.0_wp, 0.0_wp]
  end type phasecenter_t

  type :: sys_antenna_phasecenters_t
     character(1) :: sat_sys  = " "
     integer(ip)  :: n_obs_types = 0
     type(phasecenter_t), allocatable :: phasecenters(:)
  end type sys_antenna_phasecenters_t
  
  type :: antenna_bsight_xyz_t
     real(wp) :: x = 0.0_wp, y = 0.0_wp, z = 0.0_wp
     logical  :: is_present = .false.
  end type antenna_bsight_xyz_t

  type :: antenna_zerodir_azi_t
     real(wp) :: azi = 0.0_wp
     logical  :: is_present = .false.
  end type antenna_zerodir_azi_t

  type :: antenna_zerodir_xyz_t
     real(wp) :: x = 0.0_wp, y = 0.0_wp, z = 0.0_wp
     logical :: is_present = .false.
  end type antenna_zerodir_xyz_t

  type :: center_of_mass_xyz_t
     real(wp) :: x = 0.0_wp, y = 0.0_wp, z = 0.0_wp
     logical  :: is_present = .false.
  end type center_of_mass_xyz_t

  type :: sys_num_obs_types_t
     character(1) :: sat_sys = " "          !< Satellite system: C, E, G, I, J, R, S
     integer(ip)  :: n_obs_types = 0               
     character(3), allocatable :: obs_types(:)
  end type sys_num_obs_types_t

  type :: signal_strength_unit_t
     character(20) :: unit = " "
     logical :: is_present = .false.
  end type signal_strength_unit_t

  type :: interval_t
     real(wp) :: interval = 0.0_wp
     logical :: is_present = .false.
  end type interval_t

  type :: time_of_first_obs_t
     integer(ip)  :: year=0, month=0, day=0, hour=0, min=0
     real(wp)     :: sec=0.0_wp
     character(3) :: time_system = " "
  end type time_of_first_obs_t

  type :: time_of_last_obs_t
     integer(ip)  :: year=0, month=0, day=0, hour=0, min=0
     real(wp)     :: sec=0.0_wp
     character(3) :: time_system = " "
     logical :: is_present = .false.
  end type time_of_last_obs_t

  type :: rcv_clock_offs_appl_t
     integer(ip) :: offs = 0
     logical :: is_present = .false.
  end type rcv_clock_offs_appl_t

  type :: sys_dcbs_applied_t
     character(1) :: sat_sys    = " "
     character(17):: prog_name  = " "
     character(40):: source_url = " "
  end type sys_dcbs_applied_t

  type :: sys_pcvs_applied_t
     character(1) :: sat_sys = " "
     character(17):: prog_name  = " "
     character(40):: source_url = " "
  end type sys_pcvs_applied_t

  type :: scale_factor_t
     integer(ip)  :: factor = 1
     integer(ip)  :: n_obs_types = 0 
     character(3), allocatable :: obs_types(:)
  end type scale_factor_t

  type :: sys_scale_factors_t
     character(1) :: sat_sys = " "
     integer(ip)  :: n_scale_factors = 0
     type(scale_factor_t), allocatable :: scale_factors(:)
  end type sys_scale_factors_t

  type :: phase_shift_t
     character(3)  :: obs_type = " "
     real(wp)      :: shift = 0.0_wp
     integer(ip)   :: n_sats = 0                
     character(3), allocatable :: sats(:)
  end type phase_shift_t

  type :: sys_phase_shifts_t
     character(1) :: sat_sys  = " "
     integer(ip) :: n_phase_shifts = 0
     type(phase_shift_t), allocatable :: phase_shifts(:)
  end type sys_phase_shifts_t
     
  type :: slot_frq_t
     integer(ip) :: slot=0, frq=0
  end type slot_frq_t

  type :: glonass_slot_frq_num_t   ! slot means prn
     integer(ip) :: n_slots = 0
     type(slot_frq_t), allocatable :: slot_frq(:)
     logical :: is_present = .false.
  end type glonass_slot_frq_num_t

  type :: glonass_cod_phs_bis_t
     character(3) :: obs_type(4) = " "
     real(wp)     :: bias(4)     = 0.0_wp
     logical :: is_present = .false.
  end type glonass_cod_phs_bis_t

  type :: leap_seconds_t
     integer(ip)  :: current = 0
     integer(ip)  :: future  = 0
     integer(ip)  :: week    = 0
     integer(ip)  :: day     = 0
     character(3) :: time_system = " "
     logical      :: is_present = .false.
  end type leap_seconds_t

  type :: num_of_satellites_t
     integer(ip) :: n_sat = 0
     logical     :: is_present = .false.
  end type num_of_satellites_t

  type :: sys_prn_num_of_obs_t
     character(1) :: sat_sys = " "
     integer(ip)  :: n_prns = 0
     integer(ip), allocatable :: prns(:)
     integer(ip), allocatable :: prn_num_of_obs(:,:)  ! Matrix (n_obs_types, n_prns) 
  end type sys_prn_num_of_obs_t

  !> Header filed names corespond to record names from RINEX
  !> specification table A2.
  !> Names of types are adjusted to say what they contain.
  type :: header_t
     type(rinex_version_type_t)        :: rinex_version_type     
     type(pgm_run_by_date_t)           :: pgm_run_by_date        
     type(marker_name_t)               :: marker_name            
     type(marker_number_t)             :: marker_number      
     type(marker_type_t)               :: marker_type        
     type(observer_agency_t)           :: observer_agency        
     type(rec_num_type_vers_t)         :: rec_num_type_vers      
     type(ant_num_type_t)              :: ant_num_type           
     type(approx_position_xyz_t)       :: approx_position_xyz    
     type(antenna_delta_hen_t)         :: antenna_delta_hen      
     type(antenna_delta_xyz_t)         :: antenna_delta_xyz  
     type(sys_antenna_phasecenters_t)  :: antenna_phasecenter(MAX_SYS)
     type(antenna_bsight_xyz_t)        :: antenna_bsight_xyz  
     type(antenna_zerodir_azi_t)       :: antenna_zerodir_azi
     type(antenna_zerodir_xyz_t)       :: antenna_zerodir_xyz
     type(center_of_mass_xyz_t)        :: center_of_mass_xyz
     type(sys_num_obs_types_t)         :: sys_num_obs_types(MAX_SYS)
     type(signal_strength_unit_t)      :: signal_strength_unit
     type(interval_t)                  :: interval
     type(time_of_first_obs_t)         :: time_of_first_obs
     type(time_of_last_obs_t)          :: time_of_last_obs
     type(rcv_clock_offs_appl_t)       :: rcv_clock_offs_appl
     type(sys_dcbs_applied_t)          :: sys_dcbs_applied(MAX_SYS)
     type(sys_pcvs_applied_t)          :: sys_pcvs_applied(MAX_SYS)
     type(sys_scale_factors_t)         :: sys_scale_factor(MAX_SYS)
     type(sys_phase_shifts_t)          :: sys_phase_shift(MAX_SYS)
     type(glonass_slot_frq_num_t)      :: glonass_slot_frq_num
     type(glonass_cod_phs_bis_t)       :: glonass_cod_phs_bis
     type(leap_seconds_t)              :: leap_seconds
     type(num_of_satellites_t)         :: num_of_satellites
     type(sys_prn_num_of_obs_t)        :: prn_num_of_obs(MAX_SYS)
  end type header_t

  
  !------------------------------------------------------------------
  ! OBSERVATION TYPES (DATA TYPES)
  !------------------------------------------------------------------

  !> Single observation
  type obs_t
     real(wp)    :: val = 0.0_wp
     integer(ip) :: lli = 0, ssi = 0
  end type obs_t


  !> Structure holding all observations for a specific satellite
  !> system within an epoch.
  type :: sys_sats_obs_t
     character(1) :: sat_sys = " "
     integer(ip)  :: n_prns = 0
     integer(ip)  :: prns(MAX_VIS_SATS_PER_SYS) = 0
     type(obs_t),  allocatable :: sys_sats_obs(:,:)    ! Matrix:(i_obs_type, i_prn)
  end type sys_sats_obs_t


  !> Core structure containing full epoch information and observations
  !> by satellite system and satellites.
  type epoch_record_t
     integer(ip)          :: year = 0, month = 0, day = 0, hour = 0, min = 0
     real(wp)             :: sec = 0.0_wp
     integer(ip)          :: flag  = 0
     integer(ip)          :: n_sats = 0
     real(wp)             :: rcv_clk_offset = 0.0_wp
     type(sys_sats_obs_t) :: sats_obs(MAX_SYS)
  end type epoch_record_t
  
end module obs_rinex3_types_mod
