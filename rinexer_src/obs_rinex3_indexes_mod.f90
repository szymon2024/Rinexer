!> This module handles table indicies. It is responsible for assigning
!> a unique index to each satellite system.
!>
!> @version 1.0.1
module obs_rinex3_indexes_mod
  use obs_rinex3_types_mod, only: ip, sys_num_obs_types_t, epoch_record_t
  implicit none

  public

contains
  
  !-------------------------------------------------------------------
  ! SATELLITE SYSTEM INDEX
  !-------------------------------------------------------------------
  
  !> Converts a RINEX satellite system character code to a unique integer index.
  !> @param[in] sat_sys 1-character code (C, E, G, I, J, R, S)
  !> @return Integer index (1-7), or 0 if unknown
  pure integer(ip) function get_sys_index(sat_sys) result(i_sys)
    character(1), intent(in) :: sat_sys
    select case (sat_sys)
    case ('C'); i_sys = 1  ! Beidou
    case ('E'); i_sys = 2  ! Galileo
    case ('G'); i_sys = 3  ! GPS
    case ('I'); i_sys = 4  ! IRNSS / NavIC
    case ('J'); i_sys = 5  ! QZSS
    case ('R'); i_sys = 6  ! GLONASS
    case ('S'); i_sys = 7  ! SBAS
    case default; i_sys = 0 ! Unknown system
    end select
  end function get_sys_index

  !> Converts an integer index back to the standard RINEX satellite system character.
  !> @param[in] i_sys Integer index (1-7)
  !> @return 1-character code, or '?' if unknown
  pure character(1) function get_sat_sys(i_sys) result(sat_sys)
    integer(ip), intent(in) :: i_sys
    select case (i_sys)
    case (1); sat_sys = 'C'
    case (2); sat_sys = 'E'
    case (3); sat_sys = 'G'
    case (4); sat_sys = 'I'
    case (5); sat_sys = 'J'
    case (6); sat_sys = 'R'
    case (7); sat_sys = 'S'
    case default; sat_sys = '' ! Indicator of an error/unknown index
    end select
  end function get_sat_sys

  !-------------------------------------------------------------------
  ! OBSERVATION INDEX
  !-------------------------------------------------------------------

  !> Retrieves the observation index for a given satellite system and
  !> observation type. Returns 0 if the specified observation type is
  !> not present in the RINEX file header for that system.
  pure integer(ip) function get_obs_index(sat_sys, obs_type, A) result(i_obs)
    character(1),              intent(in) :: sat_sys
    character(len=3),          intent(in) :: obs_type
    type(sys_num_obs_types_t), intent(in) :: A(:)

    integer(ip) :: i_sys, i

    i_obs = 0
    i_sys = get_sys_index(sat_sys)

    if (i_sys > 0) then
       
       do i = 1, size(A(i_sys)%obs_types)
          if (A(i_sys)%obs_types(i) == obs_type) then
             i_obs = i
             return
          end if
       end do

    end if
    
  end function get_obs_index

  
  !-------------------------------------------------------------------
  ! PRN INDEX
  !-------------------------------------------------------------------
  
  !> Retrieves the prn index for a given satellite system and
  !> prn from epoch record. Returns 0 if the specified prn is
  !> not present in the epoch record for that system.
  pure integer(ip) function get_prn_index(sat_sys, prn, epr) result(i_prn)
    character(1),         intent(in) :: sat_sys
    integer(ip),          intent(in) :: prn
    type(epoch_record_t), intent(in) :: epr

    integer(ip) :: i_sys, i

    i_prn = 0
    i_sys = get_sys_index(sat_sys)

    if (i_sys > 0) then
       associate(A => epr%sats_obs)
         do i = 1, size(A(i_sys)%prns)
            if (A(i_sys)%prns(i) == prn) then
               i_prn = i
               return
            end if
         end do
       end associate
    end if

  end function get_prn_index
  
end module obs_rinex3_indexes_mod
