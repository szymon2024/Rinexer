!> This module handles writing data to an observation RINEX 3.x format
!> file or to the screen.
!>
!> @version 1.0.1
module obs_rinex3_writer_mod
  use, intrinsic :: iso_fortran_env, only: iostat_end, output_unit
  use obs_rinex3_types_mod
  use obs_rinex3_indexes_mod, only: get_sys_index, get_sat_sys
  
  implicit none

  public
  
contains

  !-------------------------------------------------------------------
  ! WRITTING HEADER
  !-------------------------------------------------------------------
  
  subroutine write_rinex_version_type(unit, r)
    integer(ip),                intent(in) :: unit
    type(rinex_version_type_t), intent(in) :: r

    write(unit,'(F9.2, 11X, A1, 19X, A1, T61, A)') &
       r%version, r%type, r%sat_sys, "RINEX VERSION / TYPE"
  end subroutine write_rinex_version_type

  
  subroutine write_pgm_run_by_date(unit, r)
    integer(ip),             intent(in) :: unit
    type(pgm_run_by_date_t), intent(in) :: r

    write(unit, '(A20, A20, A20, A)') &
         r%pgm, r%run_by, r%date, "PGM / RUN BY / DATE"
  end subroutine write_pgm_run_by_date

  
  subroutine write_marker_name(unit, r)
    integer(ip),         intent(in) :: unit
    type(marker_name_t), intent(in) :: r
    
    write(unit, '(A60, A)') r%name, "MARKER NAME"
  end subroutine write_marker_name

  
  subroutine write_marker_number(unit, r)
    integer(ip),           intent(in) :: unit
    type(marker_number_t), intent(in) :: r
    
    write(unit, '(A20, 40X, A)') r%number, "MARKER NUMBER"
  end subroutine write_marker_number

  
  subroutine write_marker_type(unit, r)
    integer(ip),         intent(in) :: unit
    type(marker_type_t), intent(in) :: r
    
    write(unit, '(A20, 40X, A)') r%type, "MARKER TYPE"
  end subroutine write_marker_type


  subroutine write_observer_agency(unit, r)
    integer(ip),             intent(in) :: unit
    type(observer_agency_t), intent(in) :: r
    
    write(unit, '(A20, A40, A)') &
         r%observer, r%agency, "OBSERVER / AGENCY"
  end subroutine write_observer_agency
  

  subroutine write_rec_num_type_vers(unit, r)
    integer(ip),               intent(in) :: unit
    type(rec_num_type_vers_t), intent(in) :: r

    write(unit, '(A20, A20, A20, A)') &
         r%num, r%type, r%vers, "REC # / TYPE / VERS"
  end subroutine write_rec_num_type_vers

  
  subroutine write_ant_num_type(unit, r)
    integer(ip),          intent(in) :: unit
    type(ant_num_type_t), intent(in) :: r
    
    write(unit, '(A20, A20, T61, A)') &
         r%num, r%type, "ANT # / TYPE"
  end subroutine write_ant_num_type


  subroutine write_approx_position_xyz(unit, r)
    integer(ip),                 intent(in) :: unit
    type(approx_position_xyz_t), intent(in) :: r

    write(unit, '(F14.4, F14.4, F14.4, T61, A)') &
         r%x, r%y, r%z, "APPROX POSITION XYZ"
  end subroutine write_approx_position_xyz


  subroutine write_antenna_delta_hen(unit, r)
    integer(ip),               intent(in) :: unit
    type(antenna_delta_hen_t), intent(in) :: r

    write(unit, '(F14.4, F14.4, F14.4, T61, A)') &
         r%h, r%e, r%n, "ANTENNA: DELTA H/E/N"
  end subroutine write_antenna_delta_hen

  subroutine write_antenna_delta_xyz(unit, r)
    integer(ip),               intent(in) :: unit
    type(antenna_delta_xyz_t), intent(in) :: r

    write(unit, '(3F14.4, T61, A)') &
         r%dx, r%dy, r%dz, "ANTENNA: DELTA X/Z/Z"
  end subroutine write_antenna_delta_xyz

       
  subroutine write_antenna_phasecenters(unit, A)
    integer(ip),                      intent(in) :: unit
    type(sys_antenna_phasecenters_t), intent(in) :: A(:)

    integer(ip) :: i, j

    do i = 1, size(A)
       if (.not. allocated(A(i)%phasecenters)) cycle

       do j = 1, size(A(i)%phasecenters)
          write(unit, '(A1, 1X, A3, F9.4, F14.4, F14.4, T61, A)') &
               A(i)%sat_sys, A(i)%phasecenters(j)%obs_type, &
               A(i)%phasecenters(j)%xyz_neu, "ANTENNA:PHASECENTER"
       end do
       
    end do
  end subroutine write_antenna_phasecenters

  
  subroutine write_antenna_bsight_xyz(unit, r)
    integer(ip),                intent(in) :: unit
    type(antenna_bsight_xyz_t), intent(in) :: r

    write(unit, '(3F14.4, T61, A)') &
         r%x, r%y, r%z, "ANTENNA: B.SIGHT XYZ"
  end subroutine write_antenna_bsight_xyz

  
  subroutine write_antenna_zerodir_azi(unit, r)
    integer(ip),                 intent(in) :: unit
    type(antenna_zerodir_azi_t), intent(in) :: r
    
    write(unit, '(F14.4, T61, A)') r%azi, "ANTENNA: ZERODIR AZI"
  end subroutine write_antenna_zerodir_azi

  
  subroutine write_antenna_zerodir_xyz(unit, r)
    integer(ip),                 intent(in) :: unit
    type(antenna_zerodir_xyz_t), intent(in) :: r

    write(unit, '(3F14.4, T61, A)') &
         r%x, r%y, r%z, "ANTENNA: ZERODIR XYZ"
  end subroutine write_antenna_zerodir_xyz


  subroutine write_center_of_mass_xyz(unit, r)
    integer(ip),                intent(in) :: unit
    type(center_of_mass_xyz_t), intent(in) :: r

    write(unit, '(3F14.4, T61, A)') &
        r%x, r%y, r%z, "CENTER OF MASS: XYZ"
  end subroutine write_center_of_mass_xyz


  subroutine write_sys_num_obs_types(unit, A)
    integer(ip),               intent(in) :: unit
    type(sys_num_obs_types_t), intent(in) :: A(:)

    integer(ip)       :: i

    do i = 1, size(A)
       if (A(i)%sat_sys == " ") cycle
       
       write(unit, '(T61, "SYS / # / OBS TYPES", T1, A1, 2X, I3, 13(1X, A3) : / &
            & (T61, "SYS / # / OBS TYPES", T1, 6X, 13(1X, A3)))') &
            A(i)%sat_sys, A(i)%n_obs_types, A(i)%obs_types
    end do
    
  end subroutine write_sys_num_obs_types


  subroutine write_signal_strength_unit(unit, r)
    integer(ip),                  intent(in) :: unit
    type(signal_strength_unit_t), intent(in) :: r
    
    write(unit, '(A20, 40X, A)') r%unit, "SIGNAL STRENGTH UNIT"
  end subroutine write_signal_strength_unit


  subroutine write_interval(unit, r)
    integer(ip),      intent(in) :: unit
    type(interval_t), intent(in) :: r
    
    write(unit, '(F10.3, T61, A)') r%interval, "INTERVAL"
  end subroutine write_interval


  subroutine write_time_of_first_obs(unit, r)
    integer(ip),               intent(in) :: unit
    type(time_of_first_obs_t), intent(in) :: r
    write(unit, '(5(I6),F13.7,5X,A3,T61, A)') &
         r%year, r%month, r%day, &
         r%hour, r%min, r%sec, &
         r%time_system, "TIME OF FIRST OBS"
  end subroutine write_time_of_first_obs


  subroutine write_time_of_last_obs(unit, r)
    integer(ip),              intent(in) :: unit
    type(time_of_last_obs_t), intent(in) :: r
    write(unit, '(5(I6),F13.7,5X,A3,T61, A)') &
         r%year, r%month, r%day, &
         r%hour, r%min, r%sec, &
         r%time_system, "TIME OF LAST OBS"
  end subroutine write_time_of_last_obs

  
  subroutine write_rcv_clock_offs_appl(unit, r)
    integer(ip),                 intent(in) :: unit
    type(rcv_clock_offs_appl_t), intent(in) :: r
    
    write(unit, '(I6, T61, A)') r%offs, "RCV CLOCK OFFS APPL"
  end subroutine write_rcv_clock_offs_appl


  subroutine write_sys_dcbs_applied(unit, A)
    integer(ip),              intent(in) :: unit
    type(sys_dcbs_applied_t), intent(in) :: A(:)

    integer(ip) :: i
    
    do i = 1, size(A)
       if (A(i)%sat_sys == "") cycle
       
       write(unit, '(A1, 1X, A17, 1X, A40, A)') &
            A(i)%sat_sys, A(i)%prog_name, A(i)%source_url, &
            "SYS / DCBS APPLIED"
    end do
    
  end subroutine write_sys_dcbs_applied

  
  subroutine write_sys_pcvs_applied(unit, A)
    integer(ip),              intent(in) :: unit
    type(sys_pcvs_applied_t), intent(in) :: A(:)

    integer(ip) :: i
    
    do i = 1, size(A)
       if (A(i)%sat_sys == "") cycle
       
       write(unit, '(A1, 1X, A17, 1X, A40, A)') &
            A(i)%sat_sys, A(i)%prog_name, A(i)%source_url, &
            "SYS / PCVS APPLIED"
    end do
    
  end subroutine write_sys_pcvs_applied

  
  subroutine write_sys_scale_factors(unit, A)
    integer(ip),               intent(in) :: unit
    type(sys_scale_factors_t), intent(in) :: A(:)

    integer(ip)   :: i, j

    do i = 1, size(A)
       if (.not. allocated(A(i)%scale_factors)) cycle

       do j = 1, size(A(i)%scale_factors)
          associate (sf => A(i)%scale_factors(j))

            if (sf%n_obs_types == 0) then
               write(unit, '(A1, 1X, I4, 2X, I2.0, T61, A)') &
                    A(i)%sat_sys, sf%factor, sf%n_obs_types, &
                    "SYS / SCALE FACTOR"
            else
               write(unit, '(T61, "SYS / SCALE FACTOR", T1, A1, 1X, I4, 2X, I2.0, 12(1X, A3) : / &
                    & (T61, "SYS / SCALE FACTOR", T1, 10X, 12(1X, A3)))') &
                    A(i)%sat_sys, sf%factor, sf%n_obs_types, sf%obs_types
            end if

          end associate
       end do
    end do
  end subroutine write_sys_scale_factors

  
  subroutine write_sys_phase_shifts(unit, A)
    integer(ip),              intent(in) :: unit
    type(sys_phase_shifts_t), intent(in) :: A(:)

    integer(ip) :: i, j

    do i = 1, size(A)
       if (.not. allocated(A(i)%phase_shifts)) cycle

       do j = 1, size(A(i)%phase_shifts)
          associate (ps => A(i)%phase_shifts(j))

            if (ps%n_sats == 0) then
               ! Don't write shift if shift = 0.0
               write(unit, '(A1, 1X, A3, 1X, A8, 2X, I2.0, T61, A)') &
                    A(i)%sat_sys, ps%obs_type, fmt_shift(ps%shift), 0, &
                    'SYS / PHASE SHIFT'
            else
               write(unit, '(T61, "SYS / PHASE SHIFT", T1,  A1, 1X, A3, 1X, F8.5, 2X, I2.0, 10(1X, A3) :/ &
                    & (T61, "SYS / PHASE SHIFT", T1, 18X, 10(1X, A3)))') & 
                  A(i)%sat_sys, ps%obs_type, ps%shift, &
                  ps%n_sats, ps%sats
            end if
            
          end associate
       end do
    end do

  contains
    
    pure character(len=8) function fmt_shift(val)
      real(wp), intent(in) :: val
      
      if (val == 0.0_wp) then
         fmt_shift = "        " ! 8 spacji
      else
         write(fmt_shift, '(F8.5)') val
      end if
    end function fmt_shift
         
  end subroutine write_sys_phase_shifts


  subroutine write_glonass_slot_frq_num(unit, r)
    integer(ip),                  intent(in) :: unit
    type(glonass_slot_frq_num_t), intent(in) :: r

    write(unit, '(T61, "GLONASS SLOT / FRQ #", T1, I3, 1X, 8("R", I2.2, 1X, I2, 1X) :/ &
         & (T61, "GLONASS SLOT / FRQ #", T1, 4X, 8("R", I2.2, 1X, I2, 1X)))') & 
         r%n_slots, r%slot_frq

  end subroutine write_glonass_slot_frq_num

  
  subroutine write_glonass_cod_phs_bis(unit, r)
    integer,                     intent(in) :: unit
    type(glonass_cod_phs_bis_t), intent(in) :: r

    integer(ip) :: i
    
    write(unit, '(4(1X, A3, 1X, F8.3), T61, A)') &
         (r%obs_type(i), r%bias(i), i = 1, 4), &
         "GLONASS COD/PHS/BIS"
  end subroutine write_glonass_cod_phs_bis


  subroutine write_leap_seconds(unit, r)
    integer,              intent(in) :: unit
    type(leap_seconds_t), intent(in) :: r

    write(unit, '(4I6, A3, T61, A)') &
         r%current, r%future, r%week, r%day, r%time_system, &
         "LEAP SECONDS"
    
  end subroutine write_leap_seconds

  
  subroutine write_num_of_satellites(unit, r)
    integer(ip),               intent(in) :: unit
    type(num_of_satellites_t), intent(in) :: r
    
    write(unit, '(I6, T61, A)') r%n_sat, "# OF SATELLITES"
  end subroutine write_num_of_satellites

  
  subroutine write_prn_num_of_obs(unit, A)
    integer(ip),                intent(in) :: unit
    type(sys_prn_num_of_obs_t), intent(in) :: A(:)

    integer(ip)   :: i, j

    do i = 1, size(A)
       if (.not. allocated(A(i)%prn_num_of_obs)) cycle

       do j = 1, size(A(i)%prns)
          associate (obs => A(i)%prn_num_of_obs(:, j), prn => A(i)%prns(j))

            write(unit, '(T61, "PRN / # OF OBS", T1, 3X, A1, I2.2, 9(I6.0) : / &
                 & (T61, "PRN / # OF OBS", T1, 6X, 9(I6.0)))') &
                 A(i)%sat_sys, prn, obs
          end associate
       end do
    end do
  end subroutine write_prn_num_of_obs


  subroutine write_end_of_header(unit)
    integer(ip), intent(in) :: unit

    write(unit, '(60X, A)') "END OF HEADER"
  end subroutine write_end_of_header

  
  subroutine write_header(unit, header)
    integer(ip),    intent(in) :: unit
    type(header_t), intent(in) :: header

    call write_rinex_version_type (unit, header%rinex_version_type)
    call write_pgm_run_by_date    (unit, header%pgm_run_by_date)
    call write_marker_name        (unit, header%marker_name)
    
    if (header%marker_number%is_present .eqv. .true.) &
         call write_marker_number (unit, header%marker_number)

    if (header%marker_type%is_present .eqv. .true.) &
         call write_marker_type (unit, header%marker_type)

    call write_observer_agency     (unit, header%observer_agency)
    call write_rec_num_type_vers   (unit, header%rec_num_type_vers)
    call write_ant_num_type        (unit, header%ant_num_type)
    call write_approx_position_xyz (unit, header%approx_position_xyz)
    call write_antenna_delta_hen   (unit, header%antenna_delta_hen)
    
    if (header%antenna_delta_xyz%is_present .eqv. .true.) &
         call write_antenna_delta_xyz (unit, header%antenna_delta_xyz)

    call write_antenna_phasecenters (unit, header%antenna_phasecenter)
    
    if (header%antenna_bsight_xyz%is_present .eqv. .true.) &
         call write_antenna_bsight_xyz (unit, header%antenna_bsight_xyz)
    
    if (header%antenna_zerodir_azi%is_present .eqv. .true.) &
         call write_antenna_zerodir_azi (unit, header%antenna_zerodir_azi)
    
    if (header%antenna_zerodir_xyz%is_present .eqv. .true.) &
         call write_antenna_zerodir_xyz (unit, header%antenna_zerodir_xyz)
    
    if (header%center_of_mass_xyz%is_present .eqv. .true.) &
         call write_center_of_mass_xyz (unit, header%center_of_mass_xyz)
    
    call write_sys_num_obs_types (unit, header%sys_num_obs_types)
    
    if (header%signal_strength_unit%is_present .eqv. .true.) &
         call write_signal_strength_unit (unit, header%signal_strength_unit)
    
    if (header%interval%is_present .eqv. .true.) &
         call write_interval (unit, header%interval)
    
    call write_time_of_first_obs (unit, header%time_of_first_obs)
    
    if (header%time_of_last_obs%is_present .eqv. .true.) &
         call write_time_of_last_obs (unit, header%time_of_last_obs)
    
    if (header%rcv_clock_offs_appl%is_present .eqv. .true.) &
         call write_rcv_clock_offs_appl  (unit, header%rcv_clock_offs_appl)
    
    call write_sys_dcbs_applied  (unit, header%sys_dcbs_applied)
    call write_sys_pcvs_applied  (unit, header%sys_pcvs_applied)
    call write_sys_scale_factors (unit, header%sys_scale_factor)
    call write_sys_phase_shifts  (unit, header%sys_phase_shift)

    if (header%glonass_slot_frq_num%is_present .eqv. .true.) &
         call write_glonass_slot_frq_num (unit, header%glonass_slot_frq_num)

    if (header%glonass_cod_phs_bis%is_present .eqv. .true.) &
         call write_glonass_cod_phs_bis (unit, header%glonass_cod_phs_bis)

    if (header%leap_seconds%is_present .eqv. .true.) &
         call write_leap_seconds (unit, header%leap_seconds)

    if (header%num_of_satellites%is_present .eqv. .true.) &
         call write_num_of_satellites (unit, header%num_of_satellites)

    call write_prn_num_of_obs (unit, header%prn_num_of_obs)
    call write_end_of_header  (unit)
    
  end subroutine write_header


  !-------------------------------------------------------------------
  ! WRITING OBSERVATIONS
  !-------------------------------------------------------------------

  subroutine write_epoch_header(unit, r)
    integer(ip),          intent(in) :: unit
    type(epoch_record_t), intent(in) :: r

    if (r%rcv_clk_offset == 0.0_wp) then
       write(unit, '(A1, 1X, I4, 4(1X,I2.2), F11.7, 2X, I1, I3)') &
            '>', r%year, r%month, r%day, r%hour, r%min, r%sec, &
            r%flag, r%n_sats
    else
       write(unit, '(A1, 1X, I4, 4(1X,I2.2), F11.7, 2X, I1, I3, 6X, F15.12)') &
            '>', r%year, r%month, r%day, r%hour, r%min, r%sec, &
            r%flag, r%n_sats, r%rcv_clk_offset
    end if
  end subroutine write_epoch_header

    
  subroutine write_epoch_obs(unit, A, B)
    integer(ip),               intent(in) :: unit
    type(sys_num_obs_types_t), intent(in) :: A(:)
    type(sys_sats_obs_t),      intent(in) :: B(:)

    character(1) :: sat_sys
    integer(ip)  :: i, j, n_obs, i_obs, last_present_obs, k

    on_sys: do i = 1, size(B)
       if (allocated(B(i)%sys_sats_obs)) then
          sat_sys = get_sat_sys(i)
          n_obs = A(i)%n_obs_types
          on_sat: do j = 1, B(i)%n_prns

             last_present_obs = 0
             do i_obs = n_obs, 1, -1
                if (B(i)%sys_sats_obs(i_obs,j)%val /= 0.0_wp) then
                   last_present_obs = i_obs
                   exit
                end if
             end do
             write(unit, '(A1, I2.2)', advance='NO') sat_sys, B(i)%prns(j)
             do k = 1, last_present_obs
                if (k == last_present_obs) then
                   if (B(i)%sys_sats_obs(k,j)%lli == 0 .and. B(i)%sys_sats_obs(k,j)%ssi == 0) then
                      write(unit, '(A14)', advance='NO') f14_3(B(i)%sys_sats_obs(k,j)%val)
                   else if (B(i)%sys_sats_obs(k,j)%ssi == 0) then
                      write(unit, '(A14, I1.0)', advance='NO') &
                           f14_3(B(i)%sys_sats_obs(k,j)%val), B(i)%sys_sats_obs(k,j)%lli
                   else
                      write(unit, '(A14, I1.0, I1.0)', advance='NO') &
                           f14_3(B(i)%sys_sats_obs(k,j)%val), &
                           B(i)%sys_sats_obs(k,j)%lli, &
                           B(i)%sys_sats_obs(k,j)%ssi
                   end if
                else
                   write(unit, '(A14, I1.0, I1.0)', advance='NO') &
                        f14_3(B(i)%sys_sats_obs(k,j)%val), &
                        B(i)%sys_sats_obs(k,j)%lli, &
                        B(i)%sys_sats_obs(k,j)%ssi
                end if
             end do
             write (unit, '()')
          end do on_sat
       end if
    end do on_sys

  end subroutine write_epoch_obs

  
  subroutine write_epoch_record(unit, A, r)
    integer(ip),               intent(in) :: unit
    type(sys_num_obs_types_t), intent(in) :: A(:)
    type(epoch_record_t),      intent(in) :: r

      call write_epoch_header(unit, r)
      call write_epoch_obs(unit, A, r%sats_obs)
  end subroutine write_epoch_record
    
    
  elemental function f14_3(val) result(res)
    real(wp), intent(in) :: val
    
    character(len=14) :: res

    if (val == 0.0_wp) then
       res = '              ' ! Exactly 14 spaces
    else
       write(res, '(F14.3)') val
    end if
  end function f14_3

end module obs_rinex3_writer_mod
