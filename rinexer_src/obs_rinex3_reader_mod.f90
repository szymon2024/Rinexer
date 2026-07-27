!> A module for reading of RINEX version 3.x observation
!> files. It can be used to read the file header and the epoch
!> records.
!> The file is read line by line. When reading epoch records, only one
!> epoch record is kept in memory.
!>
!> Note on terminology: An epoch represents a specific moment in time.
!>
!> @version 1.0.1
module obs_rinex3_reader_mod
  use, intrinsic :: iso_fortran_env, only: iostat_end
  use obs_rinex3_types_mod
  use obs_rinex3_indexes_mod, only: get_sys_index

  implicit none

  private

  public :: read_header
  public :: allocate_epoch_record
  public :: read_next_epoch_record

contains

  !-------------------------------------------------------------------
  ! READING HEADER
  !-------------------------------------------------------------------
  
  subroutine allocate_header(unit, h, err_msg)
    integer,          intent(in)    :: unit
    type(header_t),   intent(inout) :: h
    character(len=*), intent(out)   :: err_msg

    integer       :: ios, i_sys
    character(1)  :: sat_sys, char
    character(80) :: line
    character(20) :: label

    err_msg = ""
    rewind(unit)

    do
       read(unit, '(A)', iostat=ios) line

       if (ios == iostat_end) then
          err_msg = "The label END OF HEADER not found."
          return
       end if
       if (ios > 0) then
          write(err_msg, *) "Unable to read RINEX file line &
               & before END OF HEADER label."
          return
       end if

       label = adjustl(line(61:80))
       select case (trim(label))

       case ("ANTENNA:PHASECENTER")
          read(line, '(A1)') sat_sys
          i_sys = get_sys_index(sat_sys)
          
          if (i_sys == 0) then
             write(err_msg, '(A, A1, 3A)') &
                  "Unsupported satelite system '", sat_sys, &
                  "' in line:", new_line('a'), trim(line)
             return
          end if
          associate (A => h%antenna_phasecenter)
            A(i_sys)%n_obs_types = A(i_sys)%n_obs_types + 1
          end associate

       case ("SYS / # / OBS TYPES")
          char = line(1:1)
          if (char == " ") cycle  ! Continuation line
          i_sys = get_sys_index(char)
          if (i_sys == 0) then
             write(err_msg, '(A, A1, 3A)') &
                  "Unsupported satelite system '", char, &
                  "' in line:", new_line('a'), trim(line)
             return
          end if
          associate(A => h%sys_num_obs_types)
            read(line, '(3X,I3)', iostat=ios) A(i_sys)%n_obs_types
          end associate
          if (ios /= 0) then
             write(err_msg, '(3A)') &
                  "Unable to read number of obs types from line:", &
                  new_line('a'), trim(line)
             return
          end if

       case ("SYS / SCALE FACTOR")
          char = line(1:1)
          if (char == " ") cycle  ! Continuation line
          i_sys = get_sys_index(char)
          if (i_sys == 0) then
             write(err_msg, '(A, A1, 3A)') &
                  "Unsupported satelite system '", char, &
                  "' in line:", new_line('a'), trim(line)
             return
          end if
          associate(A => h%sys_scale_factor)
            A(i_sys)%n_scale_factors = A(i_sys)%n_scale_factors + 1
          end associate

       case ("SYS / PHASE SHIFT")
          char = line(1:1)
          if (char == " ") cycle  ! Continuation line
          i_sys = get_sys_index(char)
          if (i_sys == 0) then
             write(err_msg, '(A, A1, 3A)') &
                  "Unsupported satelite system '", char, &
                  "' in line:", new_line('a'), trim(line)
             return
          end if
          associate(A => h%sys_phase_shift)
            A(i_sys)%n_phase_shifts = A(i_sys)%n_phase_shifts + 1
          end associate

       case ("PRN / # OF OBS")
          char = line(4:4)
          if (char == " ") cycle ! Continuation line
          i_sys = get_sys_index(char)
          if (i_sys == 0) then
             write(err_msg, '(A, A1, 3A)') &
                  "Unsupported satelite system '", char, &
                  "' in line:", new_line('a'), trim(line)
             return
          end if
          associate (A => h%prn_num_of_obs)
            A(i_sys)%n_prns = A(i_sys)%n_prns + 1
          end associate

       case ("END OF HEADER")
          exit

       case default

       end select

    end do

    call allocate_antenna_phasecenter(h%antenna_phasecenter)
    call allocate_sys_num_obs_types(h%sys_num_obs_types)
    call allocate_sys_phase_shift(h%sys_phase_shift)
    call allocate_sys_scale_factor(h%sys_scale_factor)
    call allocate_prn_num_of_obs(h%sys_num_obs_types, h%prn_num_of_obs)

  contains

    subroutine allocate_antenna_phasecenter(A)
      type(sys_antenna_phasecenters_t), intent(inout) :: A(:)

      integer(ip) :: i, k, n
      type(phasecenter_t) :: default_pc

      do i = 1, size(A)
         n = A(i)%n_obs_types

         if (n > 0) then
            A(i)%phasecenters = [(default_pc, k = 1, n)]
            A(i)%n_obs_types = 0
         end if
      end do
    end subroutine allocate_antenna_phasecenter

    
    subroutine allocate_sys_num_obs_types(A)
      type(sys_num_obs_types_t), intent(inout) :: A(:)

      integer(ip) :: i, k, n

      do i = 1, size(A)
         n = A(i)%n_obs_types

         if (n > 0) then
            A(i)%obs_types = [(" ", k = 1, n)]
         end if
      end do
    end subroutine allocate_sys_num_obs_types


    subroutine allocate_sys_scale_factor(A)
      type(sys_scale_factors_t), intent(inout) :: A(:)

      integer(ip) :: i, k, n
      type(scale_factor_t) :: default_sf

      do i = 1, size(A)
         n = A(i)%n_scale_factors

         if (n > 0) then
            A(i)%scale_factors = [(default_sf, k = 1, n)]
            A(i)%n_scale_factors = 0
         end if
      end do
    end subroutine allocate_sys_scale_factor


    subroutine allocate_sys_phase_shift(A)
      type(sys_phase_shifts_t), intent(inout) :: A(:)

      integer(ip) :: i, k, n
      type(phase_shift_t) :: default_ps

      do i = 1, size(A)
         n = A(i)%n_phase_shifts

         if (n > 0) then
            A(i)%phase_shifts = [(default_ps, k = 1, n)]
            A(i)%n_phase_shifts = 0
         end if
      end do
    end subroutine allocate_sys_phase_shift


    subroutine allocate_prn_num_of_obs(A, B)
      type(sys_num_obs_types_t),  intent(in)    :: A(:)
      type(sys_prn_num_of_obs_t), intent(inout) :: B(:)

      integer(ip) :: i, k, n_obs, n_prn

      do i = 1, MAX_SYS
         n_obs = A(i)%n_obs_types
         n_prn = B(i)%n_prns
         if (n_prn > 0 .and. n_obs>0) then
            B(i)%prns = [(0, k = 1, n_prn)]
            B(i)%prn_num_of_obs = reshape([(0, k = 1, n_obs * n_prn)], [n_obs, n_prn])
            B(i)%n_prns = 0
         end if
      end do
    end subroutine allocate_prn_num_of_obs

  end subroutine allocate_header
  
  !> Reading the RINEX file header
  subroutine read_header(unit, h, err_msg)
    integer(ip),      intent(in)    :: unit
    type(header_t),   intent(inout) :: h
    character(len=*), intent(out)   :: err_msg

    integer(ip) :: ios
    character(len=1024) :: line
    character(len=20)   :: label

    err_msg = ""
    
    read(unit, '(A)', iostat=ios) line
    if (ios /= 0) then
       if (ios == iostat_end) then
          return
       else
          write(err_msg, '(A)') "Unable to read RINEX file line &
               & before END OF HEADER."
          return
       end if
    end if

    label = adjustl(line(61:80))
    
    if (label /= "RINEX VERSION / TYPE") then
       err_msg = "RINEX VERSION / TYPE label not found in the first &
            & line of the RINEX file."
       return
    end if
    call read_rinex_version_type(line, h%rinex_version_type, err_msg)
    if (err_msg(1:1) /= " ") return
    
    call allocate_header(unit, h, err_msg)
    if (err_msg(1:1) /= " ") return
    rewind(unit)
    
    do
       read(unit, '(A)', iostat=ios) line
       if (ios /= 0) then
          if (ios == iostat_end) then
             return
          else
             write(err_msg, '(A)') "Critical error: Unable to read RINEX file line &
                  & before END OF HEADER."
             return
          end if
       end if

       label = adjustl(line(61:80))

       select case (label)
          
       case ("PGM / RUN BY / DATE")
          call read_pgm_run_by_date(line, h%pgm_run_by_date, err_msg)
          
       case ("MARKER NAME")
          call read_marker_name(line, h%marker_name, err_msg)
          
       case ("MARKER NUMBER")
          call read_marker_number(line, h%marker_number, err_msg)
          
       case ("MARKER TYPE")
          call read_marker_type(line, h%marker_type, err_msg)
          
       case ("OBSERVER / AGENCY")
          call read_observer_agency(line, h%observer_agency, err_msg)

       case ("REC # / TYPE / VERS")
          call read_rec_num_type_vers(line, h%rec_num_type_vers, err_msg)

       case ("APPROX POSITION XYZ")
          call read_approx_position_xyz(line, h%approx_position_xyz, err_msg)

       case ("ANT # / TYPE")
          call read_ant_num_type(line, h%ant_num_type, err_msg)

       case ("ANTENNA: DELTA H/E/N")
          call read_antenna_delta_hen(line, h%antenna_delta_hen, err_msg)

       case ("ANTENNA: DELTA X/Y/Z")
          call read_antenna_delta_xyz (line, h%antenna_delta_xyz , err_msg)

       case ("ANTENNA:PHASECENTER")
          call read_antenna_phasecenter(line, h%antenna_phasecenter, err_msg)

       case ("ANTENNA: B.SIGHT XYZ")
          call read_antenna_bsight_xyz(line, h%antenna_bsight_xyz, err_msg)

       case ("ANTENNA: ZERODIR AZI")
          call read_antenna_zerodir_azi(line, h%antenna_zerodir_azi, err_msg)

       case ("ANTENNA: ZERODIR XYZ")
          call read_antenna_zerodir_xyz(line, h%antenna_zerodir_xyz, err_msg)

       case ("CENTER OF MASS: XYZ")
          call read_center_of_mass_xyz(line, h%center_of_mass_xyz, err_msg)
          
       case ("SYS / # / OBS TYPES")
          call read_sys_num_obs_types(unit, line, h%sys_num_obs_types, err_msg)

       case ("SIGNAL STRENGTH UNIT")
          call read_signal_strength_unit(line, h%signal_strength_unit, err_msg)

       case ("INTERVAL")
          call read_interval(line, h%interval, err_msg)
          
       case ("TIME OF FIRST OBS")
          call read_time_of_first_obs(line, h%time_of_first_obs, err_msg)

       case ("TIME OF LAST OBS")
          call read_time_of_last_obs(line, h%time_of_last_obs, err_msg)

       case ("RCV CLOCK OFFS APPL")
          call read_rcv_clock_offs_appl(line, h%rcv_clock_offs_appl, err_msg)

       case ("SYS / DCBS APPLIED")
          call read_sys_dcbs_applied(line, h%sys_dcbs_applied, err_msg)

       case ("SYS / PCVS APPLIED")
          call read_sys_pcvs_applied(line, h%sys_pcvs_applied, err_msg)

       case ("SYS / SCALE FACTOR")
          call read_sys_scale_factors(unit, line, h%sys_scale_factor, err_msg)

       case ("SYS / PHASE SHIFT")
          call read_sys_phase_shifts(unit, line, h%sys_phase_shift, err_msg)

       case ("GLONASS SLOT / FRQ #")
          call read_glonass_slot_frq_num(unit, line, h%glonass_slot_frq_num, err_msg)

       case ("GLONASS COD/PHS/BIS")
          call read_glonass_cod_phs_bis(line, h%glonass_cod_phs_bis, err_msg)

       case("LEAP SECONDS")
          call read_leap_seconds(line, h%leap_seconds, err_msg)

       case("# OF SATELLITES")
          call read_num_of_satellites(line, h%num_of_satellites, err_msg)

       case("PRN / # OF OBS")
          call read_prn_num_of_obs(unit, line, h%prn_num_of_obs, err_msg)
          
       case ("END OF HEADER")
          exit
          
       case default

       end select
       if (err_msg /= "") exit
    end do
    
    call validate_header(h, err_msg)
    
  end subroutine read_header


  subroutine validate_header(h, err_msg)
    type(header_t), intent(in)  :: h
    character(len=*),   intent(out) :: err_msg

    character(len=1024) :: prev_err_msg

    err_msg = ""
    call validate_sys_num_obs_types &
         (h%sys_num_obs_types, prev_err_msg)
    if (prev_err_msg /= "") &
         write(err_msg, '(3A)') &
         "Header Control:", new_line('a'), prev_err_msg
    
  contains
    
    subroutine validate_sys_num_obs_types(A, err_msg)
      type(sys_num_obs_types_t), intent(in)  :: A(:)
      character(len=*),          intent(out) :: err_msg

      err_msg = ""
      if (all(A%sat_sys == "")) &
           err_msg = "No SYS / # / OBS TYPES records found."

    end subroutine validate_sys_num_obs_types
    
  end subroutine validate_header

  
  subroutine read_rinex_version_type(line, r, err_msg)
    character(len=*),           intent(in)    :: line
    type(rinex_version_type_t), intent(inout) :: r
    character(len=*),           intent(out)   :: err_msg

    integer(ip) :: ios

    err_msg = ""
    read(line, '(F9.2,11X,A1,19X,A1,19X)', iostat = ios) &
         r%version, r%type, r%sat_sys
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read RINEX VERSION / TYPE line.", &
            new_line('a'), trim(line)
       return
    end if
    
    call validate_rinex_version_type(r, err_msg)

  end subroutine read_rinex_version_type

  
  subroutine validate_rinex_version_type(r, err_msg)
    type(rinex_version_type_t), intent(inout) :: r
    character(len=*),           intent(out)   :: err_msg

    character(len = 1), parameter :: A(8) = ['M','C','E','G','I','J','R','S']

    err_msg = ""

    if (r%version < 3.0_wp .or. r%version >= 4.0_wp) then
       write(err_msg, '(A, F9.2, 3A)') "Unsupported rinex version: ", &
            r%version, ". ", new_line('a'), &
            "This program supports only versions 3.x."
       return
    end if

    if (r%type /= 'O') then
       write(err_msg, '(A, A1, A)') "Unsupported RINEX file type '", &
            r%type, "'. Only Observation Data ('O') is serviced."
       return
    endif

    if (findloc(A, r%sat_sys, dim=1) == 0) then
       write(err_msg, '(A, A1, 3A)') &
            "Unknown RINEX satellite system '", r%sat_sys, &
            "' in RINEX VERSION / TYPE line.", new_line('a'),  &
            "Supported systemes: M, C, E, G, I, J, R, S."
    end if

  end subroutine validate_rinex_version_type


  subroutine read_pgm_run_by_date(line, r, err_msg)
    character(len=*),        intent(in)    :: line
    type(pgm_run_by_date_t), intent(inout) :: r
    character(len=*),        intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    read(line, '(3A20)', iostat=ios) r%pgm, r%run_by, r%date
     if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read PGM / RUN BY / DATE line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_pgm_run_by_date


  subroutine read_marker_name(line, r, err_msg)
    character(len=*),    intent(in)    :: line
    type(marker_name_t), intent(inout) :: r
    character(len=*),    intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    read(line, '(A60)', iostat=ios) r%name
     if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read MARKER NAME line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_marker_name


  subroutine read_marker_number(line, r, err_msg)
    character(len=*),      intent(in)    :: line
    type(marker_number_t), intent(inout) :: r
    character(len=*),      intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    r%is_present = .true.
    read(line, '(A20)', iostat=ios) r%number
     if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read MARKER NUMBER line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_marker_number


  subroutine read_marker_type(line, r, err_msg)
    character(len=*),    intent(in)    :: line
    type(marker_type_t), intent(inout) :: r
    character(len=*),    intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    r%is_present = .true.
    read(line, '(A20)', iostat=ios) r%type
     if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read MARKER TYPE line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_marker_type
  

  subroutine read_observer_agency(line, r, err_msg)
    character(len=*),        intent(in)    :: line
    type(observer_agency_t), intent(inout) :: r
    character(len=*),        intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    read(line, '(A20, A40)', iostat=ios) r%observer, r%agency
     if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read OBSERVER / AGENCY line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_observer_agency


  subroutine read_rec_num_type_vers(line, r, err_msg)
    character(len=*),          intent(in)    :: line
    type(rec_num_type_vers_t), intent(inout) :: r
    character(len=*),          intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    read(line, '(3A20)', iostat=ios) r%num, r%type, r%vers
     if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read REC # / TYPE / VERS line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_rec_num_type_vers


  subroutine read_ant_num_type(line, r, err_msg)
    character(len=*),     intent(in)    :: line
    type(ant_num_type_t), intent(inout) :: r
    character(len=*),     intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    read(line, '(2A20)', iostat=ios) r%num, r%type
     if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read ANT # / TYPE line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_ant_num_type


  subroutine read_approx_position_xyz(line, r, err_msg)
    character(len=*),            intent(in)    :: line
    type(approx_position_xyz_t), intent(inout) :: r
    character(len=*),            intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    read(line, '(3F14.4)', iostat=ios) r%x, r%y, r%z
     if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read APPROX POSITION XYZ line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_approx_position_xyz
  

  subroutine read_antenna_delta_hen(line, r, err_msg)
    character(len=*),          intent(in)    :: line
    type(antenna_delta_hen_t), intent(inout) :: r
    character(len=*),          intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    read(line, '(3F14.4)', iostat=ios) r%h, r%e, r%n
     if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read ANTENNA: DELTA H/E/N line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_antenna_delta_hen


  subroutine read_antenna_delta_xyz (line, r, err_msg)
    character(len=*),          intent(in)    :: line
    type(antenna_delta_xyz_t), intent(inout) :: r
    character(len=*),          intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    r%is_present = .true.
    read(line, '(3F14.4)', iostat=ios) r%dx, r%dy, r%dz
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read ANTENNA: DELTA X/Y/Z line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_antenna_delta_xyz 

  
  subroutine read_antenna_phasecenter(line, A, err_msg)
    character(len=*),                 intent(in)    :: line
    type(sys_antenna_phasecenters_t), intent(inout) :: A(:)
    character(len=*),                 intent(out)   :: err_msg

    integer(ip)  :: i_sys, ios
    character(1) :: sat_sys

    err_msg = ""

    read(line, '(A1)') sat_sys
    i_sys = get_sys_index(sat_sys)
    if (i_sys == 0) then
       write(err_msg, '(A, A1, 3A)') &
            "Unsupported satelite system '", sat_sys, &
            "' in line:", new_line('a'), trim(line)
       return
    end if

    associate ( pc => A(i_sys)%phasecenters, n => A(i_sys)%n_obs_types)
      n = n + 1
      if (A(i_sys)%sat_sys == " ") A(i_sys)%sat_sys = sat_sys
      read(line, '(1X, 1X, A3, F9.4, 2F14.4)', iostat=ios) &
           pc(n)%obs_type, pc(n)%xyz_neu
      if (ios /= 0) then
         write(err_msg, '(3A)') &
              "Unable to read ANTENNA:PHASECENTER line:", &
              new_line('a'), trim(line)
         return
      end if
    end associate
    
  end subroutine read_antenna_phasecenter


  subroutine read_antenna_bsight_xyz(line, r, err_msg)
    character(len=*),           intent(in)    :: line
    type(antenna_bsight_xyz_t), intent(inout) :: r
    character(len=*),           intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    r%is_present = .true.
    read(line, '(3F14.4)', iostat=ios) r%x, r%y, r%z
     if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read ANTENNA: B.SIGHT XYZ line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_antenna_bsight_xyz


  subroutine read_antenna_zerodir_azi(line, r, err_msg)
    character(len=*),            intent(in)    :: line
    type(antenna_zerodir_azi_t), intent(inout) :: r
    character(len=*),            intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    r%is_present = .true.
    read(line, '(F14.4)', iostat=ios) r%azi
     if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read ANTENNA: ZERODIR AZI line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_antenna_zerodir_azi


  subroutine read_antenna_zerodir_xyz(line, r, err_msg)
    character(len=*),            intent(in)    :: line
    type(antenna_zerodir_xyz_t), intent(inout) :: r
    character(len=*),            intent(out)   :: err_msg

    integer(ip) :: ios

    err_msg = ""
    r%is_present = .true.
    read(line, '(3F14.4)', iostat=ios) r%x, r%y, r%z
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read ANTENNA: ZERODIR XYZ line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_antenna_zerodir_xyz


  subroutine read_center_of_mass_xyz(line, r, err_msg)
    character(len=*),           intent(in)    :: line
    type(center_of_mass_xyz_t), intent(inout) :: r
    character(len=*),           intent(out)   :: err_msg

    integer(ip) :: ios

    err_msg = ""
    r%is_present = .true.
    read(line, '(3F14.4)', iostat=ios) r%x, r%y, r%z
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read CENTER OF MASS: XYZ line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_center_of_mass_xyz
  
  
  !> Reads SYS / # / OBS TYPES lines for single sattellite system
  subroutine read_sys_num_obs_types(unit, line, A, err_msg)
    integer(ip),               intent(in)    :: unit
    character(len=*),          intent(inout) :: line
    type(sys_num_obs_types_t), intent(inout) :: A(:)
    character(len=*),          intent(out)   :: err_msg

    integer(ip)  :: ios, i_sys, n_obs_types
    character(1) :: sat_sys

    err_msg = ""
    read(line, '(A1, 2X, I3)', iostat = ios) sat_sys, n_obs_types
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read SYS / # / OBS TYPES line:", &
            new_line('a'), trim(line)
       return
    end if
    i_sys = get_sys_index(sat_sys)
    if (i_sys == 0) then
       write(err_msg, '(A, A1, 3A)') &
            "Unsupported satelite system '", sat_sys, &
            "' in SYS / # / OBS TYPES line:", new_line('a'), &
            trim(line)
       return
    end if

    A(i_sys)%sat_sys = sat_sys
    A(i_sys)%n_obs_types = n_obs_types

    backspace (unit)
    ! Parentheses are very important in the format. 
    read (unit, '((6X, 13(1X, A3)))', iostat=ios) A(i_sys)%obs_types
    if (ios /= 0) then
       if (ios == iostat_end) then
          err_msg = "Unexpected end of file while reading SYS / # / OBS TYPES record."
       else
          backspace (unit)
          read(unit, '(A)', iostat=ios) line
          write (err_msg, '(3A)') &
               "Unable to read obs types &
               &from SYS / # / OBS TYPES line:", &
               new_line('a'), trim(line)
       end if
       return
    end if
    
  end subroutine read_sys_num_obs_types


  subroutine read_signal_strength_unit(line, r, err_msg)
    character(len=*),             intent(in)    :: line
    type(signal_strength_unit_t), intent(inout) :: r
    character(len=*),             intent(out)   :: err_msg

    integer(ip) :: ios

    err_msg = ""
    r%is_present = .true.
    read(line, '(A20)', iostat=ios) r%unit
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read SIGNAL STRENGTH UNIT line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_signal_strength_unit


  subroutine read_interval(line, r, err_msg)
    character(len=*), intent(in)    :: line
    type(interval_t), intent(inout) :: r
    character(len=*), intent(out)   :: err_msg

    integer(ip) :: ios

    err_msg = ""
    r%is_present = .true.
    read(line, '(F10.3)', iostat=ios) r%interval
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read INTERVAL line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_interval


  subroutine read_time_of_first_obs(line, r, err_msg)
    character(len=*),          intent(in)    :: line
    type(time_of_first_obs_t), intent(inout) :: r
    character(len=*),          intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    read(line, '(5I6, F13.7, 5X, A3)', iostat=ios) &
         r%year, r%month, r%day, r%hour, r%min, r%sec, r%time_system
     if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read TIME OF FIRST OBS line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_time_of_first_obs


  subroutine read_time_of_last_obs(line, r, err_msg)
    character(len=*),         intent(in)    :: line
    type(time_of_last_obs_t), intent(inout) :: r
    character(len=*),         intent(out)   :: err_msg

    integer(ip) :: ios
    
    err_msg = ""
    r%is_present = .true.
    read(line, '(5I6, F13.7, 5X, A3)', iostat=ios) &
         r%year, r%month, r%day, r%hour, r%min, r%sec, r%time_system
     if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read TIME OF LAST OBS line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_time_of_last_obs
  

  subroutine read_rcv_clock_offs_appl(line, r, err_msg)
    character(len=*),            intent(in)    :: line
    type(rcv_clock_offs_appl_t), intent(inout) :: r
    character(len=*),            intent(out)   :: err_msg

    integer(ip) :: ios

    err_msg = ""
    r%is_present = .true.
    read(line, '(I6)', iostat=ios) r%offs
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read RCV CLOCK OFFS APPL line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_rcv_clock_offs_appl


  !> Reads SYS / DCBS APPLIED record for single sattellite system
  !> Each satellite system can have one SYS / DCBS APPLIED record.
  subroutine read_sys_dcbs_applied(line, A, err_msg)
    character(len=*),         intent(in)    :: line
    type(sys_dcbs_applied_t), intent(inout) :: A(:)
    character(len=*),         intent(out)   :: err_msg

    integer(ip)  :: i_sys, ios
    character(1) :: sat_sys

    err_msg = ""
    sat_sys = line(1:1)

    i_sys = get_sys_index(sat_sys)
    if (i_sys == 0) then
       write (err_msg, '(A, A1, 3A)') &
            "Unsupported satellite system '" , sat_sys, &
            "' in SYS / DCBS APPLIED line:", &
            new_line('a'), trim(line)
       return
    end if

    A(i_sys)%sat_sys = sat_sys

    read(line, '(1X, 1X, A17, 1X, A40)', iostat=ios) &
         A(i_sys)%prog_name, A(i_sys)%source_url

  end subroutine read_sys_dcbs_applied


  !> Reads SYS / PCVS APPLIED record for single sattellite system
  !> Each satellite system can have one SYS / PCVS APPLIED record.
  subroutine read_sys_pcvs_applied(line, A, err_msg)
    character(len=*),         intent(in)    :: line
    type(sys_pcvs_applied_t), intent(inout) :: A(:)
    character(len=*),         intent(out)   :: err_msg

    integer(ip)  :: i_sys, ios
    character(1) :: sat_sys

    err_msg = ""
    read(line, '(A1)', iostat=ios) sat_sys

    i_sys = get_sys_index(sat_sys)
    if (i_sys == 0) then
       write (err_msg, '(A, A1, 3A)') &
            "Unsupported satellite system '" , sat_sys, &
            "' in SYS / PCVS APPLIED line:", &
            new_line('a'), trim(line)
       return
    end if

    A(i_sys)%sat_sys = sat_sys

    read(line, '(1X, 1X, A17, 1X, A40)', iostat=ios) &
         A(i_sys)%prog_name, A(i_sys)%source_url

  end subroutine read_sys_pcvs_applied


  subroutine read_sys_scale_factors(unit, line, A, err_msg)
    integer(ip),               intent(in)    :: unit
    character(len=*),          intent(inout) :: line
    type(sys_scale_factors_t), intent(inout) :: A(:)
    character(len=*),          intent(out)   :: err_msg

    integer(ip)  :: i_sys, i_sf, k, ios
    character(1) :: sat_sys

    err_msg = ""

    sat_sys = line(1:1)

    i_sys = get_sys_index(sat_sys)
    if (i_sys == 0) then
       write(err_msg, '(A, A1, 3A)') &
            "Unsupported satellite system '", sat_sys, "' in line:", &
            new_line('a'), trim(line)
       return
    end if

    if (A(i_sys)%sat_sys == " ") A(i_sys)%sat_sys = sat_sys
        
    A(i_sys)%n_scale_factors = A(i_sys)%n_scale_factors + 1
    i_sf = A(i_sys)%n_scale_factors

    associate (sf => A(i_sys)%scale_factors(i_sf))

      if (i_sf > size(A(i_sys)%scale_factors)) then
         write(err_msg, '(A, I3, A, I3, A)') "Array overflow: Scale factor index ", i_sf, &
              ", but max allocated size is ", size(A(i_sys)%scale_factors), "."
         return
      end if

      read(line, '(1X, 1X, I4, 2X, I2)', iostat = ios) &
           sf%factor, sf%n_obs_types
      if (ios /= 0) then
         write(err_msg, '(3A)') &
              "Unable to read SYS / SCALE FACTOR line:", &
              new_line('a'), trim(line)
         return
      end if

      if (sf%n_obs_types > 0) then
         sf%obs_types = [(" ", k = 1, sf%n_obs_types)]  ! allocate by assignment

         backspace (unit)
         read(unit, '((10X, 12(1X, A3)))', iostat=ios) sf%obs_types
         if (ios /= 0) then
            if (ios == iostat_end) then
               err_msg = "Unexpected end of file while reading SYS / SCALE FACTOR."
            else
               backspace (unit)
               read(unit, '(A)', iostat=ios) line
               write (err_msg, '(3A)') &
                    "Unable to read number of obs types &
                    &from SYS / SCALE FACTOR line:", &
                    new_line('a'), trim(line)
            end if
            return
         end if
         
      end if
      
    end associate

  end subroutine read_sys_scale_factors


  !> Reads SYS / PHASE SHIFT record for a single sattellite system.
  !> Each satellite system can have many SYS / PHASE SHIFT records.
  subroutine read_sys_phase_shifts (unit, line, A, err_msg)
    integer(ip),               intent(in)    :: unit
    character(len=*),          intent(inout) :: line
    type(sys_phase_shifts_t),  intent(inout) :: A(:)
    character(len=*),          intent(out)   :: err_msg

    integer(ip) :: ios, i_sys, i_ps, k
    character(1) :: sat_sys

    err_msg = ""
    
    sat_sys = line(1:1)
    i_sys = get_sys_index(sat_sys)
    if (i_sys == 0) then
       write(err_msg, '(A, A1, 3A)') &
            "Unsupported satelite system '", sat_sys, &
            "' in SYS / PHASE SHIFT line:", new_line('a'), &
            trim(line)
       return
    end if

    if (A(i_sys)%sat_sys == " ") A(i_sys)%sat_sys = sat_sys

    A(i_sys)%n_phase_shifts = A(i_sys)%n_phase_shifts + 1
    i_ps = A(i_sys)%n_phase_shifts

    associate (ps => A(i_sys)%phase_shifts(i_ps))

      if (i_ps > size(A(i_sys)%phase_shifts)) then
         write(err_msg, '(A, I3, A, I3, A)') "Array overflow: Phase shift index ", i_ps, &
              ", but max allocated size is ", size(A(i_sys)%phase_shifts), "."
         return
      end if

      read(line, '(1X, 1X, A3, 1X, F8.5, 2X, I2.2)', iostat = ios) &
           ps%obs_type, ps%shift, ps%n_sats
      if (ios /= 0) then
         write(err_msg, '(3A)') &
              "Unable to read SYS / PHASE SHIFT  line:", &
              new_line('a'), trim(line)
         return
      end if

      if (ps%n_sats > 0) then
         ps%sats = [("   ", k = 1, ps%n_sats)]  ! allocate by assignment

         backspace (unit)
         read(unit, '((18X, 10(1X, A3)))', iostat=ios) ps%sats
         if (ios /= 0) then
            if (ios == iostat_end) then
               err_msg = "Unexpected end of file while reading SYS / PHASE SHIFT."
            else
               backspace (unit)
               read(unit, '(A)', iostat=ios) line
               write (err_msg, '(3A)') &
                    "Unable to read satellites &
                    &from SYS / PHASE SHIFT line:", &
                    new_line('a'), trim(line)
            end if
            return
         end if
      end if
    end associate
    
  end subroutine read_sys_phase_shifts


  subroutine read_glonass_slot_frq_num(unit, line, r, err_msg)
    integer(ip),                  intent(in)    :: unit
    character(len=*),             intent(inout) :: line
    type(glonass_slot_frq_num_t), intent(inout) :: r
    character(len=*),             intent(out)   :: err_msg

    integer(ip)      :: ios, k
    type(slot_frq_t) :: default_sf

    err_msg = ""
    r%is_present = .true.
    read(line, '(I3)', iostat = ios) r%n_slots  ! slot means prn
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read number of satellites GLONASS SLOT / FRQ # line:", &
            new_line('a'), trim(line)
       return
    end if

    if (r%n_slots > 0) then
       r%slot_frq = [(default_sf, k=1, r%n_slots)]

       backspace (unit)
       read(unit, '((4X, 8(1X, I2.2, 1X, I2, 1X)))', iostat=ios) r%slot_frq
       if (ios /= 0) then
          if (ios == iostat_end) then
             err_msg = "Unexpected end of file while reading GLONASS SLOT / FRQ #."
          else
             backspace (unit)
             read(unit, '(A)', iostat=ios) line
             write (err_msg, '(3A)') &
                  "Unable to read number of slot frq &
                  &from GLONASS SLOT / FRQ # line:", &
                  new_line('a'), trim(line)
          end if
          return
       end if

    end if

  end subroutine read_glonass_slot_frq_num


  subroutine read_glonass_cod_phs_bis(line, r, err_msg)
    character(len=*),            intent(in)    :: line
    type(glonass_cod_phs_bis_t), intent(inout) :: r
    character(len=*),            intent(out)   :: err_msg

    integer(ip) :: ios, i

    err_msg = ""
    r%is_present = .true.
    read(line, '(4(1X, A3, 1X, F8.3))',  iostat=ios) &
         (r%obs_type(i), r%bias(i), i = 1, 4)
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read GLONASS COD/PHS/BIS line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_glonass_cod_phs_bis


  subroutine read_leap_seconds(line, r, err_msg)
    character(len=*),     intent(in)    :: line
    type(leap_seconds_t), intent(inout) :: r
    character(len=*),     intent(out)   :: err_msg

    integer(ip) :: ios

    err_msg = ""
    r%is_present = .true.
    read(line, '(4I6, A3)',  iostat=ios) &
         r%current, r%future, r%week, r%day, r%time_system
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read LEAP SECONDS line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_leap_seconds


  subroutine read_num_of_satellites(line, r, err_msg)
    character(len=*),          intent(in)    :: line
    type(num_of_satellites_t), intent(inout) :: r
    character(len=*),          intent(out)   :: err_msg

    integer(ip) :: ios

    err_msg = ""
    r%is_present = .true.
    read(line, '(I6)',  iostat=ios) r%n_sat
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read # OF SATELLITES line:", &
            new_line('a'), trim(line)
       return
    end if
  end subroutine read_num_of_satellites

  
  subroutine read_prn_num_of_obs(unit, line, A, err_msg)
    integer(ip),                intent(in)    :: unit
    character(len=*),           intent(inout) :: line
    type(sys_prn_num_of_obs_t), intent(inout) :: A(:)
    character(len=*),           intent(out)   :: err_msg

    integer(ip)  :: i_sys, i_prn, prn, ios
    character(1) :: sat_sys

    err_msg = ""
    read(line, '(3X, A1, I2.2)', iostat = ios) sat_sys, prn
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Unable to read PRN / # OF OBS line:", &
            new_line('a'), trim(line)
       return
    end if

    i_sys = get_sys_index(sat_sys)
    if (i_sys == 0) then
       write(err_msg, '(A, A1, 3A)') &
            "Unsupported satellite system '", sat_sys, &
            "' in line:", new_line('a'), trim(line)
       return
    end if

    if (A(i_sys)%sat_sys == " ") A(i_sys)%sat_sys = sat_sys
    A(i_sys)%n_prns = A(i_sys)%n_prns + 1
    i_prn = A(i_sys)%n_prns

    if (i_prn > size(A(i_sys)%prns)) then
       write(err_msg, '(A, I3, A, I3, A)') &
            "Array overflow: Attempted to add PRN index ", i_prn, &
            ", but max allocated size is ", size(A(i_sys)%prns), "."
       return
    end if

    A(i_sys)%prns(i_prn) = prn

    backspace (unit)
    read(unit, '(6X, 9(I6))', iostat=ios) A(i_sys)%prn_num_of_obs(:,i_prn)
    if (ios /= 0) then
       if (ios == iostat_end) then
          err_msg = "Unexpected end of file while reading PRN / # OF OBS record."
       else
          backspace (unit)
          read(unit, '(A)', iostat=ios) line
          write (err_msg, '(3A)') &
               "Unable to read number of observations &
               &from PRN / # OF OBS line:", &
               new_line('a'), trim(line)
       end if
       return
    end if

  end subroutine read_prn_num_of_obs


  !-------------------------------------------------------------------
  ! READING OBSERVATIONS
  !-------------------------------------------------------------------


  !> One-time memory allocation to do after rinex header reading.
  subroutine allocate_epoch_record(A, r)
    type(sys_num_obs_types_t), intent(in)    :: A(:)
    type(epoch_record_t),      intent(inout) :: r

    integer(ip) :: i, k, n_obs
    type(obs_t) :: default_obs

    associate (B => r%sats_obs)
      do i = 1, size(B)
         n_obs = A(i)%n_obs_types
         B(i)%sat_sys = A(i)%sat_sys
         B(i)%sys_sats_obs = &
              reshape([(default_obs, k=1, n_obs *  MAX_VIS_SATS_PER_SYS)], &
              [n_obs, MAX_VIS_SATS_PER_SYS])
      end do
  end associate
  
  end subroutine allocate_epoch_record
  

  !> Read observations for one satellite
  subroutine read_sat_obs(line, sys_num_obs_types, A, err_msg)
    character(len=*),          intent(in)    :: line
    type(sys_num_obs_types_t), intent(in)    :: sys_num_obs_types(:)
    type(sys_sats_obs_t),      intent(inout) :: A(:)
    character(len=*),          intent(out)   :: err_msg
    
    integer      :: i_sys, i_prn, prn, n_obs
    integer      :: ios
    character(1) :: sat_sys
    type(obs_t)  :: obs = obs_t(0.0_wp, 0, 0)

    err_msg = ""
    
    read(line, '(A1, I2.2)', iostat=ios) sat_sys, prn
    if (ios /= 0) then
       write(err_msg, '(3A)') &
            "Error reading satelite id from observation line:", &
            new_line('a'),  trim(line)
       return
    end if

    i_sys = get_sys_index(sat_sys)
    if (i_sys == 0) then
       write(err_msg, '(3A)') &
            "Unknown satellite system in observation line:", &
            new_line('a'), trim(line)
       return
    end if

    if (A(i_sys)%sat_sys == " ") A(i_sys)%sat_sys = sat_sys

    A(i_sys)%n_prns = A(i_sys)%n_prns + 1
    i_prn = A(i_sys)%n_prns

    if (i_prn > MAX_VIS_SATS_PER_SYS) then
       write(err_msg, '(3A, I2, 2A)') "The number visible satellites  &
            &for ", sat_sys, " is exeeded ",  MAX_VIS_SATS_PER_SYS, &
            new_line('a'), "Look at MAX_VIS_SATS_PER_SYS constant &
            &in obs_rinex3_types."
       return
    end if

    A(i_sys)%prns(i_prn) = prn

    ! Clearing a matrix row of old data from the previous epoch
    n_obs = sys_num_obs_types(i_sys)%n_obs_types
    A(i_sys)%sys_sats_obs(1:n_obs, i_prn) = obs

    read(line, '(3X, *(F14.3, I1, I1))', iostat=ios) &
         A(i_sys)%sys_sats_obs(1:n_obs, i_prn)

  end subroutine read_sat_obs


  !> Reads the next epoch record from the RINEX file, including its header line
  !> and the observation data lines for all satellites listed in that epoch.
  !> @param[out]   ios     - I/O status specifier (returns 0 on
  !>                         success, positive on fatal error, negative on EOF).
  !> @param[out]   err_msg - Text buffer containing the description of
  !>                         a parsing error, spaces on success.
  subroutine read_next_epoch_record(unit, A, r, ios, err_msg)
    integer(ip),               intent(in)    :: unit
    type(sys_num_obs_types_t), intent(in)    :: A(:)
    type(epoch_record_t),      intent(inout) :: r
    integer(ip),               intent(out)   :: ios
    character(len=*),          intent(out)   :: err_msg
    
    integer(ip)         :: s, i_sys
    character(len=1024) :: line, prev_err_msg

    err_msg = ""
    
    read(unit, '(A)', iostat=ios) line
    if (ios /= 0) then
       if (ios == iostat_end) then
          return
       else
          write(err_msg, '(A)') "Unable to read RINEX file line &
               & when reading epoch record."
          return
       end if
    end if

    ! Verify the mandatory epoch identifier character '>'
    if (line(1:1) /= '>') then
       write(err_msg, '(3A)') &
            "Cannot find epoch record start marker '>' in line:", &
            new_line('a'), trim(line)
       return
    end if

    read(line, '(1X,1X,I4,4(1X,I2.2),F11.7,2X,I1,I3,6X,F15.12)', iostat=ios) &
         r%year, r%month, r%day, r%hour, r%min, r%sec, &
         r%flag, r%n_sats, r%rcv_clk_offset
    ! Only positive iostat indicates a fatal formatting error.
    ! Negative values (like EOR when clock offset is absent) are ignored.
    if (ios > 0) then ! rcv_clk_offset is optional
       write (err_msg, '(3A)') "Invalid epoch header line:", &
            new_line('a'), trim(line)
       return
    end if

    associate (B => r%sats_obs)
      do i_sys = 1, MAX_SYS
         B(i_sys)%n_prns = 0
      end do

      ! Main loop iterating over all satellites present in the current
      ! epoch
      sat_loop: do s = 1, r%n_sats
         read(unit, '(A)', iostat=ios) line
         if (ios /= 0) then
            if (ios == iostat_end) then
               return
            else
               write(err_msg, '(A)') "Unable to read RINEX file line."
               return
            end if
         end if
         call read_sat_obs(line, A, B, prev_err_msg)
         if (prev_err_msg /= "") then
            write(err_msg, '(A, I4, 4(1X,I2.2), F11.7, 2A)') &
                 "Epoch: ", r%year, r%month, r%day, r%hour, r%min, r%sec, &
                 new_line('a'), trim(prev_err_msg)
            return
         end if
      end do sat_loop
    end associate

  end subroutine read_next_epoch_record
  
end module obs_rinex3_reader_mod
