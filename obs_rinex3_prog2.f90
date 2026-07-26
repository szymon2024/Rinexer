!>--------------------------------------------------------------------
!> SAMPLE PROGRAM
!> Prints the observation types from the file header and the L1C
!> observations for GPS satellites from the first epoch.
!>
!> Note on terminology: An epoch represents a specific moment in time
!> (a timestamp).
!>
!> Input:  The FILE_NAME constant, which can be modified in the code.
!> Output: Displays data on the screen (standard output).
!>
!> Main steps:
!> 1. Open the RINEX observation file for reading.
!> 2. Read the header.
!> 3. Print observation types.
!> 4. Mandatory preparing for reading epoch recrods
!>    - Allocate memory where the epoch record will be read.
!> 5. Reading the first epoch record.
!> 6. Get the satellite system index and observation index.
!> 7. Print the selected data.
!>
!> Note: The file is read line by line, so these steps must be
!> executed in sequential order.
!>
!> Compilation on Linux:  make -f Makefile.obs_rinex3_prog2
!> Run: obs_rinex3_prog2
!>
!> Compilaton on Windows: mingw32-make -f Makefile.obs_rinex3_prog2_win
!> Run: obs_rinex3_prog2.exe
!>
!> Sample output:
!> C   24 C1P C2I C5P C6I C7D C7I D1P D2I D5P D6I D7D D7I L1P  SYS / # / OBS TYPES
!>        L2I L5P L6I L7D L7I S1P S2I S5P S6I S7D S7I          SYS / # / OBS TYPES
!> E   20 C1C C5Q C6C C7Q C8Q D1C D5Q D6C D7Q D8Q L1C L5Q L6C  SYS / # / OBS TYPES
!>        L7Q L8Q S1C S5Q S6C S7Q S8Q                          SYS / # / OBS TYPES
!> G   20 C1C C1L C2S C2W C5Q D1C D1L D2S D2W D5Q L1C L1L L2S  SYS / # / OBS TYPES
!>        L2W L5Q S1C S1L S2S S2W S5Q                          SYS / # / OBS TYPES
!> J   16 C1C C1L C2S C5Q D1C D1L D2S D5Q L1C L1L L2S L5Q S1C  SYS / # / OBS TYPES
!>        S1L S2S S5Q                                          SYS / # / OBS TYPES
!> R   16 C1C C2C C2P C3Q D1C D2C D2P D3Q L1C L2C L2P L3Q S1C  SYS / # / OBS TYPES
!>        S2C S2P S3Q                                          SYS / # / OBS TYPES
!> S    4 C1C D1C L1C S1C                                      SYS / # / OBS TYPES
!> Epoch: 2026 04 01 00 00  0.0000000
!>  Satellite: G05 | L1C:   23807845.540
!>  Satellite: G07 | L1C:          0.000
!>  Satellite: G08 | L1C:          0.000
!>  Satellite: G09 | L1C:   23967844.994
!>  Satellite: G11 | L1C:          0.000
!>  Satellite: G13 | L1C:       1116.767
!>  Satellite: G14 | L1C:          0.000
!>  Satellite: G18 | L1C:          0.000
!>  Satellite: G20 | L1C:       2818.259
!>  Satellite: G21 | L1C:          0.000
!>  Satellite: G22 | L1C:  125436598.314
!>  Satellite: G27 | L1C:          0.000
!>  Satellite: G30 | L1C:          0.000
!>--------------------------------------------------------------------

program obs_rinex3_prog2
  use, intrinsic :: iso_fortran_env, only: output_unit

  use obs_rinex3_types_mod
  use obs_rinex3_reader_mod, only: &
       read_header, &
       allocate_epoch_record, &
       read_next_epoch_record
  use obs_rinex3_writer_mod, only: &
       write_sys_num_obs_types, &
       write_epoch_record

  use obs_rinex3_indexes_mod, only: get_sys_index, get_obs_index
  
  implicit none

  integer(ip)          :: unit, ios
  type(header_t)       :: h
  character(len=2048)  :: err_msg = ""    ! Don't declare to small len
  type(epoch_record_t) :: epr

  integer(ip)          :: i_G, i_L1C, i_prn
  
  character(len=*), parameter   :: FILE_NAME = "example.rnx"

  ! 1. Open the RINEX observation file for reading
  open(newunit=unit, file=file_name, status='old', action='read', iostat=ios)
  if (ios /= 0) then
     print *, "Error: Unable to open RINEX file: " // FILE_NAME
     stop
  end if

  ! 2. Read the header
  call read_header(unit, h, err_msg)
  if (err_msg /= "") then
     print '(A)', "Error reading RINEX header"
     print '(A)', trim(err_msg)
     close(unit)
     stop
  end if

  associate (A => h%sys_num_obs_types)
    ! 3. Print observation types
    call write_sys_num_obs_types(output_unit, A)

    ! 4. Mandatory preparing for reading epoch recrods
    call allocate_epoch_record(A, epr)

    ! 5. Reading the first epoch record
    call read_next_epoch_record(unit, A, epr, ios, err_msg)
    if (err_msg /= "") then
       print '(A)', "Error reading RINEX observation data"
       print '(A)', trim(err_msg)
       close(unit)
       stop
    end if
  end associate

  ! 6. Get the satellite system index and observation index
  associate (B => epr%obs_by_sys)
    i_G = get_sys_index('G')
    if (i_G > 0 .and. i_G <= size(B)) then
       i_L1C = get_obs_index('G', 'L1C', h%sys_num_obs_types)
       
       ! 7. Print the selected data
       if (i_L1C > 0) then
          print '(A, I4.4,4(1X,I2.2),F11.7)', &
               "Epoch: ", epr%year, epr%month, epr%day, epr%hour, epr%min, epr%sec

          do i_prn = 1, B(i_G)%n_prns
             print '(A,I2.2,A,F14.3)', &
                  "  Satellite: G", B(i_G)%prns(i_prn), &
                  " | L1C: ", B(i_G)%sys_obs_by_sat(i_prn, i_L1C)%val
          end do
       else
          print '(A)', "No GPS L1C observations found in the first epoch."
       end if
    else
       print '(A)', "An unknown satellite system was entered."
    end if
  end associate

  close(unit)
end program obs_rinex3_prog2
