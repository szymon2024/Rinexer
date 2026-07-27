!>--------------------------------------------------------------------
!> SAMPLE PROGRAM 3
!> Computes the Ionosphere-Free phase combination for GPS satellites
!> for all epochs in the RINEX file and saves the results to a text
!> file.
!>
!> Note on terminology: An epoch represents a specific moment in time
!> (a timestamp).
!>
!> Input:  The FILE_NAME constant (input RINEX) and OUT_FILE_NAME
!>         constant.
!> Output: A text file containing computed L1/L2 combinations per
!>         satellite and epoch.
!>
!> Main steps:
!> 1. Open the RINEX observation file and the output text file.
!> 2. Read the RINEX header.
!> 3. Allocate memory for epoch records.
!> 4. Get GPS satellite system index and GPS L1 and L2 phase
!> observations indexes.
!> 5. Loop through all epochs until the end of the file is reached.
!>    Loop through all GPS satellites in each epoch, compute the
!>    combination, and write to file with the current epoch.
!> 6. Close all files.
!>
!> Compilation on Linux:  make -f Makefile.obs_rinex3_prog2
!> Run: obs_rinex3_prog2
!>
!> Compilaton on Windows: mingw32-make -f Makefile.obs_rinex3_prog2_win
!> Run: obs_rinex3_prog2.exe
!>
!> @version 1.0.0
!>--------------------------------------------------------------------
program obs_rinex3_prog3
  use, intrinsic :: iso_fortran_env, only: output_unit, iostat_end

  use obs_rinex3_types_mod
  use obs_rinex3_reader_mod, only: &
       read_header, &
       allocate_epoch_record, &
       read_next_epoch_record

  use obs_rinex3_indexes_mod, only: get_sys_index, get_obs_index
  
  implicit none

  integer(ip)          :: unit_in, unit_out, ios
  type(header_t)       :: h
  character(len=2048)  :: err_msg = ""
  type(epoch_record_t) :: epr

  integer(ip)          :: i_G, i_l1, i_l2, i_prn, epoch_count
  real(wp)             :: l1_val, l2_val, l_if
  
  ! Frequencies for GPS L1 and L2 (needed for Ionosphere-Free combination)
  real(wp), parameter  :: F1 = 1575.42_wp ! MHz
  real(wp), parameter  :: F2 = 1227.60_wp ! MHz
  real(wp), parameter  :: C1 = (F1**2) / (F1**2 - F2**2)
  real(wp), parameter  :: C2 = (F2**2) / (F1**2 - F2**2)

  character(len=*), parameter :: FILE_NAME     = "example.rnx"
  character(len=*), parameter :: OUT_FILE_NAME = "gps_L_IF_Comb_all_epochs.txt"

  ! 1. Open files
  open(newunit=unit_in, file=FILE_NAME, status='old', action='read', iostat=ios)
  if (ios /= 0) then
     print *, "Error: Unable to open RINEX file: " // FILE_NAME
     stop
  end if

  open(newunit=unit_out, file=OUT_FILE_NAME, status='replace', action='write', iostat=ios)
  if (ios /= 0) then
     print *, "Error: Unable to create output file: " // OUT_FILE_NAME
     close(unit_in)
     stop
  end if

  print '(A)', "Work in progress.."
  
  ! 2. Read header
  call read_header(unit_in, h, err_msg)
  if (len_trim(err_msg) > 0) then
     print '(A)', "Error reading RINEX header."
     print '(A)', trim(err_msg)
     close(unit_in)
     close(unit_out)
     stop
  end if

  ! 3. Allocate memory for epoch records
  call allocate_epoch_record(h%sys_num_obs_types, epr)


  ! Write header metadata to the output file
  write(unit_out, '(A)') "# GPS Ionosphere-Free Phase Combination - All Epochs"
  write(unit_out, '(A)') "# Epoch_Time               |Sat|   L1_Phase   |   L2_Phase   |   L_IF_Comb   "
  write(unit_out, '(A)') "#-------------------------------------------------------------------------------"

  epoch_count = 0
  ! 4. Get satellite system index and observation indexes
  i_G = get_sys_index('G')
  if (i_G <= 0 .or. i_G > size(epr%sats_obs)) then
     print *, "Error: Unkown satellite system."
     close(unit_in)
     close(unit_out)
     stop
  end if
  i_l1 = get_obs_index('G', 'L1C', h%sys_num_obs_types)
  i_l2 = get_obs_index('G', 'L2W', h%sys_num_obs_types)

  ! 5. Main loop over all epochs
  main_loop: do

     call read_next_epoch_record(unit_in, h%sys_num_obs_types, epr, ios, err_msg)
     if (len_trim(err_msg) > 0) then
        print '(A)', "Error reading RINEX observations data."
        print '(A)', trim(err_msg)
        close(unit_in)
        close(unit_out)
        stop
     end if

     if (ios == iostat_end) exit main_loop

     epoch_count = epoch_count + 1

     ! Process GPS data for the current epoch
     if (i_l1 > 0 .and. i_l2 > 0) then

        associate (B => epr%sats_obs)
          do i_prn = 1, B(i_G)%n_prns
             l1_val = B(i_G)%sys_sats_obs(i_l1, i_prn)%val
             l2_val = B(i_G)%sys_sats_obs(i_l2, i_prn)%val

             if (l1_val /= 0.0_wp .and. l2_val /= 0.0_wp) then

                ! Ionosphere-Free combination: L_IF = C1*L1 - C2*L2
                l_if = C1 * l1_val - C2 * l2_val

                write(unit_out, '(I4.4,4(1X,I2.2),F11.7,1X,A1,I2.2,3(1X,F14.3))') &
                     epr%year, epr%month, epr%day, epr%hour, epr%min, epr%sec, &
                     "G", B(i_G)%prns(i_prn), l1_val, l2_val, l_if
             end if
          end do
        end associate
        
     end if
  end do main_loop

  print '(A,I5,A)', "Success: Processed ", epoch_count, " epochs."
  print '(A)', "Results saved to " // OUT_FILE_NAME

  ! 6. Close all files
  close(unit_in)
  close(unit_out)
end program obs_rinex3_prog3
