!>--------------------------------------------------------------------
!> @version 1.0.3
!>
!> SAMPLE PROGRAM #1
!> Prints the header of a RINEX observation file.
!>
!> Input:  The FILE_NAME constant, which can be modified in the code.
!> Output: Displays the header on the screen (standard output).
!>
!> Main steps of program:
!> 1. Open the RINEX observation file for reading.
!> 2. Read the header.
!> 3. Print the header.
!> 4. Close the RINEX observation file.
!>
!> Note on implementation: The file is read line by line until the
!> entire header is read.
!>
!>--------------------------------------------------------------------
program obs_rinex3_prog1
  use, intrinsic :: iso_fortran_env, only: output_unit

  use obs_rinex3
  
  implicit none

  integer(ip)         :: unit, ios
  type(header_t)      :: obs_header
  character(len=2048) :: err_msg = ""
  
  character(len=*), parameter :: FILE_NAME = "example.rnx"

  ! 1. Open the RINEX observation file for reading.
  open(newunit=unit, file=file_name, status='old', action='read', iostat=ios)
  if (ios /= 0) then
     print *, "Error: Unable to open RINEX file: " // FILE_NAME
     stop
  end if

  ! 2. Read the header.
  call read_header(unit, obs_header, err_msg)
  if (err_msg(1:1) /= "") then
     print '(A)', "Error reading RINEX header."
     print '(A)', trim(err_msg)
     close(unit)
     stop
  end if

  ! 3. Print the header.
  call write_header(output_unit, obs_header)

  ! 4. Close the RINEX observation file.
  close(unit)
  
end program obs_rinex3_prog1
