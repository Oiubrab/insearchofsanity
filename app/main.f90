program main
  use, intrinsic :: iso_fortran_env, only : output_unit
  use demo, only : substitute
  implicit none
  character(len=256) :: pattern, replacement, input_file
  integer :: input

  call get_command_argument(1, pattern)
  call get_command_argument(2, replacement)
  call get_command_argument(3, input_file)

  open(newunit=input, file=input_file, status='old')
  call substitute(input, output_unit, trim(pattern), trim(replacement))
  close(input)
end program main
