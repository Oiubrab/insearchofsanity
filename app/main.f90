program main
  use, intrinsic :: iso_fortran_env, only : output_unit
  use demo, only : substitute
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char, c_null_ptr
  ! We will use those GTK functions and values. The "only" statement can improve
  ! significantly the compilation time:
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use handlers
  implicit none
  character(len=256) :: pattern, replacement, input_file
  integer :: input
  integer(c_int)     :: status
  type(c_ptr)        :: app


  call get_command_argument(1, pattern)
  call get_command_argument(2, replacement)
  call get_command_argument(3, input_file)

  open(newunit=input, file=input_file, status='old')
  call substitute(input, output_unit, trim(pattern), trim(replacement))
  close(input)
  
    ! First, let's create a GTK application (it will initialize GTK).
  ! The application ID must contain at least one point:
  ! https://developer.gnome.org/gio/stable/GApplication.html#g-application-id-is-valid
  app = gtk_application_new("gtk-fortran.examples.gtkhello"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)
  ! The activate signal will be sent by g_application_run().
  ! The c_funloc() function returns the C address of the callback function.
  ! The c_null_ptr means no data is transfered to the callback function.
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)
  ! Now, the whole application will be managed by GLib (=> main loop).
  ! Note that commandline arguments argc, argv are not passed.
  ! https://developer.gnome.org/gio/stable/GApplication.html#g-application-run
  status = g_application_run(app, 0_c_int, [c_null_ptr])

  print *, "You have exited the GLib main loop, bye, bye..."

  ! Memory is freed:
  call g_object_unref(app)
end program main
