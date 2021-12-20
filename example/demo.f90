program demo
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use paramcard, only: get_param, write_param_summary
    implicit none
    integer :: a, b
    real(dp) :: x, y
    character(:), allocatable :: msg

    call get_param('a', a, 1)
    call get_param('b', b, 2)
    call get_param('x', x, 0.3d0)
    call get_param('y', y, 0.4d0)
    call get_param('msg', msg, '')
    call write_param_summary

    print *, 'a + b = ', a + b
    print *, 'x + y = ', x + y
    if (msg /= '') print *, msg
end program demo
