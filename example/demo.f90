program demo
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use paramcard, only: paramcard_get, paramcard_summary
    implicit none
    integer :: a, b
    real(dp) :: x, y
    character(:), allocatable :: msg

    call paramcard_get('a', a, 1)
    call paramcard_get('b', b, 2)
    call paramcard_get('x', x, 0.3d0)
    call paramcard_get('y', y, 0.4d0)
    call paramcard_get('msg', msg, '')
    call paramcard_summary

    print *, 'a + b = ', a + b
    print *, 'x + y = ', x + y
    if (msg /= '') print *, msg
end program demo
