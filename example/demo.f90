program demo
    use paramcard
    implicit none

    integer :: n1, n2
    double precision :: x1, x2
    character(:), allocatable :: str

    print *, '========== demo program begin =========='
    call get_param('n1', n1, 10)
    call get_param('n2', n2, 20)
    call get_param('x1', x1, 3.d0)
    call get_param('x2', x2, 4.d0)
    call get_param('str', str, 'abc')
    call write_param_summary()
    print *, '========== demo program end ============'
end program demo
