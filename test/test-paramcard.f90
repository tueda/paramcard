module test_paramcard
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use paramcard
    use paramcard_util, only: remove_spaces, to_str
    implicit none
    private

    public :: collect_test_paramcard
    public :: param_changed

contains

    subroutine collect_test_paramcard(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest('get_param_str', test_get_param_str), &
                    new_unittest('get_param_int8', test_get_param_int8), &
                    new_unittest('get_param_int16', test_get_param_int16), &
                    new_unittest('get_param_int32', test_get_param_int32), &
                    new_unittest('get_param_int64', test_get_param_int64), &
                    new_unittest('get_param_real32', test_get_param_real32), &
                    new_unittest('get_param_real64', test_get_param_real64), &
                    new_unittest('set_param_str', test_set_param_str), &
                    new_unittest('set_param_int8', test_set_param_int8), &
                    new_unittest('set_param_int16', test_set_param_int16), &
                    new_unittest('set_param_int32', test_set_param_int32), &
                    new_unittest('set_param_int64', test_set_param_int64), &
                    new_unittest('set_param_real32', test_set_param_real32), &
                    new_unittest('set_param_real64', test_set_param_real64), &
                    new_unittest('parse_param', test_parse_param), &
                    new_unittest('format_param', test_format_param) &
                    ]
    end subroutine collect_test_paramcard

    subroutine test_get_param_str(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: s

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call parse_param('paramcard command: clear')
        call parse_param('AB c  =  aB 10  ')

        call get_param(' a bC ', s)
        call check(error, s, 'aB 10')
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call parse_param('paramcard command: clear')

        call get_param('abc', s, 'aB 20')
        call check(error, s, 'aB 20')
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = aB 30')

        call get_param('abc', s, 'aB 40')
        call check(error, s, 'aB 30')

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_get_param_str

    subroutine test_get_param_int8(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int8) :: n, x

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call parse_param('paramcard command: clear')
        call parse_param('AB c  =  10  ')

        x = 10
        call get_param(' a bC ', n)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call parse_param('paramcard command: clear')

        x = 20
        call get_param('abc', n, x)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 30')

        x = 40
        call get_param('abc', n, x)
        x = 30
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden but unchanged.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 50')

        x = 50
        call get_param('abc', n, x)
        call check(error, n, x)

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value is given by the variable to be store the parameter.
        ! The default value is overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 60')

        n = 70
        call get_param('abc', n, n)
        x = 60
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_get_param_int8

    subroutine test_get_param_int16(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int16) :: n, x

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call parse_param('paramcard command: clear')
        call parse_param('AB c  =  10  ')

        x = 10
        call get_param(' a bC ', n)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call parse_param('paramcard command: clear')

        x = 20
        call get_param('abc', n, x)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 30')

        x = 40
        call get_param('abc', n, x)
        x = 30
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden but unchanged.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 50')

        x = 50
        call get_param('abc', n, x)
        call check(error, n, x)

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value is given by the variable to be store the parameter.
        ! The default value is overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 60')

        n = 70
        call get_param('abc', n, n)
        x = 60
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_get_param_int16

    subroutine test_get_param_int32(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int32) :: n, x

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call parse_param('paramcard command: clear')
        call parse_param('AB c  =  10  ')

        x = 10
        call get_param(' a bC ', n)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call parse_param('paramcard command: clear')

        x = 20
        call get_param('abc', n, x)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 30')

        x = 40
        call get_param('abc', n, x)
        x = 30
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden but unchanged.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 50')

        x = 50
        call get_param('abc', n, x)
        call check(error, n, x)

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value is given by the variable to be store the parameter.
        ! The default value is overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 60')

        n = 70
        call get_param('abc', n, n)
        x = 60
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_get_param_int32

    subroutine test_get_param_int64(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int64) :: n, x

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call parse_param('paramcard command: clear')
        call parse_param('AB c  =  10  ')

        x = 10
        call get_param(' a bC ', n)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call parse_param('paramcard command: clear')

        x = 20
        call get_param('abc', n, x)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 30')

        x = 40
        call get_param('abc', n, x)
        x = 30
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden but unchanged.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 50')

        x = 50
        call get_param('abc', n, x)
        call check(error, n, x)

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value is given by the variable to be store the parameter.
        ! The default value is overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 60')

        n = 70
        call get_param('abc', n, n)
        x = 60
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_get_param_int64

    subroutine test_get_param_real32(error)
        type(error_type), allocatable, intent(out) :: error

        real(kind=real32) :: n, x

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call parse_param('paramcard command: clear')
        call parse_param('AB c  =  10  ')

        x = 10
        call get_param(' a bC ', n)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call parse_param('paramcard command: clear')

        x = 20
        call get_param('abc', n, x)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 30')

        x = 40
        call get_param('abc', n, x)
        x = 30
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden but unchanged.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 50')

        x = 50
        call get_param('abc', n, x)
        call check(error, n, x)

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value is given by the variable to be store the parameter.
        ! The default value is overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 60')

        n = 70
        call get_param('abc', n, n)
        x = 60
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_get_param_real32

    subroutine test_get_param_real64(error)
        type(error_type), allocatable, intent(out) :: error

        real(kind=real64) :: n, x

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call parse_param('paramcard command: clear')
        call parse_param('AB c  =  10  ')

        x = 10
        call get_param(' a bC ', n)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call parse_param('paramcard command: clear')

        x = 20
        call get_param('abc', n, x)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 30')

        x = 40
        call get_param('abc', n, x)
        x = 30
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden but unchanged.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 50')

        x = 50
        call get_param('abc', n, x)
        call check(error, n, x)

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value is given by the variable to be store the parameter.
        ! The default value is overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 60')

        n = 70
        call get_param('abc', n, n)
        x = 60
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_get_param_real64

    subroutine test_set_param_str(error)
        type(error_type), allocatable, intent(out) :: error

        call parse_param('paramcard command: clear')
        call set_param('a', 'abc123')
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_set_param_str

    subroutine test_set_param_int8(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int8) :: x

        x = 123

        call parse_param('paramcard command: clear')
        call set_param('a', x)
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_set_param_int8

    subroutine test_set_param_int16(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int16) :: x

        x = 123

        call parse_param('paramcard command: clear')
        call set_param('a', x)
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_set_param_int16

    subroutine test_set_param_int32(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int32) :: x

        x = 123

        call parse_param('paramcard command: clear')
        call set_param('a', x)
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_set_param_int32

    subroutine test_set_param_int64(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int64) :: x

        x = 123

        call parse_param('paramcard command: clear')
        call set_param('a', x)
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_set_param_int64

    subroutine test_set_param_real32(error)
        type(error_type), allocatable, intent(out) :: error

        real(kind=real32) :: x

        x = 123

        call parse_param('paramcard command: clear')
        call set_param('a', x)
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_set_param_real32

    subroutine test_set_param_real64(error)
        type(error_type), allocatable, intent(out) :: error

        real(kind=real64) :: x

        x = 123

        call parse_param('paramcard command: clear')
        call set_param('a', x)
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_set_param_real64

    subroutine test_parse_param(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: n1
        integer :: n2
        character(len=:), allocatable :: s

        call parse_param('paramcard command: clear')
        call parse_param('n1 = 123')
        call parse_param('n2 = 456')
        call parse_param('n1 = 789')

        ! NOTE: the initial capacity of `params` is 8.
        call extend_params_array(8)

        call parse_param('s  = abc')

        call get_param('n1', n1)
        call check(error, n1, 789)
        if (allocated(error)) return

        call get_param('n2', n2)
        call check(error, n2, 456)
        if (allocated(error)) return

        call get_param('s', s)
        call check(error, s, 'abc')
        if (allocated(error)) return
    end subroutine test_parse_param

    subroutine test_format_param(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: s
        integer :: n

        call parse_param('paramcard command: clear')
        call parse_param('n1 = 123')
        call parse_param('n2 = 456')

        ! NOTE: the initial capacity of `params` is 8.
        call extend_params_array(8)

        call get_param('n1', n)

        ! NOTE: the initial capacity of `logs` is 8.
        call extend_logs_array(8)

        call get_param('n2', s)

        s = format_param('ab{n1}cd{n2}ef')
        call check(error, s, 'ab123cd456ef')
        if (allocated(error)) return

        ! Test for a parameter set by set_param.
        call parse_param('paramcard command: clear')
        call set_param('x', 42)
        s = format_param('{x}')
        call check(error, s, '42')
    end subroutine test_format_param

    subroutine extend_params_array(n)
        !! Extend the internal dynamic array of parameters.
        integer, intent(in) :: n
            !! The number of parameters to be added.

        integer :: i

        do i = 1, n
            call parse_param('dummy'//to_str(i)//' = 1')
        end do
    end subroutine extend_params_array

    subroutine extend_logs_array(n)
        !! Extend the internal dynamic array of logs.
        integer, intent(in) :: n
            !! The number of log items to be added.

        integer :: i, m

        call extend_params_array(n)

        do i = 1, n
            call get_param('dummy'//to_str(i), m)
        end do
    end subroutine extend_logs_array

    function param_changed() result(res)
        !! Return `.true.` if any of the parameters have been changed from their default values.

        logical :: res
            !! Whether any parameters have been changed.

        integer :: lun, ios
        character(200) :: buf

        open (newunit=lun, status='scratch')
        call write_param_summary(unit=lun, only_changed=.true., check_unused=.false.)
        rewind (lun)
        read (lun, '(a)', iostat=ios) buf
        close (lun)

        if (ios < 0) then
            res = .false.
        else
            res = .not. (remove_spaces(buf) == '')
        end if
    end function param_changed

    function param_unused() result(res)
        !! Return `.true.` if any of the parameters have been not consumed.

        logical :: res
            !! Whether any parameters have been unused.

        integer :: lun, ios
        character(200) :: buf

        ! NOTE: currently this is not correctly implemented: abort for any
        ! unused parameters.
        open (newunit=lun, status='scratch')
        call write_param_summary(unit=lun, check_unused=.true.)
        rewind (lun)
        read (lun, '(a)', iostat=ios) buf
        close (lun)

        if (ios < 0) then
            res = .false.
        else
            res = .true.
        end if
    end function param_unused

end module test_paramcard

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_paramcard, only: collect_test_paramcard
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite('paramcard', collect_test_paramcard) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) 'Testing:', testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, 'test(s) failed!'
        error stop
    end if
end program tester
