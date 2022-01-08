module test_paramcard
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_util
    use paramcard
    implicit none
    private

    public :: collect_test_paramcard

contains

    subroutine collect_test_paramcard(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest('paramcard_get_str', test_paramcard_get_str), &
                    new_unittest('paramcard_get_int8', test_paramcard_get_int8), &
                    new_unittest('paramcard_get_int16', test_paramcard_get_int16), &
                    new_unittest('paramcard_get_int32', test_paramcard_get_int32), &
                    new_unittest('paramcard_get_int64', test_paramcard_get_int64), &
                    new_unittest('paramcard_get_real32', test_paramcard_get_real32), &
                    new_unittest('paramcard_get_real64', test_paramcard_get_real64), &
                    new_unittest('paramcard_set_str', test_paramcard_set_str), &
                    new_unittest('paramcard_set_int8', test_paramcard_set_int8), &
                    new_unittest('paramcard_set_int16', test_paramcard_set_int16), &
                    new_unittest('paramcard_set_int32', test_paramcard_set_int32), &
                    new_unittest('paramcard_set_int64', test_paramcard_set_int64), &
                    new_unittest('paramcard_set_real32', test_paramcard_set_real32), &
                    new_unittest('paramcard_set_real64', test_paramcard_set_real64), &
                    new_unittest('paramcard_load', test_paramcard_load), &
                    new_unittest('paramcard_parse', test_paramcard_parse), &
                    new_unittest('paramcard_format', test_paramcard_format) &
                    ]
    end subroutine collect_test_paramcard

    subroutine test_paramcard_get_str(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: s

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('AB c  =  aB 10  ')

        call paramcard_get(' a bC ', s)
        call check(error, s, 'aB 10')
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call paramcard_parse('paramcard command: clear')

        call paramcard_get('abc', s, 'aB 20')
        call check(error, s, 'aB 20')
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = aB 30')

        call paramcard_get('abc', s, 'aB 40')
        call check(error, s, 'aB 30')

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_get_str

    subroutine test_paramcard_get_int8(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int8) :: n, x

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('AB c  =  10  ')

        x = 10
        call paramcard_get(' a bC ', n)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call paramcard_parse('paramcard command: clear')

        x = 20
        call paramcard_get('abc', n, x)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 30')

        x = 40
        call paramcard_get('abc', n, x)
        x = 30
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden but unchanged.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 50')

        x = 50
        call paramcard_get('abc', n, x)
        call check(error, n, x)

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value is given by the variable to be store the parameter.
        ! The default value is overridden.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 60')

        n = 70
        call paramcard_get('abc', n, n)
        x = 60
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_get_int8

    subroutine test_paramcard_get_int16(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int16) :: n, x

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('AB c  =  10  ')

        x = 10
        call paramcard_get(' a bC ', n)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call paramcard_parse('paramcard command: clear')

        x = 20
        call paramcard_get('abc', n, x)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 30')

        x = 40
        call paramcard_get('abc', n, x)
        x = 30
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden but unchanged.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 50')

        x = 50
        call paramcard_get('abc', n, x)
        call check(error, n, x)

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value is given by the variable to be store the parameter.
        ! The default value is overridden.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 60')

        n = 70
        call paramcard_get('abc', n, n)
        x = 60
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_get_int16

    subroutine test_paramcard_get_int32(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int32) :: n, x

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('AB c  =  10  ')

        x = 10
        call paramcard_get(' a bC ', n)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call paramcard_parse('paramcard command: clear')

        x = 20
        call paramcard_get('abc', n, x)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 30')

        x = 40
        call paramcard_get('abc', n, x)
        x = 30
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden but unchanged.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 50')

        x = 50
        call paramcard_get('abc', n, x)
        call check(error, n, x)

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value is given by the variable to be store the parameter.
        ! The default value is overridden.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 60')

        n = 70
        call paramcard_get('abc', n, n)
        x = 60
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_get_int32

    subroutine test_paramcard_get_int64(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int64) :: n, x

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('AB c  =  10  ')

        x = 10
        call paramcard_get(' a bC ', n)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call paramcard_parse('paramcard command: clear')

        x = 20
        call paramcard_get('abc', n, x)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 30')

        x = 40
        call paramcard_get('abc', n, x)
        x = 30
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden but unchanged.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 50')

        x = 50
        call paramcard_get('abc', n, x)
        call check(error, n, x)

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value is given by the variable to be store the parameter.
        ! The default value is overridden.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 60')

        n = 70
        call paramcard_get('abc', n, n)
        x = 60
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_get_int64

    subroutine test_paramcard_get_real32(error)
        type(error_type), allocatable, intent(out) :: error

        real(kind=real32) :: n, x

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('AB c  =  10  ')

        x = 10
        call paramcard_get(' a bC ', n)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call paramcard_parse('paramcard command: clear')

        x = 20
        call paramcard_get('abc', n, x)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 30')

        x = 40
        call paramcard_get('abc', n, x)
        x = 30
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden but unchanged.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 50')

        x = 50
        call paramcard_get('abc', n, x)
        call check(error, n, x)

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value is given by the variable to be store the parameter.
        ! The default value is overridden.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 60')

        n = 70
        call paramcard_get('abc', n, n)
        x = 60
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_get_real32

    subroutine test_paramcard_get_real64(error)
        type(error_type), allocatable, intent(out) :: error

        real(kind=real64) :: n, x

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('AB c  =  10  ')

        x = 10
        call paramcard_get(' a bC ', n)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call paramcard_parse('paramcard command: clear')

        x = 20
        call paramcard_get('abc', n, x)
        call check(error, n, x)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 30')

        x = 40
        call paramcard_get('abc', n, x)
        x = 30
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden but unchanged.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 50')

        x = 50
        call paramcard_get('abc', n, x)
        call check(error, n, x)

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value is given by the variable to be store the parameter.
        ! The default value is overridden.

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('ABC = 60')

        n = 70
        call paramcard_get('abc', n, n)
        x = 60
        call check(error, n, x)

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_get_real64

    subroutine test_paramcard_set_str(error)
        type(error_type), allocatable, intent(out) :: error

        call paramcard_parse('paramcard command: clear')
        call paramcard_set('a', 'abc123')
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_paramcard_set_str

    subroutine test_paramcard_set_int8(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int8) :: x

        x = 123

        call paramcard_parse('paramcard command: clear')
        call paramcard_set('a', x)
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_paramcard_set_int8

    subroutine test_paramcard_set_int16(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int16) :: x

        x = 123

        call paramcard_parse('paramcard command: clear')
        call paramcard_set('a', x)
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_paramcard_set_int16

    subroutine test_paramcard_set_int32(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int32) :: x

        x = 123

        call paramcard_parse('paramcard command: clear')
        call paramcard_set('a', x)
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_paramcard_set_int32

    subroutine test_paramcard_set_int64(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int64) :: x

        x = 123

        call paramcard_parse('paramcard command: clear')
        call paramcard_set('a', x)
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_paramcard_set_int64

    subroutine test_paramcard_set_real32(error)
        type(error_type), allocatable, intent(out) :: error

        real(kind=real32) :: x

        x = 123

        call paramcard_parse('paramcard command: clear')
        call paramcard_set('a', x)
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_paramcard_set_real32

    subroutine test_paramcard_set_real64(error)
        type(error_type), allocatable, intent(out) :: error

        real(kind=real64) :: x

        x = 123

        call paramcard_parse('paramcard command: clear')
        call paramcard_set('a', x)
        call check(error,.not. param_unused())
        if (allocated(error)) return
    end subroutine test_paramcard_set_real64

    subroutine test_paramcard_load(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: text(:)

        integer :: x

        call paramcard_parse('paramcard command: clear')
        call save_text('test_load_1.txt', &
                       '# comment line', &
                       '', &
                       'x = 42', &
                       '  ! x = 57' &
                       )
        call paramcard_parse('paramcard command: load, test_load_1.txt')
        call delete_file('test_load_1.txt')
        call paramcard_get('x', x)
        call check(error, x, 42)
        if (allocated(error)) return
    end subroutine test_paramcard_load

    subroutine test_paramcard_parse(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: n1
        integer :: n2
        character(len=:), allocatable :: s

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('n1 = 123')
        call paramcard_parse('n2 = 456')
        call paramcard_parse('n1 = 789')

        ! NOTE: the initial capacity of `params` is 8.
        call extend_params_array(8)

        call paramcard_parse('s  = abc')

        call paramcard_get('n1', n1)
        call check(error, n1, 789)
        if (allocated(error)) return

        call paramcard_get('n2', n2)
        call check(error, n2, 456)
        if (allocated(error)) return

        call paramcard_get('s', s)
        call check(error, s, 'abc')
        if (allocated(error)) return
    end subroutine test_paramcard_parse

    subroutine test_paramcard_format(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: s
        integer :: n

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('n1 = 123')
        call paramcard_parse('n2 = 456')

        ! NOTE: the initial capacity of `params` is 8.
        call extend_params_array(8)

        call paramcard_get('n1', n)

        ! NOTE: the initial capacity of `logs` is 8.
        call extend_logs_array(8)

        call paramcard_get('n2', s)

        s = paramcard_format('ab{n1}cd{n2}ef')
        call check(error, s, 'ab123cd456ef')
        if (allocated(error)) return

        ! Test for a parameter set by set_param.
        call paramcard_parse('paramcard command: clear')
        call paramcard_set('x', 42)
        s = paramcard_format('{x}')
        call check(error, s, '42')
    end subroutine test_paramcard_format

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
