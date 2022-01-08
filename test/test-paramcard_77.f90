module test_paramcard_77
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_util
    use paramcard, only: paramcard_parse
    implicit none
    private

    public :: collect_test_paramcard_77
contains

    subroutine collect_test_paramcard_77(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest('paramcard_get_s', test_paramcard_get_s), &
                    new_unittest('paramcard_get_i', test_paramcard_get_i), &
                    new_unittest('paramcard_get_r', test_paramcard_get_r), &
                    new_unittest('paramcard_get_d', test_paramcard_get_d), &
                    new_unittest('paramcard_set_s', test_paramcard_set_s), &
                    new_unittest('paramcard_set_i', test_paramcard_set_i), &
                    new_unittest('paramcard_set_r', test_paramcard_set_r), &
                    new_unittest('paramcard_set_d', test_paramcard_set_d) &
                    ]
    end subroutine collect_test_paramcard_77

    subroutine test_paramcard_get_s(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=5) :: s

        ! Value being longer than the length -> truncated.
        call paramcard_parse('paramcard command: clear')
        call paramcard_get_s('a', s, 'aB 123')
        call check(error, s, 'aB 12')
        if (allocated(error)) return
        call check(error, param_changed())  ! truncated
        if (allocated(error)) return

        ! Value being shorter than the length.
        call paramcard_parse('paramcard command: clear')
        call paramcard_get_s('a', s, 'a1')
        call check(error, s, 'a1   ')
        if (allocated(error)) return
        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! Same variable for `variable` and `default_value`, unchanged.
        call paramcard_parse('paramcard command: clear')
        s = 'a1234'
        call paramcard_get_s('a', s, s)
        call check(error, s, 'a1234')
        if (allocated(error)) return
        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! Same variable for `variable` and `default_value`, changed.
        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('a = b1234')
        s = 'a1234'
        call paramcard_get_s('a', s, s)
        call check(error, s, 'b1234')
        if (allocated(error)) return
        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_get_s

    subroutine test_paramcard_get_i(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: x, y

        call paramcard_parse('paramcard command: clear')
        x = 456
        call paramcard_get_i('x', x, x)
        y = 456
        call check(error, x, y)
        if (allocated(error)) return
        call check(error,.not. param_changed())
        if (allocated(error)) return

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('x = 123')
        x = 456
        call paramcard_get_i('x', x, x)
        y = 123
        call check(error, x, y)
        if (allocated(error)) return
        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_get_i

    subroutine test_paramcard_get_r(error)
        type(error_type), allocatable, intent(out) :: error

        real :: x, y

        call paramcard_parse('paramcard command: clear')
        x = 456
        call paramcard_get_r('x', x, x)
        y = 456
        call check(error, x, y)
        if (allocated(error)) return
        call check(error,.not. param_changed())
        if (allocated(error)) return

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('x = 123')
        x = 456
        call paramcard_get_r('x', x, x)
        y = 123
        call check(error, x, y)
        if (allocated(error)) return
        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_get_r

    subroutine test_paramcard_get_d(error)
        type(error_type), allocatable, intent(out) :: error

        double precision :: x, y

        call paramcard_parse('paramcard command: clear')
        x = 456
        call paramcard_get_d('x', x, x)
        y = 456
        call check(error, x, y)
        if (allocated(error)) return
        call check(error,.not. param_changed())
        if (allocated(error)) return

        call paramcard_parse('paramcard command: clear')
        call paramcard_parse('x = 123')
        x = 456
        call paramcard_get_d('x', x, x)
        y = 123
        call check(error, x, y)
        if (allocated(error)) return
        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_get_d

    subroutine test_paramcard_set_s(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=5) :: a, b, c

        call paramcard_parse('paramcard command: clear')
        a = 'abc42'
        b = a
        c = 'fail'
        call paramcard_set_s('a', a)
        call paramcard_get_s('a', a, c)
        call check(error, a, b)
        if (allocated(error)) return
        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_set_s

    subroutine test_paramcard_set_i(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: a, b, c

        call paramcard_parse('paramcard command: clear')
        a = 42
        b = a
        c = 0
        call paramcard_set_i('a', a)
        call paramcard_get_i('a', a, c)
        call check(error, a, b)
        if (allocated(error)) return
        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_set_i

    subroutine test_paramcard_set_r(error)
        type(error_type), allocatable, intent(out) :: error

        real :: a, b, c

        call paramcard_parse('paramcard command: clear')
        a = 42
        b = a
        c = 0
        call paramcard_set_r('a', a)
        call paramcard_get_r('a', a, c)
        call check(error, a, b)
        if (allocated(error)) return
        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_set_r

    subroutine test_paramcard_set_d(error)
        type(error_type), allocatable, intent(out) :: error

        double precision :: a, b, c

        call paramcard_parse('paramcard command: clear')
        a = 42
        b = a
        c = 0
        call paramcard_set_d('a', a)
        call paramcard_get_d('a', a, c)
        call check(error, a, b)
        if (allocated(error)) return
        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_paramcard_set_d

end module test_paramcard_77

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_paramcard_77, only: collect_test_paramcard_77
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite('paramcard_77', collect_test_paramcard_77) &
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
