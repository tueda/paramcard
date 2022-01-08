module test_paramcard_util
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use paramcard_util
    implicit none
    private

    public :: collect_test_paramcard_util

contains

    subroutine collect_test_paramcard_util(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest('is_close_real32', test_is_close_real32), &
                    new_unittest('is_close_real64', test_is_close_real64), &
                    new_unittest('remove_spaces', test_remove_spaces), &
                    new_unittest('to_string_int8', test_to_string_int8), &
                    new_unittest('to_string_int16', test_to_string_int16), &
                    new_unittest('to_string_int32', test_to_string_int32), &
                    new_unittest('to_string_int64', test_to_string_int64), &
                    new_unittest('to_string_real32', test_to_string_real32), &
                    new_unittest('to_string_real64', test_to_string_real64), &
                    new_unittest('to_upper', test_to_upper) &
                    ]
    end subroutine collect_test_paramcard_util

    subroutine test_is_close_real32(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error,.not. is_close(1.0_real32, 1.001_real32))
        if (allocated(error)) return

        call check(error, is_close(1.000_real32, 1.001_real32, 0.1_real32))
        if (allocated(error)) return

        call check(error, is_close(1.000_real32, 1.001_real32, 0.0_real32, 0.1_real32))
        if (allocated(error)) return
    end subroutine test_is_close_real32

    subroutine test_is_close_real64(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error,.not. is_close(1.0_real64, 1.001_real64))
        if (allocated(error)) return

        call check(error, is_close(1.000_real64, 1.001_real64, 0.1_real64))
        if (allocated(error)) return

        call check(error, is_close(1.000_real64, 1.001_real64, 0.0_real64, 0.1_real64))
        if (allocated(error)) return
    end subroutine test_is_close_real64

    subroutine test_remove_spaces(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, remove_spaces(' a b  cd e  '), 'abcde')
        if (allocated(error)) return

        call check(error, remove_spaces('     '), '')
        if (allocated(error)) return
    end subroutine test_remove_spaces

    subroutine test_to_string_int8(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: s
        integer(kind=int8) :: x, y

        x = 123_int8
        s = to_string(x)
        call check(error, s, '123')
        if (allocated(error)) return

        x = -123_int8
        s = to_string(x)
        read (s, *) y
        call check(error, x, y)
        if (allocated(error)) return
    end subroutine test_to_string_int8

    subroutine test_to_string_int16(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: s
        integer(kind=int16) :: x, y

        x = 123_int16
        s = to_string(x)
        call check(error, s, '123')
        if (allocated(error)) return

        x = -123_int16
        s = to_string(x)
        read (s, *) y
        call check(error, x, y)
        if (allocated(error)) return
    end subroutine test_to_string_int16

    subroutine test_to_string_int32(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: s
        integer(kind=int32) :: x, y

        x = 123_int32
        s = to_string(x)
        call check(error, s, '123')
        if (allocated(error)) return

        x = -123_int32
        s = to_string(x)
        read (s, *) y
        call check(error, x, y)
        if (allocated(error)) return
    end subroutine test_to_string_int32

    subroutine test_to_string_int64(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: s
        integer(kind=int64) :: x, y

        x = 123_int64
        s = to_string(x)
        call check(error, s, '123')
        if (allocated(error)) return

        x = -123_int64
        s = to_string(x)
        read (s, *) y
        call check(error, x, y)
        if (allocated(error)) return
    end subroutine test_to_string_int64

    subroutine test_to_string_real32(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: s
        real(kind=real32) :: x, y

        x = 3
        x = 1 / x
        s = to_string(x)
        read (s, *) y
        call check(error, is_close(x, y))
        if (allocated(error)) return
    end subroutine test_to_string_real32

    subroutine test_to_string_real64(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: s
        real(kind=real64) :: x, y

        x = 3
        x = 1 / x
        s = to_string(x)
        read (s, *) y
        call check(error, is_close(x, y))
        if (allocated(error)) return
    end subroutine test_to_string_real64

    subroutine test_to_upper(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, to_upper('abC1@'), 'ABC1@')
        if (allocated(error)) return
    end subroutine test_to_upper

end module test_paramcard_util

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_paramcard_util, only: collect_test_paramcard_util
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite('paramcard_util', collect_test_paramcard_util) &
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
