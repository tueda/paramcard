module test_util
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use paramcard_util
    implicit none
    private

    public :: collect_test_util

contains

    subroutine collect_test_util(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("is_close", test_is_close), &
                    new_unittest("remove_spaces", test_remove_spaces), &
                    new_unittest("to_str", test_to_str), &
                    new_unittest("to_upper", test_to_upper) &
                    ]
    end subroutine collect_test_util

    subroutine test_is_close(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error,.not. is_close(1.000e0, 1.001e0))
        if (allocated(error)) return

        call check(error,.not. is_close(1.000d0, 1.001d0))
        if (allocated(error)) return
    end subroutine test_is_close

    subroutine test_remove_spaces(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, remove_spaces(' a b  cd e  ') == 'abcde')
        if (allocated(error)) return
    end subroutine test_remove_spaces

    subroutine test_to_str(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, to_str(12345) == '12345')
        if (allocated(error)) return

        call check(error, remove_spaces(to_str(-12345)) == '-12345')
        if (allocated(error)) return
    end subroutine test_to_str

    subroutine test_to_upper(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, to_upper('abC1@') == 'ABC1@')
        if (allocated(error)) return
    end subroutine test_to_upper

end module test_util

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_util, only: collect_test_util
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("util", collect_test_util) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program tester
