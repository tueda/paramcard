module test_paramcard
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use paramcard
    use paramcard_util, only: remove_spaces
    implicit none
    private

    public :: collect_test_paramcard

contains

    subroutine collect_test_paramcard(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest('get_param', test_get_param) &
                    ]
    end subroutine collect_test_paramcard

    subroutine test_get_param(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: n

        ! Parameter names are case-insensitive and spaces are removed, as in the FORTRAN tradition.

        call parse_param('paramcard command: clear')
        call parse_param('AB c = 10')

        call get_param('a bC', n)
        call check(error, n, 10)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! If the parameter is not given then the default value will be used.

        call parse_param('paramcard command: clear')

        call get_param('abc', n, 20)
        call check(error, n, 20)
        if (allocated(error)) return

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 30')

        call get_param('abc', n, 40)
        call check(error, n, 30)

        call check(error, param_changed())
        if (allocated(error)) return

        ! The case where the default value was overridden but unchanged.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 50')

        call get_param('abc', n, 50)
        call check(error, n, 50)

        call check(error,.not. param_changed())
        if (allocated(error)) return

        ! The case where the default value is given by the variable to be store the parameter.
        ! The default value is overridden.

        call parse_param('paramcard command: clear')
        call parse_param('ABC = 60')

        n = 70
        call get_param('abc', n, n)
        call check(error, n, 60)

        call check(error, param_changed())
        if (allocated(error)) return
    end subroutine test_get_param

    function param_changed() result(res)
        !! Return `.true.` if any of the parameters have been changed from their default values.

        logical :: res
            !! Whether any parameters have been changed.

        integer :: lun, ios
        character(200) :: buf

        open (newunit=lun, status='scratch')
        call write_param_summary(unit=lun, only_changed=.true.)
        rewind (lun)
        read (lun, '(a)', iostat=ios), buf
        close (lun)

        if (ios < 0) then
            res = .false.
        else
            res = .not. (remove_spaces(buf) == '')
        end if
    end function param_changed

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
                 new_testsuite('util', collect_test_paramcard) &
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
