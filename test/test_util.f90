module test_util
    !! Helper routines for testing.

    use paramcard, only: get_param, parse_param, write_param_summary
    use paramcard_util, only: remove_spaces, to_str
    implicit none
    private

    public :: extend_params_array
    public :: extend_logs_array
    public :: param_changed
    public :: param_unused

contains

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

end module test_util
