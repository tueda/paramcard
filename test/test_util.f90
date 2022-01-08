module test_util
    !! Helper routines for testing.

    use paramcard, only: paramcard_get, paramcard_parse, paramcard_summary
    use paramcard_util, only: remove_spaces, to_string
    implicit none
    private

    public :: save_text
    public :: delete_file
    public :: extend_params_array
    public :: extend_logs_array
    public :: param_changed
    public :: param_unused

    interface save_text
        !! Save text into a file.
        module procedure save_text_1
        module procedure save_text_2
        module procedure save_text_3
        module procedure save_text_4
        module procedure save_text_5
        module procedure save_text_6
        module procedure save_text_7
        module procedure save_text_8
        module procedure save_text_9
        module procedure save_text_10
        module procedure save_text_11
        module procedure save_text_12
        module procedure save_text_13
        module procedure save_text_14
    end interface save_text

contains

    subroutine save_text_1(filename &
                           , text1 &
                           )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 1).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        close (lun)
    end subroutine save_text_1

    subroutine save_text_2(filename &
                           , text1 &
                           , text2 &
                           )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 2).
        character(len=*), intent(in) :: text2
            !! A line in the text (2 / 2).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        write (lun, '(a)') text2
        close (lun)
    end subroutine save_text_2

    subroutine save_text_3(filename &
                           , text1 &
                           , text2 &
                           , text3 &
                           )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 3).
        character(len=*), intent(in) :: text2
            !! A line in the text (2 / 3).
        character(len=*), intent(in) :: text3
            !! A line in the text (3 / 3).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        write (lun, '(a)') text2
        write (lun, '(a)') text3
        close (lun)
    end subroutine save_text_3

    subroutine save_text_4(filename &
                           , text1 &
                           , text2 &
                           , text3 &
                           , text4 &
                           )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 4).
        character(len=*), intent(in) :: text2
            !! A line in the text (2 / 4).
        character(len=*), intent(in) :: text3
            !! A line in the text (3 / 4).
        character(len=*), intent(in) :: text4
            !! A line in the text (4 / 4).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        write (lun, '(a)') text2
        write (lun, '(a)') text3
        write (lun, '(a)') text4
        close (lun)
    end subroutine save_text_4

    subroutine save_text_5(filename &
                           , text1 &
                           , text2 &
                           , text3 &
                           , text4 &
                           , text5 &
                           )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 5).
        character(len=*), intent(in) :: text2
            !! A line in the text (2 / 5).
        character(len=*), intent(in) :: text3
            !! A line in the text (3 / 5).
        character(len=*), intent(in) :: text4
            !! A line in the text (4 / 5).
        character(len=*), intent(in) :: text5
            !! A line in the text (5 / 5).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        write (lun, '(a)') text2
        write (lun, '(a)') text3
        write (lun, '(a)') text4
        write (lun, '(a)') text5
        close (lun)
    end subroutine save_text_5

    subroutine save_text_6(filename &
                           , text1 &
                           , text2 &
                           , text3 &
                           , text4 &
                           , text5 &
                           , text6 &
                           )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 6).
        character(len=*), intent(in) :: text2
            !! A line in the text (2 / 6).
        character(len=*), intent(in) :: text3
            !! A line in the text (3 / 6).
        character(len=*), intent(in) :: text4
            !! A line in the text (4 / 6).
        character(len=*), intent(in) :: text5
            !! A line in the text (5 / 6).
        character(len=*), intent(in) :: text6
            !! A line in the text (6 / 6).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        write (lun, '(a)') text2
        write (lun, '(a)') text3
        write (lun, '(a)') text4
        write (lun, '(a)') text5
        write (lun, '(a)') text6
        close (lun)
    end subroutine save_text_6

    subroutine save_text_7(filename &
                           , text1 &
                           , text2 &
                           , text3 &
                           , text4 &
                           , text5 &
                           , text6 &
                           , text7 &
                           )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 7).
        character(len=*), intent(in) :: text2
            !! A line in the text (2 / 7).
        character(len=*), intent(in) :: text3
            !! A line in the text (3 / 7).
        character(len=*), intent(in) :: text4
            !! A line in the text (4 / 7).
        character(len=*), intent(in) :: text5
            !! A line in the text (5 / 7).
        character(len=*), intent(in) :: text6
            !! A line in the text (6 / 7).
        character(len=*), intent(in) :: text7
            !! A line in the text (7 / 7).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        write (lun, '(a)') text2
        write (lun, '(a)') text3
        write (lun, '(a)') text4
        write (lun, '(a)') text5
        write (lun, '(a)') text6
        write (lun, '(a)') text7
        close (lun)
    end subroutine save_text_7

    subroutine save_text_8(filename &
                           , text1 &
                           , text2 &
                           , text3 &
                           , text4 &
                           , text5 &
                           , text6 &
                           , text7 &
                           , text8 &
                           )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 8).
        character(len=*), intent(in) :: text2
            !! A line in the text (2 / 8).
        character(len=*), intent(in) :: text3
            !! A line in the text (3 / 8).
        character(len=*), intent(in) :: text4
            !! A line in the text (4 / 8).
        character(len=*), intent(in) :: text5
            !! A line in the text (5 / 8).
        character(len=*), intent(in) :: text6
            !! A line in the text (6 / 8).
        character(len=*), intent(in) :: text7
            !! A line in the text (7 / 8).
        character(len=*), intent(in) :: text8
            !! A line in the text (8 / 8).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        write (lun, '(a)') text2
        write (lun, '(a)') text3
        write (lun, '(a)') text4
        write (lun, '(a)') text5
        write (lun, '(a)') text6
        write (lun, '(a)') text7
        write (lun, '(a)') text8
        close (lun)
    end subroutine save_text_8

    subroutine save_text_9(filename &
                           , text1 &
                           , text2 &
                           , text3 &
                           , text4 &
                           , text5 &
                           , text6 &
                           , text7 &
                           , text8 &
                           , text9 &
                           )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 9).
        character(len=*), intent(in) :: text2
            !! A line in the text (2 / 9).
        character(len=*), intent(in) :: text3
            !! A line in the text (3 / 9).
        character(len=*), intent(in) :: text4
            !! A line in the text (4 / 9).
        character(len=*), intent(in) :: text5
            !! A line in the text (5 / 9).
        character(len=*), intent(in) :: text6
            !! A line in the text (6 / 9).
        character(len=*), intent(in) :: text7
            !! A line in the text (7 / 9).
        character(len=*), intent(in) :: text8
            !! A line in the text (8 / 9).
        character(len=*), intent(in) :: text9
            !! A line in the text (9 / 9).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        write (lun, '(a)') text2
        write (lun, '(a)') text3
        write (lun, '(a)') text4
        write (lun, '(a)') text5
        write (lun, '(a)') text6
        write (lun, '(a)') text7
        write (lun, '(a)') text8
        write (lun, '(a)') text9
        close (lun)
    end subroutine save_text_9

    subroutine save_text_10(filename &
                            , text1 &
                            , text2 &
                            , text3 &
                            , text4 &
                            , text5 &
                            , text6 &
                            , text7 &
                            , text8 &
                            , text9 &
                            , text10 &
                            )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 10).
        character(len=*), intent(in) :: text2
            !! A line in the text (2 / 10).
        character(len=*), intent(in) :: text3
            !! A line in the text (3 / 10).
        character(len=*), intent(in) :: text4
            !! A line in the text (4 / 10).
        character(len=*), intent(in) :: text5
            !! A line in the text (5 / 10).
        character(len=*), intent(in) :: text6
            !! A line in the text (6 / 10).
        character(len=*), intent(in) :: text7
            !! A line in the text (7 / 10).
        character(len=*), intent(in) :: text8
            !! A line in the text (8 / 10).
        character(len=*), intent(in) :: text9
            !! A line in the text (9 / 10).
        character(len=*), intent(in) :: text10
            !! A line in the text (10 / 10).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        write (lun, '(a)') text2
        write (lun, '(a)') text3
        write (lun, '(a)') text4
        write (lun, '(a)') text5
        write (lun, '(a)') text6
        write (lun, '(a)') text7
        write (lun, '(a)') text8
        write (lun, '(a)') text9
        write (lun, '(a)') text10
        close (lun)
    end subroutine save_text_10

    subroutine save_text_11(filename &
                            , text1 &
                            , text2 &
                            , text3 &
                            , text4 &
                            , text5 &
                            , text6 &
                            , text7 &
                            , text8 &
                            , text9 &
                            , text10 &
                            , text11 &
                            )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 11).
        character(len=*), intent(in) :: text2
            !! A line in the text (2 / 11).
        character(len=*), intent(in) :: text3
            !! A line in the text (3 / 11).
        character(len=*), intent(in) :: text4
            !! A line in the text (4 / 11).
        character(len=*), intent(in) :: text5
            !! A line in the text (5 / 11).
        character(len=*), intent(in) :: text6
            !! A line in the text (6 / 11).
        character(len=*), intent(in) :: text7
            !! A line in the text (7 / 11).
        character(len=*), intent(in) :: text8
            !! A line in the text (8 / 11).
        character(len=*), intent(in) :: text9
            !! A line in the text (9 / 11).
        character(len=*), intent(in) :: text10
            !! A line in the text (10 / 11).
        character(len=*), intent(in) :: text11
            !! A line in the text (11 / 11).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        write (lun, '(a)') text2
        write (lun, '(a)') text3
        write (lun, '(a)') text4
        write (lun, '(a)') text5
        write (lun, '(a)') text6
        write (lun, '(a)') text7
        write (lun, '(a)') text8
        write (lun, '(a)') text9
        write (lun, '(a)') text10
        write (lun, '(a)') text11
        close (lun)
    end subroutine save_text_11

    subroutine save_text_12(filename &
                            , text1 &
                            , text2 &
                            , text3 &
                            , text4 &
                            , text5 &
                            , text6 &
                            , text7 &
                            , text8 &
                            , text9 &
                            , text10 &
                            , text11 &
                            , text12 &
                            )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 12).
        character(len=*), intent(in) :: text2
            !! A line in the text (2 / 12).
        character(len=*), intent(in) :: text3
            !! A line in the text (3 / 12).
        character(len=*), intent(in) :: text4
            !! A line in the text (4 / 12).
        character(len=*), intent(in) :: text5
            !! A line in the text (5 / 12).
        character(len=*), intent(in) :: text6
            !! A line in the text (6 / 12).
        character(len=*), intent(in) :: text7
            !! A line in the text (7 / 12).
        character(len=*), intent(in) :: text8
            !! A line in the text (8 / 12).
        character(len=*), intent(in) :: text9
            !! A line in the text (9 / 12).
        character(len=*), intent(in) :: text10
            !! A line in the text (10 / 12).
        character(len=*), intent(in) :: text11
            !! A line in the text (11 / 12).
        character(len=*), intent(in) :: text12
            !! A line in the text (12 / 12).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        write (lun, '(a)') text2
        write (lun, '(a)') text3
        write (lun, '(a)') text4
        write (lun, '(a)') text5
        write (lun, '(a)') text6
        write (lun, '(a)') text7
        write (lun, '(a)') text8
        write (lun, '(a)') text9
        write (lun, '(a)') text10
        write (lun, '(a)') text11
        write (lun, '(a)') text12
        close (lun)
    end subroutine save_text_12

    subroutine save_text_13(filename &
                            , text1 &
                            , text2 &
                            , text3 &
                            , text4 &
                            , text5 &
                            , text6 &
                            , text7 &
                            , text8 &
                            , text9 &
                            , text10 &
                            , text11 &
                            , text12 &
                            , text13 &
                            )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 13).
        character(len=*), intent(in) :: text2
            !! A line in the text (2 / 13).
        character(len=*), intent(in) :: text3
            !! A line in the text (3 / 13).
        character(len=*), intent(in) :: text4
            !! A line in the text (4 / 13).
        character(len=*), intent(in) :: text5
            !! A line in the text (5 / 13).
        character(len=*), intent(in) :: text6
            !! A line in the text (6 / 13).
        character(len=*), intent(in) :: text7
            !! A line in the text (7 / 13).
        character(len=*), intent(in) :: text8
            !! A line in the text (8 / 13).
        character(len=*), intent(in) :: text9
            !! A line in the text (9 / 13).
        character(len=*), intent(in) :: text10
            !! A line in the text (10 / 13).
        character(len=*), intent(in) :: text11
            !! A line in the text (11 / 13).
        character(len=*), intent(in) :: text12
            !! A line in the text (12 / 13).
        character(len=*), intent(in) :: text13
            !! A line in the text (13 / 13).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        write (lun, '(a)') text2
        write (lun, '(a)') text3
        write (lun, '(a)') text4
        write (lun, '(a)') text5
        write (lun, '(a)') text6
        write (lun, '(a)') text7
        write (lun, '(a)') text8
        write (lun, '(a)') text9
        write (lun, '(a)') text10
        write (lun, '(a)') text11
        write (lun, '(a)') text12
        write (lun, '(a)') text13
        close (lun)
    end subroutine save_text_13

    subroutine save_text_14(filename &
                            , text1 &
                            , text2 &
                            , text3 &
                            , text4 &
                            , text5 &
                            , text6 &
                            , text7 &
                            , text8 &
                            , text9 &
                            , text10 &
                            , text11 &
                            , text12 &
                            , text13 &
                            , text14 &
                            )
        !! Save text into a file.

        character(len=*), intent(in) :: filename
            !! The file name.
        character(len=*), intent(in) :: text1
            !! A line in the text (1 / 14).
        character(len=*), intent(in) :: text2
            !! A line in the text (2 / 14).
        character(len=*), intent(in) :: text3
            !! A line in the text (3 / 14).
        character(len=*), intent(in) :: text4
            !! A line in the text (4 / 14).
        character(len=*), intent(in) :: text5
            !! A line in the text (5 / 14).
        character(len=*), intent(in) :: text6
            !! A line in the text (6 / 14).
        character(len=*), intent(in) :: text7
            !! A line in the text (7 / 14).
        character(len=*), intent(in) :: text8
            !! A line in the text (8 / 14).
        character(len=*), intent(in) :: text9
            !! A line in the text (9 / 14).
        character(len=*), intent(in) :: text10
            !! A line in the text (10 / 14).
        character(len=*), intent(in) :: text11
            !! A line in the text (11 / 14).
        character(len=*), intent(in) :: text12
            !! A line in the text (12 / 14).
        character(len=*), intent(in) :: text13
            !! A line in the text (13 / 14).
        character(len=*), intent(in) :: text14
            !! A line in the text (14 / 14).

        integer :: lun

        open (newunit=lun, file=filename, status='replace')
        write (lun, '(a)') text1
        write (lun, '(a)') text2
        write (lun, '(a)') text3
        write (lun, '(a)') text4
        write (lun, '(a)') text5
        write (lun, '(a)') text6
        write (lun, '(a)') text7
        write (lun, '(a)') text8
        write (lun, '(a)') text9
        write (lun, '(a)') text10
        write (lun, '(a)') text11
        write (lun, '(a)') text12
        write (lun, '(a)') text13
        write (lun, '(a)') text14
        close (lun)
    end subroutine save_text_14

    subroutine delete_file(filename)
        !! Delete a file.

        character(len=*), intent(in) :: filename
            !! The file name.

        integer :: lun, ios

        open (newunit=lun, file=filename, status='old', iostat=ios)
        if (ios == 0) then
            close (lun, status='delete')
        end if
    end subroutine delete_file

    subroutine extend_params_array(n)
        !! Extend the internal dynamic array of parameters.
        integer, intent(in) :: n
            !! The number of parameters to be added.

        integer :: i

        do i = 1, n
            call paramcard_parse('dummy'//to_string(i)//' = 1')
        end do
    end subroutine extend_params_array

    subroutine extend_logs_array(n)
        !! Extend the internal dynamic array of logs.
        integer, intent(in) :: n
            !! The number of log items to be added.

        integer :: i, m

        call extend_params_array(n)

        do i = 1, n
            call paramcard_get('dummy'//to_string(i), m)
        end do
    end subroutine extend_logs_array

    function param_changed() result(res)
        !! Return `.true.` if any of the parameters have been changed from their default values.

        logical :: res
            !! Whether any parameters have been changed.

        integer :: lun, ios
        character(200) :: buf

        open (newunit=lun, status='scratch')
        call paramcard_summary(unit=lun, only_changed=.true., check_unused=.false.)
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
        call paramcard_summary(unit=lun, check_unused=.true.)
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
