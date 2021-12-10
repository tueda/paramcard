module paramcard_util
    !! This module contains utility routines.

    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    implicit none
    private

    public is_close
    public remove_spaces
    public to_str
    public to_upper

    interface is_close
        !! Return true if two values are close to each other.
        module procedure is_close_real32
        module procedure is_close_real64
    end interface is_close

    interface to_str
        !! Return the string representation of the given value.
        module procedure to_str_int8
        module procedure to_str_int16
        module procedure to_str_int32
        module procedure to_str_int64
        module procedure to_str_real32
        module procedure to_str_real64
    end interface to_str

    integer, parameter :: MAX_BUF = 50
    integer, parameter :: DEFAULT_EPSILON_COEFF = 100

contains

    pure function is_close_real32(a, b, rel_tol, abs_tol) result(res)
        !! Return true if `a` and `b` are close to each other.

        real(kind=real32), intent(in) :: a
            !! The first number.
        real(kind=real32), intent(in) :: b
            !! The second number.
        real(kind=real32), optional, intent(in) :: rel_tol
            !! The relative tolerance.
        real(kind=real32), optional, intent(in) :: abs_tol
            !! The absolute tolerance.
        logical :: res
            !! The result.

        real(kind=real32) :: rtol, atol

        if (present(rel_tol)) then
            rtol = abs(rel_tol)
        else
            rtol = DEFAULT_EPSILON_COEFF * epsilon(rtol)
        end if

        if (present(abs_tol)) then
            atol = abs(abs_tol)
        else
            atol = 0
        end if

        res = abs(a - b) <= max(rtol * max(abs(a), abs(b)), atol)
    end function is_close_real32

    pure function is_close_real64(a, b, rel_tol, abs_tol) result(res)
        !! Return true if `a` and `b` are close to each other.

        real(kind=real64), intent(in) :: a
            !! The first number.
        real(kind=real64), intent(in) :: b
            !! The second number.
        real(kind=real64), optional, intent(in) :: rel_tol
            !! The relative tolerance.
        real(kind=real64), optional, intent(in) :: abs_tol
            !! The absolute tolerance.
        logical :: res
            !! The result.

        real(kind=real64) :: rtol, atol

        if (present(rel_tol)) then
            rtol = abs(rel_tol)
        else
            rtol = DEFAULT_EPSILON_COEFF * epsilon(rtol)
        end if

        if (present(abs_tol)) then
            atol = abs(abs_tol)
        else
            atol = 0
        end if

        res = abs(a - b) <= max(rtol * max(abs(a), abs(b)), atol)
    end function is_close_real64

    pure function remove_spaces(str) result(res)
        !! Remove whitespace in the given string.

        character(len=*), intent(in) :: str
            !! The string.
        character(len=:), allocatable :: res
            !! The string without spaces.

        character(len=len_trim(str)) :: tmp
        integer :: i, j, c

        tmp = adjustl(str)

        j = 0

        do i = 1, len_trim(tmp)
            c = iachar(tmp(i:i))
            if ((c == int(z'20')) .or. (c >= int(z'09') .and. c <= int(z'0D'))) then
                cycle
            end if
            j = j + 1
            tmp(j:j) = tmp(i:i)
        end do

        if (j == 0) then
            res = ''
        else
            res = tmp(1:j)
        end if
    end function remove_spaces

    pure function to_str_int8(value) result(res)
        !! Return the string representation of the given value.

        integer(kind=int8), intent(in) :: value
            !! The value.
        character(len=:), allocatable :: res
            !! The string representation.

        character(len=MAX_BUF) :: tmp

        write (tmp, *) value
        res = trim(adjustl(tmp))
    end function to_str_int8

    pure function to_str_int16(value) result(res)
        !! Return the string representation of the given value.

        integer(kind=int16), intent(in) :: value
            !! The value.
        character(len=:), allocatable :: res
            !! The string representation.

        character(len=MAX_BUF) :: tmp

        write (tmp, *) value
        res = trim(adjustl(tmp))
    end function to_str_int16

    pure function to_str_int32(value) result(res)
        !! Return the string representation of the given value.

        integer(kind=int32), intent(in) :: value
            !! The value.
        character(len=:), allocatable :: res
            !! The string representation.

        character(len=MAX_BUF) :: tmp

        write (tmp, *) value
        res = trim(adjustl(tmp))
    end function to_str_int32

    pure function to_str_int64(value) result(res)
        !! Return the string representation of the given value.

        integer(kind=int64), intent(in) :: value
            !! The value.
        character(len=:), allocatable :: res
            !! The string representation.

        character(len=MAX_BUF) :: tmp

        write (tmp, *) value
        res = trim(adjustl(tmp))
    end function to_str_int64

    pure function to_str_real32(value) result(res)
        !! Return the string representation of the given value.

        real(kind=real32), intent(in) :: value
            !! The value.
        character(len=:), allocatable :: res
            !! The string representation.

        character(len=MAX_BUF) :: tmp

        write (tmp, *) value
        res = trim(adjustl(tmp))
    end function to_str_real32

    pure function to_str_real64(value) result(res)
        !! Return the string representation of the given value.

        real(kind=real64), intent(in) :: value
            !! The value.
        character(len=:), allocatable :: res
            !! The string representation.

        character(len=MAX_BUF) :: tmp

        write (tmp, *) value
        res = trim(adjustl(tmp))
    end function to_str_real64

    pure function to_upper(str) result(res)
        !! Convert a string into uppercase.

        character(len=*), intent(in) :: str
            !! The string to be converted.
        character(len=len(str)) :: res
            !! The converted uppercase string.

        integer :: i, c

        res = str

        do i = 1, len_trim(res)
            c = iachar(res(i:i))
            select case (c)
            case (97:122)
                res(i:i) = achar(c - 32)
            end select
        end do
    end function to_upper

end module paramcard_util

module paramcard
    !! This module provides a way to conveniently give parameters to a program
    !! from its command line arguments.
    use paramcard_util
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    implicit none
    private

    public :: get_param
    public :: write_param_summary
    public :: parse_param

    interface get_param
        !! Retrieve a parameter.
        module procedure get_param_str
        module procedure get_param_len_str
        module procedure get_param_int8
        module procedure get_param_int16
        module procedure get_param_int32
        module procedure get_param_int64
        module procedure get_param_real32
        module procedure get_param_real64
    end interface get_param

    type param_type
        !! The type to store information of a parameter.
        character(len=:), allocatable :: name
            !! The parameter name. All spaces removed, uppercase.
        character(len=:), allocatable :: value
            !! The parameter value.
        logical :: consumed
            !! Whether or not this parameter has been consumed.
    end type param_type

    type param_log_type
        !! The type to store information on how a parameter was consumed via `get_param`.
        character(len=:), allocatable :: name
            !! The name with which the parameter was consumed.
            !! All spaces removed, but not necessarily uppercase.
        character(len=:), allocatable :: value
            !! The string representation of the consumed value.
        character(len=:), allocatable :: default_value
            !! Optional. The default value (/= consumed value) provided when this parameter was consumed.
    end type param_log_type

    logical :: inited = .false.
        !! Whether or not the library is initialized.
    integer :: n_params
        !! The number of stored parameters.
    integer :: params_capacity
        !! The capacity of the `params` dynamic array.
    type(param_type), allocatable :: params(:)
        !! Stored parameters.
    integer :: n_logs
        !! The number of items for the consumption logs.
    integer :: logs_capacity
        !! The capacity of the `logs` dynamic array.
    type(param_log_type), allocatable :: logs(:)
        !! Stored log items.

    integer, parameter :: MIN_BUF = 40
    integer, parameter :: INITIAL_CAPACITY = 8

contains
    subroutine get_param_str(name, variable, default_value)
        !! Retrieve a string parameter.

        character(len=*), intent(in) :: name
            !! The name of the parameter.
        character(len=:), intent(out), allocatable :: variable
            !! The variable to store the parameter value.
        character(len=*), intent(in), optional :: default_value
            !! The value to be used if the parameter not found.

        character(len=:), allocatable :: tmp, canon_name

        call get_param_str_impl(name, tmp, canon_name)

        if (allocated(tmp)) then
            variable = tmp
        else if (present(default_value)) then
            variable = trim(adjustl(default_value))
        else
            write (error_unit, '(a)') '[ERROR] paramcard: parameter undefined: '//canon_name
            error stop
        end if

        if (present(default_value)) then
            if (variable /= trim(adjustl(default_value))) then
                call add_log(canon_name, variable, trim(adjustl(default_value)))
                return
            end if
        end if
        call add_log(canon_name, variable)
    end subroutine get_param_str

    subroutine get_param_len_str(length, name, variable, default_value)
        !! Retrieve a string parameter.

        integer, intent(in) :: length
            !! The length of `variable`.
        character(len=*), intent(in) :: name
            !! The name of the parameter.
        character(len=length), intent(out) :: variable
            !! The variable to store the parameter value.
        character(len=*), intent(in) :: default_value
            !! The value to be used if the parameter not found.

        character(len=:), allocatable :: tmp, canon_name, default_value_

        call get_param_str_impl(name, tmp, canon_name)

        ! This procedure aims to be called via the FORTRAN 77 interface.
        ! `variable` and `default_value` may point to the same memory location.
        ! Copy `default_value` to a temporary buffer.
        default_value_ = default_value

        if (allocated(tmp)) then
            variable = tmp
        else
            variable = trim(adjustl(default_value_))
        end if

        if (variable /= trim(adjustl(default_value_))) then
            call add_log(canon_name, trim(variable), trim(adjustl(default_value_)))
            return
        end if
        call add_log(canon_name, trim(variable))
    end subroutine get_param_len_str

    subroutine get_param_int8(name, variable, default_value)
        !! Retrieve an integer parameter.

        character(len=*), intent(in) :: name
            !! The name of the parameter.
        integer(kind=int8), intent(out) :: variable
            !! The variable to store the parameter value.
        integer(kind=int8), intent(in), optional :: default_value
            !! The value to be used if the parameter not found.

        character(len=:), allocatable :: tmp, canon_name
        integer :: iostat

        call get_param_str_impl(name, tmp, canon_name)

        if (allocated(tmp)) then
            read (tmp, *, iostat=iostat) variable
            if (iostat /= 0) then
                write (error_unit, '(a)') '[ERROR] paramcard: failed to parse an integer value: '// &
                    canon_name//' = '//tmp
                error stop
            end if
        else if (present(default_value)) then
            variable = default_value
        else
            write (error_unit, '(a)') '[ERROR] paramcard: integer parameter undefined: '//canon_name
            error stop
        end if

        if (present(default_value)) then
            if (variable /= default_value) then
                call add_log(canon_name, to_str(variable), to_str(default_value))
                return
            end if
        end if
        call add_log(canon_name, to_str(variable))
    end subroutine get_param_int8

    subroutine get_param_int16(name, variable, default_value)
        !! Retrieve an integer parameter.

        character(len=*), intent(in) :: name
            !! The name of the parameter.
        integer(kind=int16), intent(out) :: variable
            !! The variable to store the parameter value.
        integer(kind=int16), intent(in), optional :: default_value
            !! The value to be used if the parameter not found.

        character(len=:), allocatable :: tmp, canon_name
        integer :: iostat

        call get_param_str_impl(name, tmp, canon_name)

        if (allocated(tmp)) then
            read (tmp, *, iostat=iostat) variable
            if (iostat /= 0) then
                write (error_unit, '(a)') '[ERROR] paramcard: failed to parse an integer value: '// &
                    canon_name//' = '//tmp
                error stop
            end if
        else if (present(default_value)) then
            variable = default_value
        else
            write (error_unit, '(a)') '[ERROR] paramcard: integer parameter undefined: '//canon_name
            error stop
        end if

        if (present(default_value)) then
            if (variable /= default_value) then
                call add_log(canon_name, to_str(variable), to_str(default_value))
                return
            end if
        end if
        call add_log(canon_name, to_str(variable))
    end subroutine get_param_int16

    subroutine get_param_int32(name, variable, default_value)
        !! Retrieve an integer parameter.

        character(len=*), intent(in) :: name
            !! The name of the parameter.
        integer(kind=int32), intent(out) :: variable
            !! The variable to store the parameter value.
        integer(kind=int32), intent(in), optional :: default_value
            !! The value to be used if the parameter not found.

        character(len=:), allocatable :: tmp, canon_name
        integer :: iostat

        call get_param_str_impl(name, tmp, canon_name)

        if (allocated(tmp)) then
            read (tmp, *, iostat=iostat) variable
            if (iostat /= 0) then
                write (error_unit, '(a)') '[ERROR] paramcard: failed to parse an integer value: '// &
                    canon_name//' = '//tmp
                error stop
            end if
        else if (present(default_value)) then
            variable = default_value
        else
            write (error_unit, '(a)') '[ERROR] paramcard: integer parameter undefined: '//canon_name
            error stop
        end if

        if (present(default_value)) then
            if (variable /= default_value) then
                call add_log(canon_name, to_str(variable), to_str(default_value))
                return
            end if
        end if
        call add_log(canon_name, to_str(variable))
    end subroutine get_param_int32

    subroutine get_param_int64(name, variable, default_value)
        !! Retrieve an integer parameter.

        character(len=*), intent(in) :: name
            !! The name of the parameter.
        integer(kind=int64), intent(out) :: variable
            !! The variable to store the parameter value.
        integer(kind=int64), intent(in), optional :: default_value
            !! The value to be used if the parameter not found.

        character(len=:), allocatable :: tmp, canon_name
        integer :: iostat

        call get_param_str_impl(name, tmp, canon_name)

        if (allocated(tmp)) then
            read (tmp, *, iostat=iostat) variable
            if (iostat /= 0) then
                write (error_unit, '(a)') '[ERROR] paramcard: failed to parse an integer value: '// &
                    canon_name//' = '//tmp
                error stop
            end if
        else if (present(default_value)) then
            variable = default_value
        else
            write (error_unit, '(a)') '[ERROR] paramcard: integer parameter undefined: '//canon_name
            error stop
        end if

        if (present(default_value)) then
            if (variable /= default_value) then
                call add_log(canon_name, to_str(variable), to_str(default_value))
                return
            end if
        end if
        call add_log(canon_name, to_str(variable))
    end subroutine get_param_int64

    subroutine get_param_real32(name, variable, default_value)
        !! Retrieve a real parameter.

        character(len=*), intent(in) :: name
            !! The name of the parameter.
        real(kind=real32), intent(out) :: variable
            !! The variable to store the parameter value.
        real(kind=real32), intent(in), optional :: default_value
            !! The value to be used if the parameter not found.

        character(len=:), allocatable :: tmp, canon_name
        integer :: iostat

        call get_param_str_impl(name, tmp, canon_name)

        if (allocated(tmp)) then
            read (tmp, *, iostat=iostat) variable
            if (iostat /= 0) then
                write (error_unit, '(a)') '[ERROR] paramcard: failed to parse a real value: '// &
                    canon_name//' = '//tmp
                error stop
            end if
        else if (present(default_value)) then
            variable = default_value
        else
            write (error_unit, '(a)') '[ERROR] paramcard: real parameter undefined: '//canon_name
            error stop
        end if

        if (present(default_value)) then
            if (.not. is_close(variable, default_value)) then
                call add_log(canon_name, to_str(variable), to_str(default_value))
                return
            end if
        end if
        call add_log(canon_name, to_str(variable))
    end subroutine get_param_real32

    subroutine get_param_real64(name, variable, default_value)
        !! Retrieve a real parameter.

        character(len=*), intent(in) :: name
            !! The name of the parameter.
        real(kind=real64), intent(out) :: variable
            !! The variable to store the parameter value.
        real(kind=real64), intent(in), optional :: default_value
            !! The value to be used if the parameter not found.

        character(len=:), allocatable :: tmp, canon_name
        integer :: iostat

        call get_param_str_impl(name, tmp, canon_name)

        if (allocated(tmp)) then
            read (tmp, *, iostat=iostat) variable
            if (iostat /= 0) then
                write (error_unit, '(a)') '[ERROR] paramcard: failed to parse a real value: '// &
                    canon_name//' = '//tmp
                error stop
            end if
        else if (present(default_value)) then
            variable = default_value
        else
            write (error_unit, '(a)') '[ERROR] paramcard: real parameter undefined: '//canon_name
            error stop
        end if

        if (present(default_value)) then
            if (.not. is_close(variable, default_value)) then
                call add_log(canon_name, to_str(variable), to_str(default_value))
                return
            end if
        end if
        call add_log(canon_name, to_str(variable))
    end subroutine get_param_real64

    subroutine write_param_summary(unit, only_changed, show_default, check_unused, prefix)
        !! Write the parameter summary.

        integer, optional, intent(in) :: unit
            !! The unit number for the output.
        logical, optional, intent(in) :: only_changed
            !! Write only parameters that are changed from those default values.
        logical, optional, intent(in) :: show_default
            !! Show the default values.
        logical, optional, intent(in) :: check_unused
            !! Check unused parameters.
        character(len=*), optional, intent(in) :: prefix
            !! The prefix to each line.

        integer unit_
        logical :: only_changed_, show_default_, check_unused_
        character(len=:), allocatable :: prefix_
        integer :: i
        logical :: found

        if (present(unit)) then
            unit_ = unit
        else
            unit_ = output_unit
        end if

        if (present(only_changed)) then
            only_changed_ = only_changed
        else
            only_changed_ = .false.
        end if

        if (present(show_default)) then
            show_default_ = show_default
        else
            show_default_ = .true.
        end if

        if (present(check_unused)) then
            check_unused_ = check_unused
        else
            check_unused_ = .true.
        end if

        if (present(prefix)) then
            prefix_ = prefix
        else
            prefix_ = ''
        end if

        do i = 1, n_logs
            if (only_changed_) then
                if (.not. allocated(logs(i)%default_value)) then
                    cycle
                end if
            end if
            if (show_default_) then
                if (allocated(logs(i)%default_value)) then
                    write (unit_, '(a,a," = ",a," (default: ",a,")")') &
                        prefix_, logs(i)%name, logs(i)%value, logs(i)%default_value
                    cycle
                end if
            end if
            write (unit_, '(a,a," = ",a)') prefix_, logs(i)%name, logs(i)%value
        end do

        if (check_unused_) then
            found = .false.
            do i = 1, n_params
                if (.not. params(i)%consumed) then
                    found = .true.
                    exit
                end if
            end do
            if (found) then
                write (error_unit, '(a)') '[ERROR] paramcard: unused parameter found'
                do i = 1, n_params
                    if (.not. params(i)%consumed) then
                        write (error_unit, '("[ERROR]            ", a," = ",a)') &
                            params(i)%name, params(i)%value
                    end if
                end do
                error stop
            end if
        end if
    end subroutine write_param_summary

    subroutine parse_param(str)
        !! Parse a string containing `'NAME = VALUE'`.

        character(len=*), intent(in) :: str
            !! The string to be parsed.

        character(len=:), allocatable :: str1

        str1 = to_upper(remove_spaces(str))

        ! NOTE: this subroutine accepts undocumented special commands.

        if (str1 == 'PARAMCARDCOMMAND:CLEAR') then
            call clear
        else
            call parse_line(str)
        end if
    end subroutine

    subroutine get_param_str_impl(name, variable, canon_name)
        !! Retrieve a string parameter.

        character(len=*), intent(in) :: name
            !! The name of the parameter.
        character(len=:), intent(out), allocatable :: variable
            !! The variable to store the parameter value.
        character(len=:), intent(out), allocatable :: canon_name
            !! The variable to store the canonicalized (space-removed) parameter name.

        integer :: i
        character(len=:), allocatable :: key_name

        canon_name = remove_spaces(name)

        if (len(canon_name) == 0) then
            write (error_unit, '(a)') '[ERROR] paramcard: empty parameter name specified in get_param'
            error stop
        end if

        call init

        key_name = to_upper(canon_name)
        i = find_param(key_name)
        if (i >= 1) then
            variable = params(i)%value
            params(i)%consumed = .true.
        end if
    end subroutine get_param_str_impl

    subroutine init
        !! Ensure that the library is initialized.

        if (inited) then
            return
        end if

        n_params = 0
        params_capacity = INITIAL_CAPACITY
        allocate (params(params_capacity))

        n_logs = 0
        logs_capacity = INITIAL_CAPACITY
        allocate (logs(logs_capacity))

        call parse_arguments

        inited = .true.
    end subroutine init

    subroutine clear
        !! Clear all parameters.

        call init

        deallocate (params)
        deallocate (logs)

        n_params = 0
        params_capacity = INITIAL_CAPACITY
        allocate (params(params_capacity))

        n_logs = 0
        logs_capacity = INITIAL_CAPACITY
        allocate (logs(logs_capacity))
    end subroutine clear

    subroutine parse_arguments
        !! Parse the command line arguments and add parameters found in them.

        integer :: i, length, buf_length
        character(len=:), allocatable :: buf

        buf_length = 0

        do i = 1, command_argument_count()
            call get_command_argument(i, length=length)
            if (length > buf_length) then
                if (buf_length > 0) then
                    deallocate (buf)
                end if
                buf_length = max(length, MIN_BUF)
                allocate (character(len=buf_length) :: buf)
            end if
            call get_command_argument(i, value=buf)
            call parse_line(buf)
        end do
    end subroutine parse_arguments

    subroutine parse_line(line)
        !! Parse a string containing `'NAME = VALUE'`.

        character(len=*), intent(in) :: line
            !! The string to be parsed.

        integer :: i
        character(len=:), allocatable :: key, val

        i = index(line, '=')

        if (i == 0) then
            write (error_unit, '(a)') '[ERROR] paramcard: not a parameter assignment: '// &
                trim(adjustl(line))
            error stop
        end if

        key = to_upper(remove_spaces(line(1:i - 1)))
        val = trim(adjustl(line(i + 1:)))

        if (len(key) == 0) then
            write (error_unit, '(a)') '[ERROR] paramcard: empty parameter name: '// &
                trim(adjustl(line))
            error stop
        end if

        i = find_param(key)
        if (i == 0) then
            call add_param(key, val)
        else
            params(i)%value = val
        end if
    end subroutine parse_line

    function find_param(name) result(res)
        !! Find a parameter and return its index, or `-1` if not found.

        character(len=*), intent(in) :: name
            !! The parameter name.
        integer :: res
            !! The index of the found parameter.

        integer :: i

        do i = 1, n_params
            if (name == params(i)%name) then
                res = i
                return
            end if
        end do

        res = 0
    end function find_param

    subroutine add_param(name, value)
        !! Add a parameter with the given name and value.

        character(len=*), intent(in) :: name
            !! The parameter name.
        character(len=*), intent(in) :: value
            !! The parameter value.

        type(param_type), allocatable :: tmp_params(:)

        if (n_params + 1 > params_capacity) then
            ! Extend the dynamic array.
            params_capacity = params_capacity * 2
            allocate (tmp_params(params_capacity))
            tmp_params(1:size(params)) = params
            deallocate (params)
            call move_alloc(tmp_params, params)
        end if

        n_params = n_params + 1
        params(n_params)%name = name
        params(n_params)%value = value
    end subroutine add_param

    subroutine add_log(name, value, default_value)
        !! Add a log item.

        character(len=*), intent(in) :: name
            !! The parameter name.
        character(len=*), intent(in) :: value
            !! The parameter value.
        character(len=*), optional, intent(in) :: default_value
            !! The parameter default value.

        type(param_log_type), allocatable :: tmp_logs(:)

        if (n_logs + 1 > logs_capacity) then
            ! Extend the dynamic array.
            logs_capacity = logs_capacity * 2
            allocate (tmp_logs(logs_capacity))
            tmp_logs(1:size(logs)) = logs
            deallocate (logs)
            call move_alloc(tmp_logs, logs)
        end if

        n_logs = n_logs + 1
        logs(n_logs)%name = name
        logs(n_logs)%value = value
        if (present(default_value)) then
            logs(n_logs)%default_value = default_value
        end if
    end subroutine add_log

end module paramcard

! Old-fashioned FORTRAN interface,
! though subroutine names are longer than 6 characters.

subroutine paramcard_get_s(name, variable, default_value)
    !! Retrieve a string parameter.
    use paramcard, only: get_param
    implicit none

    character(len=*), intent(in) :: name
        !! The name of the parameter.
    character(len=*), intent(out) :: variable
        !! The variable to store the parameter value.
    character(len=*), intent(in) :: default_value
        !! The value to be used if the parameter not found.

    call get_param(len(variable), name, variable, default_value)

end subroutine paramcard_get_s

subroutine paramcard_get_i(name, variable, default_value)
    !! Retrieve an integer parameter.
    use paramcard, only: get_param
    implicit none

    character(len=*), intent(in) :: name
        !! The name of the parameter.
    integer, intent(out) :: variable
        !! The variable to store the parameter value.
    integer, intent(in) :: default_value
        !! The value to be used if the parameter not found.

    call get_param(name, variable, default_value)
end subroutine paramcard_get_i

subroutine paramcard_get_r(name, variable, default_value)
    !! Retrieve a real parameter.
    use paramcard, only: get_param
    implicit none

    character(len=*), intent(in) :: name
        !! The name of the parameter.
    real, intent(out) :: variable
        !! The variable to store the parameter value.
    real, intent(in) :: default_value
        !! The value to be used if the parameter not found.

    call get_param(name, variable, default_value)
end subroutine paramcard_get_r

subroutine paramcard_get_d(name, variable, default_value)
    !! Retrieve a (double precision) real parameter.
    use paramcard, only: get_param
    implicit none

    character(len=*), intent(in) :: name
        !! The name of the parameter.
    double precision, intent(out) :: variable
        !! The variable to store the parameter value.
    double precision, intent(in) :: default_value
        !! The value to be used if the parameter not found.

    call get_param(name, variable, default_value)
end subroutine paramcard_get_d

subroutine paramcard_summary
    !! Write the parameter summary.
    use paramcard, only: write_param_summary
    implicit none

    call write_param_summary
end subroutine paramcard_summary
