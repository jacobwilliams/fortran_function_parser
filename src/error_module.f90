!*******************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  A simple type for storing error messages.
!  Used by the [[function_parser] module.
!
!@note The error message is stored internally as an
!      allocatable character string. So it can be
!      as large as it needs to be.

    module error_module

    implicit none

    private

    type :: error
        !! A error message in the [[list_of_errors]].
        private
        character(len=:),allocatable :: content  !! the error message string
    end type error

    type,public :: list_of_errors
        !! A list of errors.
        !!
        !! This is implemented as a simple allocatable
        !! array of [[error]] types.
        private
        type(error),dimension(:),allocatable :: head !! the error list
    contains
        private
        procedure,public :: add        => add_error_to_list
        procedure,public :: print      => print_errors
        procedure,public :: has_errors => list_has_errors
        procedure,public :: destroy    => destroy_list
        final :: list_finalizer
    end type list_of_errors

    contains
!*******************************************************************************

!*******************************************************************************
!>
!  Will be called automatically when the list goes out of scope.

    pure elemental subroutine list_finalizer(me)

    implicit none

    type(list_of_errors),intent(inout) :: me

    call me%destroy()

    end subroutine list_finalizer
!*******************************************************************************

!*******************************************************************************
!>
!  To manually destroy the list.
!
!  Also note that there is a finalizer in the [[list_of_errors]],
!  so if the caller doesn't call this routine, it will be destroyed
!  when it goes out of scope, assuming the compiler is standard-conforming.

    pure elemental subroutine destroy_list(me)

    implicit none

    class(list_of_errors),intent(inout) :: me

    integer :: i !! counter

    if (allocated(me%head)) then
        do i = 1, size(me%head)
            if (allocated(me%head(i)%content)) &
                deallocate(me%head(i)%content)
        end do
        deallocate(me%head)
    end if

    end subroutine destroy_list
!*******************************************************************************

!*******************************************************************************
!>
!  Add an error message to the list.

    subroutine add_error_to_list(me,string)

    implicit none

    class(list_of_errors),intent(inout) :: me
    character(len=*),intent(in) :: string  !! the error message to add.

    type(error),dimension(:),allocatable :: tmp !! for expanding the array
    integer :: n !! number of errors currently in the list

    if (.not. allocated(me%head)) then

        !first error in the list
        allocate(me%head(1))
        me%head(1)%content = string

    else

        ! add to the list
        n = size(me%head)
        allocate(tmp(n+1))
        tmp(1:n) = me%head
        tmp(n+1)%content = string
        call move_alloc(tmp,me%head)

    end if

    end subroutine add_error_to_list
!*******************************************************************************

!*******************************************************************************
!>
!  Returns true if the list contains any error messages.

    pure elemental function list_has_errors(me)

    implicit none

    class(list_of_errors),intent(in) :: me
    logical :: list_has_errors

    list_has_errors = allocated(me%head)

    end function list_has_errors
!*******************************************************************************

!*******************************************************************************
!>
!  Print all the error messages in the list.

    subroutine print_errors(me,iunit)

    implicit none

    class(list_of_errors),intent(in) :: me
    integer,intent(in) :: iunit  !! unit number for printing
                                 !! (assumed to be open)

    integer :: i !! counter

    if (allocated(me%head)) then
        do i = 1, size(me%head)
            write(iunit,fmt='(A)') me%head(i)%content
        end do
    end if

    end subroutine print_errors
!*******************************************************************************

!*******************************************************************************
    end module error_module
!*******************************************************************************
