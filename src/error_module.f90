!*******************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  A simple linked list for storing error messages.
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
        type(error),pointer :: next => null()  !! next error message in the list
    end type error

    type,public :: list_of_errors
        !! A list of errors.
        private
        integer :: n_errors = 0 !! number of errors in the list
        type(error),pointer :: head => null()  !! first error in the list
        type(error),pointer :: tail => null()  !! last error in the list
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

    subroutine list_finalizer(me)

    implicit none

    type(list_of_errors),intent(inout) :: me

    call me%destroy()

    end subroutine list_finalizer
!*******************************************************************************

!*******************************************************************************
!>
!  To manually destroy the list.
!
!  This list must be destroyed when finished in order to present a memory leak.
!
!  Also note that there is a finalizer in the [[list_of_errors]],
!  so if the caller doesn't call this routine, it will be destroyed
!  when it goes out of scope, assuming the compiler is standard-conforming.


    subroutine destroy_list(me)

    implicit none

    class(list_of_errors),intent(inout) :: me

    type(error),pointer :: p  !! temp pointer
    type(error),pointer :: q  !! temp pointer

    p => me%head
    do
        if (.not. associated(p)) exit
        q => p%next
        deallocate(p%content)
        deallocate(p)
        nullify(p)
        p => q
    end do

    nullify(me%head)
    nullify(me%tail)
    me%n_errors = 0

    end subroutine destroy_list
!*******************************************************************************

!*******************************************************************************
!>
!  Add an error message to the list.

    subroutine add_error_to_list(me,string)

    implicit none

    class(list_of_errors),intent(inout) :: me
    character(len=*),intent(in) :: string  !! the error message to add.

    if (.not. associated(me%head)) then
        !first error in the list
        me%n_errors = 1
        allocate(me%head)
        me%head%content = string
        me%tail => me%head
    else
        me%n_errors = me%n_errors + 1
        allocate(me%tail%next)
        me%tail%next%content = string
        me%tail => me%tail%next
    end if

    end subroutine add_error_to_list
!*******************************************************************************

!*******************************************************************************
!>
!  Returns true if the list contains any error messages.

    function list_has_errors(me)

    implicit none

    class(list_of_errors),intent(in) :: me
    logical :: list_has_errors

    list_has_errors = associated(me%head)

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

    type(error),pointer :: p  !! temp pointer
    integer :: i !! counter

    p => me%head
    do
        if (.not. associated(p)) exit
        write(iunit,fmt='(A)') p%content
        p => p%next
    end do

    end subroutine print_errors
!*******************************************************************************

!*******************************************************************************
    end module error_module
!*******************************************************************************
