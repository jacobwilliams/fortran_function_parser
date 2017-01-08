!*******************************************************************************
!> license: BSD
!
!  Test cases for the function parser module

    program tests

    use function_parser
    use iso_fortran_env, only: wp => real64, output_unit

    implicit none

    call fptest()
    call fptest2()
    call fptest3()
    call fptest4()
    call fptest5()
    call fptest6()

    contains
!*******************************************************************************

!*******************************************************************************
    subroutine fptest()

    character(len=*),parameter :: func = '-x'
    integer,parameter :: nvar = 1  !! number of variables
    character(len=*),dimension(nvar),parameter :: var  = [ 'x' ]
    real(wp),dimension(nvar),parameter :: val  = [  2.0_wp  ]

    type(fparser) :: parser
    type(list_of_errors) :: error_msg
    real(wp) :: res
    real(wp) :: x

    write(*,*) ''
    write(*,*) ' Test 1'
    write(*,*) ''

    call parser%parse(func, var, .false., error_msg)  ! parse and bytecompile function string
    if (error_msg%has_errors()) then
        call error_msg%print(output_unit)
    else

        write(*,*)'==> bytecode evaluation:'
        call parser%evaluate(val,res,error_msg)  ! interprete bytecode representation of function
        if (error_msg%has_errors()) then
            call error_msg%print(output_unit)
        else
            write(*,*) func,'=',res
            write(*,*)'==> direct evaluation:'
            x  = val(1)
            write(*,*)'-x=',-x
        end if
    end if

    end subroutine fptest
!*******************************************************************************

!*******************************************************************************
    subroutine fptest2()

    implicit none

    integer, parameter :: nfunc = 3
    character (len=*), dimension(nfunc), parameter :: func = [  'a0*b0 ', &
                                                                'a1/b1 ', &
                                                                'a3**b3' ]
    integer, parameter :: nvar = 6
    character (len=*), dimension(nvar),  parameter :: var  = [ 'a0', &
                                                               'b0', &
                                                               'a1', &
                                                               'b1', &
                                                               'a3', &
                                                               'b3' ]
    real(wp),dimension(nvar),parameter :: val  = [ 1.0_wp, 2.0_wp, 3.0_wp, &
                                                   1.0_wp, 5.0_wp, 6.0_wp ]

    type(fparser_array) :: parser
    type(list_of_errors) :: error_msg
    real(wp),dimension(nfunc) :: res
    integer :: i  !! counter
    real(wp) :: a0,b0,a1,b1,a3,b3

    write(*,*) ''
    write(*,*) ' Test 2'
    write(*,*) ''

    call parser%parse(func, var, .false., error_msg)  ! parse and bytecompile function string
    if (error_msg%has_errors()) then
        call error_msg%print(output_unit)
    else

        write(*,*)'==> bytecode evaluation:'
        call parser%evaluate(val,res,error_msg)  ! interprete bytecode representation of function
        if (error_msg%has_errors()) then
            call error_msg%print(output_unit)
        else
            do i=1,nfunc
                write(*,*) func(i),'=',res(i)
            end do
            write(*,*)'==> direct evaluation:'
            a0 = val(1)
            b0 = val(2)
            a1 = val(3)
            b1 = val(4)
            a3 = val(5)
            b3 = val(6)
            write(*,*)'res=',a0*b0
            write(*,*)'res=',a1/b1
            write(*,*)'res=',a3**b3
        end if
    end if

    end subroutine fptest2
!*******************************************************************************

!*******************************************************************************
    subroutine fptest3()

    implicit none

    integer,                             parameter :: nfunc = 3
    character (len=*), dimension(nfunc), parameter :: func = [  'vel*cos(beta)           ', &
                                                                'vel*sin(beta)*cos(alpha)', &
                                                                'vel*sin(beta)*sin(alpha)' ]
    integer,                             parameter :: nvar = 3
    character (len=*), dimension(nvar),  parameter :: var  = [  'vel  ', &
                                                                'alpha', &
                                                                'beta ' ]
    real(wp),          dimension(nvar),  parameter :: val  = [  10., 1.5, 2.0  ]

    type(fparser_array) :: parser
    type(list_of_errors) :: error_msg
    real(wp),dimension(nfunc) :: res
    integer :: i  !! counter
    real(wp) :: vel,alpha,beta

    write(*,*) ''
    write(*,*) ' Test 3'
    write(*,*) ''

    call parser%parse(func, var, .false., error_msg)  ! parse and bytecompile function string
    if (error_msg%has_errors()) then
        call error_msg%print(output_unit)
    else

        write(*,*)'==> bytecode evaluation:'
        call parser%evaluate(val,res,error_msg)  ! interprete bytecode representation of function
        if (error_msg%has_errors()) then
            call error_msg%print(output_unit)
        else
            do i=1,nfunc
                write(*,*) func(i),'=',res(i)
            end do
            write(*,*)'==> direct evaluation:'
            vel   = val(1)
            alpha = val(2)
            beta  = val(3)
            write(*,*)'res=',vel*cos(beta)
            write(*,*)'res=',vel*sin(beta)*cos(alpha)
            write(*,*)'res=',vel*sin(beta)*sin(alpha)
        end if
    end if

    end subroutine fptest3
!*******************************************************************************

!*******************************************************************************
!>
!  Assesses how fast the interpreter is compared against a direct evaluation.

    subroutine fptest4()

    implicit none

    integer, parameter :: neval = 1000000
    integer, parameter :: nfunc = 3
    character (len=*), dimension(nfunc), parameter :: func = [ 'vel*cos(beta)           ', &
                                                               'vel*sin(beta)*cos(alpha)', &
                                                               'vel*sin(beta)*sin(alpha)' ]
    integer, parameter :: nvar = 3
    character (len=*), dimension(nvar),  parameter :: var  = [ 'vel  ', &
                                                               'alpha', &
                                                               'beta ' ]
    real(wp), dimension(nvar),  parameter :: val  = [  10., 1.5, 2.0  ]

    type(fparser_array) :: parser
    type(list_of_errors) :: error_msg
    real(wp),dimension(nfunc) :: res
    integer  :: i,n
    real     :: rt1,rt2,rt3
    real(wp) :: vel,alpha,beta

    write(*,*) ''
    write(*,*) ' Test 4'
    write(*,*) ''

    call parser%parse(func, var, .false., error_msg)  ! parse and bytecompile function string
    if (error_msg%has_errors()) then
        call error_msg%print(output_unit)
    else

        vel   = val(1)
        alpha = val(2)
        beta  = val(3)
        call cpu_time (rt1)
        do n=1,neval
            call parser%evaluate(val,res,error_msg)  ! interprete bytecode representation of function
            if (error_msg%has_errors()) then
                call error_msg%print(output_unit)
                return
            end if
        end do
        write(*,*)'==> bytecode evaluation:'
        write(*,*) 'res=',res
        call cpu_time (rt2)
        do n=1,neval
            res(1) = vel*cos(beta)
            res(2) = vel*sin(beta)*cos(alpha)
            res(3) = vel*sin(beta)*sin(alpha)
        end do
        write(*,*)'==> direct evaluation:'
        write(*,*) 'res=',res
        call cpu_time (rt3)
        write(*,*)'function evaluation:'
        write(*,*)'- bytecode interpreter cpu time = ',rt2-rt1
        write(*,*)'- machine code         cpu time = ',rt3-rt2,' = ',(rt3-rt2)/(rt2-rt1)*100.0_wp,'%'

    end if

    end subroutine fptest4
!*******************************************************************************

!*******************************************************************************
!>
!  An example with no variables.
!
!@warning This one fails on gfortran with `-O2` optimization enabled.

    subroutine fptest5()

    implicit none

    character(len=*),parameter :: func = '1.0e0 + 5.e1'
    character(len=*),dimension(1),parameter :: var  = 'a' !! not really used here
    real(wp),dimension(1),parameter :: val  = [0.0_wp]    !! not really used here

    type(fparser) :: parser
    type(list_of_errors) :: error_msg
    real(wp) :: res
    real(wp) :: x

    write(*,*) ''
    write(*,*) ' Test 5'
    write(*,*) ''

    call parser%parse(func, var, .false., error_msg)  ! parse and bytecompile function string
    if (error_msg%has_errors()) then
        call error_msg%print(output_unit)
    else
        write(*,*)'==> bytecode evaluation:'
        call parser%evaluate(val,res,error_msg)  ! interprete bytecode representation of function
        if (error_msg%has_errors()) then
            call error_msg%print(output_unit)
        else
            write(*,*) func,'=',res
        end if
    end if

    end subroutine fptest5
!*******************************************************************************

!*******************************************************************************
    subroutine fptest6()

    implicit none

    integer, parameter :: nfunc = 4
    character (len=*), dimension(nfunc), parameter :: func = [  '-1.0*x        ',  &
                                                                '-x            ',  &
                                                                'a*COS(b*x)+5  ',  &
                                                                'a*COS(b*x)+5.0' ]
    integer, parameter :: nvar = 3
    character (len=*), dimension(nvar),  parameter :: var  = [  'x', &
                                                                'a', &
                                                                'b'  ]
    real(wp), dimension(nvar),  parameter :: val  = [  2.0_wp, 3.0_wp, 4.0_wp ]

    type(fparser_array) :: parser
    type(list_of_errors) :: error_msg
    real(wp),dimension(nfunc) :: res
    integer :: i  !! counter
    real(wp) :: x,a,b

    write(*,*) ''
    write(*,*) ' Test 6'
    write(*,*) ''

    call parser%parse(func, var, .false., error_msg)  ! parse and bytecompile function string
    if (error_msg%has_errors()) then
        call error_msg%print(output_unit)
    else

        write(*,*)'==> bytecode evaluation:'
        call parser%evaluate(val,res,error_msg)  ! interprete bytecode representation of function
        if (error_msg%has_errors()) then
            call error_msg%print(output_unit)
        else
            do i=1,nfunc
                write(*,*) func(i),'=',res(i)
            end do
            write(*,*)'==> direct evaluation:'
            x  = val(1)
            a  = val(2)
            b  = val(3)
            write(*,*)'-1.0*x        =',-1.0_wp*x
            write(*,*)'-x            =',-x
            write(*,*)'a*cos(b*x)+5  =',a*cos(b*x)+5
            write(*,*)'a*cos(b*x)+5.0=',a*cos(b*x)+5.0_wp
        end if
    end if

    end subroutine fptest6
!*******************************************************************************

!*******************************************************************************
    end program tests
!*******************************************************************************
