!*******************************************************************************
!> license: BSD
!
!  Test cases for the function parser module.

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
    call error_tests()

    contains
!*******************************************************************************

!*******************************************************************************
    subroutine fptest()

    character(len=*),parameter :: func = '-x'
    integer,parameter :: nvar = 1  !! number of variables
    character(len=*),dimension(nvar),parameter :: var  = [ 'x' ]
    real(wp),dimension(nvar),parameter :: val  = [  2.0_wp  ]

    type(fparser) :: parser
    real(wp) :: res
    real(wp) :: x

    write(*,*) ''
    write(*,*) ' Test 1'
    write(*,*) ''

    call parser%parse(func, var, .false.)  ! parse and bytecompile function string
    if (parser%error()) then
        call parser%print_errors(output_unit)
        error stop
    end if

    call parser%evaluate(val,res)  ! interprete bytecode representation of function
    if (parser%error()) then
        call parser%print_errors(output_unit)
    else
        x  = val(1)
        call compare('-x', -x, res)
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
    real(wp),dimension(nfunc) :: res
    integer :: i  !! counter
    real(wp) :: a0,b0,a1,b1,a3,b3

    write(*,*) ''
    write(*,*) ' Test 2'
    write(*,*) ''

    call parser%parse(func, var, .false.)  ! parse and bytecompile function string
    if (parser%error()) then
        call parser%print_errors(output_unit)
        error stop
    end if

    call parser%evaluate(val,res)  ! interprete bytecode representation of function
    if (parser%error()) then
        call parser%print_errors(output_unit)
    else
        a0 = val(1)
        b0 = val(2)
        a1 = val(3)
        b1 = val(4)
        a3 = val(5)
        b3 = val(6)
        call compare('a0*b0', a0*b0, res(1))
        call compare('a1/b1', a1/b1, res(2))
        call compare('a3**b3', a3**b3, res(3))
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
    real(wp),dimension(nfunc) :: res
    integer :: i  !! counter
    real(wp) :: vel,alpha,beta

    write(*,*) ''
    write(*,*) ' Test 3'
    write(*,*) ''

    call parser%parse(func, var, .false.)  ! parse and bytecompile function string
    if (parser%error()) then
        call parser%print_errors(output_unit)
        error stop
    end if

    call parser%evaluate(val,res)  ! interprete bytecode representation of function
    if (parser%error()) then
        call parser%print_errors(output_unit)
    else
        vel   = val(1)
        alpha = val(2)
        beta  = val(3)
        call compare('vel*cos(beta)',            vel*cos(beta), res(1)) 
        call compare('vel*sin(beta)*cos(alpha)', vel*sin(beta)*cos(alpha), res(2)) 
        call compare('vel*sin(beta)*sin(alpha)', vel*sin(beta)*sin(alpha), res(3)) 
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
    real(wp),dimension(nfunc) :: res
    integer  :: i,n
    real     :: rt1,rt2,rt3
    real(wp) :: vel,alpha,beta

    write(*,*) ''
    write(*,*) ' Test 4'
    write(*,*) ''

    call parser%parse(func, var, .false.)  ! parse and bytecompile function string
    if (parser%error()) then
        call parser%print_errors(output_unit)
        error stop
    end if

    vel   = val(1)
    alpha = val(2)
    beta  = val(3)
    call cpu_time (rt1) ! ----- 
    do n=1,neval
        call parser%evaluate(val,res)  ! interprete bytecode representation of function
    end do
    call cpu_time (rt2) ! ----- 
    if (parser%error()) then
        call parser%print_errors(output_unit)
        error stop
    end if

    call cpu_time (rt2) ! ----- 
    do n=1,neval
        res(1) = vel*cos(beta)
        res(2) = vel*sin(beta)*cos(alpha)
        res(3) = vel*sin(beta)*sin(alpha)
    end do
    call cpu_time (rt3) ! ----- 

    write(*,*)'function evaluation:'
    write(*,*)'  * bytecode interpreter cpu time = ',rt2-rt1
    write(*,*)'  * machine code         cpu time = ',rt3-rt2,' = ',(rt3-rt2)/(rt2-rt1)*100.0_wp,'%'

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
    real(wp) :: res
    real(wp) :: x

    write(*,*) ''
    write(*,*) ' Test 5'
    write(*,*) ''

    call parser%parse(func, var, .false.)  ! parse and bytecompile function string
    if (parser%error()) then
        call parser%print_errors(output_unit)
        error stop
    end if

    call parser%evaluate(val,res)  ! interprete bytecode representation of function
    if (parser%error()) then
        call parser%print_errors(output_unit)
        error stop
    else
        call compare('1.0e0 + 5.e1', real(1.0e0 + 5.e1, wp), res)
    end if

    end subroutine fptest5
!*******************************************************************************

!*******************************************************************************
    subroutine fptest6()

    implicit none

    integer, parameter :: nfunc = 12
    character (len=*), dimension(nfunc), parameter :: func = [  '-1.0*x                           ',  &
                                                                '-sqrt(x)                         ',  &
                                                                'a*COS(b*x)+5                     ',  &
                                                                'a*COS(b*x)+5.0                   ', &
                                                                'exp(x)-abs(x)+log(1.0)+log10(1.0)',&
                                                                'sinh(x)                          ', &
                                                                'cosh(x)                          ', &
                                                                'tanh(x)                          ', &
                                                                'tan(x)                           ', &
                                                                'asin(y)                          ', &
                                                                'acos(y)                          ', &
                                                                'atan(y)                          '  ]
    integer, parameter :: nvar = 4
    character (len=*), dimension(nvar),  parameter :: var  = [  'x', &
                                                                'a', &
                                                                'b', &
                                                                'y'  ]
    real(wp), dimension(nvar),  parameter :: val  = [  2.0_wp, 3.0_wp, 4.0_wp, 0.1_wp ]

    type(fparser_array) :: parser
    real(wp),dimension(nfunc) :: res
    integer :: i  !! counter
    real(wp) :: x,a,b,y

    write(*,*) ''
    write(*,*) ' Test 6'
    write(*,*) ''

    call parser%parse(func, var, .false.)  ! parse and bytecompile function string
    if (parser%error()) then
        call parser%print_errors(output_unit)
        error stop
    end if

    call parser%evaluate(val,res)  ! interprete bytecode representation of function
    if (parser%error()) then
        call parser%print_errors(output_unit)
        error stop
    else
        x  = val(1)
        a  = val(2)
        b  = val(3)
        y  = val(4)
        call compare(func(1),  -1.0_wp*x, res(1))
        call compare(func(2),  -sqrt(x), res(2))
        call compare(func(3),  a*cos(b*x)+5, res(3))
        call compare(func(4),  a*cos(b*x)+5.0, res(4))
        call compare(func(5),  exp(x)-abs(x)+log(1.0)+log10(1.0), res(5))
        call compare(func(6),  sinh(x), res(6))
        call compare(func(7),  cosh(x), res(7))
        call compare(func(8),  tanh(x), res(8))
        call compare(func(9),  tan(x),  res(9))
        call compare(func(10), asin(y), res(10))
        call compare(func(11), acos(y), res(11))
        call compare(func(12), atan(y), res(12))
    end if

    end subroutine fptest6
!*******************************************************************************

!*******************************************************************************
!>
!  Test some of the error cases.

    subroutine error_tests()

    implicit none

    integer, parameter :: nvar = 3
    character (len=*), dimension(nvar),  parameter :: var  = [  'x', &
                                                                'a', &
                                                                'b'  ]
    real(wp), dimension(nvar),  parameter :: val  = [  2.0_wp, 3.0_wp, 4.0_wp ]
    type(fparser_array) :: parser

    write(*,*) ''
    write(*,*) ' Test 7 - Test error conditions'
    write(*,*) ''

    call parse_error(parser,'st(-x)',var,val)
    call parse_error(parser,'x * 452d3234.2323',var,val)
    call parse_error(parser,'x * (123',var,val)
    call parse_error(parser,'x +-* y',var,val)
    call parse_error(parser,'x + sin',var,val)
    call parse_error(parser,'x + ()',var,val)
    call parse_error(parser,'x +',var,val)

    call eval_error(parser,'sqrt(-x)',var,val)
    call eval_error(parser,'acos(10.0)',var,val)
    call eval_error(parser,'asin(10.0)',var,val)
    call eval_error(parser,'log(-x)',var,val)
    call eval_error(parser,'1/0',var,val)

    end subroutine error_tests
!*******************************************************************************

    subroutine parse_error(parser,str,var,val)
        type(fparser_array),intent(inout) :: parser
        character(len=*),intent(in) :: str !! expression with a parsing error
        real(wp),dimension(1) :: res
        character(len=*),dimension(:),intent(in) :: var
        real(wp),dimension(:),intent(in) :: val
        call parser%parse([str], var, .false.)  ! parse and bytecompile function string
        if (parser%error()) then
            call parser%print_errors(output_unit)
            write(*,*) 'PASSED : parsing error'
        else 
            error stop 'FAILED : there should have been a parsing error'
        end if
        call parser%clear_errors()
        call parser%destroy()
    end subroutine parse_error

    subroutine eval_error(parser,str,var,val)
        type(fparser_array),intent(inout) :: parser
        character(len=*),intent(in) :: str !! expression with a parsing error
        real(wp),dimension(1) :: res
        character(len=*),dimension(:),intent(in) :: var
        real(wp),dimension(:),intent(in) :: val
        call parser%parse([str], var, .false.)  ! parse and bytecompile function string
        if (parser%error()) then
            call parser%print_errors(output_unit)
            error stop
        end if
        call parser%evaluate(val,res)  ! interprete bytecode representation of function
        if (parser%error()) then
            call parser%print_errors(output_unit)
            write(*,*) 'PASSED : evaluation errors detected'
        else
            error stop 'FAILED : there should have been evaluation errors'
        end if        
        call parser%clear_errors()
        call parser%destroy()
    end subroutine eval_error

!*******************************************************************************
!>
!  Compare the results from the parser to the actualy expression

    subroutine compare(expression, truth, parser)

    implicit none
    
    character(len=*),intent(in) :: expression 
    real(wp),intent(in) :: truth 
    real(wp),intent(in) :: parser

    if (truth == parser) then
        write(*,'(A30,A10,G0)') trim(expression), ' PASSED: ', truth 
    else 
        write(*,'(A30,A10,*(G0,1X))') trim(expression), ' FAILED: ', truth , parser
        error stop 'error evaluating expression'
    end if

    end subroutine compare
!*******************************************************************************

!*******************************************************************************
    end program tests
!*******************************************************************************
