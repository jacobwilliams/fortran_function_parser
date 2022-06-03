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
    call fptest7()
    call error_tests()
    call fptest8()

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
    integer  :: n
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

    subroutine fptest5()

    implicit none

    character(len=*),parameter :: func = '1.0e0 + 5.e1'
    character(len=*),dimension(1),parameter :: var  = 'a' !! not really used here
    real(wp),dimension(1),parameter :: val  = [0.0_wp]    !! not really used here

    type(fparser) :: parser
    real(wp) :: res

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
!>
!
!### Notes
!  * This one has cases where a variable has the same name as a built-in function.
!    So, for some expressions we want the function, and others we want the variable.

    subroutine fptest6()

    implicit none

    integer, parameter :: nfunc = 26
    character (len=*), dimension(nfunc), parameter :: func = [  '-1.0*x                               ',  &
                                                                '-sqrt(x)                             ',  &
                                                                'a*COS(b*x)+5                         ',  &
                                                                'a*COS(b*x)+5.0                       ', &
                                                                'exp(x)-abs(x)+log(1.0)+log10(1.0)    ', &
                                                                'sinh(x)                              ', &
                                                                'cosh(x)                              ', &
                                                                'tanh(x)                              ', &
                                                                'tan(x)                               ', &
                                                                'asin(y)                              ', &
                                                                'acos(y)                              ', &
                                                                'atan(y)                              ', &
                                                                '-x**2                                ', &
                                                                '-x^2                                 ', &
                                                                'sin(x)                               ', &
                                                                'sin*2                                ', &
                                                                '2*(sin)*sin+1-sin(x)                 ', &
                                                                'ceiling(1.1) + ceiling(1.1)          ', &
                                                                'floor(1.1) + floor(1.1)              ', &
                                                                'gamma(2.0)                           ', &
                                                                'hypot(1.0, 2.0)                      ', &
                                                                'max(1.0, 2.0)                        ', &
                                                                'min(1.0, 2.0)                        ', &
                                                                'modulo(180.0, -23.0)+mod(180.0, 23.0)', &
                                                                'mod(180.0, 23.0)+modulo(180.0, 23.0) ', &
                                                                'sign(1.0, -2.0)                      ' ]

    integer, parameter :: nvar = 5
    character (len=*), dimension(nvar),  parameter :: var  = [  'x  ', &
                                                                'a  ', &
                                                                'b  ', &
                                                                'y  ', &
                                                                'sin'  ]  !! sin is a built-in function
    real(wp), dimension(nvar),  parameter :: val  = [  2.0_wp, 3.0_wp, 4.0_wp, 0.1_wp, 1.0_wp ]

    type(fparser_array) :: parser
    real(wp),dimension(nfunc) :: res
    real(wp) :: x,a,b,y,s

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
        s  = val(5)
        call compare(func(1),  -1.0_wp*x,                          res(1))
        call compare(func(2),  -sqrt(x),                           res(2))
        call compare(func(3),  a*cos(b*x)+5,                       res(3))
        call compare(func(4),  a*cos(b*x)+5.0,                     res(4))
        call compare(func(5),  exp(x)-abs(x)+log(1.0)+log10(1.0),  res(5))
        call compare(func(6),  sinh(x),                            res(6))
        call compare(func(7),  cosh(x),                            res(7))
        call compare(func(8),  tanh(x),                            res(8))
        call compare(func(9),  tan(x),                             res(9))
        call compare(func(10), asin(y),                            res(10))
        call compare(func(11), acos(y),                            res(11))
        call compare(func(12), atan(y),                            res(12))
        call compare(func(13), -x**2,                              res(13))
        call compare(func(14), -x**2,                              res(14))
        call compare(func(15), sin(x),                             res(15))
        call compare(func(16), 2.0_wp,                             res(16))
        call compare(func(17), 2*(s)*s+1-sin(x),                   res(17))
        call compare(func(18), real(ceiling(1.1_wp) + ceiling(1.1_wp), wp) , res(18))
        call compare(func(19), real(floor(1.1_wp) + floor(1.1_wp), wp)     , res(19))
        call compare(func(20), gamma(2.0_wp)                     , res(20))
        call compare(func(21), hypot(1.0_wp, 2.0_wp)             , res(21))
        call compare(func(22), max(1.0_wp, 2.0_wp)               , res(22))
        call compare(func(23), min(1.0_wp, 2.0_wp)               , res(23))
        call compare(func(24), modulo(180.0_wp, -23.0_wp)+mod(180.0_wp, 23.0_wp)            , res(24))
        call compare(func(25), mod(180.0_wp, 23.0_wp)+modulo(180.0_wp, 23.0_wp)         , res(25))
        call compare(func(26), sign(1.0_wp, -2.0_wp)             , res(26))

    end if

    end subroutine fptest6
!*******************************************************************************

!*******************************************************************************
!>
!  Functions of multiple arguments.

    subroutine fptest7()

    implicit none

    integer, parameter :: nfunc = 18
    character (len=*), dimension(nfunc), parameter :: func = [  ' atan(2.0)                                             ',  &
                                                                ' atan(2.0, 1.7)                                        ',  &
                                                                ' atan(x)                                               ',  &
                                                                ' atan(x, x)                                            ',  &
                                                                ' atan(x, 3.5)                                          ',  &
                                                                ' atan(x, cos(4.5 + atan(3.0 + atan(7.0, x))))*atan(4.6)',  &
                                                                ' atan(y, x)                                            ',  &
                                                                ' atan(y, x*z)                                          ',  &
                                                                ' atan(abs(y)*atan(x + y, z))                           ',  &
                                                                '-atan(y - z, x + 3.0)                                  ',  &
                                                                '-atan(x*y*z)*atan(y, x*y*z)                            ',  &
                                                                '-atan(y)                                               ',  &
                                                                ' atan2(4.5, y)                                         ',  &
                                                                ' -atan2(x, z)                                          ',  &
                                                                ' 5+cos(pi())                                           ',  &
                                                                '-pi()                                                  ',  &
                                                                ' if(x,y,z) - x - y - z + 1                             ',  &
                                                                '-if(pi()*0, y, z)                                      '   ]
    integer, parameter :: nvar = 3
    character (len=*), dimension(nvar),  parameter :: var  = [  'x', &
                                                                'y', &
                                                                'z'  ]

    real(wp), dimension(nvar),  parameter :: val  = [  2.0_wp, -3.0_wp, 4.7_wp ]

    type(fparser_array) :: parser
    real(wp),dimension(nfunc) :: res
    real(wp) :: x,y,z

    write(*,*) ''
    write(*,*) ' Test 7'
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
    end if

    x  = val(1)
    y  = val(2)
    z  = val(3)

    ! Use atan2(x,y) instead of atan(x,y); some Intel ifort compiler versions
    ! that technically support atan(x,y) have bugs that can occasionally cause
    ! internal compiler error segfaults.
    call compare(func(1),   atan(2.0_wp),                                                         res(1) )
    call compare(func(2),   atan2(2.0_wp, 1.7_wp),                                                res(2) )
    call compare(func(3),   atan(x),                                                              res(3) )
    call compare(func(4),   atan2(x, x),                                                          res(4) )
    call compare(func(5),   atan2(x, 3.5_wp),                                                     res(5) )
    call compare(func(6),   atan2(x, cos(4.5_wp + atan(3.0_wp + atan2(7.0_wp, x))))*atan(4.6_wp), res(6) )
    call compare(func(7),   atan2(y, x),                                                          res(7) )
    call compare(func(8),   atan2(y, x*z) ,                                                       res(8) )
    call compare(func(9),   atan(abs(y)*atan2(x + y, z)),                                         res(9) )
    call compare(func(10), -atan2(y - z, x + 3.0_wp),                                             res(10))
    call compare(func(11), -atan(x*y*z)*atan2(y, x*y*z),                                          res(11))
    call compare(func(12), -atan(y),                                                              res(12))
    call compare(func(13),  atan2(4.5_wp, y) ,                                                    res(13))
    call compare(func(14), -atan2(x,z),                                                           res(14))
    call compare(func(15),  5.0_wp + cos(acos(-1.0_wp)),                                          res(15))
    call compare(func(16), -acos(-1.0_wp),                                                        res(16))
    call compare(func(17),  y - x - y - z + 1,                                                    res(17))
    call compare(func(18),  -z,                                                                   res(18))

    end subroutine fptest7
!*******************************************************************************


!*******************************************************************************
!>
!  Testing order of operations

    subroutine fptest8()

    implicit none

    integer, parameter :: nfunc = 6
    character (len=*), dimension(nfunc), parameter :: func = [  'x+y*x/x            ',  &
                                                                'x+y/x*x            ',  &
                                                                '(x*y/x)**2+1       ',  &
                                                                '(x*y/x)**2*2**2+55 ',  &
                                                                '1/2+4*1+4*6/2/3*5/6',  &
                                                                '1/2+4*1-4*6*2*3/5*6'  ]
    integer, parameter :: nvar = 3
    character (len=*), dimension(nvar),  parameter :: var  = [  'x  ', &
                                                                'y  ', &
                                                                'z  '  ]

    real(wp), dimension(nvar),  parameter :: val  = [  2.0_wp, -3.0_wp, 4.7_wp ]

    type(fparser_array) :: parser
    real(wp),dimension(nfunc) :: res
    real(wp) :: x,y,z

    write(*,*) ''
    write(*,*) ' Test 8'
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
    end if

    x  = val(1)
    y  = val(2)
    z  = val(3)

    call compare(func(1), x+y*x/x ,                        res(1) )
    call compare(func(2), x+y/x*x ,                        res(2) )
    call compare(func(3), (x*y/x)**2+1.0_wp ,              res(3) )
    call compare(func(4), (x*y/x)**2*2.0_wp**2+55.0_wp ,   res(4) )
    call compare(func(5), 1.0_wp/2.0_wp+4.0_wp*1.0_wp+4*6.0_wp/2.0_wp/3.0_wp*5.0_wp/6.0_wp ,      res(5) )
    call compare(func(6), 1.0_wp/2.0_wp+4.0_wp*1.0_wp-4.0_wp*6.0_wp*2.0_wp*3.0_wp/5.0_wp*6.0_wp , res(6) )

    end subroutine fptest8
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
    call parse_error(parser,'-(1) + (+x) + ()',var,val)
    call parse_error(parser,'x +',var,val)
    call parse_error(parser,'cos()',var,val)
    call parse_error(parser,'log(a, b)',var,val)
    call parse_error(parser,'atan2(x)',var,val)
    call parse_error(parser,'atan(, 3)',var,val)
    call parse_error(parser,'abs(x, a, b, 3)',var,val)
    call parse_error(parser,'(((x))',var,val)
    call parse_error(parser,')*sin(b)',var,val)
    call parse_error(parser,'sin',var,val)

    call eval_error(parser,'sqrt(-x)',var,val)
    call eval_error(parser,'acos(10.0)',var,val)
    call eval_error(parser,'asin(10.0)',var,val)
    call eval_error(parser,'log(-x)',var,val)
    call eval_error(parser,'log10(-x)',var,val)
    call eval_error(parser,'1/0',var,val)

    end subroutine error_tests
!*******************************************************************************

    subroutine parse_error(parser,str,var,val)
        type(fparser_array),intent(inout) :: parser
        character(len=*),intent(in) :: str !! expression with a parsing error
        character(len=*),dimension(:),intent(in) :: var
        real(wp),dimension(:),intent(in) :: val
        call parser%parse([str], var)  ! parse and bytecompile function string
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
        call parser%parse([str], var, .True.)  ! parse and bytecompile function string [case sensitive]
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
!  Compare the results from the parser with the actual expression

    subroutine compare(expression, truth, parser)

    implicit none

    character(len=*),intent(in) :: expression
    real(wp),intent(in) :: truth
    real(wp),intent(in) :: parser

    character(len=:), allocatable :: disp_expr

    disp_expr = trim(expression)
    if (len(disp_expr) > 30) disp_expr = disp_expr(1:26) // ' ...'

   ! if (truth == parser) then
    if (abs(truth-parser) <= 1000*epsilon(1.0_wp)) then
        write(*,'(1p,A30,A10,*(G0,1X))') disp_expr, ' PASSED: ', truth
    else
        write(*,'(1p,A30,A10,*(G0,1X))') disp_expr, ' FAILED: ', truth , parser, parser-truth
        error stop 'error evaluating expression'
    end if

    end subroutine compare
!*******************************************************************************

!*******************************************************************************
    end program tests
!*******************************************************************************
