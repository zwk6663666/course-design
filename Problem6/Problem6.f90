    program Problem6

    implicit none
    
    real a, b, value
    integer method, n
    do
        print*, '----------------------------------------'
        print*, "��ѡ����ⷽ��method:"
        print*, 'method=1 ���η�         method=2  ���η�1'
        print*, 'method=3 ���η�2        method=4 ��������'
        print*, 'method=����  �˳�'
        read*, method
        !���η�
        if(method == 1) then
            print*, '����A��B��N��ֵ��'
            read*, a, b, n
            call rectangle(a, b, n, value)
            print 10, a, b, n
            print 20, value

        !���η�1
        else if(method == 2) then
            print*, '����A��B��N��ֵ��'
            read*, a, b, n
            call trapezoid1(a, b, n, value)
            print 10, a, b, n
            print 20, value
        !���η�2
        else if(method == 3) then
            print*, '����A��B��N��ֵ��'
            read*, a, b, n
            call trapezoid2(a, b, n, value)
            print 10, a, b, n
            print 20, value
        !��������
        else if(method == 4) then
            print*, '����A��B��N��ֵ��'
            read*, a, b, n
            call simpson(a, b, n, value)
            print 10, a, b, n
            print 20, value
        !�������˳�
        else
            exit
        endif
    enddo

10  format('A=', F5.2, 3x, 'B=', F5.2, 3x, 'N=', I4)    
20  format('value=', F15.8)
    
    end program Problem6
    
    !���̺��� f(x)
    real function func(x)
        real x
        func = 1 + exp(x)
    end function func
     
    !���η�  
    subroutine rectangle(a, b, n, value)
        real a, b, h, value, x
        integer i, n
        x = a
        h = (b - a)/n
        value = 0.0
        do i = 1, n
            value = value + func(x)*h
            x = x+h
        enddo
    end subroutine rectangle

    !���η�1
    subroutine trapezoid1(a, b, n, value)
        real a, b, h, value, x
        integer i, n
        x = a
        h = (b - a)/n
        value = 0.0
        do i = 1, n
            value = value + (func(x + (i - 1)*h) + func(x + i*h))*h/2.0
        enddo
    end subroutine trapezoid1
    
    !���η�2
    subroutine trapezoid2(a, b, n, value)
        real a, b, h, value, x
        integer i, n
        x = a
        h = (b - a)/n
        value = 0.0
        do i = 1, n-1
            value = value + 2*func(x + i*h)
        enddo
        value = (value + func(a) + func(b))*h/2.0
    end subroutine trapezoid2
    
    !��������
    subroutine simpson(a, b, n, value)
        real a, b, h, value, x, f2, f4
        integer i, n
        h = (b - a)/(2.0 * n)
        x = a + h
        f2 = 0
        f4 = func(x)
        value = 0.0
        do i = 1, n-1
            x = x + h
            f2 = f2 + func(x)
            x = x + h
            f4 = f4 + func(x)
        enddo
        value = (func(a) + func(b) + 4.0*f4 + 2.0*f2)*h/3.0
    end subroutine simpson