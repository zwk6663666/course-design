    program Problem1

    implicit none
    
    real x1, x2, x, func, x0
    integer method
    do
        print*, "��ѡ����ⷽ��method:"
        print*, 'method=1 ������        method=2  ţ�ٵ�����'
        print*, 'method=3 ���ַ�        method=4  �ҽط�'
        print*, 'method=���� �˳�'
        read*, method
        !������
        if(method == 1) then
            print*, '�����ֵx0'
            read*, x0
            call iterative(x0)  
        !ţ�ٵ�����
        else if(method == 2) then
            print*, '�����ֵx0'
            read*, x0
            call newton(x0) 
        !���ַ�
        else if(method == 3) then
             do 
                print*, "����x1,x2��ֵ"
                read*, x1, x2
                if(func(x1)*func(x2) < 0.0) exit
                print*, "�������޸�������������"
            enddo
            call dichotomy(x, x1, x2)
            print 40, 'x=', x
        !�ҽط�
        else if(method == 4) then
            do 
                print*, "����x1,x2��ֵ"
                read*, x1, x2
                if(func(x1)*func(x2) < 0.0) exit
                print*, "�������޸�������������"
            enddo
            call chord(x, x1, x2)
            print 40, 'x=', x
        !�������˳�
        else
            exit
        endif
    enddo
40  format(A, F15.7)
    end program Problem1
    
    !���̺��� f(x)
    real function func(x)
        real x
        func = x**3 - 2*x**2 + 7*x+4
    end function func
    
    !g(x)
    real function g(x)
        real x
        g = (-x**3 + 2*x**2 -4)/7
    end function g
    
    !����
    real function dfunc(x)
        real x
        dfunc = 3*x**2 - 4*x+7
    end function dfunc
    
    
    !���ַ�  
    subroutine dichotomy(x, X1, x2)
        real x1, x2, x, f1, f2, fx
        x = (x1 + x2)/2.0
        fx = func(x)
        do while(abs(fx) > 1E-6)
            f1 = func(x1)
            f2 = func(x2)
            if(f1*fx < 0) then
                x2 = x
            else
                x1 = x
            endif
            x = (x1+x2)/2.0
            fx = func(x)
        enddo
    end subroutine dichotomy

    !�ҽط�
    subroutine chord(x, x1, x2)
        real x1, x2, x, f1, f2, fx
        x = x2 - (x2 - x1)/(func(x2) - func(x1))*func(x2)
        fx = func(x)
        do while (abs(fx) > 1E-6)
            f1 = func(x1)
            f2 = func(x2)
            if(f1*f2 < 0) then
                x2 = x
            else
                x1 = x
            endif
            x = x2 - (x2 - x1)/(func(x2) - func(x1))*func(x2)
            fx = func(x)
        enddo
    end subroutine chord
    !������
    subroutine iterative(x)
        real x, x1
        integer i
        integer:: MAX = 200
        i = 1
        x1 = g(x)
        do while (abs(x - x1) > 1E-6.AND.i <= MAX)
            print 10, i, x1
            x = x1
            i = i + 1
            x1 = g(x)
        enddo
        if(i <= MAX) then
            print 20, 'x=', x1
        else
            print 30, '����',MAX ,'�ε�������δ����'
        endif
        
10      format('I=', I4, 6X, 'x=', F15.7)
20      format(A, F15.7)
30      format(A, I4, A) 
    end subroutine iterative
    !ţ�ٵ�����
    subroutine newton(x)
        real x, x1
        integer i
        integer:: MAX = 200
        i = 1
        x1 = x - func(x)/dfunc(x)
        do while(abs(x - x1) > 1E-6.AND. i <= MAX)
            print 10, i , x1
            x = x1
            i = i + 1
            x1 = x - func(x)/dfunc(x)
        enddo
        if(i <= MAX) then
            print 20, 'x=', x1
        else
            print 30, '����',MAX ,'�ε�������δ����'
        endif
10      format('I=', I4, 6X, 'x=', F15.7)
20      format(A, F15.7)
30      format(A, I4, A)    
    end subroutine newton